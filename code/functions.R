
getAllMondays <- function(year) {
  days <- as.POSIXlt(paste(year, 1:366, sep="-"), format="%Y-%j")
  Ms <- days[days$wday==1]
  Ms[!is.na(Ms)]  # Needed to remove NA from day 366 in non-leap years
}

mondays <- getAllMondays(2020) %>% as_tibble() %>%
  rename(monday = value) %>%
  mutate(year = year(monday)) %>%
  mutate(month = month(monday)) %>%
  mutate(week = week(monday))

# compile apc data
clean_apc_trips_input <- function(raw_apc_trips_data) {
  apc_data <- raw_apc_trips_data %>%
    select("trip_id" = "Trip",
           "stop_id" = "Stop",
           "total_ons" = "TotalIns", 
           "total_offs" = "TotalOuts",
           "number_trips" = "NbrTrips")
}

clean_apc_stops_input <- function(raw_apc_stops_data, weekdays = 5) {
  apc_stops <- raw_apc_stops_data %>%
    rename("stop_id" = "Stop",
           "year" = "Year",
           "week" = "Week") %>%
    group_by(week, stop_id) %>%
    summarise(total_ons = sum(Total_Ins),
              total_offs = sum(Total_Outs),
              number_samples = sum(NbrStops),
              number_sampled_trips = sum(NbrTrips)) %>%
    left_join(mondays, by = c("week" = "week")) %>%
    mutate(stop_id = as.numeric(stop_id))
    #group_by(stop_id) %>%
    #summarise(daily_ons = total_ons/weekdays)
  
  return(apc_stops)
}

clean_apc_routes_input <- function(raw_apc_route_data) {
  apc_routes <- raw_apc_route_data %>% 
    rename("stop_id" = "Stop",
           "route_id" = "Route",
           "year" = "Year",
           "month" = "Month") %>%
    group_by(route_id, stop_id, month) %>%
    summarise(total_ons = sum(Total_Ins),
              total_offs = sum(Total_Outs),
              number_samples = sum(NbrStops),
              number_sampled_trips = sum(NbrTrips))
    #left_join(mondays, by = c("week" = "week")) %>%
    #mutate(stop_id = as.numeric(stop_id))
  
  return(apc_routes)
}

# convert monthly APC data to daily
# build list of apc sampled trips

get_apc_trips_summary <- function(clean_apc_input_result) {
  apc_trips <- clean_apc_input_result %>%
    group_by(trip_id) %>%
    summarise(total = n())
  
  return(apc_trips)
}

#### GTFS DATA FUNCTIONS ------------------------------------------- ####
# build list of all trips and stops from gtfs
get_gtfs_trips <- function(gtfs_df) {
  gtfs_trips <- gtfs_df$trips %>% mutate(trip_id = as.numeric(trip_id))
    
}
get_gtfs_stops <- function(gtfs_df) {
  output <- gtfs_df$stops %>% mutate(stop_id = as.numeric(stop_id))
  
  return(output)
}
#test <- get_gtfs_stops(march_2020_gtfs)

get_stop_frequencies <- function(gtfs_df) {
  stops <- get_gtfs_stops(gtfs_df)
  frequencies <- get_stop_frequency(gtfs_df) %>% mutate(stop_id = as.numeric(stop_id))
  
  ouput <- stops %>% left_join(frequencies) %>%
    select(stop_id, stop_name, stop_lat, stop_lon, route_id, direction_id, departures)
  return(ouput)
}
#test2 <- get_stop_frequencies(march_2020_gtfs)

# input: gtfs object
# output: df of each day in gtfs object's calendar w/ number of services that date
get_gtfs_service_calendar <- function(gtfs) {
  x <- gtfs %>% set_date_service_table()
  output <- x$.$date_service_table %>% group_by(date) %>% count()
  return(output)
}
#calendar <- get_gtfs_service_calendar(gtfs_df$gtfs[[4]])

#input: gtfs_service_calendar created with get_gtfs_service_calendar
#output: lubridate::interval object representing the time span that gtfs object is active for
get_gtfs_service_interval <- function(gtfs) {
  calendar <- get_gtfs_service_calendar(gtfs)
  list <- calendar$date
  interval <- interval(start = first(list), end = last(list))
  return(interval)
}
#get_gtfs_service_interval(gtfs_df$gtfs[[4]])

#test3 <- get_gtfs_service_calendar(gtfs_df$gtfs[[4]])

#builds a df with four columns:
# - file: the file name of the gtfs file stored to path
# - file_path: the path of that file
# - gtfs: the gtfs object rendered by that file
# - frequencies: a df of each bus stop served in that gtfs file, with the frequency of service
# - interval: a lubridate::interval object representing the lifespan of the gtfs object
build_gtfs_df <- function(path = "./inputs/gtfs") {
  print("Building GTFS dataframe - apologies for taking too long!")
  
  l1 <- list.files("./inputs/gtfs")
  df <- tibble(file = l1) %>% 
    mutate(file_path = paste("./inputs/gtfs", file, sep = "/")) 
  
  gtfs_df_list <- df %>%
    group_by(file, file_path) %>%
    mutate(gtfs = map(file_path, read_gtfs)) 
  
  output <- gtfs_df_list %>%
    group_by(file) %>%
    mutate(frequencies = map(gtfs, get_stop_frequencies)) %>%
    mutate(interval = map(gtfs, get_gtfs_service_interval))
  return(output)
}

get_daily_stop_departures <- function(frequency_df) {
  output <- frequency_df %>%
    group_by(stop_id) %>%
    summarise(daily_departures = sum(departures))
  return(output)
}

get_daily_stop_route_departures <- function(frequency_df) {
  output <- frequency_df %>%
    group_by(stop_id, route_id) %>%
    summarise(daily_departures = sum(departures))
  return(output)
}

get_daily_route_departures <- function(frequency_df) {
  output <- frequency_df %>%
    group_by(route) %>%
    summarise(daily_departures = sum(departures))
  return(output)
}

get_stop_departure_df <- function(gtfs_df) {
  test <- gtfs_df %>% select(file, frequencies, interval) %>%
    mutate(start = map(interval, int_start)) %>%
    mutate(end = map(interval, int_end)) %>%
    mutate(start_date = as_date(start[[1]])) %>%
    arrange(start_date) %>% 
    select(file, frequencies, start_date) %>%
    mutate(daily_departures = map(frequencies, get_daily_stop_departures)) %>%
    select(file, start_date, daily_departures)
  
  test$end_date <- lead(test$start_date - 1, default = today())
  
  departure_df <- test %>%
    unnest(daily_departures) %>%
    ungroup() %>%
    select(stop_id, start_date, end_date, daily_departures) 
  
  return(departure_df)
  
}

get_stop_route_departure_df <- function(gtfs_df) {
  test <- gtfs_df %>% select(file, frequencies, interval) %>%
    mutate(start = map(interval, int_start)) %>%
    mutate(end = map(interval, int_end)) %>%
    mutate(start_date = as_date(start[[1]])) %>%
    arrange(start_date) %>% 
    select(file, frequencies, start_date) %>%
    mutate(daily_departures = map(frequencies, get_daily_stop_route_departures)) %>%
    select(file, start_date, daily_departures)
  
  test$end_date <- lead(test$start_date - 1, default = today())
  
  departure_df <- test %>%
    unnest(daily_departures) %>%
    ungroup() %>%
    select(stop_id, start_date, end_date, daily_departures) 
  
  return(departure_df)
  
}

build_stop_weekly_df <- function(stop_level_data, departure_df) {
  print("Building bus stop database - again sorry taking awhile.")
  
  stop_dt <- stop_level_data %>%
    lazy_dt() %>%
    select(stop_id, monday, total_ons, total_offs, number_samples, number_sampled_trips) %>%
    group_by(stop_id) %>%
    as.data.frame()
  
  db <- departure_df %>% mutate(interval = interval(start_date, end_date)) %>% as.data.table()
  
  # utilize data.table so it doesn't take days to do
  output <- db[stop_dt, on=.(stop_id = stop_id), 
             allow.cartesian=TRUE,
             nomatch = NA, 
             .(stop_id, x.stop_id, monday, total_ons, total_offs, number_samples, number_sampled_trips,
               x.start_date, x.end_date, daily_departures)] %>%
    mutate(interval = interval(x.start_date, x.end_date)) %>% 
    rowwise() %>%
    mutate(filter_on = monday %within% interval) %>%
    ungroup()  %>%
    filter(filter_on == TRUE) %>%
    select(stop_id, monday, interval, total_ons, total_offs,  number_samples, number_sampled_trips, daily_departures) %>%
    mutate(
      ons_per_trip = total_ons / number_sampled_trips,
      off_per_trip = total_offs / number_sampled_trips,
      sample_rate = number_sampled_trips / (daily_departures * 5),
      daily_ons = ons_per_trip * daily_departures
      ) 
  
  return(output)
}
#test <- build_stop_weekly_df(stop_level_data %>% filter(stop_id == 2), departure_df)

#### COMPARATIVE DATA FRAME ----------------------------------------------------------####
# build before and after dataframe
build_comp_df <- function(tracts_weekly_ridership, cut = c("2020-03-16", "2020-03-30")) {
  # get average pre-covid ridership data for each census tract
  pre_covid <- tracts_weekly_ridership %>%
    filter(monday <= cut[1]) %>%
    group_by(GEOID, ccd, DIST_NAME) %>%
    summarise(pre_covid_ridership = mean(ridership, na.rm = TRUE)) %>%
    replace_na(list(post_covid_ridership = 0))
  
  #average post-covid ridership for each census tract
  post_covid <- tracts_weekly_ridership %>%
    filter(monday >= cut[2]) %>%
    group_by(GEOID, ccd, DIST_NAME) %>%
    summarise(post_covid_ridership = mean(ridership, na.rm = TRUE)) %>%
    replace_na(list(post_covid_ridership = 0))
  
  # put pre and post covid dfs together for comparison table
  comp <- pre_covid %>%
    st_join(post_covid) %>%
    mutate(diff = post_covid_ridership -  pre_covid_ridership,
           pct_change = diff / pre_covid_ridership) %>%
    filter(pct_change != Inf) %>%
    select(GEOID.x, ccd.x, DIST_NAME.x, pre_covid_ridership, post_covid_ridership, diff, pct_change) %>%
    left_join(acs_spread, by = c("GEOID.x" = "GEOID"))
  
  return(comp)
}


##### ACS DATA FUNCTIONS ------------------------------------------------------------------ ####
#test <- build_comp_df(tracts_weekly_ridership, cut = "2020-03-30")
import_acs <- function(key = "", county_ls = c("Philadelphia")) { # David todo: turn variables into parameter of function
  # Read in Census Data - build a lookup table if you want an easy reference point
  #census_api_key(key) # Supply your census API key
  #v18 <- load_variables(2018, "acs5", cache = TRUE) # all of the 2018 5-Year ACS variables for easy viewing
  # get the ACS data we'll be using
  acs_data <- get_acs(geography = "tract", state = "PA", 
                      county = county_ls,
                      variables = c(
                        total_pop = "B01001_001",
                        working_pop = "B08006_001",
                        med_age = "B01002_001",
                        med_income = "B19013_001",
                        education_tot = "B15002_001",
                        eductation_HS = "B15002_011",
                        education_BA = "B15002_015",
                        education_MS = "B15002_016",
                        veh_own_tot = "B08014_001",
                        veh_own_0 = "B08014_002",
                        veh_own_1 = "B08014_003",
                        veh_own_2 = "B08014_004",
                        veh_own_3 = "B08014_005",
                        veh_own_4 = "B08014_006",
                        veh_own_5plus = "B08014_007",
                        race_white = "B01001A_001",
                        race_black = "B01001B_001",
                        race_asian = "B01001D_001",
                        race_latinx = "B01001I_001",
                        total_male_commute = "B08006_018",
                        total_female_commute = "B08006_035",
                        car_commute = "B08006_002",
                        transit_commute = "B08006_008",
                        bike_commute = "B08006_014",
                        walk_commute = "B08006_015",
                        wfh_commute = "B08006_017",
                        industry_1 = "B08126_002",
                        industry_2 = "B08126_003",
                        industry_3 = "B08126_004",
                        industry_4 = "B08126_005",
                        industry_5 = "B08126_006",
                        industry_6 = "B08126_007",
                        industry_7 = "B08126_008",
                        industry_8 = "B08126_009",
                        industry_9 = "B08126_010",
                        industry_10 = "B08126_011",
                        industry_11 = "B08126_012",
                        industry_12 = "B08126_013",
                        industry_13 = "B08126_014"
                      ))
  # going to factor the income bins - store them here in order
  income_class <- c("very_low_income", "low_income", "mid_income", "high_income", "very_high_income")
  
  # spread the acs data to a more usable format for visulization (and build the income brackets)
  acs_spread <- acs_data %>%
    select(-moe) %>%
    spread(key = `variable`, value = `estimate`) %>%
    mutate(education_HS_p = eductation_HS / working_pop,
           education_BA_p = education_BA / working_pop,
           education_MS_p = education_MS / working_pop) %>%
    mutate_if(is.numeric, round, 2) %>%
    mutate(income_bucket = case_when(
      med_income < 20000 ~ "very_low_income",
      med_income >= 20000 & med_income < 35000 ~ "low_income",
      med_income >= 35000 & med_income < 55000 ~ "mid_income",
      med_income >= 55000 & med_income < 100000 ~ "high_income",
      med_income >100000 ~ "very_high_income")) %>%
    mutate(income_bucket = factor(
      income_bucket, 
      ordered = TRUE,
      levels = income_class)) %>% # order the factor by the levels listed above
    mutate(no_car_pct = veh_own_0 / veh_own_tot,
           yes_car_pct = (veh_own_1 + veh_own_2 + veh_own_3 + veh_own_4 + veh_own_5plus) / veh_own_tot) %>%
    mutate_if(is.numeric, round, 2) %>%
    mutate(white_pct = race_white / total_pop,
           black_pct = race_black / total_pop,
           asian_pct = race_asian / total_pop,
           latinx_pct = race_latinx / total_pop) %>%
    mutate_if(is.numeric, round, 2) %>%
    mutate(car_pct = car_commute / (total_male_commute + total_female_commute),
           transit_pct = transit_commute / (total_male_commute + total_female_commute),
           bike_pct = bike_commute / (total_male_commute + total_female_commute),
           walk_pct = walk_commute / (total_male_commute + total_female_commute),
           wfh_pct  = wfh_commute / (total_male_commute + total_female_commute)) %>%
    mutate_if(is.numeric, round, 2) %>%
    mutate(industry_total = industry_1 + industry_2 + industry_3 + industry_4 + industry_5 + industry_6 + 
             industry_7 + industry_8 + industry_9 + industry_10 + industry_11 + industry_12 + industry_13) %>%
    mutate(industry1_pct = industry_1 / industry_total,
           industry2_pct = industry_2 / industry_total,
           industry3_pct = industry_3 / industry_total,
           industry4_pct = industry_4 / industry_total,
           industry5_pct = industry_5 / industry_total,
           industry6_pct = industry_6 / industry_total,
           industry7_pct = industry_7 / industry_total,
           industry8_pct = industry_8 / industry_total,
           industry9_pct = industry_9 / industry_total,
           industry10_pct = industry_10 / industry_total,
           industry11_pct = industry_11 / industry_total,
           industry12_pct = industry_12 / industry_total,
           industry13_pct = industry_13 / industry_total) %>%
    mutate_if(is.numeric, round, 2)
}

  
#### OTHER - NOT IN USE ------------------------------------ ####
validate_apc_trips_input <- function(apc_trips_clean, gtfs_df) {
  apc_trips <- get_apc_trips_summary(apc_trips_clean)
  gtfs_trips <- get_gtfs_trips(gtfs_df)
  
  joined <- full_join(apc_trips, gtfs_trips, by = "trip_id")
  
  return(joined)
}

validate_apc_stops_input <- function(apc_stops_clean, gtfs_df) {
  
  apc_stops <- (apc_stops_clean)
  gtfs_stops <- get_gtfs_stops(gtfs_df)
  
  joined <- left_join(gtfs_stops, apc_stops, by = "stop_id")
  
  return(joined)
}


# assign periods to APC data
assign_periods <- function(apc_data) {
  
}

# generate stop level ridership for an APC trip file 

# for whole day
get_daily_stop_data <- function(apc_data) {
  output <- apc_data %>%
    group_by(stop_id) %>%
    summarise(ons = sum(ons), 
              offs = sum(offs),
              mean_load = mean(load))
  
  return(output)
}

# by hour
get_hourly_stop_data <- function(apc_data) {
  output <- apc_data %>%
    group_by(stop_id, hour_bin) %>%
    summarise(ons = sum(ons), offs = sum(offs))
  
  return(output)
}

# by period
get_period_stop_data <- function(apc_data) {
  output <- apc_data %>%
    mutate(period = case_when(
      hour_bin >= 0 & hour_bin <= 5 ~ "early_am", 
      hour_bin > 5 & hour_bin <= 10 ~ "am_peak",
      hour_bin > 10 & hour_bin <= 15 ~ "afternoon",
      hour_bin > 15 & hour_bin <= 20 ~ "pm_peak",
      hour_bin > 20 & hour_bin <= 24 ~ "late_night"
    )) %>%
    group_by(stop_id, period) %>%
    summarise(ons = sum(ons), offs = sum(offs))
  
  return(output)
}

get_routes_by_stop <- function(gtfs_obj) {
  stops <- gtfs_obj$stops 

  }
