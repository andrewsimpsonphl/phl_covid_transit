require(googledrive)  ;  require(tidyverse)  ;  require(feather)  ;  require(tidytransit) ; require(lubridate)
require(broom) ; require(sf) ; library(fuzzyjoin) ; require(data.table)
require(dtplyr) ; require(tidyfast)
require(rphl) # City branded GGPLOT template; download off github for first use
require(tidycensus)
options(tigris_use_cache = TRUE)

# source the function code
source("./code/functions.R")

# FIRST TIME RUN - download 2019 big APC data from google drive (need access from Andrew S.)
# data is too big for github, so stick it in your inputs folder but COMMIT/PUSH IT TO GIT
#drive_auth(use_oob = TRUE)
#drive_download("preped_apc_data.feather", path = "./inputs/spring2019_apc_data.feather", overwrite = TRUE)

#Basic work flow:
#  Calculate ridership per trip per stop for each week
#  Calculate how many trips per day serve that stop for that week
#  Multiply per trip per stop ridership by trips per day for each stop 
#  Aggregate new daily per stop ridership (by week) to census tracts/planning districts
#  Produce visuals

#### Import Data ---------------------------------------- ####
# Read in apc data from 2019
#spring_2019 <- read_feather("./inputs/spring2019_apc_data.feather")

# import dataset from Infodev of stop level data aggregated to the week
year_month_week_stop <- read.csv("./inputs/new_input/Data_year_Month_Week_Stop.csv")
stop_level_data <- year_month_week_stop %>% clean_apc_stops_input()

# import gtfs files in path
gtfs_df <- build_gtfs_df(path = "./inputs/gtfs")
#build departure df
departure_df <- get_departure_df(gtfs_df)

# build stop by week with daily ridership computed 
stops_byweek_ridership <- build_stop_weekly_df(stop_level_data, departure_df)

# create a spatial object of bus stops - using January gtfs data is  lazy but fine
gtfs_sf <- gtfs_as_sf(gtfs_df$gtfs[[1]])

# creates a spatial object of the bus stop df
stops_byweek_ridership_sf <- stops_byweek_ridership %>%
  mutate(stop_id = as.character(stop_id)) %>% 
  left_join(gtfs_sf$stops, by=c("stop_id" = "stop_id")) %>%
  st_sf() %>%
  st_transform(crs = 4269)

# Get bus network-wide ridership by week
weekly_ridership <- stops_byweek_ridership %>%
  group_by(monday) %>%
  summarise(daily_ridership = sum(daily_ons, na.rm = TRUE))

# visualize that weekly ridership - yikes
ggplot(weekly_ridership, aes(x = as_date(monday), y = daily_ridership)) +
  geom_bar(position="dodge", stat="identity") + 
  scale_y_continuous(labels = scales::comma, limits = c(0, 70000), breaks = seq(0, 70000, by=10000)) +
  scale_x_date(date_breaks = "1 week") + 
  xlab("Week Starting With") + 
  ylab("Daily Bus Ridership") + 
  ggtitle("SEPTA Daily Bus Ridership (2020)") + 
  theme_phl() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Read in Census Data - build a lookup table if you want an easy reference point
census_api_key("eac2c3c2ce9ca6550b188093d03c636351a563e4") # Andrew's census API Key
#v18 <- load_variables(2018, "acs5", cache = TRUE) # all of the 2018 5-Year ACS variables for easy viewing

# get the ACS data we'll be using
acs_data <- get_acs(geography = "tract", state = "PA", 
                    county = c("Philadelphia"),
                variables = c(
                  total_pop = "B01001_001",
                  med_age = "B01002_001",
                  med_income = "B19013_001",
                  education_tot = "B15002_001",
                  eductation_HS = "B15002_011",
                  education_BA = "B15002_015",
                  education_MS = "B15002_016"
                ))

# going to factor the income bins - store them here in order
income_class <- c("very_low_income", "low_income", "mid_income", "high_income", "very_high_income")

# spread the acs data to a more usable format for visulization (and build the income brackets)
acs_spread <- acs_data %>%
  select(-moe) %>%
  spread(key = `variable`, value = `estimate`) %>%
  mutate(education_HS_p = eductation_HS / total_pop,
         education_BA_p = education_BA / total_pop,
         education_MS_p = education_MS / total_pop) %>%
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
    levels = income_class) # order the factor by the levels listed above
    )


# Import shapes
# census tracts
tract_sf <- read_sf("./inputs/tl_2019_42_tract/tl_2019_42_tract.shp")

# planning districts
planningdistricts_sf <- read_sf("./inputs/Planning_Districts.geojson") %>% st_transform(crs = 4269)

# center city
ccd_sf <- read_sf("./inputs/CCD_Boundary.geojson") %>% st_transform(crs = 4269) %>% mutate(ccd = TRUE)

#### Join Data Spatially -------------------------------------------####
stops_tracts <- st_join(stops_byweek_ridership_sf,
                            left = FALSE,
                            tract_sf["GEOID"]) 

tracts_weekly_ridership <- stops_tracts %>%
  group_by(GEOID, monday) %>%
  summarise(ridership = sum(daily_ons)) %>%
  right_join(acs_spread) %>%
  st_join(ccd_sf) %>% 
  st_join(planningdistricts_sf)

#### Analyze & Visualize Data ------------------------------------- ####

# get average pre-covid ridership data for each census tract
pre_covid <- tracts_weekly_ridership %>%
  filter(monday < "2020-03-30") %>%
  group_by(GEOID, ccd, DIST_NAME) %>%
  summarise(pre_covid_ridership = mean(ridership, na.rm = TRUE)) %>%
  replace_na(list(post_covid_ridership = 0))

#average post-covid ridership for each census tract
post_covid <- tracts_weekly_ridership %>%
  filter(monday >= "2020-03-30") %>%
  group_by(GEOID, ccd, DIST_NAME) %>%
  summarise(post_covid_ridership = mean(ridership, na.rm = TRUE)) %>%
  replace_na(list(post_covid_ridership = 0))

comp <- pre_covid %>%
  st_join(post_covid) %>%
  mutate(diff = post_covid_ridership -  pre_covid_ridership,
         pct_change = diff / pre_covid_ridership) %>%
  filter(pct_change != Inf) %>%
  select(GEOID.x, ccd.x, DIST_NAME.x, pre_covid_ridership, post_covid_ridership, diff, pct_change) %>%
  left_join(acs_spread, by = c("GEOID.x" = "GEOID"))

ggplot(comp %>% filter(pct_change < 0.25), aes(x = med_income, y = pct_change)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  scale_y_continuous(labels = scales::percent) + 
  theme_phl()

income_tracts <- comp %>%
  group_by(income_bucket) %>%
  summarise(num_tracts = n(),
            pre_covid = sum(pre_covid_ridership),
            post_covid = sum(post_covid_ridership),
            diff = post_covid - pre_covid,
            pct_change = diff / pre_covid) %>%
  filter(is.na(income_bucket) == FALSE)

# need to lable n() for each bar
ggplot(income_tracts %>% filter(pct_change < 0.25), aes(x = income_bucket, y = pct_change)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) + 
  scale_x_discrete(labels = c("Very Low Income (< 20k)", "Low Income (<35k)", "Med Income (<50k)", "High Income (<100k)", "Very High (>100k)")) + 
  coord_cartesian(ylim = c(-1, -.5)) +
  theme_phl() +
  ggtitle("Bus Ridership Changes by Income Level of Census Tract") +
  ylab("Bus Ridership Change Post-COVID") + 
  xlab("Census Tracts Binned by Income Level") 

income_tracts_ccd <- comp %>%
  group_by(income_bucket, ccd.x) %>%
  summarise(pre_covid = sum(pre_covid_ridership),
            post_covid = sum(post_covid_ridership),
            diff = post_covid - pre_covid,
            pct_change = diff / pre_covid) %>%
  mutate(ccd = case_when(
    ccd.x == TRUE ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  filter(is.na(income_bucket) == FALSE & ccd == FALSE)

ggplot(income_tracts_ccd, aes(x = income_bucket, y = pct_change)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) + 
  scale_x_discrete(labels = c("Very Low Income (< 20k)", "Low Income (<35k)", "Med Income (<50k)", "High Income (<100k)", "Very High (>100k)")) + 
  coord_cartesian(ylim = c(-1, -.5)) +
  theme_phl() +
  ggtitle("Bus Ridership Changes by Income Level of Census Tract") +
  ylab("Bus Ridership Change Post-COVID") + 
  xlab("Census Tracts Binned by Income Level (Excluding Center City)")

income_planning <- comp %>%
  group_by(DIST_NAME.x) %>%
  summarise(pre_covid = sum(pre_covid_ridership),
            post_covid = sum(post_covid_ridership),
            avg_med_income = mean(med_income, na.rm = TRUE),
            diff = post_covid - pre_covid,
            pct_change = diff / pre_covid)

# visualize ridership change by planning district
ggplot(income_planning, aes(x = reorder(DIST_NAME.x, avg_med_income), y = pct_change, fill = avg_med_income)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) + 
  coord_cartesian(ylim = c(-1, -.5)) + 
  theme_phl() + 
  scale_fill_phl(palette = "blues", discrete = F) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.95)) +
  xlab("Planning District (Sorted by Income)") +
  ylab("Bus Ridership Change Post-COVID")

