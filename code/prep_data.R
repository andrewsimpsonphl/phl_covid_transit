##### RUN.R ---------------------------- ####
# Run this code to pull in the data sources and prepare them for visualization 
# Only needs to be run when updating the APC or GTFS data
# Visualization.Rmd produces all of the visuales from prepared data stored in repo

require(googledrive)  ;  require(tidyverse)  ;  require(feather)  ;  require(tidytransit) ; require(lubridate)
require(broom) ; require(sf) ; library(fuzzyjoin) ; require(data.table)
require(dtplyr) ; require(tidyfast)
require(rphl) # City branded GGPLOT template; download off github for first use
require(tidycensus)
options(tigris_use_cache = TRUE)

# source the function code
source("./code/functions.R")

#Basic work flow:
#  Calculate ridership per trip per stop for each week
#  Calculate how many trips per day serve that stop for that week
#  Multiply per trip per stop ridership by trips per day for each stop 
#  Aggregate new daily per stop ridership (by week) to census tracts/planning districts
#  Produce visuals

#thigns to add:
#  - route level visuals (including income by route data)
#  - convert run.R to store all of the outputs into files that are red into the rmd file 
#     (this will mean you don't need to run run.R to see the visualizations each time)

#### Import Data ---------------------------------------- ####
# Read in apc data from 2019
spring_2019_stops <- read_csv("./prepped_data/spring_2019.csv")

# import gtfs files in path
gtfs_df <- build_gtfs_df(path = gtfs_path)

# assembly function to generate the main stop level database to be used in the analysis
build_stop_data <- function(gtfs_path = "./inputs/gtfs") {
  
  # import dataset from Infodev of stop level data aggregated to the week
  year_week_stop <- read.csv("./inputs/infodev_input/Data_Year_Week_Stop.csv")
  
  # clean up that apc data
  stop_level_data <- year_week_stop %>% clean_apc_stops_input()
  
  #build departure df of gtfs data
  departure_df <- get_stop_departure_df(gtfs_df)
  
  # build stop by week with daily ridership computed using average samples and actual departures 
  stops_byweek_ridership <- build_stop_weekly_df(stop_level_data, departure_df)
  
  # create a spatial object of bus stops - using January gtfs data is  lazy but fine
  gtfs_sf <- gtfs_as_sf(gtfs_df$gtfs[[1]])
  
  # creates a spatial object of the bus stop df so that is easy map and join to other spatial data
  stops_byweek_ridership_sf <- stops_byweek_ridership %>%
    mutate(stop_id = as.character(stop_id)) %>% 
    left_join(gtfs_sf$stops, by=c("stop_id" = "stop_id")) %>%
    st_sf() %>%
    st_transform(crs = 4269) # make the data play nice with apc data spatially
  
  return(stops_byweek_ridership_sf)
}

# beginning to look at route level data - not yet working. need to pull the GTFS schedules for each route
build_route_data <- function() {
  # import SEPTA route level data (aggregated at month level due to sample size limitations on weekly data here - requesting weekly route level data though)
  year_month_route_stop <- read.csv("./infodev_input/new_input/Data_Year_Month_Route_stop.csv")
  
  # TO DO - GET MONTHLY DEPARTURES BY STOP BY ROUTE
  route_level_data <- year_month_route_stop %>% clean_apc_routes_input() %>% 
    group_by(route_id, month) %>% 
    summarise(total_ons)
  
  stop_route_departure_df <- get_stop_route_departure_df(gtfs_df)
  
  #TODO create a spatial object of bus routes
  
  return()
}

# call build_stop_data function once to put stop data into this spatial data frame
stops_byweek_ridership_sf <- build_stop_data()
routes_byweek_ridership_sf <- build_route_data()


# import and build an apc dataframe (using dplyr::spread approach)
census_api_key(key = "", install =TRUE)
acs_spread <- import_acs(key = "653cde4d4e1b34d554142871fc2a111eefdbe25d",
                         county_ls = c("Philadelphia"))

# store the data that needs to be put pulled for visualizations
st_write(stops_byweek_ridership_sf, "./prepped_data/stop_data.geojson", overwrite = TRUE, driver = "GeoJSON") # store stop data
st_write(routes_byweek_ridership_sf, "./prepped_data/route_month_data.geojson", overwrite = TRUE, driver = "GeoJSON") # store route data
write_csv(acs_spread, "./prepped_data/acs_spread.csv") # store census data


ggplot(stops_byweek_ridership_sf, aes(x = sample_rate)) + geom_histogram()




