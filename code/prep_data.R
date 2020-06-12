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

# assembly function to generate the main stop level database to be used in the analysis
build_stop_data <- function(apc_path, gtfs_path) {
  
  # import dataset from Infodev of stop level data aggregated to the week
  year_month_week_stop <- read.csv("./inputs/new_input/Data_Year_Month_Week_Stop.csv")
  year_month_route_stop <- read.csv("./inputs/new_input/Data_Year_Month_Route_stop.csv")
  year_week_stop <- read.csv("./inputs/new_input/Data_Year_Week_Stop.csv") # should use this instead of month_week
  
  
  # clean up that apc data
  stop_level_data <- year_week_stop %>% clean_apc_stops_input()
  
  # import gtfs files in path
  gtfs_df <- build_gtfs_df(path = gtfs_path)
  
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

stops_byweek_ridership_sf <- build_stop_data(
  apc_path = ,
  gtfs_path = "./inputs/gtfs"
)

# import and build an apc dataframe (using dplyr::spread approach)
census_api_key(key = "", install =TRUE)
acs_spread <- import_acs(key = "653cde4d4e1b34d554142871fc2a111eefdbe25d",
                         county_ls = c("Philadelphia"))

# store the data that needs to be put pulled for visualizations
st_write(stops_byweek_ridership_sf, "./prepped_data/stop_data.geojson", overwrite = TRUE, driver = "GeoJSON")
write_csv(acs_spread, "./prepped_data/acs_spread.csv")







