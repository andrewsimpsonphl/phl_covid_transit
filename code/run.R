require(googledrive)  ;  require(tidyverse)  ;  require(feather)  ;  require(tidytransit) ; require(lubridate)
require(broom) ; require(sf) ; library(fuzzyjoin)
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
# Read in apc data
spring_2019 <- read_feather("./inputs/spring2019_apc_data.feather")

# import dataset from Infodev of stop level data aggregated to the week
year_month_week_stop <- read.csv("./inputs/new_input/Data_year_Month_Week_Stop.csv")
stop_level_data <- year_month_week_stop %>% clean_apc_stops_input()

# import gtfs files in path
gtfs_df <- build_gtfs_df(path = "./inputs/gtfs")
#build departure df
departure_df <- get_departure_df(gtfs_df)
#build departure board
departure_board <- build_stop_departure_board(stop_level_data, departure_df)

# create frame of stops with monthly frequencies using GTFS


# Read in Census Data
census_api_key("eac2c3c2ce9ca6550b188093d03c636351a563e4") # Andrew's census API Key
v18 <- load_variables(2018, "acs5", cache = TRUE) # all of the 2018 5-Year ACS variables for easy viewing

# get the ACS data we'll be using
acs_data <- get_acs(geography = "tract", state = "PA", county = "Philadelphia",
                variables = c(
                  total_pop = "B01001_001",
                  med_age = "B01002_001",
                  med_income = "B19013_001",
                  education_tot = "B15002_001",
                  eductation_HS = "B15002_011",
                  education_BA = "B15002_015",
                  education_MS = "B15002_016"
                ))

apc_spread <- acs_data %>%
  select(-moe) %>%
  spread(key = `variable`, value = `estimate`) %>%
  mutate(education_HS_p = eductation_HS / total_pop,
         education_BA_p = education_BA / total_pop,
         education_MS_p = education_MS / total_pop) %>%
  mutate_if(is.numeric, round, 2)

clusters <- tibble(k = 1:9) %>%
  mutate(
    kclust = map(k, ~kmeans(apc_spread, .x))
  )

# Import shapes
# census tracts
tract_sf <- read_sf("./inputs/tl_2019_42_tract/tl_2019_42_tract.shp")

# planning districts
planningdistricts_sf <- read_sf("./inputs/Planning_Districts.geojson")

# center city
ccd_sf <- read_sf("./inputs/CCD_Boundary.geojson")


#### Clean Data ---------------------------------------- ####

# clean up the stop level data
stop_level_data <- year_month_week_stop %>% clean_apc_stops_input()

#### Validate Data ------------------------------------- ####
test_stops <- validate_apc_stops_input(march_2020_stops, march_2020_gtfs)

test_trips <- validate_apc_stops_input(march_2020_apctrips_clean, march_2020_gtfs)


# prepare data frames
spring_2019_daily_stops <- get_daily_stop_data(spring_2019)
spring_2019_hourly_stops <- get_hourly_stop_data(spring_2019)
spring_2019_period_stops <- get_period_stop_data(spring_2019)


get_stop_frequency(march_2020_gtfs)


#### Analyze & Visualize Data ------------------------------------- ####

