require(googledrive)  ;  require(tidyverse)  ;  require(feather)

source("./code/functions.R")

# FIRST TIME RUN - download data from google drive (need access from Andrew S.)
#drive_auth(use_oob = TRUE)
#drive_download("preped_apc_data.feather", path = "./inputs/spring2019_apc_data.feather", overwrite = TRUE)


# Read in apc data
spring_2019 <- read_feather("./inputs/spring2019_apc_data.feather")
summary(spring_2019)



# prepare data frames
spring_2019_daily_stops <- get_daily_stop_data(spring_2019)
spring_2019_hourly_stops <- get_hourly_stop_data(spring_2019)
spring_2019_period_stops <- get_period_stop_data(spring_2019)

