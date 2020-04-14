

# compile apc data


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



