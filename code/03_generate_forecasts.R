# GENERATE FORECASTS

# initialise project if necessary
if (!exists("mutate")) {
  source(here::here("code/00_initialise.R"))
} else {
  message("Project already initialised")
}

# load crime counts
crime_counts <- here::here("analysis_data/crime_counts.rds") %>% 
  read_rds() %>% 
  as_tsibble(key = c(city_name, category), index = week) %>% 
  fill_gaps()

# check for gaps in time series
filter(crime_counts, is.na(crimes_per_day))

# estimate models, if none exist
if (!file.exists(here::here("analysis_data/crime_models.RData"))) {
  
  # estimate models
  crime_models <- crime_counts %>% 
    # use data from before first US case to train model
    filter(week < ymd("2020-01-20")) %>% 
    model(arima = ARIMA(crimes_per_day ~ trend() + season()))
  
  # save models
  # write_rds(crime_models, here::here("analysis_data/crime_models.rds"))
  save(crime_models, file = here::here("analysis_data/crime_models.RData"))
  
} else {
  
  # load existing models
  # crime_models <- read_csv(here::here("analysis_data/crime_models.rds"))
  
}

# generate forecasts
crime_forecasts <- forecast(
  crime_models, 
  h = glue::glue("{as.integer(difftime(now(), ymd('2020-01-20'), units = 'weeks')) + 3} weeks")
)

# save forecasts
write_rds(crime_forecasts, here::here("analysis_data/crime_forecasts.rds"))

