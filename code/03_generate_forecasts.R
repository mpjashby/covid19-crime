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
  fill_gaps(crimes_per_day = 0) %>% 
  # remove burglaries and vehicle thefts for Dallas, since they're missing 
  # before 2017
  filter(
    !(city_name == "Dallas, TX" & 
        category %in% c("residential burglary", "non-residential burglary",
                        "theft of vehicle", "theft from vehicle"))
  ) %>% 
  # remove days in week before first full week of 2016
  filter(week >= ymd("2016-01-01"))

# check for gaps in time series
filter(crime_counts, is.na(crimes))

# estimate models, if none exist
if (!file.exists(here::here("analysis_data/crime_models.RData"))) {
  
  # estimate models
  crime_models <- crime_counts %>% 
    # use data from before first US case to train model
    filter(week < ymd("2020-01-20")) %>% 
    model(arima = ARIMA(crimes ~ trend() + season() + holidays))
  
  # save models
  save(crime_models, file = here::here("analysis_data/crime_models.RData"))
  
} else {
  
  # load existing models
  load(here::here("analysis_data/crime_models.RData"))
  
}

# generate data for forecasts
forecast_data <- expand.grid(
  city_name = unique(crime_counts$city_name),
  category = unique(crime_counts$category),
  date = seq.Date(
    ymd("2020-01-20"), 
    ymd("2020-01-20") + 
      weeks(as.integer(difftime(now(), ymd("2020-01-20"), units = "weeks")) + 1), 
    by = "days"
  ),
  stringsAsFactors = FALSE
) %>% 
  mutate(
    holiday = date %in% as_date(timeDate::holidayNYSE(year = 2016:2020)),
    week = yearweek(date)
  ) %>% 
  group_by(city_name, category, week) %>% 
  summarise(holidays = sum(holiday)) %>% 
  ungroup() %>% 
  as_tsibble(key = c(city_name, category), index = week)

# generate forecasts
crime_forecasts <- forecast(
  filter(crime_models, city_name != "Sacramento, CA"), 
  filter(forecast_data, city_name != "Sacramento, CA")
)

# save forecasts
write_rds(crime_forecasts, here::here("analysis_data/crime_forecasts.rds"))

