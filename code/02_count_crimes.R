# COUNT CRIMES IN EACH CITY

# initialise project if necessary
if (!exists("mutate")) {
  source(here::here("code/00_initialise.R"))
} else {
  message("Project already initialised")
}

# load crime data
crime_data <- read_csv(here::here("analysis_data/crime_data.csv.gz"))

# count crimes data
crime_data %>%
  mutate(
    # create new categories for analysis
    category = case_when(
      nibrs_offense_code %in% c("09A", "13A") & location_category %in% 
        c("commercial", "leisure", "open space", "retail", "street", 
          "transportation") ~ "serious assaults in public",
      nibrs_offense_code %in% c("09A", "13A") & location_category == "residence" 
      ~ "serious assaults in residences",
      nibrs_offense_code == "22A" ~ "residential burglary",
      nibrs_offense_code == "22B" ~ "non-residential burglary",
      nibrs_offense_code == "240" ~ "theft of vehicle",
      nibrs_offense_code == "23F" ~ "theft from vehicle",
      nibrs_offense_code == "12A" ~ "personal robbery",
      TRUE ~ "other"
    ),
    # remove time portion from date, so it can be used in count()
    date = date(date_single)
  ) %>% 
  # remove crimes not needed
  filter(category != "other") %>%
  # calculate crimes per day
  count(city_name, category, date, name = "crimes") %>% 
  as_tsibble(key = c(city_name, category), index = date) %>% 
  fill_gaps(crimes = 0) %>% 
  # add if date is holiday
  mutate(
    holiday = date %in% as_date(timeDate::holidayNYSE(year = 2016:2020)),
    week = yearweek(date)
  ) %>% 
  as_tibble() %>% 
  group_by(city_name, category, week) %>%
  summarise(
    crimes = sum(crimes), 
    days_in_week = length(unique(date)), 
    holidays = sum(holiday)
  ) %>% 
  ungroup() %>% 
  mutate(crimes_per_day = crimes / days_in_week) %>% 
  # export data
  write_rds(here::here("analysis_data/crime_counts.rds")) %>% 
  group_by(city_name) %>%
  summarise(last_date = as_date(max(week))) %>% 
  arrange(last_date)

# check most recent crimes
crime_data %>% 
  group_by(city_name) %>%
  # filter(date(date_single) < max(date(date_single)) - days(2)) %>% 
  # filter(yearweek(date_single) < yearweek(today())) %>%
  summarise(last_date = max(date_single)) %>% 
  arrange(last_date)
  