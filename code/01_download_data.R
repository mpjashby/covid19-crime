# This file downloads data from the city website for each city

# initialise project if necessary
if (!exists("mutate")) {
  source(here::here("code/00_initialise.R"))
} else {
  message("Project already initialised")
}

# create tibble of data URLs
data_urls <- tribble(
  ~city, ~url,
  "Austin", "https://data.austintexas.gov/api/views/fdj4-gpfu/rows.csv?accessType=DOWNLOAD",
  "Boston", "https://data.boston.gov/dataset/6220d948-eae2-4e4b-8723-2dc8e67722a3/resource/12cb3883-56f5-47de-afa5-3b1cf61b257b/download/tmpeb3z80ty.csv",
  "Chicago", "https://data.cityofchicago.org/api/views/ijzp-q8t2/rows.csv?accessType=DOWNLOAD",
  # "Los Angeles early", "https://data.lacity.org/api/views/63jg-8b9z/rows.csv?accessType=DOWNLOAD",
  "Los Angeles late", "https://data.lacity.org/api/views/2nrs-mtv8/rows.csv?accessType=DOWNLOAD",
  # "Louisville 2016", "https://data.louisvilleky.gov/sites/default/files/Crime_Data_2016_39.csv",
  # "Louisville 2017", "https://data.louisvilleky.gov/sites/default/files/Crime_Data_2017_9.csv",
  # "Louisville 2018", "http://lky-open-data.s3.amazonaws.com/LMPD/Crime_Data_2018.csv",
  "Louisville 2019", "https://data.louisvilleky.gov/sites/default/files/27091/Crime_Data_2019.csv", # includes 2020 for now
  "Memphis", "https://memphisinternal.data.socrata.com/api/views/ybsi-jur4/rows.csv?accessType=DOWNLOAD",
  # "Nashville 2016", "https://data.nashville.gov/api/views/tpvn-3k6v/rows.csv?accessType=DOWNLOAD",
  # "Nashville 2017", "https://data.nashville.gov/api/views/ei8z-vngg/rows.csv?accessType=DOWNLOAD",
  # "Nashville 2018", "https://data.nashville.gov/api/views/we5n-wkcf/rows.csv?accessType=DOWNLOAD",
  # "Nashville 2019", "https://data.nashville.gov/api/views/a88c-cc2y/rows.csv?accessType=DOWNLOAD",
  "Nashville 2020", "https://data.nashville.gov/api/views/sie3-y9k4/rows.csv?accessType=DOWNLOAD",
  # "San Francisco early", "https://data.sfgov.org/api/views/tmnf-yvry/rows.csv?accessType=DOWNLOAD",
  "San Francisco late", "https://data.sfgov.org/api/views/wg3w-h783/rows.csv?accessType=DOWNLOAD"
) %>% 
  mutate(string = str_to_lower(str_replace_all(city, "\\s", "_")))

# download data
walk2(data_urls$string, data_urls$url, function (string, url) {
  raw_data_file <- glue::glue("original_data/raw_{string}.csv")
  GET(url, write_disk(raw_data_file, overwrite = TRUE), progress())
})



# process Austin data
here::here("original_data/raw_austin.csv") %>% 
  read_csv(
    col_types = cols(
      .default = col_character(),
      `Highest Offense Code` = col_integer(),
      Latitude = col_double(),
      Longitude = col_double()
    )
  ) %>% 
  janitor::clean_names() %>% 
  rename(location_type_raw = location_type) %>% 
  # add date variable
  add_date_var("occurred_date_time", "mdY IMS p", "America/Chicago") %>% 
  # add year variable and remove offenses before start date
  filter_by_year(yearFirst, yearLast) %>%
  # remove variables containing duplicate information
  select(-occurred_date, -occurred_time, -report_date, -report_time, 
         -x_coordinate, -y_coordinate, -location) %>%
  # harmonise offence categories
  mutate(
    nibrs_offense_code = case_when(
      ucr_category == "120" ~ "12U",
      ucr_category == "220" & location_type_raw == "RESIDENCE / HOME" ~ "22A",
      ucr_category == "220" ~ "22B",
      !is.na(ucr_category) ~ ucr_category,
      highest_offense_code %in% c(100, 102, 108) ~ "09A", # non-negligent homicide
      highest_offense_code == 103 ~ "09B", # negligent homicide
      highest_offense_code == 208 ~ "36B", # statutory rape
      highest_offense_code == 500 ~ "22A", # residential burglary
      highest_offense_code %in% c(608, 615, 621, 1102, 1198) ~ "26A", # false pretenses
      highest_offense_code %in% c(800, 801, 802) ~ "200", # arson
      highest_offense_code %in% c(900, 909, 2000, 2011, 2012, 2013) ~ "13A", # agg assault
      highest_offense_code == 902 & 
        highest_offense_description %in% c(
          "ASSAULT BY CONTACT", "ASSAULT BY CONTACT FAM/DATING") ~ 
        "13B", # simple assault
      highest_offense_code == 902 & 
        highest_offense_description == "ASSAULT CONTACT-SEXUAL NATURE" ~ 
        "11D", # fondling
      highest_offense_code %in% c(903, 906) ~ "13B", # simple assault
      highest_offense_code %in% c(901, 2701, 2702, 2704) ~ "13C", # intimidation
      highest_offense_code %in% c(902, 910, 1709) ~ "11D", # fondling
      highest_offense_code %in% c(1000, 1001, 1002, 1003, 1006, 1007, 1008, 
                                  1099, 1111) ~ "250", # forgery
      highest_offense_code %in% c(1005, 1103, 1106, 1108, 1112) ~ "26B", # credit card fraud
      highest_offense_code %in% c(1009, 1010, 1011) ~ "26A", # false pretenses
      highest_offense_code %in% c(1004, 4022, 4027) ~ "26C", # impersonation
      highest_offense_code %in% c(1100, 1101, 1104) ~ "90A", # bad checks
      highest_offense_code %in% c(1105) ~ "26A", # false pretenses
      highest_offense_code %in% c(1199) ~ "26U", # bad checks
      highest_offense_code %in% c(1200, 1202) ~ "270", # embezzlement
      highest_offense_code == 1300 ~ "280", # stolen property offenses
      highest_offense_code %in% c(1400, 1401, 1402, 3817, 3832) ~ "290", # destruction of property
      highest_offense_code %in% c(1500, 1501, 1502, 1503, 1504, 1506, 1507, 
                                  1509, 2408, 2409, 2410, 3304) ~ "520", # weapon offenses
      highest_offense_code == 1600 ~ "40A", # prostitution
      highest_offense_code %in% c(1601, 1602) ~ "40B", # assisting prostitution
      highest_offense_code == 1604 ~ "40C", # purchasing prostitution
      highest_offense_code %in% c(1603, 4199) ~ "64A", # human trafficking - sex
      highest_offense_code %in% c(1705, 1706, 2401, 2402, 2403, 2404, 2405, 
                                  2406, 2407, 2411, 2415, 3201, 3207, 3212, 
                                  3215, 3218, 3297, 3301, 3303, 3305, 3401) ~ 
        "90C", # disorderly conduct
      highest_offense_code %in% c(1710, 2001, 2003, 2004, 2005, 2006, 2008, 
                                  2009, 2010) ~ "90F", # family, nonviolent
      highest_offense_code == 1715 ~ "36A", # incest
      highest_offense_code %in% c(1800, 1801, 1802, 1803, 1804, 1805, 1806, 
                                  1807, 1808, 1809, 1810, 1811, 1812, 1813, 
                                  1814, 1815, 1819, 1820, 1821, 1822, 1823, 
                                  1825, 1826) ~ "35A", # drug violations
      highest_offense_code == 1818 ~ "35B", # drug equipment
      highest_offense_code %in% c(1900) ~ "39A", # betting
      highest_offense_code %in% c(1901, 1902, 1903) ~ "39B", # betting promotion
      highest_offense_code %in% c(1904, 1905) ~ "39C", # betting equipment
      highest_offense_code %in% c(2100, 2102, 2103, 2104, 2105, 2106, 2107, 
                                  2108, 2109, 2110, 2111) ~ "90D", # DUI
      highest_offense_code %in% c(2200, 2201, 2202, 2203, 2205, 2206, 2207, 
                                  2208, 2209, 2210, 3210, 3211) ~ 
        "90G", # liquor violations
      highest_offense_code %in% c(2300, 2302) ~ "90E", # drunkenness
      highest_offense_code %in% c(2416, 2500, 2501, 3203, 3213, 3214, 3217, 
                                  3295, 3302) ~ "90B", # curfew/vagrancy
      highest_offense_code %in% c(2417, 2609, 3414) ~ "90H", # peeping tom
      highest_offense_code %in% c(2600, 2601, 2602, 2603, 2605, 2610, 2611) ~ 
        "370", # porn
      highest_offense_code %in% c(2716, 2712, 2722, 2727, 2721) ~ 
        "90J", # trespass
      highest_offense_code %in% c(2718) ~ "210", # extortion/blackmail
      highest_offense_code %in% c(2800, 2801, 2802, 2803, 2805) ~ 
        "100", # kidnapping
      highest_offense_code %in% c(3102, 3111, 3116) ~ "510", # bribery
      highest_offense_code %in% c(616, 702, 904, 905, 907, 1110, 1301, 1711, 
                                  1799, 2099, 2412, 2413, 2606, 2607, 2608, 
                                  2612, 2700, 2703, 2705, 2706, 2707, 2708, 
                                  2709, 2710, 2711, 2712, 2715, 2717, 2719, 
                                  2720, 2723, 2724, 2728, 2731, 2732, 2733, 
                                  2735, 2899, 2900, 2901, 2902, 2903, 2904, 
                                  2905, 2907, 2908, 3001, 3002, 3003, 3006, 
                                  3007, 3008, 3009, 3010, 3020, 3021, 3103, 
                                  3104, 3105, 3106, 3107, 3108, 3109, 3112, 
                                  3114, 3115, 3117, 3118, 3200, 3205, 3206, 
                                  3209, 3216, 3294, 3296, 3298, 3299, 3300, 
                                  3306, 3310, 3311, 3312, 3313, 3395, 3396, 
                                  3397, 3398, 3399, 3402, 3604, 3720, 3724, 
                                  4111, 8905) ~ "90Z", # all other offenses
      highest_offense_code %in% c(107, 1505, 1824, 2002, 2400, 3722, 3400, 3442, 
                                  3458, 3459, 4100, 3403, 3113, 3829, 4003, 
                                  4200, 4202, 4203, 4204, 4205) ~ 
        "99Z" # non-criminal incidents
    )
  ) %>% 
  left_join(read_csv(here::here("analysis_data/nibrs_categories.csv")), 
            by = "nibrs_offense_code") %>% 
  select(-ucr_category, -category_description) %>% 
  # identify location types
  left_join(
    read_csv(here::here("analysis_data/location_types_austin.csv"), 
             col_types = cols(.default = col_character())),
    by = "location_type_raw"
  ) %>% 
  # convert variable types
  mutate(family_violence = family_violence == "Y") %>% 
  # add city name
  mutate(city_name = "Austin") %>% 
  # select core variables
  select(one_of(common_vars)) %>% 
  # save data
  write_rds(here::here("analysis_data/crime_data_austin.rds"), compress = "gz")



# process Boston data
here::here("original_data/raw_boston.csv") %>% 
  read_csv(col_types = cols(.default = col_character())) %>% 
  janitor::clean_names() %>% 
  # add date variable
  add_date_var("occurred_on_date", "Ymd T", "America/New_York") %>% 
  # filter by year
  filter_by_year(yearFirst, yearLast) %>% 
  # remove variables containing duplicate information
  select(-year, -month, -day_of_week, -hour, -location) %>% 
  mutate(
    nibrs_offense_code = case_when(
      offense_code_group == "Arson" ~ "200",
      offense_code_group == "Burglary - No Property Taken" ~ "22U",
      offense_code %in% c("00121", "00123") ~ "09B", # negligent manslaughter
      # this line must be after the lines above otherwise some incidents listed
      # as being UCR Part 3 but actually being within Part 2 will be incorrectly
      # excluded
      ucr_part %in% c("Other", "Part Three") ~ "99Z", # non-criminal incidents
      offense_code_group == "Aggravated Assault" ~ "13A",
      offense_code_group == "Auto Theft" ~ "240",
      offense_code_group == "Ballistics" ~ "99Z",
      offense_code_group == "Biological Threat" ~ "13C",
      offense_code_group == "Commercial Burglary" ~ "22B",
      offense_code_group == "Homicide" ~ "09A",
      offense_code == "00611" ~ "23A", # Pocket-picking
      offense_code == "00612" ~ "23B", # Purse-snatching
      offense_code %in% c("00613", "0633") ~ "23C", # Shoplifting
      offense_code %in% c("00617", "0617", "0627", "0637") ~ "23D", # Theft From Building
      offense_code == "00618" ~ "23E", # Theft From Coin-Operated Machine
      offense_code %in% c("00614", "0614", "0634") ~ "23F", # Theft From Motor Vehicle except Parts
      offense_code == "00615" ~ "23G", # Theft of Motor Vehicle Parts
      offense_code %in% c("00616", "0616", "0619", "0629", "00619") ~ "23H", # All Other Larceny
      offense_code_group == "Other Burglary" ~ "22U",
      offense_code_group == "Residential Burglary" ~ "22A",
      offense_code %in% c("00301", "00371", "00381", "0338", "0339") ~ "12A", # personal robbery
      offense_code %in% c("00311", "00351", "0335") ~ "12B", # commercial robbery
      offense_code %in% c("00361") ~ "12U", # other robbery
      offense_code_group == "Bomb Hoax" ~ "13C", 
      offense_code_group == "Confidence Games" ~ "26A", 
      offense_code_group == "Counterfeiting" ~ "250", 
      offense_code_group == "Criminal Harassment" ~ "90Z", 
      offense_code_group == "Disorderly Conduct" ~ "90C", 
      offense_code_group == "Drug Violation" ~ "35A", 
      offense_code_group == "Evading Fare" ~ "23H",
      offense_code_group == "Embezzlement" ~ "270",
      offense_code_group %in% c("Explosives", "Firearm Violations") ~ "520", 
      offense_code %in% c("01102", "1102") ~ "26A", 
      offense_code == "01107" ~ "26C", 
      offense_code == "01108" ~ "26D", 
      offense_code == "01109" ~ "26E", 
      offense_code_group == "Gambling" ~ "39A", 
      offense_code_group == "Harassment" ~ "90Z", 
      offense_code_group == "Liquor Violation" ~ "90G", 
      offense_code_group == "Offenses Against Child / Family" ~ "90F", 
      offense_code_group == "Operating Under the Influence" ~ "90D", 
      offense_code == "01504" ~ "520",
      offense_code %in% c("02511", "02611", "02622", "02664") ~ "100",
      offense_code == "02604" ~ "210",
      offense_code %in% c("02610", "2610") ~ "90J",
      offense_code %in% c("02613", "02616", "02641", "02657", "02660", "02663", 
                          "02900", "2616", "02628") ~ "90Z",
      offense_code == "02623" ~ "370",
      offense_code %in% c("02647", "2647") ~ "13C",
      offense_code_group == "Prisoner Related Incidents" ~ "90Z",
      offense_code %in% c("01601", "01602", "01605", "1601", "1602", 
                          "1605") ~ "40A",
      offense_code == "01603" ~ "40B",
      offense_code_group == "Recovered Stolen Property" ~ "280",
      offense_code_group == "Restraining Order Violations" ~ "90Z",
      offense_code_group == "Simple Assault" ~ "13B",
      offense_code_group == "Vandalism" ~ "290",
      offense_code_group == "Violations" ~ "90Z",
      offense_code_group == "HOME INVASION" ~ "12A",
      offense_code_group == "HUMAN TRAFFICKING" ~ "64A",
      offense_code_group == "HUMAN TRAFFICKING - INVOLUNTARY SERVITUDE" ~ "64B",
      offense_code_group %in% c("INVESTIGATE PERSON", "Missing Person Reported", 
                                "Fire Related Reports") ~ "99Z",
      # some recent Boston crimes have no offense_code_group, so for these we
      # categorise only those offences for which we're interested
      offense_description == "MURDER, NON-NEGLIGIENT MANSLAUGHTER" ~ "09A",
      offense_code == "301" ~ "12A",
      offense_description == "ASSAULT - AGGRAVATED" ~ "13A",
      offense_description == "BURGLARY - RESIDENTIAL" ~ "22A",
      TRUE ~ NA_character_
    )
  ) %>% 
  left_join(read_csv(here::here("analysis_data/nibrs_categories.csv")), 
            by = "nibrs_offense_code") %>% 
  # add city name
  mutate(city_name = "Boston") %>% 
  # select core variables
  select(one_of(common_vars)) %>% 
  # save data
  write_rds(here::here("analysis_data/crime_data_boston.rds"), compress = "gz")



# process Chicago data
here::here("original_data/raw_chicago.csv") %>% 
  read_csv() %>% 
  janitor::clean_names() %>% 
  # add date variable
  add_date_var("date", "mdY T", "America/Chicago") %>% 
  # filter by year
  filter_by_year(yearFirst, yearLast) %>% 
  # add crime categories
  join_nibrs_cats(
    here::here("analysis_data/categories_chicago.csv"),
    by = c("primary_type", "description")
  ) %>% 
  # separate burglaries and robberies into sub-categories
  mutate(nibrs_offense_type = case_when(
    # residential burglary
    # CONSIDER ADDING "HOTEL/MOTEL", "RESIDENTIAL YARD (FRONT/BACK)", 
    # "BOAT/WATERCRAFT", "DRIVEWAY - RESIDENTIAL"
    nibrs_offense_code == "220" & location_description %in% c(
      "RESIDENCE", "APARTMENT", "RESIDENCE-GARAGE", "CHA APARTMENT", 
      "RESIDENCE PORCH/HALLWAY", "NURSING HOME/RETIREMENT HOME", 
      "COLLEGE/UNIVERSITY RESIDENCE HALL"
    ) ~ 
      "22A Residential Burglary/Breaking & Entering",
    nibrs_offense_code == "220" ~ 
      "22B Non-residential Burglary/Breaking & Entering",
    nibrs_offense_code == "120" & location_description %in% 
      c("SMALL RETAIL STORE", "GAS STATION", "RESTAURANT", 
        "GROCERY FOOD STORE", "BANK", "CONVENIENCE STORE", "DEPARTMENT STORE", 
        "DRUG STORE", "TAVERN/LIQUOR STORE", "BARBERSHOP", "CURRENCY EXCHANGE", 
        "CLEANING STORE", "BAR OR TAVERN", "COMMERCIAL / BUSINESS OFFICE", 
        "CAR WASH", "VEHICLE-COMMERCIAL", "CONSTRUCTION SITE", 
        "POLICE FACILITY/VEH PARKING LOT", "CHURCH/SYNAGOGUE/PLACE OF WORSHIP", 
        "WAREHOUSE", "DELIVERY TRUCK", "FACTORY/MANUFACTURING BUILDING", 
        "GOVERNMENT BUILDING/PROPERTY", "APPLIANCE STORE", 
        "MEDICAL/DENTAL OFFICE", "MOVIE HOUSE/THEATER", "PAWN SHOP", 
        "SAVINGS AND LOAN", "NEWSSTAND", "POOL ROOM", 
        "AIRPORT BUILDING NON-TERMINAL - NON-SECURE AREA", "CREDIT UNION", 
        "BOWLING ALLEY", "VEHICLE - DELIVERY TRUCK") ~ "12B Commercial Robbery",
    nibrs_offense_code == "120" & location_description == "OTHER" ~ 
      "12U Other Robbery",
    nibrs_offense_code == "120" ~ "12A Personal Robbery",
    TRUE ~ nibrs_offense_type
  ),
  # where necessary, update the NIBRS code to match the new type
  nibrs_offense_code = case_when(
    nibrs_offense_type == "22A Residential Burglary/Breaking & Entering" ~ "22A",
    nibrs_offense_type == "22B Non-residential Burglary/Breaking & Entering" ~ 
      "22B",
    nibrs_offense_type == "12A Personal Robbery" ~ "12A",
    nibrs_offense_type == "12B Commercial Robbery" ~ "12B",
    nibrs_offense_type == "12U Other Robbery" ~ "12U",
    TRUE ~ nibrs_offense_code
  )) %>% 
  # identify location types
  left_join(
    read_csv("analysis_data/location_types_chicago.csv", 
             col_types = cols(.default = col_character())),
    by = c("location_description" = "Location Description")
  ) %>% 
  # add city name
  mutate(city_name = "Chicago") %>% 
  # select core variables
  select(one_of(common_vars)) %>% 
  # save data
  write_rds(here::here("analysis_data/crime_data_chicago.rds"), compress = "gz")



# process Los Angeles data
dir("original_data", pattern = "raw_los_angeles", full.names = TRUE) %>% 
  map_dfr(
    read_csv, 
    col_types = cols(
      .default = col_character(),
      `Crm Cd 1` = col_integer(),
      `Crm Cd 2` = col_integer(),
      `Crm Cd 3` = col_integer(),
      `Crm Cd 4` = col_integer()
    )
  ) %>% 
  janitor::clean_names() %>% 
  # convert from wide to long
  # This code converts the four crime-code fields into wide format while 
  # duplicating all the other columns across each new row. The `gather` function 
  # requires that all variables to be left alone be specified, so a `setdiff` 
  # function is used to return all the variables that are not one of the 
  # crime-code fields.
  gather(key = "which_crime", value = "crime_type", crm_cd_1, crm_cd_2, crm_cd_3, crm_cd_4) %>% 
  filter(!is.na(crime_type)) %>% 
  mutate(date_occurred = paste(str_sub(date_occ, 0, 10), time_occ)) %>% 
  add_date_var("date_occurred", "mdY HM", "America/Los_Angeles") %>%
  filter_by_year(yearFirst, yearLast) %>%
  mutate(
    # convert empty `MO Codes` fields from NA to an empty string, which makes it 
    # easier to search that field later
    mocodes = if_else(is.na(mocodes), "", mocodes),
    nibrs_offense_type = case_when(
      
      # Arson
      crm_cd_desc %in% c('ARSON') ~ '200 Arson',
      
      # Assault
      crm_cd_desc %in% c(
        'ASSAULT WITH DEADLY WEAPON ON POLICE OFFICER', 
        'ASSAULT WITH DEADLY WEAPON, AGGRAVATED ASSAULT', 
        'INTIMATE PARTNER - AGGRAVATED ASSAULT', 
        'CHILD ABUSE (PHYSICAL) - AGGRAVATED ASSAULT') ~ 
        '13A Aggravated assault',
      crm_cd_desc %in% c(
        'BATTERY - SIMPLE ASSAULT', 'BATTERY ON A FIREFIGHTER', 
        'BATTERY POLICE (SIMPLE)', 'INTIMATE PARTNER - SIMPLE ASSAULT', 
        'OTHER ASSAULT', 'CHILD ABUSE (PHYSICAL) - SIMPLE ASSAULT', 
        'RESISTING ARREST') ~ '13B Simple assault',
      crm_cd_desc %in% c(
        'CRIMINAL THREATS - NO WEAPON DISPLAYED', 'STALKING', 'BOMB SCARE', 
        'THREATENING PHONE CALLS/LETTERS', 
        "LETTERS, LEWD  -  TELEPHONE CALLS, LEWD") ~ '13C Intimidation',
      
      # Bribery
      crm_cd_desc %in% c('BRIBERY') ~ '510 Bribery',
      
      # Burglary/Breaking & Entering
      crm_cd_desc %in% c('BURGLARY', 'BURGLARY, ATTEMPTED') &
        premis_desc %in% c(
          "SINGLE FAMILY DWELLING", 
          "MULTI-UNIT DWELLING (APARTMENT, DUPLEX, ETC)", "GARAGE/CARPORT", 
          "OTHER RESIDENCE", "CONDOMINIUM/TOWNHOUSE", 
          "MOBILE HOME/TRAILERS/CONSTRUCTION TRAILERS/RV'S/MOTORHOME", 
          "NURSING/CONVALESCENT/RETIREMENT HOME", 
          "FRAT HOUSE/SORORITY/DORMITORY", "PROJECT/TENEMENT/PUBLIC HOUSING", 
          "GROUP HOME", "PORCH, RESIDENTIAL", "FOSTER HOME BOYS OR GIRLS*") ~ 
        "22A Residential Burglary/Breaking & Entering",
      crm_cd_desc %in% c('BURGLARY', 'BURGLARY, ATTEMPTED') ~ 
        "22B Non-residential Burglary/Breaking & Entering",
      
      # Counterfeiting/Forgery
      crm_cd_desc %in% c('COUNTERFEIT', 'DOCUMENT FORGERY / STOLEN FELONY') ~ 
        '250 Counterfeiting/Forgery',
      
      # Destruction/Damage/Vandalism of Property (except Arson)
      crm_cd_desc %in% c(
        'VANDALISM - FELONY ($400 & OVER, ALL CHURCH VANDALISMS) 0114', 
        'VANDALISM - MISDEAMEANOR ($399 OR UNDER)', 
        'TELEPHONE PROPERTY - DAMAGE', "TRAIN WRECKING", 
        "VANDALISM - FELONY ($400 & OVER, ALL CHURCH VANDALISMS)") ~ 
        '290 Destruction/Damage/Vandalism of Property (except Arson)',
      
      # Embezzlement
      crm_cd_desc %in% c(
        'EMBEZZLEMENT, GRAND THEFT ($950.01 & OVER)', 
        'EMBEZZLEMENT, PETTY THEFT ($950 & UNDER)', 
        'DISHONEST EMPLOYEE - GRAND THEFT', 'DISHONEST EMPLOYEE - PETTY THEFT', 
        'DISHONEST EMPLOYEE ATTEMPTED THEFT') ~ '270 Embezzlement',
      
      # Extortion/Blackmail
      crm_cd_desc %in% c('EXTORTION') ~ '210 Extortion/Blackmail',
      
      # Fraud Offenses (except Counterfeiting/Forgery and Bad Checks)
      crm_cd_desc %in% c(
        'DEFRAUDING INNKEEPER/THEFT OF SERVICES, $400 & UNDER', 
        'DEFRAUDING INNKEEPER/THEFT OF SERVICES, OVER $400', 'BUNCO, ATTEMPT', 
        'BUNCO, GRAND THEFT', 'BUNCO, PETTY THEFT', 
        'GRAND THEFT / INSURANCE FRAUD') ~ 
        '26A False Pretenses/Swindle/Confidence Game',
      crm_cd_desc %in% c('CREDIT CARDS, FRAUD USE ($950 & UNDER', 
                                    'CREDIT CARDS, FRAUD USE ($950.01 & OVER)') ~ 
        '26B Credit Card/Automated Teller Machine Fraud',
      crm_cd_desc %in% c('THEFT OF IDENTITY') ~ 
        '26C Impersonation',
      crm_cd_desc %in% c('UNAUTHORIZED COMPUTER ACCESS') ~ 
        '26E Wire Fraud',
      
      # Homicide Offenses
      crm_cd_desc %in% c('CRIMINAL HOMICIDE') ~ 
        '09A Murder and Nonnegligent Manslaughter',
      crm_cd_desc %in% c('MANSLAUGHTER, NEGLIGENT') ~ 
        '09B Negligent Manslaughter',
      
      # Kidnapping/Abduction
      crm_cd_desc %in% c('CHILD STEALING', 'FALSE IMPRISONMENT', 
                                    'KIDNAPPING', 'KIDNAPPING - GRAND ATTEMPT') ~ 
        '100 Kidnapping/Abduction',
      
      # Larceny/Theft Offenses
      crm_cd_desc %in% c('PICKPOCKET', 'PICKPOCKET, ATTEMPT', 
                                    'DRUNK ROLL', 'DRUNK ROLL - ATTEMPT') ~ '23A Pocket-picking',
      crm_cd_desc %in% c('PURSE SNATCHING', 
                                    'PURSE SNATCHING - ATTEMPT') ~ '23B Purse-snatching',
      crm_cd_desc %in% c('SHOPLIFTING - ATTEMPT', 
                                    'SHOPLIFTING - PETTY THEFT ($950 & UNDER)', 
                                    'SHOPLIFTING-GRAND THEFT ($950.01 & OVER)') ~ '23C Shoplifting',
      crm_cd_desc %in% c('TILL TAP - ATTEMPT', 
                                    'TILL TAP - GRAND THEFT ($950.01 & OVER)', 
                                    'TILL TAP - PETTY ($950 & UNDER)') ~ '23D Theft From Building',
      crm_cd_desc %in% c('THEFT, COIN MACHINE - ATTEMPT', 
                                    'THEFT, COIN MACHINE - GRAND ($950.01 & OVER)', 
                                    'THEFT, COIN MACHINE - PETTY ($950 & UNDER)') ~ 
        '23E Theft From Coin-Operated Machine or Device',
      crm_cd_desc %in% c('THEFT FROM MOTOR VEHICLE - ATTEMPT', 
                                    'THEFT FROM MOTOR VEHICLE - GRAND ($400 AND OVER)', 
                                    'THEFT FROM MOTOR VEHICLE - PETTY ($950 & UNDER)', 
                                    'BURGLARY FROM VEHICLE', 'BURGLARY FROM VEHICLE, ATTEMPTED') ~ 
        '23F Theft From Motor Vehicle (except Theft of Motor Vehicle Parts or Accessories)',
      crm_cd_desc %in% c('THEFT PLAIN - ATTEMPT', 
                                    'THEFT PLAIN - PETTY ($950 & UNDER)', 
                                    'THEFT-GRAND ($950.01 & OVER)EXCPT,GUNS,FOWL,LIVESTK,PROD0036', 
                                    'BIKE - ATTEMPTED STOLEN', 'BIKE - STOLEN', 'BOAT - STOLEN', 
                                    'GRAND THEFT / AUTO REPAIR', 'PETTY THEFT - AUTO REPAIR', 
                                    'THEFT, PERSON', 'THEFT FROM PERSON - ATTEMPT', 
                                    "THEFT-GRAND ($950.01 & OVER)EXCPT,GUNS,FOWL,LIVESTK,PROD") ~ 
        '23H All Other Larceny',
      # some 23A and 23B offences are coded using a combination of crime codes 
      # and MO codes, so have to be done after the other larceny categories are 
      # created
      crm_cd_desc %in% c('THEFT, PERSON', 
                                    'THEFT FROM PERSON - ATTEMPT') & 
        str_count(mocodes, pattern = '0357') > 0 ~ '23A Pocket-picking',
      crm_cd_desc %in% c('THEFT, PERSON', 'THEFT FROM PERSON - ATTEMPT') & 
        str_count(mocodes, pattern = '0336|0337|0341|0342|0343|0346|0355') > 0 ~ 
        '23B Purse-snatching',
      # 23G offences are identified using an MO code that can be present in 
      # several crime categories, so this assignment has to be done after all 
      # the other theft categories are created
      crm_cd_desc %in% c(
        'BURGLARY FROM VEHICLE', 'BURGLARY FROM VEHICLE, ATTEMPTED', 
        'THEFT FROM MOTOR VEHICLE - ATTEMPT', 
        'THEFT FROM MOTOR VEHICLE - GRAND ($400 AND OVER)', 
        'THEFT FROM MOTOR VEHICLE - PETTY ($950 & UNDER)', 
        'THEFT PLAIN - ATTEMPT', 'THEFT PLAIN - PETTY ($950 & UNDER)', 
        'THEFT-GRAND ($950.01 & OVER)EXCPT,GUNS,FOWL,LIVESTK,PROD0036'
      ) & 
        str_count(mocodes, pattern = '0385') > 0 ~ 
        '23G Theft of Motor Vehicle Parts or Accessories',
      
      # Motor Vehicle Theft
      crm_cd_desc %in% c('VEHICLE - STOLEN', 
                                    'VEHICLE - ATTEMPT STOLEN', 'DRIVING WITHOUT OWNER CONSENT (DWOC)') ~ 
        '240 Motor Vehicle Theft',
      
      # Prostitution Offenses
      crm_cd_desc %in% c('PIMPING') ~ 
        '40B Assisting or Promoting Prostitution',
      crm_cd_desc %in% c('PANDERING') ~ 
        '40C Purchasing Prostitution',
      
      # Robbery
      crm_cd_desc %in% c('ROBBERY', 'ATTEMPTED ROBBERY') & 
        premis_desc %in% c(
          "STREET", "SIDEWALK", "PARKING LOT", 
          "MULTI-UNIT DWELLING (APARTMENT, DUPLEX, ETC)", 
          "SINGLE FAMILY DWELLING", "MARKET", "ALLEY", "BUS STOP", 
          "PARK/PLAYGROUND", "DRIVEWAY", "HIGH SCHOOL", 
          "YARD (RESIDENTIAL/BUSINESS)", "VEHICLE, PASSENGER/TRUCK", 
          "BUS STOP OR LAYOVER", "GARAGE/CARPORT", "JUNIOR HIGH SCHOOL",
          "PARKING UNDERGROUND/BUILDING", "AUTOMATED TELLER MACHINE (ATM)",
          "BEACH", "PROJECT/TENEMENT/PUBLIC HOUSING", "OTHER RESIDENCE",
          "ELEMENTARY SCHOOL", "PORCH, RESIDENTIAL", 
          "CHARTER BUS AND PRIVATELY OWNED BUS", "FREEWAY", 
          "PUBLIC RESTROOM/OUTSIDE*", "STAIRWELL*", "MTA BUS", 
          "PUBLIC RESTROOM(INDOORS-INSIDE)", "RIVER BED*", 
          "CONDOMINIUM/TOWNHOUSE", "SKATEBOARD FACILITY/SKATEBOARD PARK*",
          "COLLEGE/JUNIOR COLLEGE/UNIVERSITY", "TRAIN", 
          "MOBILE HOME/TRAILERS/CONSTRUCTION TRAILERS/RV'S/MOTORHOME",
          "MTA PROPERTY OR PARKING LOT", "UNDERPASS/BRIDGE*", "ELEVATOR",
          "GROUP HOME", "PATIO*", "PEDESTRIAN OVERCROSSING", 
          "SPECIALTY SCHOOL/OTHER", "METROLINK TRAIN", "VACANT LOT", 
          "ABANDONED BUILDING ABANDONED HOUSE", "REDLINE ENTRANCE/EXIT",
          "TRANSPORTATION FACILITY (AIRPORT)", "BUS, SCHOOL, CHURCH", 
          "TRAIN TRACKS", "TUNNEL", "OTHER RR TRAIN (UNION PAC, SANTE FE ETC",
          "PAY PHONE", "REDLINE SUBWAY PLATFORM", "TERMINAL", 
          "MUNICIPAL BUS LINE INCLUDES LADOT/DASH", 
          "POOL-PUBLIC/OUTDOOR OR INDOOR*", "PRIVATE SCHOOL/PRESCHOOL",
          "REDLINE (SUBWAY TRAIN)", "SLIPS/DOCK/MARINA/BOAT", 
          "BLUE LINE (ABOVE GROUND SURFACE TRAIN)", 
          "FOSTER HOME BOYS OR GIRLS*", "FRAT HOUSE/SORORITY/DORMITORY", 
          "GOLF COURSE*", "GREEN LINE (I-105 FWY LEVEL TRAIN)") 
      ~ "12A Personal Robbery",
      crm_cd_desc %in% c('ROBBERY', 'ATTEMPTED ROBBERY') & 
        (premis_desc %in% c("OTHER PREMISE", "OTHER/OUTSIDE") | 
           is.na(premis_desc)) ~ "12U Other Robbery",
      crm_cd_desc %in% c('ROBBERY', 'ATTEMPTED ROBBERY') ~ 
        "12B Commercial Robbery",
      
      # Sex Offenses
      crm_cd_desc %in% c('RAPE, ATTEMPTED', 'RAPE, FORCIBLE') ~ 
        '11A Rape (except Statutory Rape)',
      crm_cd_desc %in% c('SODOMY/SEXUAL CONTACT B/W PENIS OF ONE PERS TO ANUS OTH 0007=02',
                         'ORAL COPULATION') ~ '11B Sodomy',
      crm_cd_desc %in% c('SEXUAL PENTRATION WITH A FOREIGN OBJECT') ~
        '11C Sexual Assault With An Object',
      crm_cd_desc %in% c('BATTERY WITH SEXUAL CONTACT') ~ 
        '11D Fondling',
      
      # Sex Offenses, Nonforcible
      crm_cd_desc %in% c('INCEST (SEXUAL ACTS BETWEEN BLOOD RELATIVES)') ~ 
        '36A Incest',
      crm_cd_desc %in% c('SEX, UNLAWFUL') ~ '36B Statutory Rape',
      # The offence of 'CRM AGNST CHLD (13 OR UNDER) (14-15 & SUSP 10 YRS OLDER)0060'
      # is problematic because it covers a wide array of behaviour. Offenses
      # matching specific criteria are allocated to particular categories, with
      # all remaining offences being allocated to category 90Z. The specific 
      # offences are ordered by decreasing severity, since an offence may match 
      # multiple MO codes.
      crm_cd_desc %in% c('CRM AGNST CHLD (13 OR UNDER) (14-15 & SUSP 10 YRS OLDER)0060') 
      & str_count(mocodes, pattern = '0527|0533') > 0 ~ '36B Statutory Rape',
      crm_cd_desc %in% c('CRM AGNST CHLD (13 OR UNDER) (14-15 & SUSP 10 YRS OLDER)0060') 
      & str_count(mocodes, pattern = '0507|0512|0519|0521|0539|0540|0541|0548|0549') > 0
      ~ '11B Sodomy',
      crm_cd_desc == "SODOMY/SEXUAL CONTACT B/W PENIS OF ONE PERS TO ANUS OTH" 
      ~ "11C Sexual Assault With An Object",
      crm_cd_desc %in% c('CRM AGNST CHLD (13 OR UNDER) (14-15 & SUSP 10 YRS OLDER)0060') 
      & str_count(mocodes, pattern = '0515') > 0 ~ 
        '11C Sexual Assault With An Object',
      crm_cd_desc %in% c("SEX,UNLAWFUL(INC MUTUAL CONSENT, PENETRATION W/ FRGN OBJ", 
                         "SEX,UNLAWFUL(INC MUTUAL CONSENT, PENETRATION W/ FRGN OBJ0059",
                         "SEXUAL PENETRATION W/FOREIGN OBJECT") ~ 
        "11C Sexual Assault With An Object",
      crm_cd_desc %in% c('CRM AGNST CHLD (13 OR UNDER) (14-15 & SUSP 10 YRS OLDER)0060') 
      & str_count(mocodes, pattern = '0500|0501|0502|0503|0504|0505|0506|0509|0510|0511|0517|0518|0522|0528|0532|0537|0543|0544|0550|0551') > 0 
      ~ '11D Fondling',
      crm_cd_desc %in% c('CRM AGNST CHLD (13 OR UNDER) (14-15 & SUSP 10 YRS OLDER)0060') 
      & str_count(mocodes, pattern = '0529|0538') > 0 ~ 
        '90C Disorderly Conduct',
      crm_cd_desc %in% c('CRM AGNST CHLD (13 OR UNDER) (14-15 & SUSP 10 YRS OLDER)0060') 
      ~ '90Z All Other Offenses',
      
      # Pornography/Obscene Material
      crm_cd_desc == "CHILD PORNOGRAPHY" ~ 
        "370 Pornography/Obscene Material",
      
      # Weapon Law Violations
      crm_cd_desc %in% c(
        'BRANDISH WEAPON', 'WEAPONS POSSESSION/BOMBING', 
        'FIREARMS EMERGENCY PROTECTIVE ORDER (FIREARMS EPO)', 
        'DISCHARGE FIREARMS/SHOTS FIRED', 
        'REPLICA FIREARMS(SALE,DISPLAY,MANUFACTURE OR DISTRIBUTE)0132', 
        'SHOTS FIRED AT INHABITED DWELLING', 
        'SHOTS FIRED AT MOVING VEHICLE, TRAIN OR AIRCRAFT', 
        "FIREARMS RESTRAINING ORDER (FIREARMS RO)",
        "REPLICA FIREARMS(SALE,DISPLAY,MANUFACTURE OR DISTRIBUTE)") ~ 
        '520 Weapon Law Violations',
      
      # Human Trafficking
      crm_cd_desc == "HUMAN TRAFFICKING - COMMERCIAL SEX ACTS" ~ 
        "64A Human Trafficking, Commercial Sex Acts",
      crm_cd_desc == "HUMAN TRAFFICKING - INVOLUNTARY SERVITUDE" ~ 
        "64B Human Trafficking, Involuntary Servitude",
      
      # Bad Checks (except Counterfeit Checks or Forged Checks)
      crm_cd_desc %in% c('DOCUMENT WORTHLESS ($200 & UNDER)', 
                                    'DOCUMENT WORTHLESS ($200.01 & OVER)') ~ 
        '90A Bad Checks (except Counterfeit Checks or Forged Checks)',
      
      # Curfew/Loitering/Vagrancy Violations
      crm_cd_desc %in% c('FAILURE TO DISPERSE', 
                                    'BLOCKING DOOR INDUCTION CENTER') ~ 
        '90B Curfew/Loitering/Vagrancy Violations',
      
      # Disorderly Conduct
      crm_cd_desc %in% c('DISTURBING THE PEACE', 'DISRUPT SCHOOL', 
                         'INCITING A RIOT', 'INDECENT EXPOSURE', 'LEWD CONDUCT', 
                         'THROWING OBJECT AT MOVING VEHICLE') ~ 
        '90C Disorderly Conduct',
      
      # Family Offenses, Nonviolent
      crm_cd_desc %in% c('CHILD ABANDONMENT', 
                                    'CHILD NEGLECT (SEE 300 W.I.C.)', 
                                    "CRM AGNST CHLD (13 OR UNDER) (14-15 & SUSP 10 YRS OLDER)",
                                    "LEWD/LASCIVIOUS ACTS WITH CHILD") ~ 
        '90F Family Offenses, Nonviolent',
      
      # Peeping Tom
      crm_cd_desc %in% c('PEEPING TOM') ~ '90H Peeping Tom',
      
      # Trespass of Real Property
      crm_cd_desc %in% c('PROWLER', 'TRESPASSING') ~ 
        '90J Trespass of Real Property',
      
      # All Other Offenses
      crm_cd_desc %in% c(
        'ABORTION/ILLEGAL', 
        'BEASTIALITY, CRIME AGAINST NATURE SEXUAL ASSLT WITH ANIM0065', 
        'BIGAMY', 'CHILD ANNOYING (17YRS & UNDER)', 'CONSPIRACY', 
        'CONTEMPT OF COURT', 'CONTRIBUTING', 'CRUELTY TO ANIMALS', 
        'FALSE POLICE REPORT', 'ILLEGAL DUMPING', 'LYNCHING', 
        'LYNCHING - ATTEMPTED', 'VIOLATION OF COURT ORDER', 
        'VIOLATION OF RESTRAINING ORDER', 
        'VIOLATION OF TEMPORARY RESTRAINING ORDER', 'LETTERS, LEWD', 
        'OTHER MISCELLANEOUS CRIME'
      ) ~ '90Z All Other Offenses',
      is.na(crm_cd_desc) ~ "90Z All Other Offenses",
      
      # Not-categorisable offences
      # Where it is clear that not all the offences that should be present are 
      # present in the data, it would be misleading to populate those 
      # categories. Offences within such categories that are present are 
      # categorised here so that they can be stripped from the analysis.
      crm_cd_desc %in% c('DRUGS, TO A MINOR') ~ 
        '99X Non-Categorisable Offenses',
      
      # Non-reportable crimes
      crm_cd_desc %in% c('FAILURE TO YIELD', 'RECKLESS DRIVING') ~
        '99Y Non-Reportable Crimes'
      
    )
  ) %>% 
  left_join(read_csv(here::here("analysis_data/nibrs_categories.csv")), 
            by = "nibrs_offense_type") %>%
  # where necessary, update the NIBRS code to match the new type
  mutate(nibrs_offense_code = case_when(
    nibrs_offense_type == "22A Residential Burglary/Breaking & Entering" ~ "22A",
    nibrs_offense_type == "22B Non-residential Burglary/Breaking & Entering" ~
      "22B",
    nibrs_offense_type == "12A Personal Robbery" ~ "12A",
    nibrs_offense_type == "12B Commercial Robbery" ~ "12B",
    nibrs_offense_type == "12U Other Robbery" ~ "12U",
    TRUE ~ nibrs_offense_code
  )) %>%
  # identify location types
  left_join(
    read_csv(here::here("analysis_data/location_types_los_angeles.csv"), 
             col_types = cols(.default = col_character())),
    by = c("premis_desc" = "premise_description")
  ) %>% 
  # add city name
  mutate(city_name = "Los Angeles") %>% 
  # select core variables
  select(one_of(common_vars)) %>% 
  # save data
  write_rds(here::here("analysis_data/crime_data_los_angeles.rds"),
            compress = "gz")



# process Louisville data
dir("original_data", pattern = "raw_louisville", full.names = TRUE) %>% 
  map_dfr(read_csv, col_types = cols(.default = col_character())) %>% 
  janitor::clean_names() %>% 
  # add date variable
  add_date_var("date_occured", "Ymd T", "America/Kentucky/Louisville") %>% 
  # add year variable and remove offenses before start date
  filter_by_year(yearFirst, yearLast) %>% 
  # create NIBRS codes
  left_join(read_csv(here::here("analysis_data/nibrs_categories.csv")), 
            by = c("nibrs_code" = "nibrs_offense_code")) %>% 
  mutate(nibrs_offense_type = case_when(
    
    # homcide
    nibrs_code == "999" & uor_desc %in% c("FETAL HOMICIDE - 1ST DEGREE",
                                          "MURDER - DOMESTIC VIOLENCE") ~ 
      "09A Murder and Nonnegligent Manslaughter",
    nibrs_code == "999" & uor_desc %in% c("RECKLESS HOMICIDE") ~ 
      "09B Negligent Manslaughter",
    
    # kidnapping
    nibrs_code == "999" & uor_desc %in% c(
      "UNLAWFUL IMPRISONMENT-1ST DEGREE", "UNLAWFUL IMPRISONMENT-2ND DEGREE", 
      "KIDNAPPING-WITH SERIOUS PHYSICAL INJURY") ~ "100 Kidnapping/Abduction",
    
    # sex offences
    # it seems to be safe to search on the word 'rape', since there aren't any
    # crimes coded as 999 and described as statutory rape
    nibrs_code == "999" & str_detect(uor_desc, "\\bRAPE\\b") == TRUE ~ 
      "11A Rape (except Statutory Rape)",
    nibrs_code == "999" & uor_desc %in% c(
      "SODOMY - 1ST DEGREE", "SODOMY - 1ST DEGREE - DOMESTIC VIOLENCE", 
      "SODOMY - 1ST DEGREE - VICTIM U/12 YEARS OF AGE") ~ "11B Sodomy",
    nibrs_code == "999" & uor_desc %in% c(
      "SEXUAL ABUSE - 1ST DEGREE", "SEXUAL ABUSE - 1ST DEGREE- VICTIM U/12 YOA", 
      "SEXUAL ABUSE - 3RD DEGREE") ~ "11D Fondling",
    
    # robbery
    (nibrs_code == "120" | str_detect(uor_desc, "\\bROBBERY\\b")) & 
      premise_type %in% c(
        "HIGHWAY / ROAD / ALLEY", "RESIDENCE / HOME", "PARKING LOT / GARAGE", 
        "OTHER RESIDENCE (APARTMENT/CONDO)", "SCHOOL - ELEMENTARY / SECONDARY", 
        "FIELD / WOODS", "AIR / BUS / TRAIN TERMINAL", "ATM SEPARATE FROM BANK",
        "SCHOOL - COLLEGE / UNIVERSITY") ~ "12A Personal Robbery",
    (nibrs_code == "120" | str_detect(uor_desc, "\\bROBBERY\\b")) & 
      premise_type == "OTHER / UNKNOWN" ~ 
      "12U Other Robbery",
    (nibrs_code == "120" | str_detect(uor_desc, "\\bROBBERY\\b")) ~ 
      "12B Commercial Robbery",
    
    # Assault offences
    # Some assault offences are listed under nibrs_code 999, so must be
    # categorised according to the degree of assault.
    str_detect(uor_desc, "ASSAULT.+?(1ST|2ND)") == TRUE | uor_desc %in% c(
      "CRIMINAL ABUSE-1ST DEGREE", 
      "CRIMINAL ABUSE-1ST DEGREE-CHILD 12 OR UNDER", 
      "CRIMINAL ABUSE-3RD DEGREE", "MURDER - DOMESTIC VIOLENCE ATTEMPTED",
      "MURDER - POLICE OFFICER ATTEMPTED", "MURDER ATTEMPTED") ~ 
      "13A Aggravated assault",
    str_detect(uor_desc, "ASSAULT.+?(3RD|4TH)") == TRUE ~ "13B Simple assault",
    nibrs_code == "999" & uor_desc %in% c(
      "ABUSE OF A TEACHER PROHIBITED", "MENACING", 
      "TERRORISTIC THREATENING 1ST DEGREE", 
      "TERRORISTIC THREATENING 2ND DEGREE", 
      "TERRORISTIC THREATENING 3RD DEGREE", 
      "INTIMIDATING A PARTICIPANT IN LEGAL PROCESS", 
      "HARASSMENT (NO PHYSICAL CONTACT)", 
      "HARASSMENT - PHYSICAL CONTACT - NO INJURY", "HARASSING COMMUNICATIONS",
      "RETALIATING AGAINST PARTICIPANT IN LEGAL PROCESS", "STALKING-1ST DEGREE", 
      "STALKING-2ND DEGREE") ~ "13C Intimidation",
    
    # burglary
    (nibrs_code == "220" | str_detect(uor_desc, "\\bBURGLARY\\b") == TRUE) & 
      premise_type %in% c(
        "RESIDENCE / HOME", "PARKING LOT / GARAGE", 
        "OTHER RESIDENCE (APARTMENT/CONDO)", "ATTACHED RESIDENTIAL GARAGE", 
        "PARKING LOT / GARAGE") ~ 
      "22A Residential Burglary/Breaking & Entering",
    (nibrs_code == "220" | str_detect(uor_desc, "\\bBURGLARY\\b") == TRUE) ~ 
      "22B Non-residential Burglary/Breaking & Entering",
    
    # theft
    nibrs_code == "999" & uor_desc %in% c(
      "THEFT BY UNLAWFUL TAKING/DISP-PURSESNATCHING  FELONY CLASS D") ~ 
      "23B Purse-snatching",
    nibrs_code == "999" & uor_desc %in% c(
      "THEFT BY UNLAWFUL TAKING/DISP-SHOPLIFTING - FELONY CLASS D",
      "THEFT BY UNLAWFUL TAKING/DISP-SHOPLIFTING -MISD") ~ "23C Shoplifting",
    nibrs_code == "999" & uor_desc %in% c(
      "THEFT BY UNLAWFUL TAKING/DISP - FROM BUILDING - MISD", 
      "THEFT BY UNLAWFUL TAKING/DISP-FROM BUILDING - $10,000 OR >",
      "THEFT BY UNLAWFUL TAKING/DISP-FROM BUILDING - FELONY D") ~ "23D Theft From Building",
    nibrs_code == "999" & uor_desc %in% c(
      "THEFT BY UNLAWFUL TAKING/DISP-CONTENTS FROM AUTO - FELONY", 
      "THEFT BY UNLAWFUL TAKING/DISP-CONTENTS FROM AUTO - MISD") ~ 
      "23F Theft From Motor Vehicle (except Theft of Motor Vehicle Parts or Accessories)",
    nibrs_code == "999" & uor_desc %in% c(
      "THEFT BY UNLAW TAKING/DISP-PARTS FROM VEHICLE $10,000 OR >",
      "THEFT BY UNLAWFUL TAKING/DISP-PARTS FROM VEHICLE - MISD",
      "THEFT OF MOTOR VEHICLE REGISTRATION PLATE/DECAL") ~ "23G Theft of Motor Vehicle Parts or Accessories",
    nibrs_code == "999" & uor_desc %in% c(
      "THEFT BY UNLAWFUL TAKING - GASOLINE - 1ST OFFENSE", 
      "THEFT BY UNLAWFUL TAKING - GASOLINE - U/$10,000", 
      "THEFT BY UNLAWFUL TAKING - GASOLINE - U/$500 MISD", 
      "THEFT BY UNLAWFUL TAKING-ALL OTHERS $10,000 OR MORE", 
      "THEFT BY UNLAWFUL TAKING/DISP-ALL OTHERS - FELONY CLASS D", 
      "THEFT BY UNLAWFUL TAKING/DISP-ALL OTHERS -MISD", "THEFT OF CARGO", 
      "THEFT OF CONTROLLED SUBSTANCE", 
      "THEFT OF CONTROLLED SUBSTANCE, 1ST OFFENSE OR UNDER $300", 
      "THEFT OF CONTROLLED SUBSTANCE, 2ND OR  > OFFENSE OR OVER $30", 
      "THEFT OF ID PRIOR TO JULY 2000", 
      "THEFT OF IDENTITY OF ANOTHER WITHOUT CONSENT", "THEFT OF MAIL MATTER", 
      "THEFT OF PROP LOST/MISLAID/DELIVERED BY MISTAKE FELONY D", 
      "THEFT OF PROPERTY LOST/MISLAID/DELIVERED BY MISTAKE MISD",
      "THEFT OF SERVICES - FELONY CLASS D", "THEFT OF SERVICES - MISD",
      "LESSER INCLUDED THEFT", "TBUT OR DISP FIREARM") ~ 
      "23H All Other Larceny",
    
    # motor vehicle theft
    nibrs_code == "999" & uor_desc %in% c(
      "THEFT BY UNLAWFUL TAKING/DISP-AUTO - FELONY CLASS D",
      "THEFT BY UNLAWFUL TAKING/DISP-AUTO - MISD",
      "THEFT BY UNLAWFUL TAKING/DISP-AUTO $10,000 OR MORE") ~ 
      "240 Motor Vehicle Theft",
    
    # forgery
    nibrs_code == "999" & uor_desc %in% c(
      "CRIMINAL POSS OF A FORGED PERSCRIPTION, 1ST OFFENSE",
      "CRIMINAL POSSESSION FORGED INSTRUMENT-1ST DEGREE-IDENTIFY",
      "CRIMINAL POSSESSION FORGED INSTRUMENT-2ND DEGREE-IDENTIFY",
      "CRIMINAL POSSESSION OF FORGED INSTRUMENT-3RD DEGREE",
      "FORGERY - 1ST DEGREE", "FORGERY - 2ND DEGREE", 
      "FORGERY OF A PRESCRIPTION - 1ST OFFENSE", "FORGERY-3RD DEGREE",
      "POSSESSION OF FORGED PRESCRIPTION FOR LEGEND DRUG 1ST",
      "UTTER FALSE/FORGED PRESCRIPTION", 
      "FALSE MAKING/EMBOSSING OF CREDIT CARD", 
      "POSSESSION OF A FORGERY DEVICE-IDENTIFY",
      "PROHIBIT COMMERCE COUNTERFEIT GOODS/SERVICES >25 ITMS 1ST OF") ~ 
      "250 Counterfeiting/Forgery",
    
    # fraud
    nibrs_code == "999" & (str_detect(uor_desc, "\\bFRAUD.+?\\bCARD") == TRUE |
                             uor_desc %in% c("MISUSE OF ELECTRONIC INFO-AUTOMATED BANKING DEVICE, ETC",
                                             "RECEIPT OF CREDIT CARD IN VIOLATION KRS 434.570, 434.610")) ~
      "26B Credit Card/Automated Teller Machine Fraud",
    nibrs_code == "999" & uor_desc %in% c("IMPERSONATING A PEACE OFFICER") ~
      "26C Impersonation",
    nibrs_code == "999" & uor_desc %in% c(
      "UNLAWFUL ACCESS TO COMPUTER-1ST DEGREE") ~ "26E Wire Fraud",
    
    # criminal damage
    nibrs_code == "999" & uor_desc %in% c("CRIMINAL MISCHIEF - 1ST DEGREE",
                                          "CRIMINAL MISCHIEF-2ND DEGREE", "CRIMINAL MISCHIEF-3RD DEGREE",
                                          "CRIMINAL USE OF NOXIOUS SUBSTANCE") ~ 
      "290 Destruction/Damage/Vandalism of Property (except Arson)",
    
    # Drugs
    # There are lots of drug offences listed under nibrs_code 999 (which does
    # not exist) and for which uor_desc involves variations on the phrase
    # possession of a controlled substance (typically abbreivated)
    str_detect(uor_desc, "\\bPOS.+?\\bCON.+?\\bSUB") == TRUE | 
      str_detect(uor_desc, "\\bTRAF.+?\\bCON.+?\\bSUB") == TRUE | 
      str_detect(uor_desc, "\\bTRF.+?\\bCON.+?\\bSUB") == TRUE |
      str_detect(uor_desc, "\\bTRAF.+?\\bMARIJUANA") == TRUE |
      (nibrs_code == "999" & uor_desc %in% c(
        "ASSUME FALSE TITLE TO OBTAIN CONT SUB-1ST OFFENSE",
        "ATT/OBTAIN CONTSUB BY FRAUD/FLS STMT/FORGERY",
        "ATTEMPT/OBTAIN CONT SUB BY FRAUD/FALSE STMT/FORGERY-1ST OFF",
        "CULTIVATE IN MARIJUANA-< 5 PLANTS-1ST OFFENSE",
        "CULTIVATE IN MARIJUANA-< 5 PLANTS-2ND OR > OFFENSE",
        "CULTIVATE IN MARIJUANA-5 PLANTS OR >-1ST OFFENSE",
        "CULTIVATE IN MARIJUANA-5 PLANTS OR >-2ND OR > OFFENSE",
        "MANUFACTURING METHAMPHETAMINE 1ST OFFENSE",
        "MANUFACTURING METHAMPHETAMINE 2ND OR > OFFENSE",
        "POSS OF MARIJUANA", 
        "PRESCRIPTION CONT SUB NOT IN ORIGINAL CONTAINER-1ST OFFENSE",
        "UNLAWFUL DISTRIBUTION OF A METH PRECURSOR - 1ST OFFENSE",
        "UNLAWFUL DISTRIBUTION OF A METH PRECURSOR - 2ND OR > OFFENSE",
        "UNLAWFUL POSSESSION OF METH PRECURSOR 1ST OFFENSE",
        "UNLAWFUL POSSESSION OF METH PRECURSOR 2ND OR > OFFENSE",
        "TRAF IN SYNTHETIC CANNABINOIL AGOINTS OR PIPEAZINES",
        "TRAFFICKING IN A LEGEND DRUG, 1ST OFFENSE", "TRAFFICKING IN SALVIA",
        "ILLEGAL POSSESSION OF A LEGEND DRUG 1ST OFFENSE", 
        "ILLEGAL POSSESSION OF LEGEND DRUG", "POSSESSION OF SALVIA",
        "POSSESSION OF SYNTHETIC CANNABINOID AGONISTS OR PIPEAZINES",
        "SELL CONTROL SUBSTANCE TO A MINOR-1ST OFFENSE",
        "VOLATILE SUBSTANCE ABUSE")) ~ 
      "35A Drug/Narcotic Violations",
    nibrs_code == "999" & uor_desc %in% c("DRUG PARAPHERNALIA - BUY/POSSESS",
                                          "DRUG PARAPHERNALIA-BUY/POSSESS-1ST OFFENSE", 
                                          "DRUG PARAPHERNALIA-BUY/POSSESS-2ND OR > OFFENSE") ~ 
      "35B Drug Equipment Violations",
    
    # non-violent sex offences
    nibrs_code == "999" & uor_desc %in% c(
      "INCEST - VICTIM U/12 YOA OR SERIOUS PHYSICAL INJURY") ~ "36A Incest",
    
    # stolen property
    nibrs_code == "999" & (str_detect(uor_desc, "STOLEN.+?PROPERTY") == TRUE |
                             uor_desc %in% c("THEFT-RECEIPT OF STOLEN CREDIT/DEBIT CARD-1 CARD",
                                             "THEFT-RECEIPT OF STOLEN CREDIT/DEBIT CARD-2 OR MORE CARDS",
                                             "TRAFFICKING IN STOLEN IDENTITIES", 
                                             "OBSCURING THE IDENTITY OF A MACHINE U/$10000",
                                             "OBSCURING THE IDENTITY OF A MACHINE U/$500")) ~ 
      "280 Stolen Property Offenses",
    
    # obscene material
    nibrs_code == "999" & uor_desc %in% c(
      "DIST OF MATTER PORTRAYING SEXUAL PERFORM BY MINOR 2ND > OFF",
      "POSSESSION OF MATTER PORTRAYING SEX PERFORMANCE BY MINOR") ~ 
      "370 Pornography/Obscene Material",
    
    # gambling
    nibrs_code == "999" & uor_desc %in% c(
      "POSSESS OF GAMBLING RECORD-1ST DEGREE", 
      "POSSESSION OF A GAMBLING DEVICE") ~ "39C Gambling Equipment Violations",
    nibrs_code == "999" & uor_desc %in% c("PROMOTING GAMBLING - 2ND DEGREE") ~
      "39B Operating/Promoting/Assisting Gambling",
    
    # prostitution
    nibrs_code == "999" & uor_desc %in% c(
      "LOITERING FOR PROSTITUTION PURPOSES - 1ST OFFENSE",
      "LOITERING FOR PROSTITUTION PURPOSES - 2ND > OFFENSE", "PROSTITUTION") ~ 
      "40A Prostitution",
    
    # weapons violations
    nibrs_code == "999" & uor_desc %in% c("CARRYING A CONCEALED DEADLY WEAPON",
                                          "DEFACING A FIREARM", "POSSESSION OF DEFACED FIREARM", 
                                          "POSSESSION OF FIREARM BY CONVICTED FELON", 
                                          "POSSESSION OF HANDGUN BY CONVICTED FELON",
                                          "UNLAWFUL POSSESSION OF WEAPON ON SCHOOL PROPERTY",
                                          "UNLAWFULLY PROV/PERMIT MINOR TO POSSESS HANDGUN",
                                          "USING RESTRICTED AMMO DURING A FELONY (NO SHOTS)", 
                                          "POSS HANDGUN BY MINOR 1ST OFFENSE", 
                                          "POSS,MANUF,TRANSPRT HANDGUN WITH EXC FOR UNLAWFUL",
                                          "POSS,MANUF,TRANSPRT HANDGUN WITH EXC FOR UNLAWFUL 2ND OFFENS") ~ 
      "520 Weapon Law Violations",
    
    # bad checks
    nibrs_code == "999" & uor_desc %in% c("THEFT BY DECEPTION-INCL COLD CHECKS",
                                          "THEFT BY DECEPTION-INCL COLD CHECKS $10,000 OR MORE",
                                          "THEFT BY DECEPTION-INCL COLD CHECKS U/$10,000",
                                          "THEFT BY DECEPTION-INCL COLD CHECKS U/$500") ~ 
      "90A Bad Checks (except Counterfeit Checks or Forged Checks)",
    
    # loitering
    nibrs_code == "999" & uor_desc %in% c("JUVENILE CURFEW ORD 17 & UNDER") ~ 
      "90B Curfew/Loitering/Vagrancy Violations",
    
    # Disorderly conduct
    uor_desc %in% c('INDECENT EXPOSURE - 1ST DEGREE - 1ST OFFENSE',
                    'INDECENT EXPOSURE - 1ST DEGREE - 2ND OFFENSE', 
                    'INDECENT EXPOSURE - 1ST DEGREE - 3RD OFFENSE', 
                    'INDECENT EXPOSURE - 1ST DEGREE - 4TH OR > OFFENSE', 
                    'INDECENT EXPOSURE - 2ND DEGREE') ~ '90C Disorderly Conduct',
    
    # Child abuse (non-violent)
    nibrs_code == "999" & uor_desc %in% c(
      "CONTROLLED SUBSTANCE ENDANGERMENT TO CHILD 2ND DEGREE",
      "CONTROLLED SUBSTANCE ENDANGERMENT TO CHILD 4TH DEGREE") ~ 
      "90F Family Offenses, Nonviolent",
    
    # trespass
    nibrs_code == "999" & uor_desc %in% c("CRIMINAL TRESPASSING-3RD DEGREE") ~ 
      "90J Trespass of Real Property",
    
    # All other offences
    uor_desc %in% c("ANY FELONY CATEGORY NOT LISTED", 
                    "ADVERTISING MATTER PORTRAY SEX PERFORMANCE BY MINOR 1ST OFF",
                    "CONTEMPT OF COURT - VIOL EMERGENCY PROTECTIVE ORDER",
                    "CRUELTY TO ANIMALS - 2ND DEGREE", "DANGEROUS ANIMAL",
                    "DANGEROUS DOG-CONTROL OF DOG", "DISARMING A PEACE OFFICER",
                    "FAILURE TO COMPLY W/SEX OFFENDER REGISTRATION - 1ST OFF",
                    "FLEEING OR EVADING POLICE - 1ST DEGREE (MOTOR VEHICLE)",
                    "FLEEING OR EVADING POLICE - 1ST DEGREE (ON FOOT)",
                    "FLEEING OR EVADING POLICE - 2ND DEGREE (MOTOR VEHICLE)",
                    "ILLEGAL DUMPING LOUISVILLE METRO ORDINANCE",
                    "LEAVING SCENE OF ACCIDENT - HIT & RUN",
                    "LEAVING SCENE OF ACCIDENT/FAILURE TO RENDER AID OR ASSISTANC",
                    "OBSTRUCTING GOVERNMENTAL OPERATIONS",
                    "WANTON ENDANGERMENT-1ST DEGREE", 
                    "WANTON ENDANGERMENT-1ST DEGREE-POLICE OFFICER", 
                    "WANTON ENDANGERMENT-2ND DEGREE",
                    "WANTON ENDANGERMENT-2ND DEGREE-POLICE OFFICER",
                    "VIOLATING GRAVES", "VIOLATION UNKNOWN", 
                    "WANTON ABUSE/NEGLECT OF ADULT BY PERON", 
                    "INTERFERE W/ENFORCEMENT PROHI", "INTERMEDIATE LICENSING VIOLATIONS",
                    "LICENSEE PERMIT NUDE PERFORMA", "OFFENSES AGAINST PUBLIC PEACE",
                    "OWNER TO CONTROL ANIMAL", "PROBATION VIOLATION (FOR FELONY OFFENSE)",
                    "PROBATION VIOLATION (FOR MISDEMEANOR OFFENSE)", 
                    "PROBATION VIOLATION (JUVENILE PUBLIC OFFENSE)",
                    "VIOLATION OF KENTUCKY EPO/DVO") ~ 
      "90Z All Other Offenses",
    
    # Non-categorisable offences
    uor_desc %in% c("DOMESTIC VIOLENCE INVOLVED INCIDENT") ~ 
      "99X Non-Categorisable Offenses",
    
    # Non-reportable crimes
    uor_desc %in% c('DISPLAY OF ILLEGAL/ALTERED REGISTRATION PLATE',
                    'FAIL OF NON-OWNER/OPER TO MAINTAIN REQ INS/SECURITY 1ST OFF',
                    'FAIL OF TRANSFEREE OF VEH TO PROMPTLY APPLY FOR NEW TITLE',
                    'FAILURE OF OWNER TO MAINTAIN REQUIRED INS/SECURITY 1ST OFF',
                    'FAILURE OF OWNER TO MAINTAIN REQUIRED INS/SECURITY 2ND OFF',
                    'FAILURE OF SELLER TO DELIVER REGISTRATION W/ASSIGNMENT FORM',
                    'IMPROPER EMERGENCY/SAFETY LIGHTS',
                    'LEAV SCENE OF ACCIDENT/FAIL TO RNDR AID/ASSIST W/DEATH OR SJ',
                    'MOTOR VEH DEALER REQ TO GIVE TITLE TO PURCHASER',
                    'MOTOR VEHICLE DEALER LICENSE REQUIRED',
                    'POSSESSING LICENSE WHEN PRIVILEGES ARE REVOKED', 
                    "OPERATING ON SUSPENDED/REVOKED OPERATORS LICENSE", "RECKLESS DRIVING",
                    "FAILURE TO OR IMPROPER SIGNAL", "NO OR EXPIRED REGISTRATION PLATES",
                    "NO OPERATORS/MOPED LICENSE", "FAILURE TO WEAR SEAT BELTS",
                    "FAILURE TO REGISTER TRANSFER OF MOTOR VEHICLE", 
                    "FAILURE TO NOTIFY ADDRESS CHANGE TO DEPT OF TRANSPORTATION",
                    "DISREGARDING STOP SIGN", "REAR LICENSE NOT ILLUMINATED",
                    "DISREGARDING TRAFFIC CONTROL DEVICE, TRAFFIC LIGHT", "CARELESS DRIVING",
                    "EXCESSIVE WINDSHIELD/ WINDOW TINTING", "FAILURE TO ILLUMINATE HEAD LAMPS",
                    "IMPROPER DISPLAY OF REGISTRATION PLATES", "NO CARRIER PERMIT",
                    "FAIL OF TRANSFEROR OF VEH TO PROP EXECUTE TITLE",
                    "FAILURE TO NOTIFY OWNER OF DAMAGE TO UNATTENDED VEHICLE",
                    "UNAUTHORIZED USE OF VEHICLE UNDER HARDSHIP DRIVERS LICENSE",
                    "FAILURE TO PRODUCE INSURANCE CARD", "IMPROPER REGISTRATION PLATE",
                    "HITCHHIKING/DISREGARDING TRAFFIC REGULATIONS BY PEDESTRIAN",
                    "IMPROPER TURNING", "OBSTRUCTED VISION AND/OR WINDSHIELD",
                    "ONE HEADLIGHT", "OPERATING VEHICLE WITH EXPIRED OPERATORS LICENSE",
                    "SPEEDING 15 MPH OVER LIMIT", "ABANDONMENT OF VEHICLE ON PUBLIC ROAD",
                    "LOCAL VIOLATION CODES 80000 - 80999", "SPEEDING - SPEED NO SPECIFIED",
                    "SPEEDING 12 MPH OVER LIMIT", "SPEEDING 14 MPH OVER LIMIT",
                    "SPEEDING 20 MPH OVER LIMIT", "SPEEDING 26 MPH OR GREATER OVER LIMIT",
                    "SPEEDING 7 MPH OVER LIMIT - LIMITED ACCESS HWY",
                    "DISPLAY/POSSESSOIN OF CANCELLED/FICTITIOUS OPERATOR LICENSE",
                    "DRIVING TOO FAST FOR TRAFFIC CONDITIONS", 
                    "FAIL TO GIVE RT OF WY TO VESS APPR SHORELINE", 
                    "FOLLOWING ANOTHER VEHICLE TOO CLOSELY",
                    "FAILURE TO GIVE RIGHT OF WAY TO EMERGENCY VEHICLE",
                    "FAILURE TO REPORT TRAFFIC ACCIDENT",
                    "FAILURE TO USE CHILD RESTRAINT DEVICE IN VEHICLE",
                    "IMPROPER SOUND DEVICE (WHISTLE, BELL, SIREN)",
                    "IMPROPER STOPPING AT FLASHING RED LIGHT", "IMPROPER USE OF BLUE LIGHTS",
                    "IMPROPER USE OF RED LIGHTS", "IMPROPERLY ON LEFT SIDE OF ROAD",
                    "HITCHHIKING ON LIMITED ACCESS FACILITIES", 
                    "INSTRUCTIONAL PERMIT VIOLATIONS", "LICENSE TO BE IN POSSESSION",
                    "NO PERSON SHALL HAVE MORE THAN ONE OPERATORS LICENSE", 
                    "NO REAR VIEW MIRROR", "NO/EXPIRED KENTUCKY REGISTRATION RECEIPT",
                    "PERMIT UNLICENSED OPERATOR TO OPERATE MOTOR VEHICLE",
                    "REPRESENTING AS ONES OWN ANOTHERS OPER LIC",
                    "RIM OR FRAME OBSCURING LETTERING OR DECAL ON PLATE",
                    "SEAT BELT ANCHORS,CHILD RESTRAINTS") ~ 
      '99Y Non-Reportable Crimes',
    
    # Non-criminal matters
    uor_desc %in% c('ACCIDENTAL DEATH (DROWNING)', 
                    'ACCIDENTAL DEATH (OTHER THAN DROWNING AND HUNTING)',
                    'ACCIDENTAL SHOOTING (OTHER THAN HUNTING)',
                    'DOMESTIC ABUSE DUTIES OF LAW ENFORCEMENT',
                    'EMERGENCY ADMIT,MENTAL HEALTH HOSP.-UNIFIED JUV CO',
                    'EMERGENCY CUSTODY ORDERS UNIFIED JUVENILE CODE',
                    'INVOLUN ADMIT MENTAL HEALTH HOSP - UNIFIED JUV CODE',
                    'INVOLUNTARY HOSPITALIZATION OF MENTALLY ILL - 60/360 DAYS',
                    'NON-CRIMINAL DEATH (NATURAL CAUSES)',
                    'PROPERTY LOST OR ABANDONED NON CRIMINAL',
                    'RECOVERY OF STOLEN PROPERTY',
                    'SERVING BENCH WARRANT FOR COURT',
                    'SERVING WARRANT (FOR OTHER POLICE AGENCY)', "SEE ACCIDENT MODULE", 
                    "PRELIMINARY REPORT NUMBER", "REPORT NUMBER NOT REQUIRED", 
                    "VOIDED REPORT NUMBER", "FINANCIAL CRIME/OUT OF JURISDICTION",
                    "RECOVERY OF STOLEN VEHICLE-OUT OF JURISDICTION", 
                    "NARC INVESTIGATION PENDING", "INJURED PERSON REQUIRING POLICE REPORT",
                    "OVERDOSE", "CIT FORM COMPLETED", "SUICIDE", "UNSUBSTANTIATED RAPE",
                    "CACU INVESTIGATION PENDING", "CACU UNSUBSTANTIATED CASES", 
                    "SVU INVESTIGATION PENDING", "COLD CASE REPORT NUMBER", 
                    "9TH MOBILE PENDING", "RUNAWAY (STATUS OFFENDERS-UNIFIED JUVENILE CODE)",
                    "CACU ASSISTING OTHER AGENCY", "UNSUBSTANTIATED SODOMY",
                    "SUSPICIOUS ACTIVITY/POSSIBLE ABDUCTOR", "JUSTIFIABLE HOMICIDE",
                    "UNSUBSTANTIATED SEXUAL ABUSE", "DV WAITING ON CHARGE",
                    "DEATH INVESTIGATION - LMPD INVOLVED",
                    "DEATH INVESTIGATION - OTHER POLICE AGENCY INVOLVED",
                    "SHOOTING INVESTIGATION - LMPD INVOLVED", 
                    "SHOOTING INVESTIGATION - OTH POLICE AGENCY INVOLVED",
                    "IN-CUSTODY DEATH - CORRECTIONS - IN FACILITY",
                    "IN-CUSTODY DEATH - CORRECTIONS - NOT IN FACILITY",
                    "IN-CUSTODY DEATH - OTHER POLICE AGENCY INVOLVED",
                    "OFFICER NEEDS TO COMPLETE INCIDENT REPORT", 
                    "(NO OFFENSE COUNT) BURGLARY 1A",
                    "(NO OFFENSE COUNT) BURGLARY 2A", "(NO OFFENSE COUNT) BURGLARY 3A", 
                    "ABANDONED ON PRIVATE PROPERTY",
                    "ANY NON CRIMINAL CHARGE NOT COVERED BY THESE CODES", "MISC PENDING",
                    "UNIDENTIFIED PERSON OR REMAINS", "CYBER CRIME PENDING",
                    "SEX OFFENDER REGISTRANT, OUT-OF-STATE, MOVE TO KY",
                    "SUICIDE INVESTIGATION (POLICE ON SCENE)", 
                    "SUICIDE INVESTIGTION - CORRECTIONS - IN FACILITY", 
                    "SUICIDE INVESTIGATION (IN POLICE CUSTODY -OTHER AGENCY)",
                    "GARBAGE AND RUBBISH", "EMA INCIDENT", "FUGITIVE - WARRANT NOT REQUIRED",
                    "FUGITIVE FROM ANOTHER STATE - WARRANT REQUIRED",
                    "INJURED PERSON (LMPD INVOLVED)", "NARC UNSUBSTANTIATED CASES",
                    "NCIC NOT NOTIFIED", "SEE KRS ENTRY") ~ 
      '99Z Non-Criminal Matters',
    
    # all other crimes keep their existing code
    TRUE ~ nibrs_offense_type
    
  )) %>% 
  # Since we've changed NIBRS_Offense_Type for some offences it is necessary to
  # also change the higher-level NIBRS categories. To do this we can remove the
  # existing NIBRS fields and join them again from the NIBRS categories object.
  select(-nibrs_offense_category, -nibrs_crime_against) %>% 
  left_join(read_csv(here::here("analysis_data/nibrs_categories.csv")), 
            by = "nibrs_offense_type") %>% 
  # identify location types
  left_join(
    read_csv("analysis_data/location_types_louisville.csv", 
             col_types = cols(.default = col_character())),
    by = "premise_type"
  ) %>% 
  # add city name
  mutate(city_name = "Louisville") %>% 
  # select core variables
  select(one_of(common_vars)) %>% 
  # save data
  write_rds(here::here("analysis_data/crime_data_louisville.rds"),
            compress = "gz")


# process Memphis data
here::here("original_data/raw_memphis.csv") %>% 
  read_csv(col_types = cols(.default = col_character())) %>% 
  janitor::clean_names() %>% 
  # add date variable
  add_date_var("offense_date", "mdY IMS p", "America/Chicago") %>% 
  # add year variable and remove offenses before start date
  filter_by_year(yearFirst, yearLast) %>% 
  # remove variables containing duplicate information
  select(-city, -state) %>% 
  # create NIBRS codes
  mutate(
    nibrs_offense_type = recode(
      agency_crimetype_id,
      "Aggravated Assault" = "13A Aggravated assault",
      "Aggravated Assault/DV" = "13A Aggravated assault",
      "Arson" = "200 Arson",
      "Burglary/Business" = "22B Non-residential Burglary/Breaking & Entering",
      "Burglary/Non-residential" = 
        "22B Non-residential Burglary/Breaking & Entering",
      "Burglary/Non-Residential" = 
        "22B Non-residential Burglary/Breaking & Entering",
      "Burglary/Residential" = "22A Residential Burglary/Breaking & Entering",
      "Carjacking" = "12A Personal Robbery",
      "Disorderly Conduct" = "90C Disorderly Conduct",
      "Driving Under the Influence" = "90D Driving Under the Influence",
      "Drug Equipment Violation" = "35B Drug Equipment Violations",
      "Drugs/Narcotics Violation/Felony" = "35A Drug/Narcotic Violations",
      "Drugs/Narcotics Violation/Misdemeanor" = "35A Drug/Narcotic Violations",
      "Drunkenness" = "90E Drunkenness (except Driving Under the Influence)",
      "Embezzlement" = "270 Embezzlement",
      "False Pretenses/Swindle/Confidence Game" = 
        "26A False Pretenses/Swindle/Confidence Game",
      "Identity Theft" = "26C Impersonation",
      "Intimidation" = "13C Intimidation",
      "Intimidation/DV" = "13C Intimidation",
      "Justifiable Homicide" = "99Z Non-Criminal Matters",
      "Kidnapping/Abduction" = "100 Kidnapping/Abduction",
      "Liquor Law Violations" = 
        "90G Liquor Law Violations (except Driving Under the Influence and Drunkenness)",
      "Murder" = "09A Murder and Nonnegligent Manslaughter",
      "MVT/Bus" = "240 Motor Vehicle Theft",
      "MVT/ISU/Passenger Vehicle" = "240 Motor Vehicle Theft",
      "MVT/Motorcycle" = "240 Motor Vehicle Theft",
      "MVT/Other" = "240 Motor Vehicle Theft",
      "MVT/Passenger Vehicle" = "240 Motor Vehicle Theft",
      "MVT/Tractor Truck" = "240 Motor Vehicle Theft",
      "Negligent Manslaughter" = "09B Negligent Manslaughter",
      "Negligent Vehicular Manslaughter" = "09B Negligent Manslaughter",
      "Other Larceny/Access Device" = 
        "26A False Pretenses/Swindle/Confidence Game",
      "Other Theft/Non-Specific" = "23H All Other Larceny",
      "Other Theft/Scrap Metal" = "23H All Other Larceny",
      "Pocket-Picking" = "23A Pocket-picking",
      "Possible Stolen MVT" = "99Z Non-Criminal Matters",
      "Prescription Forgery" = "250 Counterfeiting/Forgery",
      "Purse-Snatching" = "23B Purse-snatching",
      "Robbery/Business" = "12B Commercial Robbery",
      "Robbery/Individual" = "12A Personal Robbery",
      "Shoplifting/Felony" = "23C Shoplifting",
      "Shoplifting/Misdemeanor" = "23C Shoplifting",
      "Simple Assault" = "13B Simple assault",
      "Simple Assault/DV" = "13B Simple assault",
      "Stolen Property" = "280 Stolen Property Offenses",
      "Stolen Property Offense" = "280 Stolen Property Offenses",
      "Theft & Recovery/Other" = "240 Motor Vehicle Theft",
      "Theft & Recovery/Passenger Vehicle" = "240 Motor Vehicle Theft",
      "Theft from Building" = "23D Theft From Building",
      "Theft from Building/Access Device" = "23D Theft From Building",
      "Theft from Motor Vehicle" = 
        "23F Theft From Motor Vehicle (except Theft of Motor Vehicle Parts or Accessories)",
      "Theft from Semi-trailer" = 
        "23F Theft From Motor Vehicle (except Theft of Motor Vehicle Parts or Accessories)",
      "Theft from Semi-Trailer" = 
        "23F Theft From Motor Vehicle (except Theft of Motor Vehicle Parts or Accessories)",
      "Theft of Construction/Farm Equipment" = "23H All Other Larceny",
      "Theft of Other Trailer" = 
        "23F Theft From Motor Vehicle (except Theft of Motor Vehicle Parts or Accessories)",
      "Theft of Semi-trailer" = 
        "23F Theft From Motor Vehicle (except Theft of Motor Vehicle Parts or Accessories)",
      "Theft of Vehicle Parts/Accessories" = 
        "23G Theft of Motor Vehicle Parts or Accessories",
      "Theft of Watercraft" = "23H All Other Larceny",
      "Threatening Phone Call" = "13C Intimidation",
      "Threatening Phone Call/DV" = "13C Intimidation",
      "Traffic/Misc." = "99Y Non-Reportable Crimes",
      "Trespass of Real Property" = "90J Trespass of Real Property",
      "Vandalism/Felony" = 
        "290 Destruction/Damage/Vandalism of Property (except Arson)",
      "Vandalism/Misdemeanor" = 
        "290 Destruction/Damage/Vandalism of Property (except Arson)",
      "Violation of Protection Order" = "90Z All Other Offenses",
      "Weapon Law Violations/Felony" = "520 Weapon Law Violations",
      "Weapon Law Violations/Misdemeanor" = "520 Weapon Law Violations",
      "Welfare Fraud" = "26D Welfare Fraud",
      "Wire Fraud" = "26E Wire Fraud"
    )
  ) %>% 
  left_join(read_csv(here::here("analysis_data/nibrs_categories.csv")), 
            by = "nibrs_offense_type") %>% 
  # add city name
  mutate(city_name = "Memphis") %>% 
  # select core variables
  select(one_of(common_vars)) %>% 
  # save data
  write_rds(here::here("analysis_data/crime_data_memphis.rds"),
            compress = "gz")



# process Nashville data
dir("original_data", pattern = "raw_nashville", full.names = TRUE) %>% 
  map_dfr(read_csv, col_types = cols(.default = col_character())) %>% 
  janitor::clean_names() %>% 
  # add date variable
  add_date_var("incident_occurred", "mdY IMS p", "America/Chicago") %>% 
  # add year variable and remove offenses before start date
  filter_by_year(yearFirst, yearLast) %>% 
  # categorise offences
  rename(nibrs_offense_code = offense_nibrs) %>% 
  mutate(
    nibrs_offense_code = recode(
      str_to_upper(nibrs_offense_code),
      "09C" = "99Z",
      "13D" = "13C",
      "26F" = "26C",
      "26G" = "26E",
      "720" = "90Z",
      "730" = "90Z",
      "850" = "90Z"
    )
  ) %>% 
  left_join(read_csv(here::here("analysis_data/nibrs_categories.csv")), 
            by = "nibrs_offense_code") %>% 
  # identify location types
  left_join(
    read_csv("analysis_data/location_types_nashville.csv",
             col_types = cols(.default = col_character())),
    by = "location_description"
  ) %>% 
  # add city name
  mutate(city_name = "Nashville") %>% 
  # select core variables
  select(one_of(common_vars)) %>% 
  # save data
  write_rds(here::here("analysis_data/crime_data_nashville.rds"),
            compress = "gz")



# process San Francisco data
list(
  read_csv(here::here("original_data/raw_san_francisco_early.csv"),
           col_types= cols(.default = col_character())) %>% 
    janitor::clean_names() %>% 
    rename(
      incident_number = incidnt_num,
      incident_category = category,
      incident_description = descript,
      incident_day_of_week = day_of_week,
      latitude = y,
      longitude = x,
      police_district = pd_district
    ) %>% 
    mutate(
      incident_date_time = parse_date_time(paste(date, time),
                                           "mdY HM"),
      file = "early"
    ) %>% 
    filter(incident_date_time < as.Date("2018-01-01")),
  read_csv(here::here("original_data/raw_san_francisco_late.csv"),
           col_types= cols(.default = col_character())) %>% 
    janitor::clean_names() %>% 
    rename(
      address = intersection,
      location = point
    ) %>% 
    mutate(
      incident_date_time = parse_date_time(paste(incident_date, incident_time),
                                           "Ymd HM"),
      file = "late"
    ) %>% 
    filter(incident_date_time >= as.Date("2018-01-01"))
) %>% 
  bind_rows() %>% 
  mutate_at(vars(one_of("incident_category", "incident_description")), 
            str_to_upper) %>% 
  mutate(
    incident_category = recode(
      incident_category,
      "LARCENY THEFT" = "LARCENY/THEFT",
      "FORGERY AND COUNTERFEITING" = "FORGERY/COUNTERFEITING",
      "HUMAN TRAFFICKING, COMMERCIAL SEX ACTS" = 
        "HUMAN TRAFFICKING (A), COMMERCIAL SEX ACTS",
      "MOTOR VEHICLE THEFT?" = "MOTOR VEHICLE THEFT",
      "OTHER" = "OTHER OFFENSES",
      "SUSPICIOUS" = "SUSPICIOUS OCC",
      "MOTOR VEHICLE THEFT" = "VEHICLE THEFT",
      "WARRANT" = "WARRANTS",
      "WEAPON LAWS" = "WEAPONS CARRYING ETC",
      "WEAPONS OFFENCE" = "WEAPONS OFFENSE"
    ),
    incident_date_time = strftime(incident_date_time, format = "%F %R")
  ) %>% 
  add_date_var("incident_date_time", "Ymd HM", "America/Los_Angeles") %>% 
  filter_by_year(yearFirst, yearLast) %>% 
  join_nibrs_cats(here::here("analysis_data/categories_san_francisco.csv"),
                  by = c('incident_category', 'incident_description')) %>% 
  # add city name
  mutate(city_name = "San Francisco") %>% 
  # select core variables
  select(one_of(common_vars)) %>% 
  # save data
  write_rds(here::here("analysis_data/crime_data_san_francisco.rds"),
            compress = "gz")



# merge data
crime_data <- here::here("analysis_data") %>% 
  dir(pattern = "^crime_data_.*rds$", full.names = TRUE) %>% 
  map_dfr(read_rds) %>% 
  write_csv(here::here("analysis_data/crime_data.csv.gz"))
