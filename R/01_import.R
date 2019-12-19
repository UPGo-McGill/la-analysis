#### LA IMPORT #################################################################

library(tidyverse)
library(upgo)
library(strr)
library(sf)
library(extrafont)

property <- read_csv("data/property_los_angeles.csv",
                     col_types = cols_only(
                       `Property ID` = col_character(),
                       `Listing Title` = col_character(),
                       `Property Type` = col_character(),
                       `Listing Type` = col_character(),
                       `Created Date` = col_date(format = ""),
                       `Last Scraped Date` = col_date(format = ""),
                       Country = col_character(),
                       Latitude = col_double(),
                       Longitude = col_double(),
                       State = col_character(),
                       City = col_character(),
                       Neighborhood = col_character(),
                       `Metropolitan Statistical Area` = col_character(),
                       `Currency Native` = col_character(),
                       Bedrooms = col_double(),
                       Bathrooms = col_double(),
                       `Max Guests` = col_double(),
                       `Response Rate` = col_double(),
                       `Airbnb Superhost` = col_logical(),
                       `HomeAway Premier Partner` = col_logical(),
                       `Cancellation Policy` = col_character(),
                       `Security Deposit (USD)` = col_double(),
                       `Cleaning Fee (USD)` = col_double(),
                       `Extra People Fee (USD)` = col_double(),
                       `Check-in Time` = col_character(),
                       `Checkout Time` = col_character(),
                       `Minimum Stay` = col_double(),
                       `Number of Reviews` = col_double(),
                       `Number of Photos` = col_double(),
                       `Instantbook Enabled` = col_logical(),
                       `Overall Rating` = col_double(),
                       `Airbnb Property ID` = col_double(),
                       `Airbnb Host ID` = col_double(),
                       `HomeAway Property ID` = col_character(),
                       `HomeAway Property Manager` = col_character()
                     )) %>% 
  set_names(c("property_ID", "listing_title", "property_type", "listing_type",
              "created", "scraped", "country", "latitude", "longitude",
              "region", "city", "neighbourhood", "metro_area", "currency",
              "bedrooms", "bathrooms", "max_guests", "response_rate",
              "superhost", "premier_partner", "cancellation", 
              "security_deposit", "cleaning_fee", "extra_people_fee",
              "check_in_time", "check_out_time", "minimum_stay", "num_reviews",
              "num_photos", "instant_book", "rating", "ab_property", "ab_host",
              "ha_property", "ha_host"))

### Produce error files ########################################################

error <- 
  problems(property) %>% 
  filter(expected != "56 columns") %>% 
  pull(row) %>% 
  {property[.,]}

if (nrow(error) > 0) {
  property <- 
    problems(property) %>% 
    filter(expected != "56 columns") %>% 
    pull(row) %>% 
    {property[-.,]}
}

error <- 
  property %>% 
  filter(is.na(property_ID) | is.na(listing_type)) %>% 
  rbind(error)

property <- 
  property %>% 
  filter(!is.na(property_ID), !is.na(listing_type))

error <- 
  property %>% 
  filter(!str_starts(property_ID, "ab-"), !str_starts(property_ID, "ha-")) %>% 
  rbind(error)

property <- 
  property %>% 
  filter(str_starts(property_ID, "ab-") | str_starts(property_ID, "ha-"))

missing_geography <- 
  property %>% 
  filter(is.na(latitude) | is.na(longitude))

property <- 
  property %>% 
  filter(!is.na(latitude), !is.na(longitude))

rm(error, missing_geography)


### Replace problematic characters #############################################

property <- 
  property %>% 
  mutate(listing_title = str_replace_all(
    listing_title, c('\n' = "", '\r' = "", '\"' = "", "\'" = "")))


### Fill in missing created and scraped values from daily table on server ######

upgo::upgo_connect(property = FALSE, multi = FALSE)

property <- 
  daily_all %>% 
  group_by(property_ID) %>% 
  filter(property_ID %in% !! filter(property, is.na(created))$property_ID,
         start_date == min(start_date[status != "U"], na.rm = TRUE)) %>% 
  collect() %>% 
  select(property_ID, created2 = start_date) %>% 
  left_join(property, .) %>% 
  mutate(created = if_else(is.na(created), created2, created)) %>% 
  select(-created2)

property <- 
  daily_all %>% 
  group_by(property_ID) %>% 
  filter(property_ID %in% !! filter(property, is.na(scraped))$property_ID,
         end_date == max(end_date[status != "U"], na.rm = TRUE)) %>% 
  collect() %>% 
  select(property_ID, scraped2 = end_date) %>% 
  left_join(property, .) %>% 
  mutate(scraped = if_else(is.na(scraped), scraped2, scraped)) %>% 
  select(-scraped2)

upgo::upgo_disconnect()


### Add host_ID field ##########################################################

property <- 
  property %>% 
  mutate(host_ID = if_else(!is.na(ab_host), as.character(ab_host), ha_host))


### Add housing field ##########################################################

property <- 
  property %>% 
  strr_housing() %>% 
  select(property_ID, host_ID, listing_title:scraped, housing,
         latitude:longitude, country, region:ha_host)


### Query DBs ##################################################################

upgo_connect()

ML_property <- 
  property_all %>% 
  filter(ab_host %in% !! property$ab_host) %>% 
  collect()

ML_daily <- 
  daily_all %>% 
  filter(property_ID %in% !! ML_property$property_ID) %>% 
  collect()

upgo_disconnect()


### Decompress daily file ######################################################

ML_daily <- 
  ML_daily %>% 
  strr_expand(cores = 4)



save(property, ML_property, ML_daily, file = "data/LA_data.Rdata")



### Produce trimmed daily file #################################################

daily <- 
  ML_daily %>% 
  filter(property_ID %in% property$property_ID, date >= "2016-11-01")

daily <- 
  daily %>% 
  select(-created, -scraped) %>% 
  left_join(select(property, property_ID, created, scraped)) %>% 
  filter(date <= scraped & (is.na(created) | date >= created)) %>% 
  select(property_ID:listing_type, created, scraped, housing:city)


### Calculate FREH #############################################################

FREH <- 
  daily %>% 
  strr_FREH(start_date = "2017-10-31", end_date = "2019-10-31", cores = 5)


save(daily, FREH, file = "data/LA_daily_FREH.Rdata")


### Calculate GH ###############################################################

GH <- 
  property %>% 
  strr_ghost(start_date = "2016-11-01", end_date = "2019-11-27")


### Calculate ML status ########################################################

## Do ML calculations

EH_ML <- 
  ML_daily %>% 
  filter(listing_type == "Entire home/apt") %>% 
  group_by(listing_type, host_ID, date) %>% 
  count() %>% 
  ungroup() %>% 
  filter(n >= 2) %>% 
  mutate(ML = TRUE)

PR_ML <- 
  ML_daily %>% 
  filter(listing_type == "Private room") %>% 
  group_by(listing_type, host_ID, date) %>% 
  count() %>% 
  ungroup() %>% 
  filter(n >= 3) %>% 
  mutate(PR_ML = TRUE)

daily <- 
  EH_ML %>% 
  select(-n) %>% 
  left_join(daily, .)

daily <- 
  PR_ML %>% 
  select(-n) %>% 
  left_join(daily, .) %>% 
  mutate(ML = if_else(is.na(ML), PR_ML, ML)) %>% 
  mutate(ML = if_else(is.na(ML), FALSE, ML)) %>% 
  select(-PR_ML)


### Save working tables ########################################################

save(daily, ML_daily, property, ML_property, FREH, GH, 
     file = "LA_working_files.Rdata")


