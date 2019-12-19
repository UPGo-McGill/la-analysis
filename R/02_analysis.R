#### LA ANALYSIS ###############################################################

property <- ungroup(property)


### Daily listings from property file ##########################################

daily_active <- tibble(date = as.Date(numeric(), origin = "1970-01-01"),
                       listings = integer())

n <- 1

for (i in as.Date("2014-11-01", origin = "1970-01-01"):
     as.Date("2019-11-27", origin = "1970-01-01")) {
  
  daily_active[n, 1] <- as.Date(i, origin = "1970-01-01")
  daily_active[n, 2] <- property %>% 
    filter(scraped >= as.Date(i, origin = "1970-01-01"),
           created <= as.Date(i, origin = "1970-01-01")) %>% 
    nrow()
  
  n <- n + 1
  
}

rm(n, i)


### Housing loss ###############################################################

FREH <- 
  FREH %>% filter(FREH == TRUE)

# FREH %>% 
#   filter(FREH == TRUE) %>% 
#   filter(date == "2019-10-31") %>% 
#   count()
# 
# FREH %>% 
#   filter(FREH == TRUE) %>% 
#   count(date) %>% 
#   ggplot() +
#   geom_line(aes(date, n), colour = "black", size = 1) +
#   theme_minimal() +
#   scale_y_continuous(name = NULL, label = scales::comma) +
#   ggtitle("FREH listings in Los Angeles")
# 
# GH %>% 
#   st_drop_geometry() %>% 
#   group_by(date) %>% 
#   summarize(GH_units = sum(housing_units)) %>%
#   ggplot() +
#   geom_line(aes(date, GH_units), colour = "black", size = 1) +
#   theme_minimal() +
#   scale_y_continuous(name = NULL, label = scales::comma) +
#   ggtitle("Units converted to ghost hostels in Halifax Regional Municipality")

GH_total <- 
  GH %>% 
  st_drop_geometry() %>% 
  group_by(date) %>% 
  summarize(GH_units = sum(housing_units)) %>% 
  pull(GH_units) %>% 
  zoo::rollmean(30, align = "right") 

GH_total <- GH_total[(length(GH_total) + 1 - 
                        n_groups(FREH %>% group_by(date))):length(GH_total)]

housing_loss <- 
  FREH %>% 
  group_by(date) %>% 
  summarize(`Entire home/apt` = n()) %>% 
  mutate(`Private room` = as.integer(GH_total)) %>% 
  gather(`Entire home/apt`, `Private room`, key = `Listing type`,
         value = `Housing units`)

# # Current housing loss figure
# sum(filter(housing_loss, date == "2019-10-31")$`Housing units`)
# 
# # YOY increase
# sum(filter(housing_loss, date == "2019-10-31")$`Housing units`) /
#   sum(filter(housing_loss, date == "2018-10-31")$`Housing units`)



### Listings likely in violation of principal residence requirement ############

## LFRML calculations

# Add ML field to property file
property <- 
  daily %>% 
  filter(date == "2019-10-15") %>% 
  select(property_ID, ML) %>% 
  left_join(property, .) %>% 
  mutate(ML = if_else(is.na(ML), FALSE, ML))

# Add n_reserved and n_available fields
property <- 
  daily %>% 
  filter(date >= "2018-11-01", status == "R") %>% 
  group_by(property_ID) %>% 
  summarize(n_reserved = n()) %>% 
  left_join(property, .)

property <- 
  daily %>% 
  filter(date >= "2018-11-01", (status == "R" | status == "A")) %>% 
  group_by(property_ID) %>% 
  summarize(n_available = n()) %>% 
  left_join(property, .)

# Add LFRML field
property <- 
  property %>%
  group_by(host_ID, listing_type) %>% 
  mutate(LFRML = case_when(
    listing_type != "Entire home/apt" ~ FALSE,
    ML == FALSE                       ~ FALSE,
    n_available == min(n_available)   ~ TRUE,
    TRUE                              ~ FALSE)) %>% 
  ungroup()


# Resolve ties
property <- 
  property %>% 
  group_by(host_ID, listing_type) %>% 
  mutate(prob = sample(0:10000, n(), replace = TRUE),
         LFRML = if_else(
           sum(LFRML) > 1 & prob != max(prob), FALSE, LFRML)) %>% 
  select(-prob) %>% 
  ungroup()

# Add GH status
GH_list <-
  GH %>% 
  filter(date == "2019-10-15") %>% 
  pull(property_IDs) %>%
  unlist() %>%
  unique()

property <-
  property %>% 
  mutate(GH = if_else(property_ID %in% GH_list, TRUE, FALSE))

# Add FREH status
property <- 
  FREH %>% 
  filter(date == "2019-10-15") %>% 
  left_join(property, .) %>% 
  mutate(FREH = if_else(is.na(FREH), FALSE, FREH))

# Add Legal field
legal <- 
  property %>%
  ungroup() %>% 
  filter(created <= "2019-10-15", scraped >= "2019-10-15") %>% 
  mutate(legal = case_when(
    housing == FALSE               ~ FALSE,
    GH == TRUE                     ~ FALSE,
    listing_type == "Shared room"  ~ TRUE,
    listing_type == "Private room" ~ TRUE,
    FREH == TRUE                   ~ FALSE,
    LFRML == TRUE                  ~ TRUE,
    ML == TRUE                     ~ FALSE,
    TRUE                           ~ TRUE))

mean(legal$FREH, na.rm = TRUE)
mean(legal$GH, na.rm = TRUE)
mean(legal$LFRML, na.rm = TRUE)
mean(legal$ML, na.rm = TRUE)
sum(legal$legal, na.rm = TRUE)

rm(GH_list, GH_total)

### Comparison of listings taken down versus listings which stayed up ##########

### Daily listings from property file ##########################################

legal_active <- tibble(date = as.Date(numeric(), origin = "1970-01-01"),
                       legal = integer(),
                       illegal = integer())

n <- 1

for (i in as.Date("2019-10-15", origin = "1970-01-01"):
     as.Date("2019-11-27", origin = "1970-01-01")) {
  
  legal_active[n, 1] <- as.Date(i, origin = "1970-01-01")
  legal_active[n, 2] <- legal %>% 
    filter(legal == TRUE, 
           scraped >= as.Date(i, origin = "1970-01-01"),
           created <= as.Date(i, origin = "1970-01-01")) %>% 
    nrow()
  legal_active[n, 3] <- legal %>% 
    filter(legal == FALSE, 
           scraped >= as.Date(i, origin = "1970-01-01"),
           created <= as.Date(i, origin = "1970-01-01")) %>% 
    nrow()
  
  n <- n + 1
  
}

rm(n)

legal_active[45, 1] <- as.Date("2019-12-10", origin = "1970-01-01")

legal_active[45, 2] <-
  legal %>% 
  filter(scraped >= "2019-11-27", legal == TRUE) %>%
  filter(property_ID %in% filter(
    LA_scrape, !is.na(country) | str_starts(property_ID, "ha-"))$property_ID) %>% 
  nrow()

legal_active[45, 3] <-
  legal %>% 
  filter(scraped >= "2019-11-27", legal == FALSE) %>%
  filter(property_ID %in% filter(
    LA_scrape, !is.na(country) | str_starts(property_ID, "ha-"))$property_ID) %>% 
  nrow()

legal_active <-
  legal_active %>%
  rename(Yes = legal, No = illegal) %>% 
  pivot_longer(-date, names_to = "Principal residence", values_to = "Active listings")
