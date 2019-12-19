#### Figures and facts for LA report ###########################################

library(extrafont)


### Active daily listings ######################################################

# Listings on Oct 15
daily_active %>% 
  filter(date == "2019-10-15")

# All listings throughout year
property %>% 
  filter(created <= "2019-10-31", scraped >= "2018-11-01") %>% 
  nrow()

# Annual revenue
daily %>% 
  filter(date >= "2018-11-01", status == "R") %>% 
  tally(price)

# Peak listings
daily_active %>% 
  filter(listings == max(listings))

# Final listings
daily_active[1852,]


  

# Active listings graph

active_listings_graph <-
  daily_active %>% 
  mutate(listings = data.table::frollmean(listings, 10)) %>% 
  ggplot() +
  geom_line(aes(date, listings), colour = "#A84268", size = 1.5) +
  geom_vline(xintercept = as.Date("2019-10-15", origin = "1970-01-01"),
             linetype = "dashed") +
  ggtitle("Figure 1. Active lisings by day") +
  scale_y_continuous(name = NULL, label = scales::comma, limits = c(0, NA)) +
  theme_minimal() +
  scale_x_date(name = NULL) +
  theme(text = element_text(family = "Futura"),
        plot.title = element_text(family = "Futura", face = "bold"))

ggsave("output/active_listings.pdf", plot = active_listings_graph, width = 8, 
       height = 4, units = "in", useDingbats = FALSE)



### Host revenue percentiles graph #############################################

# How many hosts earned revenue
daily %>% 
  filter(date >= "2018-11-01", status == "R") %>% 
  count(host_ID)


revenue_graph <-
  daily %>%
  filter(housing == TRUE, date >= "2018-11-01", status == "R") %>%
  group_by(host_ID) %>%
  summarize(rev = sum(price)) %>%
  filter(rev > 0) %>%
  summarize(
    `Top 1%`  = sum(rev[rev > quantile(rev, c(0.99))] / sum(rev)),
    `Top 5%`  = sum(rev[rev > quantile(rev, c(0.95))] / sum(rev)),
    `Top 10%` = sum(rev[rev > quantile(rev, c(0.90))] / sum(rev)),
    `Top 20%` = sum(rev[rev > quantile(rev, c(0.80))] / sum(rev))) %>% 
  gather(`Top 1%`, `Top 5%`, `Top 10%`, `Top 20%`, key = "percentile", 
         value = "value") %>% 
  mutate(percentile = factor(percentile, 
                             levels = c('Top 1%', 'Top 5%', 'Top 10%', 
                                        'Top 20%'))
  ) %>% 
  ggplot() +
  geom_bar(aes(percentile, value), stat = "identity", fill = "#A84268") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Figure 2. Host revenue by percentile") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold", 
                                    size = 10),
        legend.text = element_text(family = "Futura", size = 10),
        legend.position = "none",
        plot.title = element_text(family = "Futura", face = "bold"))

ggsave("output/revenue_graph.pdf", plot = revenue_graph, width = 8, height = 4, 
       units = "in", useDingbats = FALSE)



### Multilistings graph ########################################################

ML_summary <- 
  daily %>% 
  group_by(date) %>% 
  summarize(Listings = mean(ML),
            Revenue = sum(price * (status == "R") * ML, na.rm = TRUE) / 
              sum(price * (status == "R"), na.rm = TRUE))

ML_graph <-
  ML_summary %>% 
  gather(Listings, Revenue, key = `Multilisting percentage`, value = Value) %>% 
  ggplot() +
  geom_line(aes(date, Value, colour = `Multilisting percentage`), alpha = 0.2) +
  geom_smooth(aes(date, Value, colour = `Multilisting percentage`), se = FALSE,
              method = "loess", span = 0.25) +
  theme_minimal() +
  ggtitle("Figure 3. Daily listings and revenue by multilisting status") +
  scale_y_continuous(name = NULL, label = scales::percent) +
  scale_x_date(name = NULL, limits = c(as.Date("2016-11-01"), NA)) +
  scale_colour_manual(values = c("#9DBF9E", "#A84268")) +
  theme(legend.position = "bottom", 
        text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold", 
                                    size = 10),
        legend.text = element_text(family = "Futura", size = 10),
        plot.title = element_text(family = "Futura", face = "bold"))

ggsave("output/ML_graph.pdf", plot = ML_graph, width = 8, height = 4, 
       units = "in", useDingbats = FALSE)


### Housing loss ###############################################################

# Housing loss on Oct 31
housing_loss %>% 
  filter(date == "2019-10-15")

housing_loss %>% 
  filter(date == "2017-11-01")

housing_graph <- 
  ggplot(housing_loss) +
  geom_col(aes(date, `Housing units`, fill = `Listing type`),
           lwd = 0) +
  theme_minimal() +
  scale_y_continuous(name = NULL, label = scales::comma, limits = c(0, 8000)) +
  scale_x_date(name = NULL) +
  scale_fill_manual(values = c("#9DBF9E", "#A84268")) +
  ggtitle("Figure 4. Housing loss caused by short-term rentals") +
  theme(legend.position = "bottom", 
        text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold", 
                                    size = 10),
        legend.text = element_text(family = "Futura", size = 10),
        plot.title = element_text(family = "Futura", face = "bold"))

ggsave("output/housing_loss.pdf", plot = housing_graph, width = 8, height = 5, 
       units = "in", useDingbats = FALSE)


### Takedowns ##################################################################


# Takedowns graph

takedowns_graph <- 
  property %>% 
  count(scraped, sort = TRUE) %>% 
  filter(scraped < "2019-11-27") %>% 
  ggplot() +
  geom_point(aes(scraped, n), size = 0.5, colour = "#A84268") +
  theme_minimal() +
  scale_x_date(name = NULL) +
  scale_y_continuous(name = NULL, label = scales::comma) +
  ggtitle("Figure 5. Listings taken down each day") +
  theme(text = element_text(family = "Futura"),
        plot.title = element_text(family = "Futura", face = "bold"))

ggsave("output/takedowns_graph.pdf", plot = takedowns_graph, width = 8, 
       height = 4, units = "in", useDingbats = FALSE)



### Legal and illegal listings #################################################

legal_graph <-
  legal_active %>% 
  ggplot() +
  geom_line(aes(date, `Active listings`, colour = `Principal residence`),
            size = 1.5) +
  theme_minimal() +
  scale_y_continuous(name = NULL, label = scales::comma, limits = c(0, 15000)) +
  scale_colour_manual(values = c("#9DBF9E", "#A84268")) +
  ggtitle("Figure 6. Change in active listings by principal residence status") +
  theme_minimal() +
  scale_x_date(name = NULL) +
  theme(legend.position = "bottom", 
        text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold", 
                                    size = 10),
        legend.text = element_text(family = "Futura", size = 10),
        plot.title = element_text(family = "Futura", face = "bold"))

ggsave("output/legal_graph.pdf", plot = legal_graph, width = 8, 
       height = 4, units = "in", useDingbats = FALSE)

legal_active %>% 
  filter(date == "2019-12-10")

