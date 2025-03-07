library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(lubridate)
library(dplyr)
library(patchwork)


# fire_data <- read_xlsx(here("data", "ca_fire_perim.xlsx")) %>% 
#   clean_names() 
# 
# fire_data_clean <- fire_data %>% 
#   mutate(
#     alarm_date = as.Date(ymd_hms(alarm_date, tz = "UTC")),
#     cont_date = as.Date(ymd_hms(cont_date, tz = "UTC")),
#     fire_days = as.numeric(difftime(cont_date, alarm_date, units = "days"))
#   ) %>% 
#   select(-inc_num, -c_method, -comments, -irwinid, -fire_num, -complex_id, -decades)
# 
# fire_data_filter <- fire_data_clean %>% 
#   filter(year(alarm_date) >= 2015 & year(alarm_date) <= 2022)
# 
# write.csv(fire_data_filter, here("data", "fire_data_filter.csv"))  
  
fire_data <- read_csv(here("data", "fire_data_filter.csv"))

fire_freq_county_year <- fire_data %>% 
  filter(county != "REMOVE") %>% 
  group_by(county, year) %>% 
  summarize(frequency=n()) %>% 
  separate_rows(county, sep = ", ") %>%  # Separate multiple counties into individual rows
  group_by(county, year) %>%
  summarise(frequency = sum(frequency), .groups = "drop")

fire_freq_county_year_plot <- ggplot(fire_freq_county_year, aes(x = year, y=frequency)) +
  geom_col() +
  facet_wrap(~ county, scales = 'free_x')

fire_freq_county_year_plot  

fire_freq_county <- fire_data %>% 
  filter(county != "REMOVE") %>% 
  group_by(county) %>% 
  summarize(frequency=n()) %>% 
  separate_rows(county, sep = ", ") %>%  # Separate multiple counties into individual rows
  group_by(county) %>%
  summarise(frequency = sum(frequency), .groups = "drop")

fire_freq_county_plot <- ggplot(fire_freq_county, aes(x = reorder(county, frequency), y=frequency)) +
  geom_col() +
  coord_flip() +
  `labs(x = "County", y = "Fire Frequency", title = "Fire Frequency by County") +
  theme_minimal()

fire_freq_county_plot  

fire_acres_county_year <- fire_data %>% 
  filter(county != "REMOVE") %>% 
  separate_rows(county, sep = ", ") %>% 
  group_by(county, year) %>% 
  summarise(total_acres = sum(acres), .groups = "drop")

fire_acres_county_year_plot <- ggplot(fire_acres_county_year, aes(x = year, y=total_acres)) +
  geom_col() +
  facet_wrap(~ county, scales = 'free_x')

fire_acres_county_year_plot  

fire_acres_county <- fire_data %>% 
  filter(county != "REMOVE") %>% 
  separate_rows(county, sep = ", ") %>% 
  group_by(county) %>% 
  summarise(total_acres = sum(acres), .groups = "drop")

fire_acres_county_plot <- ggplot(fire_acres_county, aes(x = reorder(county, total_acres), y=total_acres)) +
  geom_col() +
  coord_flip() +
  labs(x = "County", y = "Total Acres Burned", title = "Total Acres Burned by County") +
  theme_minimal()

fire_acres_county_plot + fire_freq_county_plot  



# aggrege fires by year aand make a ggplot line graph of fire frequency by year 

fire_freq_county_year <- fire_data %>% 
  filter(county != "REMOVE") %>% 
  group_by(year) %>% 
  summarize(frequency=n()) %>% 
  ungroup() %>% 
  filter(year >= 2013 & year <= 2020)

fire_freq_plot <- ggplot(fire_freq_county_year, aes(x = year, y=frequency)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Fire Frequency", title = "Fire Frequency by Year") +
  theme_minimal()

fire_freq_plot




fire_data_2020 <- read_csv(here("data", "fire_data_filter.csv")) %>% 
  clean_names() %>% select(year, county, acres, name) %>% 
  filter(year == 2020) %>% 
  rename(acres_burn = acres) %>%   
  drop_na(county) 

fire_data_mod_2020 <- fire_data_2020 %>% 
  group_by(county, year) %>%
  summarise(
    avg_acres_burned = mean(acres_burn, na.rm = TRUE),
    avg_fires_per_year = n() / n_distinct(year),
    total_acres_burned = sum(acres_burn, na.rm = TRUE),
    total_fires = n()
  ) %>%
  ungroup()

write_csv(fire_data_mod_2020, here("data", "fire_data_mod_2020.csv"))


