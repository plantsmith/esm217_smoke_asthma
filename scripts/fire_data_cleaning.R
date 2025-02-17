library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(lubridate)
library(dplyr)


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
  
fire_data <- read_csv(here("data", "new_fire_data_filter.csv"))

fire_freq_county <- fire_data %>% 
  filter(county != "REMOVE") %>% 
  group_by(county, year) %>% 
  summarize(frequency=n()) %>% 
  separate_rows(county, sep = ", ") %>%  # Separate multiple counties into individual rows
  group_by(county, year) %>%
  summarise(frequency = sum(frequency), .groups = "drop")

fire_freq_plot <- ggplot(fire_freq_county, aes(x = year, y=frequency)) +
  geom_col() +
  facet_wrap(~ county, scales = 'free_x')

fire_freq_plot  

fire_acres_county <- fire_data %>% 
  filter(county != "REMOVE") %>% 
  separate_rows(county, sep = ", ") %>%
  group_by(county, year) %>% 
  summarize(sum=acres)

  
  
  
  
  
  
  