
library(tidyverse)
library(here)
library(janitor)
library(sf)

# --------------------------------------------------------------------------------
# Load and Clean Data --- TABULAR
# --------------------------------------------------------------------------------
df_smoke_predictions_full <- read_csv(here("data/ca_PM25_monthly_county/PM2_5_CA_monthly_counties.csv")) %>%
  clean_names()

df_smoke_predictions_clean <- df_smoke_predictions_full %>%
  select(year, month, smoke_pm_pred, name) %>% 
  rename(county = name) %>% 
  filter(year >= 2013)

# find the median smoke prediction for each county per year
smoke_pm_median <- df_smoke_predictions_clean %>% 
  group_by(county, year, month) %>% 
  summarise(median_smoke_pm = median(smoke_pm_pred))

# load fire data 

df_fires <- read_csv(here("data/fire_data_clean.csv")) %>%
  clean_names()

# join smoke_pm_median with fire data by county, year, and month
fires_smoke_join <- df_fires %>%
  left_join(smoke_pm_median, by = c("county", "year", "month")) %>% 
  rename(fire_name = name,
         acres_burned = acres,
         fire_start_date = date_start,
         fire_end_date = date_end,
         days_burned = days,
         median_annual_smoke_pm = median_smoke_pm) 



# join asthma data by county and year
# run the asthma script and use asthma_year_county_enviroscreen df

fire_smoke_asthma_join <- fires_smoke_join %>%
  left_join(asthma_year_county_enviroscreen, by = c("county", "year")) %>% 
  select(-highlight)

