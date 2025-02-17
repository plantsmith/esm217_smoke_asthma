
library(tidyverse)
library(here)
library(janitor)
library(sf)

# # --------------------------------------------------------------------------------
# # Load and Clean Data --- TABULAR
# # --------------------------------------------------------------------------------
# df_smoke_predictions_full <- read_csv(here("data/ca_PM25_monthly_county/PM2_5_CA_monthly_counties.csv")) %>% 
#   clean_names()
# 
# 
# df_smoke_predictions_clean <- df_smoke_predictions_full %>% 
#   select(-namelsad) %>%  
#   select(-statefp) %>% 
#   rename(county = name)
# 
# 
# # --------------------------------------------------------------------------------
# # Load and Clean Data --- SPATIAL
# # --------------------------------------------------------------------------------
# 
# # the shapefile doesnt work ....
# 
# sf_smoke_predictions_full <- read_csv(here("data/ca_PM25_monthly_county/PM2_5_CA_monthly_counties.shp")) %>% 
#   clean_names()

