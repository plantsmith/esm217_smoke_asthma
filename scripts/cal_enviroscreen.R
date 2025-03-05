library(tidyverse)
library(here)
library(janitor)

# -------- T A B U L A R --------

# load in the data and clean names: 
enviroscreen_df <- read.csv(here("data/enviroscreen_data/enviroscreen_csv/enviroscreen_data.csv")) %>% 
  clean_names()

# tidy data ---
clean_screen <- enviroscreen_df %>% 
  # select for only the columns we are interested in:
  select(census_tract, 
         total_population, 
         california_county, 
         zip, 
         approximate_location, 
         longitude, 
         latitude,
         ces_4_0_score,
         ces_4_0_percentile,
         ces_4_0_percentile_range, 
         asthma,
         asthma_pctl) %>% 
  # rename columns to be easier to work with:
  rename(
         county = california_county,
         zip = zip,
         location = approximate_location,
         ces_score = ces_4_0_score,
         ces_percentile = ces_4_0_percentile,
         ces_percentile_range = ces_4_0_percentile_range,
         asthma = asthma,
         asthma_percentile = asthma_pctl)

# some quick variable explanations: 
# ces_4_0_score: Raw calculated score that combines all environmental and socioeconomic indicators (not meant to be used for direct comparisons)
# ces_4_0_percentile: How a census tract's total CalEnviroScreen score compares to other census tracts. 
      #For example: percentile of 85 means this area has a higher score than 85% of census tracts in California
# ces_4_0_percentile_range: Groups the percentiles into broader categories, used in policy, EJ funding, etc. 
      # For Example, SB 535 designates census tracts with percentiles above 75% as "disadvantaged communities"



# -------- S P A T I A L --------

library(sf)
library(ggplot2)
library(viridis)

# Load and clean the data
enviroscreen_sf <- read_sf(here("data/enviroscreen_data/enviroscreen_shapefiles/CES4_final_shapefile.shp")) %>% 
  clean_names()

# Tidy data
clean_sf <- enviroscreen_sf %>% 
  select(tract, 
         zip, 
         county, 
         approx_loc, 
         c_iscore,
         c_iscore_p,
         asthma,
         asthma_p,
         shape_area, 
         geometry) %>% 
  #convert -999.00000000 to NAs and drop NAs
  mutate(asthma = ifelse(asthma == -999, NA, asthma),
         asthma_p = ifelse(asthma_p == -999, NA, asthma_p)) 

# make a quick map
map <- ggplot(clean_sf) +
  # Add base map layer
  geom_sf(aes(fill = asthma_p), 
          color = "white",          # White borders between tracts
          size = 0.1,              # Thinner borders
          alpha = 0.9) +           # Slight transparency
  
  # Use a custom color scale
  scale_fill_viridis_c(
    option = "magma",             # Different viridis palette
    name = "Asthma percentile",  # Better legend title
    labels = scales::label_percent(scale = 1) # Format as percentages
  )

map

# reminder: A percentile of 90 means that census tract has more asthma-related... 
# ...ER visits than 90% of all census tracts in California
