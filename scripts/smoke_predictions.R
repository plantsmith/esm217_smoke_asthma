
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

# find the median smoke prediction for CA per year: 
smoke_pm_annual_median <- smoke_pm_median %>% 
  group_by(year) %>% 
  summarise(median_annual_smoke_pm = median(median_smoke_pm))

# plot the annual median per year: 
smoke_pm_annual_plot <- ggplot(smoke_pm_annual_median, aes(x = year, y = median_annual_smoke_pm)) +
  geom_point(size = 3) +
  geom_line() +
  labs(
    title = "Median Annual PM2.5 from Wildfires in California (2015-2022)",
    x = "Year",
    y = "Median Annual PM2.5 (µg/m³)"
  ) +
  scale_y_continuous(labels = scales::comma) +  # Show full numbers with commas
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "#E0E0E0"),
    panel.grid.minor = element_blank()
  )

smoke_pm_annual_plot

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
  select(-highlight) %>% 
  drop_na()


# show 
 
# plot median smoke over the years
smoke_pm_plot <- ggplot(fire_smoke_asthma_join, aes(x = year, y = median_annual_smoke_pm)) +
  geom_point(size = 3, color = "#3B4E36") +
  geom_smooth(method = "lm", se = FALSE, color = "#D8483B", size = 1.5) +
  labs(
    title = "Median Annual PM2.5 from Wildfires in California (2015-2022)",
    subtitle = "Linear regression of median annual PM2.5 from wildfires",
    x = "Year",
    y = "Median Annual PM2.5 (µg/m³)"
  ) +
  scale_x_continuous(labels = scales::comma) +  # Show full numbers with commas
  scale_y_continuous(labels = scales::comma) +  # Show full numbers with commas (log-transformed scale)
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "#E0E0E0"),
    panel.grid.minor = element_blank()
  )

smoke_pm_plot