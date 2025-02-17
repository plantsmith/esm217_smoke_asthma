
library(tidyverse)
library(here)
library(janitor)

# --------------------------------------------------------------------------------
# Load and Clean Data
# --------------------------------------------------------------------------------

# Metadata: https://data.ca.gov/dataset/asthma-hospitalization-rates-by-county/resource/608ea657-47ef-4616-a92c-79daf746bdf5
# - Number of hospitalizations: Total asthma-related hospitalizations in a specific county.
# - Age-adjusted hospitalization rate: Hospitalizations per 10,000 people, adjusting for population size and age distribution.

# Load asthma data
asthma <- read_csv(here("data", "asthma_2022.csv")) %>% 
  clean_names()

# Clean and format asthma data
asthma_clean <- asthma %>% 
  select(-comment) %>%  # Remove unnecessary column
  relocate(year, county, number_of_hospitalizations, age_group, age_adjusted_hospitalization_rate, strata, strata_name) %>%  # Reorder columns
  filter(county != "California")  # Remove state-wide summary row

# --------------------------------------------------------------------------------
# Hospitalization Trends Over Time
# --------------------------------------------------------------------------------

# Summarize total hospitalizations by year
asthma_yearly_summary <- asthma_clean %>% 
  group_by(year) %>% 
  summarise(total_hospitalizations = sum(number_of_hospitalizations, na.rm = TRUE)) %>% 
  ungroup()


# Plot: Total asthma hospitalizations over time
yearly_plot <- ggplot(asthma_yearly_summary, aes(x = year, y = total_hospitalizations)) +
  geom_line(size = 1, color = "#F4BEB6", linetype = "solid") +  # Solid line with a distinct color
  geom_point(size = 2, color = "#D8483B") +  # Larger points with color
  geom_smooth(method = "loess", color = "#3B4E36", size = 1, linetype = "dashed") +  # Add smooth trend line (LOESS)
  labs(
    title = "Total Asthma Hospitalizations in California by Year",
    subtitle = "Trends in asthma-related hospitalizations from 2015-2022",
    y = "Total Hospitilizations"
  ) +
  theme_minimal(base_size = 15) +  # Increase base font size for readability
  theme(
    legend.position = "none",  # Remove legend (not needed here)
    plot.title = element_text(face = "bold", size = 18),  # Bold title
    plot.subtitle = element_text(size = 14),  # Subtitle size
    axis.title = element_text(size = 14),  # Axis title size
    axis.text = element_text(size = 12),  # Axis text size
    axis.title.x = element_blank(),  # Remove x-axis title
    panel.grid.major = element_line(color = "#E0E0E0"),  # Light grid lines for better readability
    panel.grid.minor = element_blank()  # Remove minor grid lines for cleaner look
  )

yearly_plot

ggsave(here("images", "asthma_hospitalization_trends.png"), yearly_plot, width = 10, height = 6, units = "in")


# --------------------------------------------------------------------------------
# Hospitalizations by County
# --------------------------------------------------------------------------------

# Summarize total hospitalizations by county
asthma_county_summary <- asthma_clean %>% 
  group_by(county) %>% 
  summarise(total_hospitalizations = sum(number_of_hospitalizations, na.rm = TRUE)) %>% 
  arrange(desc(total_hospitalizations)) %>% 
  ungroup()

# --------------------------------------------------------------------------------
# Population-Adjusted Hospitalization Rates by County
# --------------------------------------------------------------------------------

# Load and clean environmental screening data
enviroscreen <- read_csv(here("data/enviroscreen_data/enviroscreen_csv/enviroscreen_data.csv")) %>% 
  clean_names() %>% 
  select(california_county, total_population) %>% 
  rename(county = california_county) %>% 
  group_by(county) %>%
  summarise(total_population = sum(total_population, na.rm = TRUE))

# Merge asthma and population data
asthma_enviroscreen <- asthma_county_summary %>% 
  left_join(enviroscreen, by = "county") %>% 
  mutate(hospitalization_rate_per_1000 = (total_hospitalizations / total_population) * 1000)  # Calculate rate per 1,000 people

# Plot: Hospitalization rate per 1,000 people by county (Bar Chart)
ggplot(asthma_enviroscreen, aes(x = reorder(county, hospitalization_rate_per_1000), y = hospitalization_rate_per_1000)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Asthma Hospitalization Rate per 1,000 People in California by County",
    x = NULL,
    y = "Hospitalization Rate per 1,000 People"
  ) +
  theme_minimal()

# Plot: Hospitalization rate per 1,000 people by county (Lollipop Chart)
# Improved Lollipop Chart without x-axis title
lollipop <- ggplot(asthma_enviroscreen, aes(x = reorder(county, hospitalization_rate_per_1000), y = hospitalization_rate_per_1000)) +
  geom_segment(aes(xend = reorder(county, hospitalization_rate_per_1000), yend = 0), color = "darkgrey") +
  geom_point() +
  coord_flip() +
  labs(
    title = "Asthma Hospitalization Rates by County in California",
    subtitle = "Hospitalizations per 1,000 people in each county (2015-2022), adjusted for population size",
    y = "Hospitalization Rate per 1,000 People"
  ) +
  theme_minimal() +
  
  theme(
    legend.position = "none",  # remove legend
    plot.title = element_text(
      face = "bold", 
      color = "black", 
      size = 14, 
      margin = margin(t = 15, b = 10)),
    plot.subtitle = element_text(
      color = "grey40", 
      size = 10, 
      margin = margin(b = 10)),
    axis.title.y = element_blank(),  # Remove x-axis title
    axis.text = element_text(
      color = "black",
      size = 8),
    axis.text.x = element_text(
      margin = margin(t = -5)  # Adjust margin to bring text closer to x-axis
    ),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )

lollipop

ggsave(here("images", "asthma_hospitalization_rates_by_county.png"), lollipop, width = 8, height = 6, units = "in")


# --------------------------------------------------------------------------------
# Hospitalization Trends by Year and County
# --------------------------------------------------------------------------------

# Summarize hospitalizations by year and county
asthma_year_county_summary <- asthma_clean %>%
  group_by(year, county) %>%
  summarize(total_hospitalizations = sum(number_of_hospitalizations, na.rm = TRUE), .groups = "drop")

# Merge asthma data with population data and calculate hospitalization rate per 1,000 people
asthma_year_county_enviroscreen <- asthma_year_county_summary %>% 
  left_join(enviroscreen, by = "county") %>% 
  mutate(hospitalization_rate_per_1000 = (total_hospitalizations / total_population) * 1000)  # Calculate rate per 1,000 people

# Identify top 5 counties based on total hospitalization rate
top_5_counties <- asthma_year_county_enviroscreen %>%
  group_by(county) %>%
  summarize(total_rate = sum(hospitalization_rate_per_1000, na.rm = TRUE)) %>%
  top_n(5, total_rate) %>%
  pull(county)  # Extract county names

# Define the counties you want to highlight
highlight_counties <- c("Fresno", "Imperial", "Los Angeles", "Sacramento", "San Bernardino")

# Create a new variable in the dataset to assign colors based on the county
asthma_year_county_enviroscreen <- asthma_year_county_enviroscreen %>%
  mutate(highlight = ifelse(county %in% highlight_counties, as.character(county), "Other"))

# Define custom colors for each highlighted county
# Create a fire-inspired color palette
county_colors<- c("Fresno" = "#E69F00", 
                  "Imperial" = "#56B4E9", 
                  "Los Angeles" = "#009E73", 
                  "Sacramento" = "#F0E442", 
                  "San Bernardino" = "#D55E00", 
                  "Other" = "grey90")

# Plot: Hospitalization rate per 1,000 people by year and county
year_county_plot <- ggplot(asthma_year_county_enviroscreen, aes(x = year, y = hospitalization_rate_per_1000, group = county, color = highlight)) +
  geom_line() +  # Line plot for hospitalization trends
  geom_point() +  # Add points to the line plot
  labs(
    title = "Trends in Asthma Hospitalization Rates by County in California (2015-2022)",
    subtitle = "Rates per 1,000 people for selected counties, adjusted for population size",
    x = "Year",
    y = "Hospitalization Rate per 1,000 People",
    color = "County"
  ) +
  scale_color_manual(values = county_colors) +  # Set custom colors for counties
  theme_minimal() +  # Clean theme
  theme(
    legend.position = "bottom",  # Place legend at the bottom
    axis.title.x = element_blank()  # Remove x-axis label
    # legend.title = element_blank()  # Remove legend title
  )

# Display the plot
year_county_plot

ggsave(here("images", "asthma_hospitalization_line.png"), year_county_plot, width = 10, height = 6, units = "in")

