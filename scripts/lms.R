# Summarize data by year and county
asthma_year_county_summary <- asthma_clean %>%
  group_by(year, county) %>%
  summarize(total_hospitalizations = sum(number_of_hospitalizations, na.rm = TRUE), .groups = "drop")

# Join Enviroscreen data by county
asthma_year_county_summary <- asthma_year_county_summary %>%
  left_join(enviroscreen, by = "county")

# Calculate hospitalization rate per 1000 people
asthma_year_county_summary <- asthma_year_county_summary %>% 
  mutate(hospitalization_rate_per_1000 = (total_hospitalizations / total_population) * 1000)

# Join asthma data with fire frequency data by year and county
asthma_fire_join <- left_join(asthma_year_county_summary, fire_freq_county, by = c("year", "county"))

# Replace NA values with 0 (assuming missing means no recorded fires)
asthma_fire_join <- asthma_fire_join %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

# --------------------------Linear Regression(s) ------------------------------
# Relationship between hospitalization rate and fire frequency
lm_freq <- lm(hospitalization_rate_per_1000 ~ frequency, data = asthma_fire_join)

summary(lm_freq)

#Relationship between hospitalization rate and fire frequency
lm_county <- lm(hospitalization_rate_per_1000 ~ frequency + county, data = asthma_fire_join)

summary(lm_county)

#plotting the relationship between hospitalization rate and fire frequency
ggplot(asthma_fire_join, aes(x = frequency, y = hospitalization_rate_per_1000)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between hospitalization rate and fire frequency",
       x = "Fire frequency",
       y = "Hospitalization rate per 1000 people") +
  theme_minimal()


