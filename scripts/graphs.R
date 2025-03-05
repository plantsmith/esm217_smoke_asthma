library(ggplot2)
library(dplyr)


#......................import Google fonts.......................
# `name` is the name of the font as it appears in Google Fonts
# `family` is the user-specified id that you'll use to apply a font in your ggpplot
font_add_google(name = "Montserrat", family = "mont")
font_add_google(name = "Open Sans", family = "open_sans")



#now we need to 'turn show text on'
#......enable {showtext} rendering for all newly opened GDs......
showtext_auto()
#---------------

# Merge on 'year'
df <- left_join(asthma_yearly_summary, fire_freq_county_year, by = "year") %>% 
  filter(year >= 2013 & year <= 2020)

# Dual-axis plot
dual_axis_plot <- ggplot(df, aes(x = year)) +
  geom_line(aes(y = total_hospitalizations, color = "Hospitalizations"), size = 1) +
  geom_point(aes(y = total_hospitalizations, color = "Hospitalizations"), size = 2) +
  geom_line(aes(y = frequency * 30, color = "Fire Frequency"), size = 1) +  # Adjust scaling factor
  geom_point(aes(y = frequency *30, color = "Fire Frequency"), size = 2) +
  scale_y_continuous(
    name = "Total Hospitalizations",
    sec.axis = sec_axis(~ . / 30, name = "Fire Frequency (Scaled)")
  ) +
  scale_color_manual(values = c("Hospitalizations" = "#D8483B", "Fire Frequency" = "#3B4E36")) +
  labs(
    title = "Asthma Hospitalizations & Fire Frequency in California (2015-2022)",
    subtitle = "Comparison of wildfire occurrences and asthma-related hospitalizations",
    x = "Year"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "#E0E0E0"),
    panel.grid.minor = element_blank()
  )

dual_axis_plot

# --------------------------------------------------------------------------------
# annual asthma hospitalization rates for powerpoint: 
# --------------------------------------------------------------------------------

yearly_plot_prez <- ggplot(asthma_yearly_summary, aes(x = year, y = total_hospitalizations)) +
  geom_line(size = 1.5, color = "black") +  
  geom_point(size = 4, color = "black") +  
  # geom_smooth(method = "loess", color = "orange", size = 1.2, linetype = "dashed") +  # Bold dashed trend line
  
  labs(
    title = "Total Asthma Hospitalizations in California by Year",
    y = "Total Hospitalizations"
  ) +
  
  scale_y_continuous(labels = comma) +  # Show full numbers with commas
  scale_x_continuous(breaks = seq(min(asthma_yearly_summary$year), max(asthma_yearly_summary$year), by = 1)) +  # Whole numbers for x-axis
  
  theme_minimal() + 
  theme(
    plot.title.position = "plot", # shift title to the left
    plot.title = element_blank(),
    plot.subtitle = ggtext::element_textbox(family = "open_sans",
                                            size = 11.5,
                                            color = "black",
                                            margin = margin(t = 2, r = 0, b = 6, l = 0)),  # move in clockwise to top, right, bottom, left
    plot.caption = ggtext::element_textbox(family = "open_sans",
                                           face = "italic",
                                           color = "black",
                                           margin = margin(t = 15, r = 0, b = 0, l = 0)),
    legend.position = "none",
    axis.title = element_text(family ="open_sans", 
                              size = 15),  # Larger axis labels
    axis.text = element_text(family = "open_sans",
                             size = 15),  # Larger axis numbers
    axis.title.x = element_blank(),
    panel.grid.major = element_line(color = "#D3D3D3"),  # Light grid lines for readability
    panel.grid.minor = element_blank()  # Remove minor grid lines
  )

yearly_plot_prez


ggsave(here("images", "asthma_hospitalization_trends_viz.png"), yearly_plot_prez, width = 10, height = 6, units = "in")



# --------------------------------------------------------------------------------
# annual pm2.5 for powerpoint: 
# --------------------------------------------------------------------------------
# plot the annual median per year: 
smoke_pm_annual_plot <- ggplot(smoke_pm_annual_median, aes(x = year, y = median_annual_smoke_pm)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  labs(
    title = "Median Annual PM2.5 from Wildfires in California (2015-2022)",
    x = "Year",
    y = "Median Annual PM2.5 (µg/m³)"
  ) +
  scale_y_continuous(labels = scales::comma) +  # Show full numbers with commas
  scale_x_continuous(breaks = seq(min(asthma_yearly_summary$year), max(asthma_yearly_summary$year), by = 1)) +  # Whole numbers for x-axis
  theme_minimal() +
  theme(
    plot.title.position = "plot", # shift title to the left
    plot.title = element_blank(),
    plot.subtitle = ggtext::element_textbox(family = "open_sans",
                                            size = 11.5,
                                            color = "black",
                                            margin = margin(t = 2, r = 0, b = 6, l = 0)),  # move in clockwise to top, right, bottom, left
    plot.caption = ggtext::element_textbox(family = "open_sans",
                                           face = "italic",
                                           color = "black",
                                           margin = margin(t = 15, r = 0, b = 0, l = 0)),
    legend.position = "none",
    axis.title = element_text(family ="open_sans", 
                              size = 15),  # Larger axis labels
    axis.text = element_text(family = "open_sans",
                             size = 15),  # Larger axis numbers
    axis.title.x = element_blank(),
    panel.grid.major = element_line(color = "#D3D3D3"),  # Light grid lines for readability
    panel.grid.minor = element_blank()  # Remove minor grid lines
  )

smoke_pm_annual_plot


# --------------------------------------------------------------------------------
# hospitalization rate & fire frequency 
# --------------------------------------------------------------------------------

# plot the linear regression between hospitalization rate and fire frequency with log-transformed hospitalization data
fire_lm_plot <- ggplot(fire_smoke_asthma_join, aes(x = days_burned, y = hospitalization_rate_per_1000)) +
  geom_point(size = 3, color = "#3B4E36") +
  geom_smooth(method = "lm", se = FALSE, color = "#D8483B", size = 1.5) +
  labs(
    title = "Asthma Hospitalizations vs. Fire Frequency in California (2015-2022)",
    subtitle = "Linear regression of asthma hospitalizations and fire frequency",
    x = "Fire Frequency",
    y = "Log-transformed Total Hospitalizations"
  ) +
  scale_x_continuous(labels = comma) +  # Show full numbers with commas
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

fire_lm_plot

# plot the linear regression between hospitalization rate and fire frequency with log-transformed hospitalization data
pm_lm_plot <- ggplot(fire_smoke_asthma_join, aes(x = median_annual_smoke_pm, y = hospitalization_rate_per_1000)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "#F2A441", size = 1.5) +
  labs(
    title = "Asthma Hospitalizations vs. Fire Frequency in California (2015-2022)",
    x = "Median Annual PM2.5 from Wildfires (µg/m³)",
    y = "Hospitalization Rate per 1000"
  ) +
  scale_x_continuous(labels = scales::comma) +  # Show full numbers with commas
  scale_y_continuous(labels = scales::comma) +  # Show full numbers with commas (log-transformed scale)
  theme_minimal() +
  theme(
    plot.title = element_blank(),
    plot.subtitle = ggtext::element_textbox(family = "open_sans",
                                            size = 11.5,
                                            color = "black",
                                            margin = margin(t = 2, r = 0, b = 6, l = 0)),  # move in clockwise to top, right, bottom, left
    plot.caption = ggtext::element_textbox(family = "open_sans",
                                           face = "italic",
                                           color = "black",
                                           margin = margin(t = 15, r = 0, b = 0, l = 0)),
    legend.position = "none",
    axis.title = element_text(family ="open_sans", 
                              size = 15),  # Larger axis labels
    axis.text = element_text(family = "open_sans",
                             size = 15),  # Larger axis numbers
    panel.grid.major = element_line(color = "#D3D3D3"),  # Light grid lines for readability
    panel.grid.minor = element_blank()  # Remove minor grid lines
  )
pm_lm_plot

ggsave(here("images", "pm_lm_plot.png"), pm_lm_plot, width = 10, height = 6, units = "in")




