library(janitor)
library(here)
library(tidyverse)
library(dplyr)
library(ggfortify)
library(viridis)

#load and clean data
ej_data <- read_csv(here("data", "enviroscreen.csv")) %>% 
  clean_names() %>% 
  drop_na()
  
ej_clean_pctl <- ej_data %>% 
  select(california_county, 
         ces_4_0_percentile_range, 
         pm2_5_pctl, 
         asthma_pctl, 
         low_birth_weight_pctl, 
         cardiovascular_disease_pctl, 
         education_pctl, 
         poverty)

ej_clean <- ej_data %>% 
  select(california_county, 
         ces_4_0_percentile_range, 
         pm2_5, 
         asthma, 
         low_birth_weight, 
         cardiovascular_disease, 
         education, 
         poverty,
         housing_burden,
         unemployment)

# ej_long <- ej_clean %>%
#   pivot_longer(names_to = 'name', values_to = 'value', where(is.numeric))

# ggplot(ej_long, aes(x=value)) +
#   geom_histogram() +
#   facet_wrap(~name, scales='free_x') +
#   theme_minimal()

ej_log <- ej_clean %>% 
  mutate(asthma=log(asthma),
         cardiovascular_disease=log(cardiovascular_disease),
         education=log(education),
         housing_burden=log(housing_burden),
         pm2_5=log(pm2_5),
         poverty=log(poverty),
         unemployment=log(unemployment)) %>%
  filter(unemployment!="-Inf", education!="-Inf") %>% 
  drop_na()

# ej_long_log <- ej_log %>% 
#   pivot_longer(names_to = 'name', values_to = 'value', where(is.numeric)) %>% 
#   drop_na()
# 
# ggplot(ej_long_log, aes(x=value)) +
#   geom_histogram() +
#   facet_wrap(~name, scales='free_x') +
#   theme_minimal()

pca_ej <- prcomp(ej_clean %>% select(where(is.numeric)), scale. = TRUE)

pca_ej_log <- prcomp(ej_log %>% select(where(is.numeric)), scale. = TRUE)


#make a pca plot
autoplot(pca_ej,
         data = ej_clean_pctl,
         loadings = TRUE,
         colour = 'ces_4_0_percentile_range',
         loadings.label = TRUE,
         loadings.colour = "black",
         loadings.label.colour = "black",
         loadings.arrow.size = 5,
         loadings.label.vjust = -0.5,
         size = 1
) +
  scale_color_viridis_d(option="rocket") +
  theme_minimal()


autoplot(pca_ej_log,
         data = ej_log,
         loadings = TRUE,
         colour = 'ces_4_0_percentile_range',
         loadings.label = TRUE,
         loadings.colour = "black",
         loadings.label.colour = "black",
         loadings.arrow.size = 5,
         loadings.label.vjust = -0.5,
         size = 1
) +
  scale_color_viridis_d(option="rocket") +
  theme_minimal()

# scree plot: 

# Calculate variance
sd_vec <- pca_ej_log$sdev
var_vec <- sd_vec^2 ### variance is square of standard deviation!
pc_names <- colnames(pca_ej_log$rotation) # column names to loadings 

# Create dataframe w PC, variance, and % var
pct_expl_df <- data.frame(pc = pc_names,
                          v = var_vec,
                          pct_v = var_vec / sum(var_vec)) %>%
  mutate(pct_lbl = paste0(round(pct_v*100, 1), '%'))

# Reorder levels of pc factor
pct_expl_df$pc <- factor(pct_expl_df$pc, levels = pc_names)

# Column plot
ggplot(pct_expl_df, aes(x = pc, y = v)) +
  geom_col(fill= "black") +
  geom_text(aes(label = pct_lbl), vjust = -0.5, nudge_y = 0.002) + 
  labs(x = 'Principal Component', y = 'Variance Explained') +
  theme_minimal()

