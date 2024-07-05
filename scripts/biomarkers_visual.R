# clear environment
rm(list = ls()); gc()


# load or install necessary libraries
pacman::p_load(tidyverse, rio, here, googlesheets4, ggsci, lubridate, dotenv, ggthemes, zoo)

biomarker_data <- import(here("output data", "cbc_chem_updated.csv"))
reference_table <- import(here("input data", "biomarker_reference_table.csv"))

biomarker_data <- biomarker_data %>% 
  group_by(metric) %>% 
  arrange(date_updated) %>% 
  mutate(observation = row_number())

# Join biomarker data with reference table
joined_data <- biomarker_data %>%
  left_join(reference_table, by = c("metric" = "biomarkers")) %>%
  filter(metric %in% c("WBC", "Total Protein", "RBC", "Albumin: Globulin Ratio", "Eosinophils", "Lymphocytes", "ALP", "ALT", "Basophils", "Monocytes", "Platelets", "Albumin", "Globulin"))

# Normalize data using the existing 'min' and 'max' columns
joined_data <- joined_data %>%
  mutate(
    norm_value = (as.numeric(value) - min) / (max - min),
    norm_min_ref = 0,  # Minimum of range after normalization
    norm_max_ref = 1   # Maximum of range after normalization
  )

# Plot using ggplot2
p1 <- ggplot(joined_data, aes(x = metric, y = norm_value, color = as.factor(observation), shape = as.factor(observation))) +
  geom_errorbar(aes(ymin = norm_min_ref, ymax = norm_max_ref), color = "black", width = 0.2) +
  geom_point(size = 4) +  # Increased size for better visibility
  scale_shape_manual(values=c(16, 17, 15)) +  # Manually specify shapes, e.g., 16 = solid circle, 17 = solid triangle
  scale_color_lancet() +
  labs(title = "Normalized Biomarker Values with Reference Bands",
       x = "Biomarker",
       y = "Normalized Value (0-1)",
       color = "Observation",
       shape = "Observation") +  # Label for shape legend
  theme_bw()

p1

ggsave(
  filename = here("images", "archive", "biomarker_graph_demo.png"),
  plot = p1,
  dpi = 600,
  width = 12,
  height = 8
)

joined_data2 <- joined_data
joined_data2 <- joined_data2 %>% 
  mutate(metric = factor(metric, levels = c("Albumin: Globulin Ratio", "Albumin", "Globulin", "Total Protein", "ALP", "ALT", "WBC", "RBC", "Platelets", "Eosinophils", "Basophils", "Lymphocytes", "Monocytes")))
# joined_data2$metric <- factor(joined_data2$metric, levels = c("WBC", "RBC", "Total Protein", "Albumin: Globulin Ratio", "Eosinophils", "Lymphocytes", "ALP", "ALT", "Basophils"))

# Plot using ggplot2
p2 <- ggplot(joined_data2, aes(x = metric, y = norm_value, color = as.factor(observation), shape = as.factor(observation))) +
  geom_errorbar(aes(ymin = norm_min_ref, ymax = norm_max_ref), color = "black", width = 0.2) +
  geom_point(aes(shape = as.factor(observation)), size = 4) +  # Increased size for better visibility
  scale_color_lancet() +
  labs(title = "Normalized Biomarkers",
       x = "",
       y = "Normalized Value",
       color = "Observation",
       shape = "Observation") +  # Label for shape legend
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "right",
        axis.title.x = element_blank())

p2

ggsave(
  filename = here("images", "archive", "biomarker_graph_demo_v2.png"),
  plot = p2,
  dpi = 600,
  width = 14,
  height = 8
)