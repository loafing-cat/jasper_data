rm(list = ls()); gc()

pacman::p_load(tidyverse, rio, here, googlesheets4, ggsci, lubridate, dotenv, ggthemes, zoo)

# import data
biomarker_data <- import(here("output data", "cbc_chem_updated.csv"))
reference_table <- import(here("input data", "biomarker_reference_table.csv"))

# use row_number() to create values for dot type/colors
biomarker_data <- biomarker_data %>% 
  group_by(metric) %>% 
  arrange(date_updated) %>% 
  mutate(observation = row_number())

# join biomarker data with reference table
joined_data <- biomarker_data %>%
  left_join(reference_table, by = c("metric" = "biomarkers")) %>%
  filter(metric %in% c("WBC", "Total Protein", "RBC", "Albumin: Globulin Ratio", "Eosinophils", "Lymphocytes", "ALP", "ALT", "Basophils", "Monocytes", "Platelets", "Albumin", "Globulin"))

# normalize data (min-max method)
joined_data <- joined_data %>%
  mutate(
    norm_value = (as.numeric(value) - min) / (max - min),
    norm_min_ref = 0,
    norm_max_ref = 1
  )

joined_data <- joined_data %>% 
  mutate(metric = factor(metric, levels = c("Albumin: Globulin Ratio", "Albumin", "Globulin", "Total Protein", "ALP", "ALT", "WBC", "RBC", "Platelets", "Eosinophils", "Basophils", "Lymphocytes", "Monocytes")))
# joined_data2$metric <- factor(joined_data2$metric, levels = c("WBC", "RBC", "Total Protein", "Albumin: Globulin Ratio", "Eosinophils", "Lymphocytes", "ALP", "ALT", "Basophils"))

# Plot using ggplot2
p1 <- ggplot(joined_data, aes(x = metric, y = norm_value, color = as.factor(observation), shape = as.factor(observation))) +
  geom_errorbar(aes(ymin = norm_min_ref, ymax = norm_max_ref), color = "black", width = 0.2) +
  geom_point(aes(shape = as.factor(observation)), size = 4) +  # Increased size for better visibility
  scale_color_lancet() +
  scale_shape_manual(values = c(16, 17, 18, 15)) +
  labs(title = "Normalized Biomarkers",
       x = "",
       y = "Normalized Value",
       color = "Observation",
       shape = "Observation") +  # Label for shape legend
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "right",
        axis.title.x = element_blank())

p1

ggsave(
  filename = paste0(here("images", "biomarkers", "biomarkers_graph"), format(Sys.Date(), "_%Y_%m_%d"), ".png"),
  plot = p1,
  dpi = 600,
  width = 14,
  height = 8
)

# p1 <- ggplot(joined_data, aes(x = metric, y = norm_value, color = as.factor(observation), shape = as.factor(observation))) +
#   geom_errorbar(aes(ymin = norm_min_ref, ymax = norm_max_ref), color = "black", width = 0.2) +
#   geom_point(size = 4) +
#   scale_shape_manual(values=c(16, 17, 15)) +
#   scale_color_lancet() +
#   labs(title = "Normalized Biomarker Values with Reference Bands",
#        x = "Biomarker",
#        y = "Normalized Value (0-1)",
#        color = "Observation",
#        shape = "Observation") + 
#   theme_bw()
# 
# p1
# 
# ggsave(
#   filename = here("images", "archive", "biomarker_graph_demo.png"),
#   plot = p1,
#   dpi = 600,
#   width = 12,
#   height = 8
# )