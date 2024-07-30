# clear environment
rm(list = ls()); gc()

# load or install necessary libraries
pacman::p_load(tidyverse, rio, here, googlesheets4, ggsci, lubridate, dotenv, ggthemes, zoo)

# load .env file containing URL to Google Sheet (OPSEC)
load_dot_env()

# read in URL and email associated with Google Sheet from .env file
google_sheet_url <- Sys.getenv("GOOGLE_SHEET_URL")
google_sheet_email <- Sys.getenv("GOOGLE_SHEET_EMAIL")

# add formatted data to the end of a file to differentiate when ran
formatted_date <- format(Sys.Date(), "_%Y_%m_%d")

# authenticate and save token for authorization
gs4_auth(cache = ".secrets", email = google_sheet_email)

# load 7-day average and daily weight sheets
seven_day_avg <- read_sheet(
  ss = google_sheet_url, # identifies the Google Sheet
  sheet = "7-day Average" # specify which sheet if multiple sheets available
)

daily_weight <- read_sheet(
  ss = google_sheet_url,
  sheet = "Daily Weight"
)

# daily_weight2 <- daily_weight %>% 
#   arrange(Day) %>% 
#   mutate(
#     row_number = row_number(),
#     week = ((row_number - 1) %/% 7) + 1
#   )

# data prep ---------------------------------------------------------------

# extract last day for each week
weeks <- daily_weight %>% 
  group_by(Week) %>%
  arrange(desc(Day)) %>% 
  slice(1) %>% 
  mutate(Day = as.Date(Day)) %>% 
  filter(Week != 0)

# convert the `Day` column in spreadsheet to a Date type
daily_weight$Day <- as.Date(daily_weight$Day)

# compute week numbers with `lubridate::week` and adjust to start from May 6th, 2024
# baseline_data <- as.Date("2024-05-06")

start_date <- as.Date("2024-05-01")

# week numbers used to track Jasper's weight throughout the 84 (12-week) GS-441524 protocol
daily_weight <- daily_weight %>% 
  arrange(Day) %>% 
  mutate(week = Week,
    # row_number = row_number(),
    # week = ((row_number - 1) %/% 7) + 1,
         is_week_max_date = case_when(
           Day %in% weeks$Day ~ "Y",
           TRUE ~ "N"
         ))

temp <- daily_weight %>%
  arrange(Day) %>%
  mutate(rnk = row_number(),
         rnk2 = dense_rank(Day)) %>% 
  group_by(rnk2) %>% 
  summarize(count = n()) %>% 
  filter(count > 1)

# export(daily_weight, here("output data", "weeks.csv"))

# correct weeks for dates before the baseline (i.e., week = 0 prior to May 6th, 2024); using base R
# daily_weight$week[daily_weight$Day < baseline_data] <- 0

### plot time-series of weekly weights; require week to have complete data i.e., not in-progress weeks are plotted

# determine which weeks have full data i.e., full 7-days and no-NAs
complete_weeks <- daily_weight %>% 
  group_by(week) %>% 
  filter(!is.na(`Weight (lbs)`)) %>% # remove days/weeks that are NAs
  mutate(freq = n()) %>%
  filter(freq == 7) %>%  # a completed week must have 7-days of non-NA measurements
  ungroup()

# compute the mean weight for each week - used in plots
mean_weight <- complete_weeks %>% 
  group_by(week) %>% 
  summarize(mean_weight = mean(`Weight (lbs)`, na.rm = TRUE)) # na.rm = TRUE optional since already taken care above

# compute changes between consecutive weeks - used in plots
mean_weights <- mean_weight %>% 
  mutate(
    week_next = lead(week),
    mean_next = lead(mean_weight),
    delta = mean_next - mean_weight,
    midpoint = (week + week_next) / 2 # used to plot the delta every 0.5 weeks on the main plot
    )

# creating ggplots --------------------------------------------------------

# time-series of weekly averages
p1 <- daily_weight %>% 
  filter(Day %in% complete_weeks$Day) %>%  # filter origin data to only where complete week data available
  group_by(week) %>% 
  mutate(mean_weight = mean(`Weight (lbs)`, na.rm = TRUE), # compute mean/median weight to add to boxplots
         median_weight = median(`Weight (lbs)`, na.rm = TRUE)) %>% 
  ggplot(aes(x = week, y = `Weight (lbs)`, group = week)) +
  geom_boxplot(fill = 'lightblue', color = 'darkblue', alpha = 1) +
  geom_point(aes(x = week, y = mean_weight), color = 'red', size = 3) +
  geom_line(aes(x = week, y = mean_weight, group = 1)) + # line connecting the mean weight across boxes
  geom_text(
    aes(x = week, y = mean_weight, label = sprintf('Mean: %.2f lbs', mean_weight)),
    vjust = 2.5,
    color = 'red'
  ) +
  geom_text(
    aes(x = week, y = median_weight, label = sprintf('Median: %.2f lbs', median_weight)),
    vjust = -1.5,
    color = 'blue'
  ) +
  geom_text(
    data = mean_weights,
    aes(x = midpoint + 0.10 , y = 9.7, label = sprintf('Change: %.2f lbs', delta)),
    vjust = -0.5,
    color = 'black'
  ) +
  labs(x = 'Week',
       y = 'Weight (lbs)',
       title = "Jasper's Weight (lbs) Distribution",
       subtitle = paste0(format(min(complete_weeks$Day), "%m/%d/%Y"), ' - ' , format(max(complete_weeks$Day), "%m/%d/%Y"))
       ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = length(unique(complete_weeks$week)))) +
  theme_bw()

p1

# dynamically save ggplot
ggsave(
  filename = paste0(here("images", "boxplot", "jasper_mean_weight_weekly_time_series"), format(Sys.Date(), "_%Y_%m_%d"), ".png"),
  plot = p1,
  dpi = 600,
  width = 12,
  height = 8
)

# time-series of daily weights
p2 <- daily_weight %>% 
  filter(!is.na(`Weight (lbs)`)) %>% # filter after May 5th, 2024 and exclude NAs
  ggplot(aes(x = Day, y = `Weight (lbs)`)) +
  geom_hline(aes(yintercept = 11), color = "red", linetype = "dashed", size = 1.5) +
  geom_hline(aes(yintercept = (11 + 12) / 2), color = "orange", linetype = "dashed", size = 1.5) + # healthy weight between [11.25, 12] (lbs) add average of two intervals as reference line
  geom_hline(aes(yintercept = 12), color = "green", linetype = "dashed", size = 1.5) +
  geom_hline(aes(yintercept = 9), color = "red", linetype = "dashed", size = 1.5) +
  # geom_vline(data = weeks, aes(xintercept = Day), linetype = "dashed", color = "lightgrey") +
  geom_point(size = 2, color = "black") +
  geom_line(size = 1.0, color = "black") +
  geom_line(aes(y = rollmean(`Weight (lbs)`, 7, na.pad = TRUE)), color = "blue", linetype = "dashed", size = 0.7) +
  geom_point(aes(color = factor(is_week_max_date)), size = 2) +
  geom_text(data = subset(daily_weight, Day == as.Date("2024-05-12")), 
            aes(label = "Red dots indicates end of week", x = Day - 3, y = `Weight (lbs)` + 0.5), size = 2.5, vjust = 0) +
  geom_segment(data = subset(daily_weight, Day == as.Date("2024-05-07")), 
               aes(xend = Day, yend = `Weight (lbs)`, x = Day, y = `Weight (lbs)` + 0.8),
               arrow = arrow(type = "closed", length = unit(0.1, "inches"), ends = "last"), color = "black") +
  geom_text(data = subset(daily_weight, Day == as.Date("2024-05-20")),
            aes(label = "Period of stinky diarrheas due to antiviral :(", x = Day,  y = 10.65), size = 2.5, vjust = 0) +
  geom_segment(data = subset(daily_weight, Day == as.Date("2024-05-20")),
               aes(xend = Day, yend = `Weight (lbs)` + 0.50, x = Day, y = 10.55),
               arrow = arrow(type = "closed", length = unit(0.1, "inches"), ends = "last"), color = "black") +
  labs(x = "Date",
       y = "Weight (lbs)",
       title = "Jasper's Daily Weight (lbs)",
       subtitle = "Healthy Weight [11, 12]; 11.5 is average of the interval"
  ) +
  scale_colour_manual(values = c("Y" = "red", "N" = "black")) +
  scale_y_continuous(expand = c(0, 0), limits = c(7, 15), 
                     breaks = c(7, 8, 9, 10, 11, 11.5, 12, 13, 14, 15)) +
  # scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) +
  scale_x_date(date_breaks = "4 days", date_labels = "%m/%d/%Y") +
  theme_clean() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none")

p2


# dynamically save ggplot
ggsave(
  filename = paste0(here("images", "line", "jasper_weight_daily_time_series"), format(Sys.Date(), "_%Y_%m_%d"), ".png"),
  plot = p2,
  dpi = 600,
  width = 16,
  height = 8
)

# violin plot
npg_palette_alpha_0.7 <- c("#E64B35B2", "#4DBBD5B2", "#00A087B2", "#3C5488B2", "#F39B7FB2", "#8491B4B2", "#91D1C2B2", "#DC0000B2", "#7E6148B2")
npg_palette_alpha_1 <-  c("#E64B35FF", "#4DBBD5FF", "#00A087FF", "#3C5488FF", "#F39B7FFF", "#8491B4FF", "#91D1C2FF", "#DC0000FF", "#7E6148FF")


custom_colors <- c("#F39B7FFF", "#4DBBD5FF", "#91D1C2FF", "#7fc8ff", "#8491B4B2","#d3860e", "#ffd145", "#A9C1A8", "#E0218A", "#ffbe79","#ff8c87","#FFF68F", "#4575F3")


###
# violin with dotplot
p3 <- daily_weight %>%
  filter(Day %in% complete_weeks$Day) %>%  # filter origin data to only where complete week data available
  group_by(week) %>% 
  mutate(mean_weight = mean(`Weight (lbs)`, na.rm = TRUE), # compute mean/median weight to add to boxplots
         median_weight = median(`Weight (lbs)`, na.rm = TRUE),
         max_weight = max(`Weight (lbs)`, na.rm = TRUE)  # calculate max weight per week
  ) %>% 
  ungroup() %>% 
  ggplot(aes(x = week, y = `Weight (lbs)`, group = week, fill = factor(week))) +
  geom_violin(aes(x = week, y = `Weight (lbs)`, fill = factor(week))) +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.2) +
  # geom_point(position = position_jitter(width = 0.15), alpha = 0.8, size = 2) +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.25, fill = "black") +
  geom_point(aes(x = week, y = mean_weight), color = "red", size = 3) +
  geom_line(aes(x = week, y = mean_weight, group = 1)) +
  geom_text(
    aes(x = week, y = max_weight + 0.25, label = sprintf('Mean: %.2f lbs', mean_weight)),  # adjust y using max_weight
    vjust = 1,
    color = 'red'
  ) +
  geom_text(
    aes(x = week, y = max_weight + 0.15, label = sprintf('Median: %.2f lbs', median_weight)),  # adjust y using max_weight
    vjust = 1,
    color = 'blue'
  ) +
  # geom_text(
  #   aes(x = week, y = mean_weight, label = sprintf('Mean: %.2f lbs', mean_weight)),
  #   vjust = 2.5,
  #   color = 'red'
  # ) +
  # geom_text(
  #   aes(x = week, y = median_weight, label = sprintf('Median: %.2f lbs', median_weight)),
  #   vjust = -1.5,
  #   color = 'blue'
  # ) +
  geom_text(
    data = mean_weights,
    aes(x = midpoint, y = 9.7, label = sprintf('Change: %.2f lbs', delta)),
    vjust = -0.5,
    color = 'black',
    size = 3
  ) +
  labs(x = 'Week',
       y = 'Weight (lbs)',
       title = "Jasper's Weight (lbs) Distribution",
       subtitle = paste0(format(min(complete_weeks$Day), "%m/%d/%Y"), ' - ' , format(max(complete_weeks$Day), "%m/%d/%Y"))
  ) +
  scale_fill_manual(values = custom_colors) +
  scale_y_continuous(expand = c(0, 0), n.breaks = 8) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = length(unique(complete_weeks$week)))) +
  # scale_x_continuous(breaks = 1:12) +
  # theme_minimal() +
  theme_bw() +
  # theme_clean() +
  theme(legend.position = "none")

p3

## OLD p3 version
# p3 <- daily_weight %>% 
#   filter(Day %in% complete_weeks$Day) %>%  # filter origin data to only where complete week data available
#   group_by(week) %>% 
#   mutate(mean_weight = mean(`Weight (lbs)`, na.rm = TRUE), # compute mean/median weight to add to boxplots
#          median_weight = median(`Weight (lbs)`, na.rm = TRUE)) %>% 
#   ggplot(aes(x = week, y = `Weight (lbs)`, group = week)) +
#   geom_violin(aes(x = week, y = `Weight (lbs)`, fill = factor(week))) +
#   geom_point(aes(x = week, y = mean_weight), color = 'red', size = 3) +
#   geom_line(aes(x = week, y = mean_weight, group = 1)) + # line connecting the mean weight across boxes
#   geom_text(
#     aes(x = week, y = mean_weight, label = sprintf('Mean: %.2f lbs', mean_weight)),
#     vjust = 2.5,
#     color = 'red'
#   ) +
#   geom_text(
#     aes(x = week, y = median_weight, label = sprintf('Median: %.2f lbs', median_weight)),
#     vjust = -1.5,
#     color = 'blue'
#   ) +
#   geom_text(
#     data = mean_weights,
#     aes(x = midpoint, y = 9.7, label = sprintf('Change: %.2f lbs', delta)),
#     vjust = -0.5,
#     color = 'black'
#   ) +
#   labs(x = 'Week',
#        y = 'Weight (lbs)',
#        title = "Jasper's Weight (lbs) Distribution",
#        subtitle = paste0(format(min(complete_weeks$Day), "%m/%d/%Y"), ' - ' , format(max(complete_weeks$Day), "%m/%d/%Y"))
#        ) +
#   scale_fill_manual(values = custom_colors) +
#   # scale_y_continuous(expand = c(0, 0), limits = c(7, 13)) +
#   scale_x_continuous(breaks = scales::pretty_breaks(n = length(unique(complete_weeks$week)))) +
#   theme_bw() +
#   theme(legend.position = "none")
# 
# p3

# dynamically save ggplot
ggsave(
  filename = paste0(here("images", "violin", "jasper_mean_weight_violin_weekly_time_series"), format(Sys.Date(), "_%Y_%m_%d"), ".png"),
  plot = p3,
  dpi = 600,
  width = 16,
  height = 8
)





###
# pull out the most recent plots for each variant and place it into images directory
boxplot_directory <- here("images", "boxplot")
line_directory <- here("images", "line")
violin_directory <- here("images", "violin")
biomarkers_directory <- here("images", "biomarkers")

directories <- list(boxplot_directory, line_directory, violin_directory, biomarkers_directory)

parent_directory <- here("images")

# place images in root folder for README (add these files to .gitignore)
root <- here()

historical_images_directory <- here("images", "historical")

# function to move files and copy the most recent one
move_most_recent_file <- function(directory) {
  
  # list all files in the child directories
  files <- list.files(directory, pattern = "\\.png$", full.names = TRUE)
  
  # skip if no files are found
  if (length(files) == 0) {
    return(NULL)
  }
  
  # grab the most recent file based on modification time
  most_recent_file <- files[which.max(file.info(files)$mtime)]
  
  # specify the destination in the parent directory
  destination_file <- file.path(parent_directory, basename(most_recent_file))
  
  # specify the destination of the root directory
  destination_file2 <- file.path(root, basename(most_recent_file))
  
  # copy the most recent file to the parent directory
  file.copy(most_recent_file, destination_file, overwrite = TRUE)
  
  # copy the most recent file to the root directory
  file.copy(most_recent_file, destination_file2, overwrite = TRUE)
}

# list files in the parent directory
parent_files <- list.files(parent_directory, pattern = "\\.png$", full.names = TRUE)

# filter out directories and only get files
parent_files <- parent_files[file.info(parent_files)$isdir == FALSE]

# move files from the parent directory to the historical directory
file.copy(parent_files, historical_images_directory, overwrite = TRUE)
file.remove(parent_files)

# apply function to move most recent image for each variant to parent at once
map(directories, move_most_recent_file)

# update the images README file
source(here("scripts", "update_images_readme.R"))

# update root README file
source(here("scripts", "update_root_readme.R"))

