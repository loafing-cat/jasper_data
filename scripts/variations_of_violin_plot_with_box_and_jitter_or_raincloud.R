# clear environment
rm(list = ls()); gc()

# load or install necessary libraries
pacman::p_load(tidyverse, rio, here, googlesheets4, ggsci, lubridate, dotenv, PupillometryR, zoo)

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


# data prep ---------------------------------------------------------------

# convert the `Day` column in spreadsheet to a Date type
daily_weight$Day <- as.Date(daily_weight$Day)

# compute week numbers with `lubridate::week` and adjust to start from May 6th, 2024
baseline_data <- as.Date("2024-05-06")

# week numbers used to track Jasper's weight throughout the 84 (12-week) GS-441524 protocol
daily_weight <- daily_weight %>% 
  mutate(week = (week(Day) - week(baseline_data) + 1))

# correct weeks for dates before the baseline (i.e., week = 0 prior to May 6th, 2024); using base R
daily_weight$week[daily_weight$Day < baseline_data] <- 0

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

# making violin -----------------------------------------------------------
npg_palette_alpha_0.7 <- c("#E64B35B2", "#4DBBD5B2", "#00A087B2", "#3C5488B2", "#F39B7FB2", "#8491B4B2", "#91D1C2B2", "#DC0000B2", "#7E6148B2")
npg_palette_alpha_1 <-  c("#E64B35FF", "#4DBBD5FF", "#00A087FF", "#3C5488FF", "#F39B7FFF", "#8491B4FF", "#91D1C2FF", "#DC0000FF", "#7E6148FF")

custom_colors <- c("#F39B7FFF", "#4DBBD5FF", "#91D1C2FF", "#93b3fc", "#8491B4B2", "#7E6148FF", "#DC0000B2")

# violin with boxplot
daily_weight %>%
  filter(Day %in% complete_weeks$Day) %>%
  ggplot(aes(x = week, y = `Weight (lbs)`, group = week, fill = factor(week))) +
  geom_violin(width = 1.4) +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.2) +
  geom_point(position = position_jitter(width = 0.15), alpha = 0.8, size = 2) +
  # geom_jitter(color="black", size=0.7, alpha=0.5) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(legend.position = "none")

# violin with dotplot
daily_weight %>%
  filter(Day %in% complete_weeks$Day) %>%
  ggplot(aes(x = week, y = `Weight (lbs)`, group = week, fill = factor(week))) +
  geom_violin(width = 1.4, color = "black") +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.2) +
  # geom_point(position = position_jitter(width = 0.15), alpha = 0.8, size = 2) +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.35, fill = "black") +
  # geom_jitter(color="black", size=0.7, alpha=0.5) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(legend.position = "none")


# rain cloud plot
daily_weight %>%
  filter(Day %in% complete_weeks$Day) %>%
  ggplot(aes(x = week, y = `Weight (lbs)`, group = week, fill = factor(week))) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), adjust = 2) +
  geom_boxplot(position = position_nudge(x = -0.15), width = 0.1, color = "black", alpha = 0.2) +
  geom_point(position = position_jitter(width = 0.05), size = 1.5, alpha = 0.8, color = "black") +
  # geom_jitter(color="black", size=0.7, alpha=0.5) +
  scale_fill_manual(values = custom_colors) +
  coord_flip() + 
  theme_minimal() +
  theme(legend.position = "none")



# line --------------------------------------------------------------------

p2 <- daily_weight %>% 
  filter(Day > as.Date('2024-05-05') & !is.na(`Weight (lbs)`)) %>% # filter after May 5th, 2024 and exclude NAs
  ggplot(aes(x = Day, y = `Weight (lbs)`)) +
  geom_hline(aes(yintercept = 11), color = "red", linetype = "dashed", size = 1.5) +
  geom_hline(aes(yintercept = (11 + 12) / 2), color = "orange", linetype = "dashed", size = 1.5) + # healthy weight between [11.25, 12] (lbs) add average of two intervals as reference line
  geom_hline(aes(yintercept = 12), color = "green", linetype = "dashed", size = 1.5) +
  geom_hline(aes(yintercept = 9), color = "red", linetype = "dashed", size = 1.5) +
  geom_point(size = 2, color = "black") +
  geom_line(size = 1.0, color = "black") +
  geom_line(aes(y = rollmean(`Weight (lbs)`, 7, na.pad = TRUE)), color = "blue", linetype = "dashed", size = 0.7) +
  labs(x = "Date",
       y = "Weight (lbs)",
       title = "Jasper's Daily Weight (lbs)",
       subtitle = "Healthy Weight [11, 12]; 11.5 is average of the interval"
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(7, 15), 
                     breaks = c(7, 8, 9, 10, 11, 11.5, 12, 13, 14, 15)) +
  # scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) +
  scale_x_date(date_breaks = "1 day", date_labels = "%m/%d/%Y") +
  # scale_y_continuous(expand = c(0, 0), limits = c(5, 15), 
  #                    breaks = c(5, 6, 7, 8, 9, 10, 11, 11.25, 11.5, 12, 13, 14, 15)) +
  # scale_y_continuous(expand = c(0, 0), limits = c(0, 15), breaks = seq(0, 15, by = 1)) +
  # theme_bw() +
  # theme_ipsum() +
  # theme_ipsum_pub() +
  theme_clean() +
  # theme_economist_white() +
  # theme_calc() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2

# dynamically save ggplot
ggsave(
  filename = paste0(here("images", "line", "jasper_weight_daily_time_series_v11"), format(Sys.Date(), "_%Y_%m_%d"), ".png"),
  plot = p2,
  dpi = 600,
  width = 12,
  height = 8
)

