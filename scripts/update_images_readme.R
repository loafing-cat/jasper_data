rm(list = ls()); gc()

pacman::p_load(tidyverse, rio, here)

# define directory where images live and where README file for images needs to be placed
image_dir <- here("images")

# images that I update weekly; needs to be dynamically placed into a READ.md file
patterns <- c("jasper_mean_weight_violin_weekly_time_series_.*\\.png",
              "jasper_weight_daily_time_series_.*\\.png",
              # "jasper_mean_weight_weekly_time_series_.*\\.png"
              "biomarkers_graph_.*\\.png"
              )

# define function that grabs the most recent images for each type that exists (pattern defined above)
get_most_recent_file <- function(pattern) {
  # list files in image directory
  files <- list.files(path = image_dir, pattern = pattern, full.names = TRUE)
  # throws error if no images found
  if (length(files) == 0) return(NULL)
  # if multiple variants of the same type of image exists, order when last created
  files <- files[order(file.info(files)$mtime, decreasing = TRUE)]
  return(basename(files[1])) # return just the filename, necessary for correct Markdown formatting
}

# initiate empty vector containing lines in README.md file
markdown_lines <- c()

# for each type of image, apply the function to grab most recent one and if exists, write each line 
for (pattern in patterns) {
  recent_file <- get_most_recent_file(pattern)
  if (!is.null(recent_file)) {
    markdown_lines <- c(markdown_lines, sprintf("![image info](%s)", recent_file))
  } else {
    markdown_lines <- c(markdown_lines, "![image info](image_not_found.png)") # placeholder if no image is found
  }
}

# indicate where the README file needs to be placed
readme_placement <- file.path(image_dir, "README.md")

# create the README.md file in specified directory
writeLines(markdown_lines, readme_placement)
