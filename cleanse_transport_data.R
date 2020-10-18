# Setup
library(tidyverse)
library(readxl)
library(readODS)
library(ggplot)

# Set working directory
setwd("C:/Users/brown/Desktop/covid_data_app/data")

# Read in data
data_orig <- read_ods("transport_use_stats.ODS",
                      skip = 6)

# Set stringr expressions
percent_find <- "\\%"
num_start <- "^[:digit:]"
percent_ext <- "[:digit:][:digit:][:digit:](?=\\%)|[:digit:][:digit:](?=\\%)|[:digit:](?=\\%)"

# Number of columns
num_cols <- ncol(data_orig)

# Tidy and categorise
daily_data <- data_orig %>%
  as_tibble() %>%
  rename(date = 1) %>%
  filter(str_detect(date, "^\\d\\d/")) %>%
  mutate_all(as.character) %>%
  pivot_longer(2:num_cols,
               names_to  = "mode",
               values_to = "percent_diff") %>%
  mutate(mode = str_replace_all(mode, "\\(|\\)", "")) %>% #separate out the notes
  separate(mode,
           sep = "(?<=[a-z]) ?(?=[0-9])",
           into = c("mode", "note")) %>%
  mutate(
    percent_diff = case_when(
      str_detect(percent_diff, percent_find) ~ as.numeric(str_extract(percent_diff, percent_ext))/100,
      str_detect(percent_diff, num_start) ~ as.numeric(percent_diff)),
    percent_diff = percent_diff * 100,
    mode = str_replace(mode, "Transport for London", "TfL"),
    category = case_when(str_detect(mode, "TfL")                          ~ "London public",
                         str_detect(mode, "Bus|National Rail")            ~ "Other public",
                         str_detect(mode, "Heavy Goods|Light Commercial") ~ "Logistics",
                         TRUE ~ mode),
    date = as.Date(date, format = "%d/%m/%y"))

# Write to file
# daily_data %>%
#  write_csv("daily_data_for_app.csv")
