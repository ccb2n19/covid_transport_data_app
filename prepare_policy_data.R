# Setup
library(tidyverse)
library(readxl)
library(readODS)
library(ggplot)
library(plotly)

# Set working directory
setwd("C:/Users/brown/Desktop/covid_data_app/data")

# Read in data
measures_1_orig <- read_xlsx("policy_measures.xlsx", sheet = 2, skip = 1)

measures_2_orig <- read_xlsx("policy_measures.xlsx", sheet = 3, skip = 1)

x <- ncol(measures_1_orig)

transport_terms <- tribble(
  ~term,
  "car",
  "bike",
  "bicycle",
  "cycle",
  "bus",
  "train",
  "plane",
  "travel",
  "transport") %>%
  mutate(caps = str_to_title(term))

transport_regex <- paste(
  paste(transport_terms$term, collapse = "\\s|\\s", sep = ""),
  paste(transport_terms$caps, collapse = "\\s|\\s", sep = ""),
  collapse = "\\s|\\s", sep = "")
  
# Join two sheets
measures_full <- bind_rows(
  measures_1_orig %>% 
    filter(!str_detect(Date, "^[:alpha:]")) %>%
    mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30")),
  measures_2_orig) %>%
  as_tibble() %>%
  pivot_longer(cols = 3:x) %>%
  mutate(name = str_to_lower(name),
         name = str_replace_all(name, " |\\/", "_"),
         name = str_replace_all(name, "\\(|\\)|\\-|\\,", ""),
         name = str_replace_all(name, "_(?=\\_)", "")) %>%
  pivot_wider(names_from = name,
              values_from = value)

measures_full %>%
  write_csv("measures_full.csv")

transport_field_title <- "Transport-specific policy"

measures_full <- measures_full %>%
  mutate(transport_policy = case_when(
    str_detect(summary_of_measure_change, transport_regex)         ~ transport_field_title,
    str_detect(introduced_by, transport_regex)                     ~ transport_field_title,
    str_detect(how_key_components_and_mechanisms, transport_regex) ~ transport_field_title,
    subcategory == "Travel measures"                               ~ transport_field_title,
    TRUE                                                           ~ "Wider policies"))

# Tile graph

text_width <- 40
b <- "<b>"
nb <- "</b>"
nl <- "<br>"
p <- "<p>"
ep <- "</p>" 
  
measures_full_graph <- measures_full %>%
  group_by(Date, transport_policy) %>%
  mutate(number = row_number()) %>%
  ungroup() %>%
  mutate(nice_date = format(Date, format="%d %B %Y"),
         header = case_when(!is.na(introduced_by)             ~ introduced_by,
                            TRUE                              ~ "Milestone"),
         body_1 = case_when(!is.na(summary_of_measure_change) ~ summary_of_measure_change,
                            TRUE                              ~ Milestone),
         body_2 = targeted_at,
         nice_date = paste0("Announced: ", nice_date, nl),
         
         header = paste0(b, str_wrap(header, text_width), nb, nl),
         body_1 = paste0(str_wrap(body_1, text_width)),
         body_2 = case_when(!is.na(targeted_at)               ~ str_wrap(paste0(nl, "Targeted at: ", body_2), text_width)),
         text = paste0(header, nice_date, body_1, body_2)) %>%
  select(Date, number, subcategory, header, nice_date, body_1, body_2, text, transport_policy)

tile <- ggplot(data = measures_full_graph, aes(x = Date, y = number, text = text)) +
  geom_tile(aes(fill = subcategory), colour = "black") +
  facet_wrap(~transport_policy,
             ncol = 1) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ggplotly(tile, tooltip = "text")