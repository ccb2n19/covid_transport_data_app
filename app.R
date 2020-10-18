# Setup
library(shiny)
library(tidyverse)
library(tmap)
library(readxl)
library(lubridate)
library(plotly)
library(egg)

# Get covid data
# setwd("C:/Users/brown/Desktop/covid_data_app")
daily_data <- read_csv("data/daily_data_for_app.csv")
measures_full <- read_csv("data/measures_full.csv")
data_notes <- read_csv("data/data_notes.csv")

# Create transport regex
transport_terms <- tribble(
  ~term,
  "car",
  "cars",
  "bike",
  "bicycle",
  "cycling",
  "cycle",
  "bus",
  "buses",
  "train",
  "trains",
  "airport",
  "station",
  "airports",
  "stations",
  "plane",
  "travel",
  "transport") %>%
  mutate(caps = str_to_title(term))

transport_regex <- paste(
  paste(transport_terms$term, collapse = "\\s|\\s", sep = ""),
  paste(transport_terms$caps, collapse = "\\s|\\s", sep = ""),
  collapse = "\\s|\\s", sep = "")

transport_field_title <- "Transport-specific policy"


# Process measures data
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

# Make nice tooltip labels
## Daily data
daily_data <- daily_data %>%
  mutate(
    nice_percent = paste0("Traffic level compared to pre-Covid: ", percent_diff, "%"),
    nice_mode = paste0(mode, nl),
    nice_date = paste0(format(date, format="%d %B %Y"), nl),
    Summary = paste0(nl, nice_date, nice_mode, nice_percent))

## Measures
measures_full_graph <- measures_full %>%
  group_by(Date, transport_policy) %>%
  mutate(number = row_number()) %>%
  ungroup() %>%
  mutate(Date = as.Date(Date),
         nice_date = format(Date, format="%d %B %Y"),
         header = case_when(!is.na(introduced_by)             ~ introduced_by,
                            TRUE                              ~ "Milestone"),
         body_1 = case_when(!is.na(summary_of_measure_change) ~ summary_of_measure_change,
                            TRUE                              ~ Milestone),
         body_2 = targeted_at,
         nice_date = paste0("Announced: ", nice_date, nl),
         
         header = paste0(b, str_wrap(header, text_width), nb, nl),
         body_1 = paste0(str_wrap(body_1, text_width)),
         body_2 = case_when(!is.na(targeted_at)               ~ str_wrap(paste0(nl, "Targeted at: ", body_2), text_width),
                            TRUE                              ~ ""),
         text = paste0(header, nice_date, body_1, body_2)) %>%
  select(Date, number, subcategory, header, nice_date, body_1, body_2, text, transport_policy)

# Create objects defined by the data source ...
startdate <- daily_data$date %>% min()
enddate <- daily_data$date %>% max()
mode_list <- daily_data %>%
  distinct(mode) %>%
  as.list()

# Things to do:
## Map showing spatial change in COVID cases across the period

ui <- fluidPage(

  # Date range
  
  h1("Transport during Covid-19: traffic and policy tracker"),
  h3("Author: ", a("Christopher C Brown", href = "https://www.linkedin.com/in/christopher-c-brown-62280842/", target="_blank")),
  p("This page presents data on UK transport activity from the", a("Department for Transport", href = "https://www.gov.uk/government/statistics/transport-use-during-the-coronavirus-covid-19-pandemic", target="_blank"), "(last updated on 14 October 2020)
    alongside information on transport and wider policy measures taken from ", a("The Health Foundation", href = "https://www.health.org.uk/news-and-comment/charts-and-infographics/covid-19-policy-tracker", target="_blank"), "(last updated 1 October 2020)."),
  br(),
  dateRangeInput(
            inputId   = "daterange",
            label     = "Change date range",
            format    = "dd/mm/yyyy",
            start     = startdate,
            end       = enddate,
            min       = startdate,
            max       = enddate,
            weekstart = 1),
  # Select mode
  selectInput(
            inputId   = "mode",
            label     = "Select mode(s)",
            choices   = mode_list,
            selected = "Cars",
            multiple  = TRUE,
            selectize = TRUE
            ),
  h2("Traffic as share of pre-pandemic traffic"),
  plotlyOutput(outputId = "line_graph"),
  tableOutput(outputId = "data_notes_table"),
  h2("Transport policy measures"),
  p("Each block represents one policy measure, arranged by the date of the policy was announced. Hover over each block to view a summary of the policy (works better on desktop)."),
  plotlyOutput(outputId = "policy_graph"),
  h2("Wider policy measures"),
  plotlyOutput(outputId = "wider_policy_graph"),
  h2("Data sources"),
  p(a("COVID-19 policy tracker, The Health Foundation", 
    href="https://www.health.org.uk/news-and-comment/charts-and-infographics/covid-19-policy-tracker", target="_blank"),   br(),
  a("Transport use during the coronavirus (COVID-19) pandemic, Department for Transport",
    href="https://www.gov.uk/government/statistics/transport-use-during-the-coronavirus-covid-19-pandemic")),
  h2("App creation"),
  p("This app was created ", a("in R Studio", href = "https://rstudio.com/"), " using ", a("Shiny", href = "https://shiny.rstudio.com/", target = "_blank"), 
    " and the packages", a("ggplot2", href = "https://ggplot2.tidyverse.org/"), " and ", a("plotly", href = "https://plotly-r.com/", target = "_blank"), "."),
  p("Source code is available on ", a("Github", href = "https://github.com/ccb2n19/covid_transport_data_app/", target = "_blank"), ".")
)

server <- function(input, output) {
  
  # Filter data to include points within range
  data_plot <- reactive({
                  daily_data %>%
                    filter(date >= input$daterange[1] & date <= input$daterange[2], # Date
                           mode %in% input$mode)                                    # Mode
  })
  
  data_notes_re <- reactive({
    data_notes %>%
      filter(Mode %in% input$mode) %>%
      arrange(Mode)
  })
  
  policy_data_plot <- reactive({
    measures_full_graph %>%
      filter(Date >= input$daterange[1] & Date <= input$daterange[2]) # Date
  })
  
  output$date_limits <- reactive({
    c(input$daterange[1], input$daterange[2]) # Date
  })
  
  date_limits <- reactive({
    c(input$daterange[1], input$daterange[2]) # Date
  })
  
  date_start <- reactive({
    as.numeric(input$daterange[1])
  })
    
  date_end <- reactive({
    as.numeric(input$daterange[2])
  })

  # Create the line graph
  output$line_graph <- renderPlotly({ 
    ggplotly(ggplot(data    = data_plot(),
           aes(
               x            = date, 
               y            = percent_diff,
               label        = Summary)) +
      geom_line(aes(colour  = mode)) +
      geom_hline(yintercept = 100,
                 linetype   = "dashed",
                 alpha      = 0.5,
                 colour     = "grey60") +
      labs(colour           = "Mode",
           y                = "%",
           x                = "") +
      theme(panel.background = element_blank(),
            axis.line        = element_line(colour = "black"),), tooltip = c("label")) %>% layout(legend = list(orientation = "h", x = 0, y = -0.2))
  })
  
  # Create data notes table
  output$data_notes_table <- renderTable({data_notes_re()})
  
  # Create policy graph
  output$policy_graph <- renderPlotly({
    ggplotly(ggplot(data = policy_data_plot() %>% filter(transport_policy == transport_field_title), 
                    aes(
                      x    = Date, 
                      y    = number, 
                      text = text)) +
                      geom_tile(aes(fill = subcategory), colour = "black") +
                      scale_x_date(limits = c(input$daterange[1], input$daterange[2])) +
                      labs(y = "Testing",
                           x = "") +
                      theme(panel.grid.major = element_blank(), 
                            panel.grid.minor = element_blank(),
                            panel.background = element_blank(),
                            axis.title.y     = element_text(colour = "white"),
                            axis.text.y      = element_text(colour = "white"),
                            axis.ticks.y     = element_line(colour = "white"),
                            axis.line.x      = element_line(colour = "black"),
                            strip.background = element_blank()),
             
             tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y = -0.2))
  })
  
  output$wider_policy_graph <- renderPlotly({
    ggplotly(ggplot(data = policy_data_plot() %>% filter(transport_policy != transport_field_title), 
                    aes(
                      x    = Date, 
                      y    = number, 
                      text = text)) +
               geom_tile(aes(fill = subcategory), colour = "black") +
               scale_x_date(limits = c(input$daterange[1], input$daterange[2])) +
               labs(y = "Testing",
                    x = "") +
               theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.title.y     = element_text(colour = "white"),
                     axis.text.y      = element_text(colour = "white"),
                     axis.ticks.y     = element_line(colour = "white"),
                     axis.line.x      = element_line(colour = "black"),
                     strip.background = element_blank()),
             
             tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y = -0.2))
  })

}

shinyApp(ui     = ui, 
         server = server)