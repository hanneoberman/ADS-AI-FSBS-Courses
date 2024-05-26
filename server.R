library(DT)
library(here)
library(dplyr)
library(r2d3)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(packcircles)
library(ggplot2)
library(RColorBrewer)
library(htmlwidgets)
library(digest)
library(bit)
library(plotly)
library(tidyverse)
library(devtools)
library(ggthemes)
library(shinyWidgets)
library(rsconnect)
library(readxl)
library(readr)
library(googledrive)

# De-authenticate if necessary
drive_deauth()

# Authenticate with the necessary scopes
drive_auth(scope = "https://www.googleapis.com/auth/drive.readonly")

# Check if authentication was successful
print(drive_user())

# Specify the path to the CSV file on Google Drive
file_id <- "1iEk6Wrv7SLSvDPVgUhg17KZBz1qKsVyDLUHKWvDavCQ"

# Check if file can be found
print(drive_get(as_id(file_id)))

# Download the file and read it into R
temp_file <- tempfile(fileext = ".csv")
drive_download(as_id(file_id), path = temp_file, overwrite = TRUE)
DS <- read.csv(temp_file) # nolint

# Print first few rows of the dataframe
print(head(DS))

short <- read_excel("C:/Users/Shahe002/OneDrive - Universiteit Utrecht/Documents/ADS Task force/final version database/Shiny-ADS-AI-Courses/short.xlsx") # nolint
levels_names <- c("1 (Bachelor Inleiding)", "2 (Bachelor Verdiepend)", "3 (Bachelor Gevorderd)", "M (Master)", "HB (Bachelor Honours)") # nolint
levels_names_discr <- c("Introduction Bachelor", "Intermediate Bachelor", "Advance Bachelor", "Master", "Bachelor Honours") # nolint
course_names <- c("Cursorisch onderwijs", "Eindscriptie", "Master thesis", "onderzoeksproject", "Practicum", "stage") # nolint
course_names_discr <- c("Course", "Final thesis", "Master thesis", "research project", "Practical", "internship") # nolint
year_names <- c(unique(DS$year)[complete.cases(unique(DS$year))])
topic_choices <- c("Data science", "Causal inference", "data collection", "data analysis", "database Management", "nonSQL databases", "Relational databases", "Data wrangling", "Data Imputation", "Data Mining", "Missing Data", "Machine Learning", "Deep learning", "Network science", "Programming", "SQL", "Python", "Data clean-up", "QGIS", "PCRaster", "R statistical software", "Rmarkdown", "clustering", "linear algebra", "mathematical concepts", "regression", "gradient descent", "feedforward neural networks", "logistic regression", "Bayesian statistics", "Statistical models", "Stochastic Modeling", "Exploratory Data Analysis", "JASP", "MplusÂ", "Haskell", "Visual Studio", "Big data", "Statistics", "Visualizations", "SPSS", "Complex systems", "Text mining", "Artificial Intelligence", "Dataverzameling", "dataverwerking", "data analyse", "Statistiek", "All" = "all")

ui <- fluidPage(
  titlePanel("Course Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("level", "Select Level:", choices = c("all", levels_names)),
      selectInput("course_type", "Select Course Type:", choices = course_names),
      selectInput("year", "Select Year:", choices = year_names),
      selectInput("topic", "Select Topics:", choices = topic_choices, multiple = TRUE),
      actionButton("Reset_table", "Reset Table"),
      actionButton("action1", "Generate Plots"),
      selectInput("overview", "Overview By:", choices = c("year", "level", "course_type")),
      numericInput("min_course_type", "Minimum Course Type Value:", 0)
    ),
    mainPanel(
      textOutput("text"),
      d3Output("d3"),
      plotlyOutput("plots2"),
      dataTableOutput("table"),
      uiOutput("plots")
    )
  )
)

server <- function(input, output, session) {
  
  level <- reactive(input$level)
  course_type <- reactive(input$course_type)
  year <- reactive(input$year)
  
  observeEvent(input$Reset_table, {
    session$sendCustomMessage(type = "resetValue", message = "click_event")
  })
  
  observe({
    x <- input$topic
    if (is.null(x)) {
      updateSelectInput(session, "topic", label = paste("No topic chosen"), choices = topic_choices, selected = "")
    } else if (length(x) > 1 && any(x == "all")) {
      idx <- which(x == "all")
      updateSelectInput(session, "topic", label = paste("Topics to compare:"), choices = topic_choices, selected = x[-idx])
    } else if (length(x) == 1 && x == "all") {
      updateSelectInput(session, "topic", label = paste("Topics to compare:"), choices = topic_choices, selected = x)
    }
  })
  
  output$d3 <- renderD3({
    if (level() == "all") {
      data <- DS %>%
        select(c(1, 4, 5, 9, 10:57)) %>%
        filter(year == year(), course_type == course_type()) %>%
        pivot_longer(cols = where(is.numeric)) %>%
        group_by(name) %>%
        summarise(value = sum(value, na.rm = TRUE)) %>%
        filter(value > 0)
      colnames(data) <- c("id", "value")
      data <- left_join(data, short, by = "id")
      r2d3(data = data, d3_version = 4, script = "bubble.js")
    } else {
      data <- DS %>%
        select(c(1, 4, 5, 9, 10:57)) %>%
        filter(year == year(), level == level())
      if (course_type() %in% data$course_type) {
        data <- data %>%
          filter(course_type == course_type()) %>%
          pivot_longer(cols = where(is.numeric)) %>%
          group_by(name) %>%
          summarise(value = sum(value, na.rm = TRUE)) %>%
          filter(value > 0)
        colnames(data) <- c("id", "value")
        data <- left_join(data, short, by = "id")
        r2d3(data = data, d3_version = 4, script = "bubble.js")
      } else {
        data <- data.frame(id = "No data", value = 20, short = "No data available")
        r2d3(data = data, d3_version = 4, script = "bubble.js")
      }
    }
  })
  
  output$text <- renderText({
    if (is.null(input$click_event)) {
      paste0("The database of courses")
    } else {
      paste0("The database of courses that cover the ", input$click_event, " topics")
    }
  })
  
  input_plots <- reactiveValues()
  observeEvent(input$action1, {
    input_plots$over <- input$overview
    input_plots$min_course_type <- input$min_course_type
    if (input_plots$over == 'year') {
      names_for_bubble <- year_names
      plot_titles <- year_names
    } else if (input_plots$over == 'level') {
      names_for_bubble <- levels_names
      plot_titles <- levels_names_discr
    } else if (input_plots$over == 'course_type') {
      names_for_bubble <- course_names
      plot_titles <- course_names_discr
    }
    
    for (i in seq_along(names_for_bubble)) {
      local({
        my_i <- i
        plotname <- paste("plot", my_i, sep = "")
        data <- DS %>%
          select(year, c(1, 4, 5, 9, 10:57)) %>%
          filter(!!sym(input_plots$over) == names_for_bubble[my_i]) %>%
          pivot_longer(cols = where(is.numeric)) %>%
          group_by(name) %>%
          summarise(value = sum(value, na.rm = TRUE))
        
        if (all(data$value < input_plots$min_course_type)) {
          data <- data.frame(id = "No data", value = 20, short = "No data available")
        } else {
          data <- data %>% filter(value > (input_plots$min_course_type - 1))
          colnames(data) <- c("id", "value")
          data <- left_join(data, short, by = "id")
        }
        output[[plotname]] <- renderD3({
          r2d3(data = data, d3_version = 4, script = "bubble_2.js")
        })
      })
    }
    
    output$plots <- renderUI({
      plot_output_list <- lapply(seq_along(names_for_bubble), function(i) {
        plotname <- paste("plot", i, sep = "")
        column(width = 6,
               h4(plot_titles[i], align = "center"),
               tags$div(style = "margin-top: 0px; margin-bottom: 0px;", d3Output(plotname))
        )
      })
      do.call(tagList, plot_output_list)
    })
  })
  
  output$table <- DT::renderDataTable({
    if (is.null(input$click_event)) {
      if (level() == 'all') {
        DS %>% filter(year == year(), DS$course_type == course_type()) %>% select(c(1, 2, 3, 6, 7, 8))
      } else {
        DS %>% filter(year == year(), level == level(), DS$course_type == course_type()) %>% select(c(1, 2, 3, 6, 7, 8))
      }
    } else {
      if (level() == 'all') {
        DS %>% filter(year == year(), DS$course_type == course_type(), (!!sym(input$click_event)) == 1) %>% select(c(1, 2, 3, 6, 7, 8))
      } else {
        DS %>% filter(year == year(), level == level(), DS$course_type == course_type(), (!!sym(input$click_event)) == 1) %>% select(c(1, 2, 3, 6, 7, 8))
      }
    }
  }, server = FALSE, options = list(searching = TRUE))
  
  output$plots2 <- renderPlotly({
    short2 <- DS %>%
      select(year, c(4, 10:57)) %>%
      pivot_longer(cols = -year) %>%
      group_by(name, year) %>%
      summarise(total = sum(as.numeric(value), na.rm = TRUE)) %>%
      na.omit()
    
    plot1_perc <- short2 %>%
      mutate(total = round((total / sum(total)) * 100, 2))
    
    if (any(input$topic != "all")) {
      plot1_perc <- plot1_perc %>% filter(name %in% input$topic)
    }
    
    ggplotly(
      plot1_perc %>% ggplot(aes(x = total, y = name, fill = as.factor(year))) +
        geom_col() +
        scale_fill_brewer(palette = "Set2") +
        ylab("") +
        xlab("percentage (%)") +
        theme_minimal()
    ) %>% layout(height = 800, width = 1000)
  })
}


      