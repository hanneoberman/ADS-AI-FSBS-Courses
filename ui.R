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
library(htmlwidgets)
library(devtools)
library(ggthemes)
library(shinyWidgets)
library(rsconnect)
library(RColorBrewer)
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
file_id <- "1iEk6Wrv7SLSvDPVgUhg17KZBz1qKsVyDLUHKWvDavCQ"  # Replace this with your actual file ID

# Check if file can be found
print(drive_get(as_id(file_id)))

# Download the file and read it into R
temp_file <- tempfile(fileext = ".csv")
drive_download(as_id(file_id), path = temp_file, overwrite = TRUE)
DS <- read.csv(temp_file)

# Print first few rows of the dataframe
print(head(DS))


short <- read_excel("C:/Users/Shahe002/OneDrive - Universiteit Utrecht/Documents/ADS Task force/final version database/Shiny-ADS-AI-Courses/short.xlsx")
levels_names<-c("1 (Bachelor Inleiding)","2 (Bachelor Verdiepend)","3 (Bachelor Gevorderd)","M (Master)","HB (Bachelor Honours)")
levels_names_discr<-c("Introduction Bachelor","Intermediate Bachelor","Advance Bachelor","Master","Bachelor Honours")
course_names<-c("Cursorisch onderwijs","Eindscriptie","Master thesis","onderzoeksproject","Practicum","stage")
course_names_discr<-c("Course","Final thesis","Master thesis","research project","Practical","internship")
year_names<-c(unique(DS$year)[complete.cases(unique(DS$year))])
topic_choices<-c("Data science","Causal inference","data collection","data analysis","database Management","nonSQL databases","Relational databases","Data wrangling", "Data Imputation",
                 "Data Mining","Missing Data","Machine Learning","Deep learning","Network science","Programming", "SQL","Python","Data clean-up","QGIS",
                 "PCRaster","R statistical software","Rmarkdown","clustering","linear algebra","mathematical concepts","regression","gradient descent","feedforward neural networks",
                 "logistic regression","Bayesian statistics","Statistical models","Stochastic Modeling","Exploratory Data Analysis","JASP","MplusÂ","Haskell",
                 "Visual Studio","Big data","Statistics","Visualizations","SPSS","Complex systems","Text mining","Artificial Intelligence","Dataverzameling", "dataverwerking",
                 "data analyse","Statistiek","All"="all")

fluidPage(navbarPage("Data Science Related Courses at FSBS", theme = shinytheme("paper"),
                     tabPanel("Topics overview",
                              sidebarLayout(
                                column(width = 4,
                                       wellPanel(style = "background: #e8f0ff",
                                                 h5("The dashboard is an interactive tool for exploring FSBS's data science-AI courses."),
                                                 
                                       ),
                                       wellPanel(
                                         shinyjs::useShinyjs(),
                                         h4("Filters:"), 
                                         selectInput("level", "Education Level:",choices = 
                                                       c("Introduction Bachelor" = "1 (Bachelor Inleiding)",
                                                         "Intermediate Bachelor" =  "2 (Bachelor Verdiepend)" ,
                                                         "Advance Bachelor" = "3 (Bachelor Gevorderd)",
                                                         "Master"="M (Master)",
                                                         "Bachelor Honours"="HB (Bachelor Honours)",
                                                         "All"="all"),selected ='all'),
                                         selectInput("year", "Year:",
                                                                  c("2024" = "2024", "2023" = "2023", "2022" = "2022", "2021" = "2021", "2020" = "2020", "2019" = "2019", "2018" = "2018", "2017" = "2017", "2016" = "2016","All"="all"),selected ='all'
                                         ),
                                         selectInput("course_type", "Course type:",
                                                     list(`Regular` = c("Course", "research project", "Practical", "internship"),
                                                          `Thesis` = c("Final thesis", "Master thesis"))
                                         ),
                                         actionButton(inputId = "Reset_table", label = "Reset table"),
                                         tags$script("
                                    Shiny.addCustomMessageHandler('resetValue', function(variableName) {
                                      Shiny.onInputChange(variableName, null);
                                    });
                                  ")
                                       )
                                       
                                ),
                                column(width = 8,
                                       
                                       h4("Main topics covered",align = "center"),
                                       d3Output("d3",height = "600px",width = "900px"),
                                       h5(textOutput("text")),
                                       DT::dataTableOutput('table')
                                       
                                )
                              )
                              
                     ),
                     tabPanel("Facet overview",
                              column(width = 12,
                                     wellPanel(
                                       style = "background: #e8f0ff",
                                       h5("Below the facet overview of the topics taught by different years, on different levels of education and for different type of course."),
                                       h6("Manual:
                                       First, choose the desired facet condition and click 'Show facet view' button. Note that the size of the bubbles represents the number of courses where a particular topic is taught. To get a clearer output, the user may set the minimum number of courses to 2 or more to get the most popular topics within each facet.")
                                     ),
                                     wellPanel(
                                       selectInput("overview", "Overview:",choices = 
                                                     c("year" = "year",
                                                       "level"="level",
                                                       "course_type"="Course type")),
                                       numericInput("min_courses","Minimum number of courses: ",min = 1,max=120,value = 1),
                                       actionButton("action1", "Show facet view")
                                       
                                     )),
                              column(width = 12,
                                     uiOutput("plots")
                              )
                              
                     ),
                     tabPanel("Year input",
                              sidebarLayout(
                                column(width = 4,
                                       wellPanel(
                                         style = "background: #e8f0ff",
                                         h5("On the left, you can see the impact that each year has on teaching a certain data science-related topic."),
                                         h6("Below user may chose topics of interests for more clear ouput.")
                                       ),
                                       wellPanel(
                                         selectizeInput(inputId = "topic", label = "Topics to compare:",choices = topic_choices
                                                        ,selected ='all', multiple = T,
                                                        options = list(placeholder = 'select topic name')
                                         )
                                       )),
                                column(width = 8, h4("Year inpact on topics taught", align = "center"),
                                       plotlyOutput("plots2")
                                )
                              ))
))
