library(DT)
library(here)
library("readxl")
library(dplyr)
library(r2d3)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(packcircles)
library(ggplot2)
library(RColorBrewer)
library(htmlwidgets)
library("digest")
library("bit")
#library(shinySignals)
library(plotly)
library(tidyverse)
library(htmlwidgets)
library(devtools)
library(ggthemes)
library(shinyWidgets)
library(rsconnect)
library(RColorBrewer)

DS <- read_excel("C:/Users/Shahe002/OneDrive - Universiteit Utrecht/Documents/ADS Task force/final version database/Shiny-ADS-AI-Courses/UU-ADS-AI-courses.xlsx")
short <- read_excel("C:/Users/Shahe002/OneDrive - Universiteit Utrecht/Documents/ADS Task force/final version database/Shiny-ADS-AI-Courses/short.xlsx")
levels_names<-c("1","2","3","M","HB")
levels_names_discr<-c("Introduction Bachelor","Intermediate Bachelor","Advance Bachelor","Master","Bachelor Honours")
#course_names<-c()
#course_names_discr<-c()
Year_number<-c(unique(DS$year)[complete.cases(unique(DS$year))])
topic_choices<-c("Data science","Causal inference","data collection","data analysis","database Management","nonSQL databases","Relational databases","Data wrangling", "Data Imputation",
                 "Data Mining","Missing Data","Machine Learning","Deep learning","Network science","Programming", "SQL","Python","Data clean-up","QGIS",
                 "PCRaster","R statistical software","Rmarkdown","clustering","linear algebra","mathematical concepts","regression","gradient descent","feedforward neural networks",
                 "logistic regression","Bayesian statistics","Statistical models","Stochastic Modeling","Exploratory Data Analysis","JASP","MplusÂ","Haskell",
                 "Visual Studio","Big data","Statistics","Visualizations","SPSS","Complex systems","Text mining","Artificial Intelligence","Dataverzameling", "dataverwerking",
                 "data analyse","Statistiek","All"="all")


function(input, output, session) { 
  
  level=reactive(input$level)
  #course=reactive(input$course)
  year=reactive(input$year)
  
  observeEvent(input$Reset_table, {
    session$sendCustomMessage(type = "resetValue", message = "click_event")
  })
  
  observe({
    x <- input$topic
    
    # Can use character(0) to remove all choices
    if (is.null(x)){
      updateSelectInput(session, "topic",
                        label = paste("No topic chosen"),
                        choices = topic_choices,
                        selected = "")
    }else if(length(x)>1 && any(x=="all")){
      idx<-which(x=="all")
      updateSelectInput(session, "topic",
                        label = paste("Topics to compare:"),
                        choices = topic_choices,
                        selected =x[-idx])
    }else if(length(x)==1 && x=="all"){
      updateSelectInput(session, "topic",
                        label = paste("Topics to compare:"),
                        choices = topic_choices,
                        selected =x)
    }
    
  })
  output$d3 <- renderD3({
    if(level()=="all"){
      data <- DS %>% select(c(1,4,5,9:56)) %>% filter(year==year()) %>%
        pivot_longer(where(is.numeric)) %>% group_by(name) %>% 
        summarise(value=sum(value,na.rm =T)) %>% filter(value>0)
      colnames(data)<-c("id","value")
      data<-left_join(data,short, by="id")
      r2d3(data=data, d3_version = 4, script ="bubble.js")
    }else{
      data <- DS %>% select(c(1,4,5,9:56)) %>% filter(year==year(),Level==level())
      if(course() %in% data$Course_type){
        data<-data %>% filter(Course_type==course()) %>% pivot_longer(where(is.numeric)) %>%
          group_by(name) %>% summarise(value=sum(value,na.rm =T)) %>% filter(value>0)
        colnames(data)<-c("id","value")
        data<-left_join(data,short, by="id")
        r2d3(data=data, d3_version = 4, script ="bubble.js")
      }else{
        data<-data.frame(id="No data",value=20,short="No data avaiable")
        r2d3(data=data, d3_version = 4, script ="bubble.js")
      }
    }
  })
  
  output$text<-renderText({
    if(is.null(input$click_event)){
      paste0("The database of courses")
    }else{
      paste0("The database of courses that cover the ", input$click_event," topics")
    }
  })
  
  
  
  input_plots <- reactiveValues()
  observeEvent(input$action1,{
    input_plots$over<-input$overview
    input_plots$min_course<-input$min_courses
    if(input_plots$over=='year'){
      names_for_bubble<-get("faculty_names")
      plot_titles<-get("year_number")
    }else if(input_plots$over=='Level'){
      names_for_bubble<-get("levels_names")
      plot_titles<-get("levels_names_discr")
    }else if(input_plots$over=='Course_type'){
      names_for_bubble<-get("course_names")
      plot_titles<-get("course_names_discr")}
    for (i in 1:length(names_for_bubble)) {
      local({
        my_i <- i
        plotname <- paste("plot", my_i, sep="")
        data <- DS %>% select(c(1,4,5,9:56))
        names<-names_for_bubble[i]
        output[[plotname]] <- renderD3({
          data<- data  %>% filter(!!sym(input_plots$over)==names) %>% pivot_longer(where(is.numeric)) %>% group_by(name) %>%
            summarise(value=sum(value,na.rm =T)) 
          if(all(data$value<input_plots$min_course)){
            data<-data.frame(id="No data",value=20,short="No data avaiable")
          }else{
            data<-data %>% filter(value>(input_plots$min_course-1))
            colnames(data)<-c("id","value")
            data<-left_join(data,short, by="id")
          }
          r2d3(data=data, d3_version = 4, script ="bubble_2.js")})
      })
    }
    output$plots <- renderUI({
      
      
      plot_output_list <- lapply(1:length(names_for_bubble), function(i) {
        plotname <- paste("plot", i, sep="")
        column(width = 6,
               h4(plot_titles[i],align = "center"),
               tags$div(style = "margin-top: 0px; margin-bottom: 0px;", d3Output(plotname))
        )
      } )
      
      do.call(tagList, plot_output_list)
    })})
  
  output$table <- DT::renderDataTable({
    
    if(is.null(input$click_event)){
      if(level()=='all'){
        DS %>% filter(DS$year==year()) %>% select(c(1,2,3,6,7,8))
      }else{
        DS %>% filter(Level==level(),DS$year==year())%>%select(c(1,2,3,6,7,8))
      }}else{
        if(level()=='all'){
          DS %>% filter(DS$year==year(),(!!sym(input$click_event))==1)%>% select(c(1,2,3,6,7,8))
        }else{
          DS %>% filter(Level==level(),DS$year==year(),(!!sym(input$clik_event))==1)%>% select(c(1,3,5,7,8,9,12))
        }
      }
  },server = F,options = list(searching = TRUE))
  
  
  output$plots2 <- renderPlotly({
    
    short2<-DS %>% select(c(1,4,5,9:56)) %>%pivot_longer(where(is.numeric)) %>% group_by(name,Faculty) %>% summarise(total=sum(value,na.rm =T)) %>% na.omit()
    
    plot1_perc<-short2 %>% mutate(total=round((total/sum(total))*100,2))
    if(any(input$topic!="all")){
      plot1_perc<-plot1_perc %>% filter(name %in% input$topic)
    }
    
    ggplotly(
      plot1_perc %>% ggplot(aes(x=total,y=name,fill=Faculty))+geom_col() + scale_fill_brewer(palette="Set2") +ylab("")+xlab("percentage (%)")+theme_minimal()
    ) %>% layout(height = 800, width = 1000) })
  
  
  
}