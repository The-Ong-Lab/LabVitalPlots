#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

################################################################################
# PROJECT: Dynamic Trajectory Analysis 
# PURPOSE: Create a shiny app that plots lab/vital measurements
# INPUTS:  Data from Roybal REDCap survey (aiming for non-HIPAA data input)
# OUTPUTS: A list of alters with an importance score
# AUTHORS: Jack Pohlmann
# CREATED: Feb 27, 2023
# LATEST:  Apr 19, 2023
# PSERIES: NA
# NSERIES: NA
# NOTES:   MVP will be on shiny server and then see if paid server needed,
#          Used: https://shiny.rstudio.com/reference/shiny/0.14/fileInput.html
###############################################################################

# Libraries
library(shiny)
library(tidyverse)
library(magrittr)
library(lubridate)
library(ggplot2) # Future iterations, change plots to use ggplot

# Global Options
options(shiny.maxRequestSize = 30*1024^2)


# Application title
title <- titlePanel("Individual Lab Value and Vital Sign Trajectory Plots")

# Upload data
ui_upload <- sidebarLayout(
  sidebarPanel(
    
    h3('One-time Variable Upload'),
    p('Dataset of admission, discharge, and one-time outcome variables.'),
    csvInput("fileOTV"),
    
    h3('Longitudinal Variable Upload'),
    p('Dataset of longitudinal, repeated measured variables.
      NOTE: This app assumes the first two columns are (1) the patient identifier and (2) the measurement datetimes.'),
    csvInput("fileLTV"),
    
    h2('Plot Details'),
    h4('Patient ID'),
    selectInput('pt', NULL, choices = ""),
    h4('Variable to Plot'),
    selectInput('var', NULL, choices = ""),
    
    h4('Refresh Plot'),
    actionButton('goButton', NULL, icon = icon('rotate-right'))
  ),
  
  mainPanel()
  
)


ui <- fluidPage(
  # UI goes here
  title,
  ui_upload,
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observe({
    inFile <- input$fileOTV
    if (is.null(inFile)){
      return(NULL)}
    
    ids <- unique(read_csv(inFile$datapath)[[1]])
    updateSelectInput(inputId = 'pt', choices = ids)
  })
  
  observe({
    inFile <- input$fileLTV
    if (is.null(inFile)){
      return(NULL)}
    
    var_names <- colnames(read_csv(inFile$datapath))[-1]
    updateSelectInput(inputId = 'var', choices = var_names)
  })
  
  
  
  
  # output$otv_preview <- renderTable({
  #   #   input$file will be NULL initially. After the user selects
  #   #   # and uploads a file, it will be a data frame with 'name',
  #   #   # 'size', 'type', and 'datapath' columns. The 'datapath'
  #   #   # column will contain the local filenames where the data can
  #   #   # be found.
  #   req(input$fileOTV, input$var)
  #   inFile <- input$fileOTV
  #   inVar <- input$var
  # 
  #   if (is.null(inFile) | is.null(inVar))
  #     return(NULL)
  # 
  #   raw <- read.csv(inFile$datapath, stringsAsFactors = FALSE)
  # 
  #   raw %>% select(1:10) %>% head()
  # })
  # 
  # output$ltv_preview <- renderTable({
  #   #   input$file will be NULL initially. After the user selects
  #   #   # and uploads a file, it will be a data frame with 'name',
  #   #   # 'size', 'type', and 'datapath' columns. The 'datapath'
  #   #   # column will contain the local filenames where the data can
  #   #   # be found.
  #   req(input$fileLTV, input$var, input$pt)
  #   inFile <- input$fileLTV
  #   inVar <- input$var
  #   inPt <- input$pt
  # 
  #   # if (is.null(inFile) | is.null(inVar))
  #   #   return(NULL)
  # 
  #   raw <- read.csv(inFile$datapath, stringsAsFactors = FALSE)
  # 
  #   raw %<>%
  #     select(ptid, dateteme=dt, inVar) %>%
  #     filter(ptid == inPt)
  # 
  #   raw[[inVar]] <- as.numeric( raw[[inVar]] )
  # 
  #   raw %>%
  #     drop_na(inVar) %>%
  #     head()
  # })
  # 
  # output$distPlot <- renderPlot({
  #   # generate bins based on input$bins from ui.R
  #   req(input$fileLTV, input$var, input$pt)
  #   inFile <- input$fileLTV
  #   inVar <- input$var
  #   inPt <- input$pt
  #   
  #   raw <- read.csv(inFile$datapath, stringsAsFactors = FALSE) %>%
  #     filter(ptid == inPt)
  #   x <- raw[[inVar]]
  #   
  #   # draw the histogram with the specified number of bins
  #   hist(x, col = 'darkgray', border = 'white', 
  #        main = paste('Hist of', inVar))
  # })
  # 
  # output$vitalPlot <- renderPlot({
  #   ### Get inputs
  #   req(input$fileOTV, input$fileLTV, input$var, input$pt)
  #   inVar <- input$var
  #   inPt <- input$pt
  #   inFileOTV <- input$fileOTV
  #   inFileLTV <- input$fileLTV
  # 
  #   
  #   ### Read Data  
  #   LTV <- read.csv(inFileLTV$datapath) %>% 
  #     rename(id=ptid) %>% 
  #     filter(id == inPt)
  #   LTV$dt %<>% as.POSIXct()
  #   
  #   OTV <- read.csv(inFileOTV$datapath) %>% 
  #     rename(id=ptid) %>% 
  #     filter(id == inPt)
  #   OTV$pres %<>% as.POSIXct()
  #   OTV$lsw %<>% as.POSIXct()
  #   OTV$surgdt %<>% as.POSIXct()
  #   
  #   
  #   ### Plot
  #   source('clean_and_plot.R', local = TRUE)
  #   clean_and_plot(inVar, inPt, OTV, TSV) %>% suppressMessages()
  #   # clean_and_plot('wbc', inPt, OTV, TSV) %>% suppressMessages()
  #   
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)

