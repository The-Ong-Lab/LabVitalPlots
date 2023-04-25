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

#Empties Global Environment cache
rm(list = ls())


#Set working directory to current file location
#To set to own working directory
#  select "Session->Set Working Directory->To Source File Location"
#  then copy result in console into current "setwd("")".
# setwd("~/Dropbox/25-Charlene-Jack-Ivy/10-Shiny App/LabVital_Plots_20230419")
library(shiny)
library(tidyverse)
library(magrittr)
library(lubridate)
library(ggplot2) # Future iterations, change plots to use ggplot
# source('R/helpers.R') # Custom functions for plotting

options(shiny.maxRequestSize = 30*1024^2)


# Application title
title <- titlePanel("Lab Value and Vital Sign Plots")

# Upload data
ui_upload <- sidebarLayout(
  sidebarPanel(
    
    fileInput("fileOTV", 
              "Choose OTV file (.csv)",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")),
    
    fileInput("fileLTV", 
              "Choose LTV file (.csv)",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")),
    
    textInput('var',
              'Name of variable to plot',
              placeholder = 'gluc, wbc, sbp, etc.'
              ),
    
    numericInput('pt',
                 'Patient ID to plot',
                 0, 0, 10000, 1),
    
    actionButton('goButton',
                 '',
                 icon = icon('rotate-right'))
  ),
  
  
  mainPanel(
    h4("Raw data preview:"),
    
    tableOutput("otv_preview"),
    
    fluidRow(
      column(width = 4, tableOutput("ltv_preview")),
      column(width = 6, plotOutput("distPlot"), offset = 2)
    ),
    
    plotOutput('vitalPlot')
  )
)


ui <- fluidPage(
  # UI goes here
  title,
  ui_upload,
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$otv_preview <- renderTable({
    #   input$file will be NULL initially. After the user selects
    #   # and uploads a file, it will be a data frame with 'name',
    #   # 'size', 'type', and 'datapath' columns. The 'datapath'
    #   # column will contain the local filenames where the data can
    #   # be found.
    req(input$fileOTV, input$var)
    inFile <- input$fileOTV
    inVar <- input$var
    
    if (is.null(inFile) | is.null(inVar))
      return(NULL)
    
    raw <- read.csv(inFile$datapath, stringsAsFactors = FALSE) 
    
    raw %>% select(1:10) %>% head()
  })
  
  output$ltv_preview <- renderTable({
    #   input$file will be NULL initially. After the user selects
    #   # and uploads a file, it will be a data frame with 'name',
    #   # 'size', 'type', and 'datapath' columns. The 'datapath'
    #   # column will contain the local filenames where the data can
    #   # be found.
    req(input$fileLTV, input$var, input$pt)
    inFile <- input$fileLTV
    inVar <- input$var
    inPt <- input$pt
    
    # if (is.null(inFile) | is.null(inVar))
    #   return(NULL)
    
    raw <- read.csv(inFile$datapath, stringsAsFactors = FALSE) 
    
    raw %<>%
      select(ptid, dateteme=dt, inVar) %>%
      filter(ptid == inPt)
    
    raw[[inVar]] <- as.numeric( raw[[inVar]] )
      
    raw %>%
      drop_na(inVar) %>%
      head()
  })
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    req(input$fileLTV, input$var, input$pt)
    inFile <- input$fileLTV
    inVar <- input$var
    inPt <- input$pt
    
    raw <- read.csv(inFile$datapath, stringsAsFactors = FALSE) %>%
      filter(ptid == inPt)
    x <- raw[[inVar]]
    
    # draw the histogram with the specified number of bins
    hist(x, col = 'darkgray', border = 'white', 
         main = paste('Hist of', inVar))
  })
  
  output$vitalPlot <- renderPlot({
    ### Get inputs
    req(input$fileOTV, input$fileLTV, input$var, input$pt)
    inVar <- input$var
    inPt <- input$pt
    inFileOTV <- input$fileOTV
    inFileLTV <- input$fileLTV

    
    ### Read Data  
    TSV <- read.csv(inFileLTV$datapath) %>% 
      rename(id=ptid) %>% 
      filter(id == inPt)
    TSV$dt <- as.POSIXct(TSV$dt)
    
    OTV <- read.csv(inFileOTV$datapath) %>% 
      rename(id=ptid) %>% 
      filter(id == inPt)
    OTV$pres %<>% ymd_hms(truncated = 3)
    OTV$lsw %<>% ymd_hms(truncated = 3)
    OTV$surgdt %<>% ymd_hms(truncated = 3)
    
    
    ### Plot
    clean_and_plot(inVar, inPt, OTV, TSV) %>% suppressMessages()
    clean_and_plot('wbc', inPt, OTV, TSV) %>% suppressMessages()
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
