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

# options(shiny.reactlog = TRUE)

# Libraries
library(shiny)
library(tidyverse)
library(magrittr)
library(lubridate)
library(ggplot2) # Future iterations, change plots to use ggplot

# Global Options
options(shiny.maxRequestSize = 30*1024^2)

# Application User Interface
ui <- fluidPage(
  titlePanel(" "),
  sidebarLayout(
    ui_sidebar,
    ui_main
  ))



# Load sample data
samp_otv <- read_csv('data/sample_otv.csv')
samp_ltv <- read_csv('data/sample_ltv.csv')


# Define server logic
server <- function(input, output) {
  
  
  ## Reactive dataframes
  OTV <- reactive({
    if (is.null(input$fileOTV))
      samp_otv
    else
      read_csv(input$fileOTV$datapath)
  })
  
  LTV <- reactive({
    if (is.null(input$fileLTV))
      samp_ltv
    else
      read_csv(input$fileLTV$datapath)
  })
  
  
  ## Update drop-down based on OTV data
  observe({
    # inFile <- input$fileOTV
    # if (is.null(inFile)){
    #   return(NULL)}
    df <- OTV() #read_csv(inFile$datapath) 
    
    ## Update drop-down list of IDs
    # ids <- unique(df[[1]])
    # updateSelectInput(inputId = 'pt', choices = ids)
    
    ## Update drop-down list of event variables
    var_names <- c("", 
                   df %>% 
                     # select(ends_with('dt')) %>% 
                     select(-1) %>%
                     colnames()
                   )
    updateSelectInput(inputId = 'out1', choices = var_names)
    updateSelectInput(inputId = 'out2', choices = var_names)
    updateSelectInput(inputId = 'out3', choices = var_names)
  })
  
  ## Update drop-downs based on LTV data
  observe({
    # inFile <- input$fileLTV
    # if (is.null(inFile)){
    #   return(NULL)}
    df <- LTV() #read_csv(inFile$datapath)
    
    ## Update drop-down list of trajectory variables
    var_names <- colnames(df)[-1:-2] #:-4]
    updateSelectInput(inputId = 'var', choices = var_names)
  })
  
  observeEvent(input$var, {
    # inFile <- input$fileLTV
    # if (is.null(inFile)){
    #   return(NULL)}
    df <- LTV() #read_csv(inFile$datapath)
    
    if (input$var != ""){
      df <- df %>%
        drop_na(input$var)
    }

    ## Update drop-down list of IDs
    ids <- unique(df[[1]])
    updateSelectInput(inputId = 'pt', choices = ids)
  })
  
  
  ## Action of goButton
  observeEvent(input$goButton, {
    # req(input$fileOTV, input$fileLTV, input$pt, input$var)
    req(input$pt, input$var)
    
    # print(input$out1)
    # print(class(input$out1))
    # print(is.null(input$out1))
    
    ptid <- input$pt
    var <- input$var
    
    ## Read data (to-do: make df reactive and only read once in script)
    otv <- OTV() %>% #read_csv(input$fileOTV$datapath) %>%
      select(id = 1, any_of(c(input$out1, input$out2, input$out3))) %>%
      filter(id == input$pt)
    ltv <- LTV() %>% #read_csv(input$fileLTV$datapath) %>%
      select(id = 1, dt = dt, all_of(c(var))) %>% ## need to update datetime to be relative position
      filter(id == input$pt)
    
    ## Set up/clean data for plotting
    trajectories <- ltv %>%
      select(id, dt, all_of(c(var))) %>%
      pivot_longer(!c(id,dt), 
                   values_drop_na = TRUE)
    
    if ( input$out1 == "" & input$out2 == "" & input$out3 == "" ){
      events <- NULL
    } else {
      events <- otv %>%
        select(id, any_of(c(input$out1, input$out2, input$out3))) %>%
        pivot_longer(!id, values_drop_na = TRUE)
      if (nrow(events) < 1){
        events <- NULL
      } else {
        events <- events
      }
    }

    ## Make plots
    output$traj_plot <- renderPlot({
      traj_plot(trajectories, events)
    })
    
  })
  
} ## End of server

# Run the application 
shinyApp(ui = ui, server = server)


