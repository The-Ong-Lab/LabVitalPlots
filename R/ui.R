##### .csv file upload #####
csvInput <- function(id){
  fileInput(
    id, 
    label = NULL,
    accept = c(
      "text/csv",
      "text/comma-separated-values,text/plain",
      ".csv")
  )}


# Upload data
ui_sidebar <- sidebarPanel(
  
  h3('One-time Variable Upload'),
  p('Dataset of admission, discharge, and one-time outcome variables.'),
  csvInput("fileOTV"),
  
  h3('Longitudinal Variable Upload'),
  p('Dataset of longitudinal, repeated measured variables.
      NOTE: This app assumes the first two columns are (1) the patient identifier and (2) the measurement datetimes.'),
  csvInput("fileLTV"),
  
  h2('Plot Details'),
  h4('Variable to Plot'),
  p('Select the variable trajectory to plot. Dropdown populates based on the available columns in the logitudinal data.'),
  selectInput('var', NULL, choices = ""),
  h4('Patient ID'),
  p('Choose the desired patient. Dropdown populates based on the uploaded data.'),
  selectInput('pt', NULL, choices = ""),
  
  
  h4('Event/Outcome Lines to Add'),
  p('Select up to three outcome date-times to add as vertical lines to the graph.Dropdown populates based on the uploaded data.'),
  selectInput('out1', NULL, choices = ""),
  selectInput('out2', NULL, choices = ""),
  selectInput('out3', NULL, choices = ""),
  
  h2('Refresh Plot'),
  actionButton('goButton', 'Click to Refresh', icon = icon('rotate-right'), width = '75%')
)


# UI for main area
ui_main <- mainPanel(
  h1('Trajectory Plot'),
  p("Once the necessary information is entered in the sidebar pannel, press the 'Refresh Plot' button. 
    Then, we will generate an individual plot of a patient's lab value or vital sign trajectory."),
  br(), br(), br()
  plotOutput('test_plot')
)
