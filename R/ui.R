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
