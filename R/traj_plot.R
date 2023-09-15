traj_plot <- function(trajecories, events=NULL, abnormalities=NULL){
  cols <- colnames(trajecories)
  
  ## Initialize graph
  plt <- ggplot(data = trajecories)
  
  ## Add in main trajectory plots
  plt <- plt + 
    geom_line(aes(
      x = .data[[cols[2]]],
      y = .data[[cols[4]]], 
      color = .data[[cols[3]]]
      ))
  
  ## Add in outcome lines
  if (!is.null(events)){
    event_cols <- colnames(events)
    plt <- plt +
      geom_textvline(data = events, 
                     aes(
                       xintercept = .data[[event_cols[2]]], 
                       label = .data[[event_cols[4]]],
                       linetype = .data[[event_cols[3]]]), 
                     size=2)
  }
  
  ## Add in stars at abnormal points
  if (!is.null(abnormalities)){
    abn_cols <- colnames(abnormalities)
    plt <- plt + 
      geom_point(data = abnormalities, 
                 aes(x = .data[[abn_cols[2]]], 
                     y = .data[[abn_cols[3]]]),
                 shape = 17)
  }
  
  ## Return ggplot object
  return(plt)
}


traj_plot(
  pup %>% filter(id==4),
  outcomes %>% filter(id==4),
  abnormal %>% filter(id==4)
  )
