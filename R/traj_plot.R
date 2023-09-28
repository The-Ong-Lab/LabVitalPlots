traj_plot <- function(trajecories, events=NULL, abnormalities=NULL){
  cols <- colnames(trajecories)
  
  ## Initialize graph
  plt <- ggplot(data = trajecories) +
    theme_classic()
  
  ## Add in main trajectory plots
  plt <- plt + 
    geom_line(aes(
      x = .data[[cols[2]]], ## dt
      y = .data[[cols[4]]], ## value
      color = .data[[cols[3]]] ## name
      ),
      linewidth = 1.5) +
    labs(color = 'Variable Trajectory')
  
  ## Add in outcome lines
  if (!is.null(events)){
    event_cols <- colnames(events)
    plt <- plt +
      geom_vline(data = events, 
                     aes(
                       xintercept = .data[[event_cols[3]]], ## dt 
                       linetype = .data[[event_cols[2]]]), ## name
                     size=1.5) +
      labs(color = 'Outcomes')
  }
  
  ## Add in stars at abnormal points
  # if (!is.null(abnormalities)){
  #   abn_cols <- colnames(abnormalities)
  #   plt <- plt + 
  #     geom_point(data = abnormalities, 
  #                aes(x = .data[[abn_cols[2]]], 
  #                    y = .data[[abn_cols[3]]]),
  #                shape = 17)
  # }
  
  ## Labels
  plt <- plt +
    labs(
      title = paste0('Trajectory Plot for ID #', trajecories[[1,1]]),
      x = 'Time',
      y = 'Value'
    )
  
  ## Return ggplot object
  return(plt)
}
