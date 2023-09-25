print('scaler')

scaler <- function(x, target_min=0, target_max=1){
  x_min <- min(x, na.rm = T)
  x_max <- max(x, na.rm = T)
  
  target <- target_min + ((target_max - target_min) / (x_max - x_min)) * (x - x_min)
  return(target)
}
