long_data_clean <- function(ltv, id, var, ptid_loc=1, dt_loc=2){
  ##assuming that first row is patient identifier and second is some time variable
  out <- ltv %>% 
    select('ptid'=ptid_loc, 'dt'=dt_loc, var) %>%
    drop_na(var) %>%
    filter(ptid==id) %>%
    suppressMessages()
  names(out) <- c('ptid','dt',var)
  return(out)
}
