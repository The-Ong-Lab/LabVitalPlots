long_data_clean <- function(ltv, var, ptid=1, dt=2){
  out <- ltv %>% 
    select(1,2, ##assuming that first row is patient identifier and second is some time variable
           var) %>%
    drop_na()
}