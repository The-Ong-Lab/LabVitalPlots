print('long_clean')

long_data_clean <- function(ltv, dt, vars, ptid_loc=1){
  ##assuming that first row is patient identifier and second is some time variable
  out <- ltv %>% 
    select('id'=ptid_loc, dt, vars) %>%
    pivot_longer(!c(id,dt)) %>%
    drop_na(value)
  
  return(out)
}


dt <- c('pupil_date','imagedt')
# rc %>%
#   select(id,dt,npil,npir,max_mls_mm_wzik) %>%
#   pivot_longer(!c(id,dt)) %>%
#   drop_na(value)