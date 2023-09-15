
library(tidyverse)

# library(ggplot2)
library(geomtextpath)

## Load data
rc <- read_csv('/Users/jack/Downloads/Aim2ProspectiveLarge-AllDataForPossiblyIn_DATA_2023-08-10_1017.csv')
ptid <- 6

## Process data
demo <- rc %>%
  select(id, mrn:lsw) %>%
  drop_na(lsw)

pup <- rc %>%
  select(id, pupil_date:minr) %>%
  drop_na(pupil_date) %>%
  mutate(
    sizediff = abs(sizel-sizer),
    npidiff = abs(npil-npir),
    avgnpi = (npil+npir)/2
  ) %>%
  select(id,pupil_date,sizediff,npidiff,avgnpi) %>%
  pivot_longer(!c(1,2))

rad <- rc %>%
  select(id, imagedt:report_number) %>%
  drop_na(report_number) %>%
  rowwise() %>%
  mutate(
    name = 'image',
    size_mls = mean(c(max_mls_mm_wzik,min_mls_mm_wzik), na.rm = T),
    size_pgs = mean(c(size_max_pgs,size_min_pgs), na.rm = T),
    txt = paste0("MLS: ", round(size_mls,2), "; PGS: ", round(size_pgs,2))
  ) %>%
  select(id, dt=imagedt, name, txt) 

surg <- rc %>%
  select(id, dt=surgdt) %>%
  drop_na(dt) %>%
  mutate(
    name = 'surg',
    txt = ''
  )

outcomes <- bind_rows(rad, surg)


abnormal <- pup %>%
  filter(name == 'avgnpi') %>%
  group_by(id) %>%
  mutate(
    pct_chg = round(1 - lag(value)/value,3)*100
  ) %>%
  drop_na(pct_chg) %>%
  filter(pct_chg < -33) %>%
  select(id, dt = 2, value)



## is old code too bespoke? it'd be had to add lines to the top of these...

max_scaler <- apply(pup, 2, max, na.rm=T)
min_scaler <- apply(pup, 2, min, na.rm=T)


## Select patients
pup <- pup %>%
  filter(id == ptid) %>%
  select(1:2, avgnpi, sizediff, npidiff)

rad$txt[4] <- ''

## Plot
plt <- ggplot(pup, aes(x=pupil_date))
col_pal <- RColorBrewer::brewer.pal(4, 'Dark2')

for ( i in 3:length(pup) ) {
  var <- colnames(pup)[i]
  plt <- plt + geom_line(aes(y = .data[[var]]), color = col_pal[i-2])
}


# plt <- plt + 
#   geom_vline(data = rad, aes(xintercept=imagedt)) +
#   geom_text(data = rad, aes(x=imagedt, y=5, label=txt, angle=90, hjust='left'))
  
plt <- plt + 
  geom_textvline(data = rad, aes(xintercept=imagedt, label=txt), size=2)

plt <- plt +
  geom_point(data = pup_abn %>% filter(id==ptid), aes(x=pupil_date, y=avgnpi))

plt  


