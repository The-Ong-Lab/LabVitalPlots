
## Load data
rc <- read_csv('/Users/jack/Downloads/Aim2ProspectiveLarge-AllDataForPossiblyIn_DATA_2023-08-10_1017.csv')

## Process data
demo <- rc %>%
  select(id, mrn:lsw) %>%
  drop_na(lsw)

pup <- rc %>%
  select(id, pupil_date:minr) %>%
  drop_na(pupil_date) %>%
  mutate(
    sizediff = abs(sizel-sizer),
    npidiff = abs(npil-npir)
  ) 

rad <- rc %>%
  select(id, imagedt:report_number) %>%
  drop_na(report_number) 
## is old code too bespoke? it'd be had to add lines to the top of these...

max_scaler <- apply(pup, 2, max, na.rm=T)
min_scaler <- apply(pup, 2, min, na.rm=T)


## Select patients
pup <- pup %>%
  filter(id == 3) %>%
  select(1:2, npil, npir, sizediff, npidiff)


## Plot
plt <- ggplot(pup, aes(x=pupil_date))
col_pal <- RColorBrewer::brewer.pal(4, 'Dark2')

for ( i in 3:length(pup) ) {
  var <- colnames(pup)[i]
  plt <- plt + geom_line(aes(y = .data[[var]]), color = col_pal[i-2])
}


plt  


