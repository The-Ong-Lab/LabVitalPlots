print('clean and plot')

clean_and_plot <- function(variable, pt, OTV, TSV){
  
  # Libraries
  library(shiny)
  library(tidyverse)
  library(magrittr)
  library(lubridate)
  library(ggplot2) 
  
  ## Define helper functions
  #find difference in time between measurements
  measure_difftime <- function(id_num, var, to_graph){
    dates <- to_graph %>% 
      filter(.$id==id_num) %>% 
      drop_na(all_of(var)) #%>% select('dt')
    dates <-dates$dt
    n <- length(dates)
    
    out <- c()
    for(i in 1:(n-1)){
      x <- difftime(dates[i+1], dates[i], units = 'hours')
      x <- as.numeric(x)
      out <- append(out, x)}
    
    return(out)
  }
  
  #function to plot everything
  var_plot <- function(i, desc=FALSE, var='XXX'){
    
    
    #custom colors for graphing
    brown = rgb(99,53,69,maxColorValue=255)
    red = rgb(220,38,127,maxColorValue=255)
    orange = rgb(254,97,0,maxColorValue=255)
    yellow = rgb(255,176,0,maxColorValue=255)
    green = rgb(96,212,141,maxColorValue=255)
    blue = rgb(100,143,255,maxColorValue=255)
    purple = rgb(177,91,255,maxColorValue=255)
    
    
    par(#mfrow = c(1,1), 
      xpd=TRUE
      #mar = c(10,8,5,10)
    )
    
    #vectors to graph
    min_max <- c(min_max_graph[min_max_graph$id==i,]$min_dt, min_max_graph[min_max_graph$id==i,]$max_dt)
    x <- to_graph[to_graph$id==i, 'dt']
    y1 <- to_graph[to_graph$id==i, var]
    
    
    #blank line to space graph min/max
    plot(min_max, c(0,0),
         type = 'l',
         lty = 0,
         ylab = '',
         xaxt='n',xlab='',
         ylim=c(min(to_graph[,var]),max(to_graph[,var])))
    
    #plot y1  
    lines(x,y1,
          type='o',
          pch=20,
          lwd=3,
          col=orange)
    
    #day axis
    axis.POSIXct(1,
                 at=unique(lubridate::floor_date(to_graph[to_graph$id==i, 'dt'], unit='day')),
                 format='%b %e',
                 col=NA,
                 col.ticks=1,
                 line=-0.25,
                 las=0
    )
    mtext('Day', 1, .75, 
          at=to_graph[to_graph$id==i, 'dt'][1]-as.numeric(tail(to_graph[to_graph$id==i, "dt"],n=1)-head(to_graph[to_graph$id==i, "dt"],n=1), units='secs')*.2)
    
    #time axis
    axis.POSIXct(1, 
                 at=seq(from=head(to_graph[which(to_graph$id==i), 'dt'], 1),
                        to=tail(to_graph[which(to_graph$id==i), 'dt'], 1),
                        length.out = 10),
                 format='%H:%M',
                 line=2,
                 las=0
    )
    mtext('Time', 1, 3, 
          at=to_graph[to_graph$id==i, 'dt'][1]-as.numeric(tail(to_graph[to_graph$id==i, "dt"],n=1)-head(to_graph[to_graph$id==i, "dt"],n=1), units='secs')*.2
    )
    
    #lsw axis
    axis.POSIXct(1, 
                 at = seq(from=head(to_graph[which(to_graph$id==i), "dt"], 1),
                          to=tail(to_graph[which(to_graph$id==i), "dt"], 1),
                          length.out = 10),
                 labels = round(seq(from=head(to_graph[which(to_graph$id==i), "tf_lsw"], 1),
                                    to=tail(to_graph[which(to_graph$id==i), "tf_lsw"], 1),
                                    length.out = 10),
                                digits=0),
                 tick=FALSE,
                 line=3.5
    )
    mtext('Hours from \nLast Seen Well', 1, 5.5, 
          at=to_graph[to_graph$id==i, 'dt'][1]-as.numeric(tail(to_graph[to_graph$id==i, "dt"],n=1)-head(to_graph[to_graph$id==i, "dt"],n=1), units='secs')*.2)
    
    
    #data legend
    legend('topright',
           title = 'Lab Data',
           legend = c(var),
           col = c(brown),
           pch = c(20),
           inset = c(-0.2,0))
    
    #Outcomes legend
    legend('right',
           title = 'Outcomes',
           legend = c('Image', 'Surgery', 'HTS 3%', 'HTS 23%', 'Mannitol'),
           col = c('black','black',purple,brown,green),
           lty = c(1,5,3,3,3),
           lwd = c(2,2,3,3,3),
           inset = c(-0.26,0))
    
    
    #y-axis and main title
    title(ylab = 'Y-axis Placeholder', col.lab=brown)
    
    #main title
    title(main=paste0(var, ' measurements for Patient ID ', i))
    
    
    par(xpd=FALSE)
    
    
    #imaging lines
    abline(v=image[image$id==i, 'dt'], 
           lwd=2)
    
    #surg lines
    surg <- to_graph[to_graph$id==i, 'surgdt'][1]
    abline(v=surg, 
           lty=5, 
           lwd=2)
    
    #osmotic therapy lines
    abline(v=hts3[hts3$id==i, 'hts3_dt'],
           col=purple,
           lty=3,
           lwd=3)
    abline(v=hts23[hts23$id==i, 'hts23_dt'],
           col=brown,
           lty=3,
           lwd=3)
    abline(v=mannitol[mannitol$id==i, 'mannitol_dt'],
           col=green,
           lty=3,
           lwd=3)
    
    
    
    #text on lines for imaging instances
    for (j in 1:length(image[image$id==i, 'dt'])){
      text(x = image[image$id==i, 'dt'][j],
           y = mean( c(min(to_graph[,var]),max(to_graph[,var])) ),
           labels = paste0('MLS: ', image[image$id==i, 'rc_size_mls'][j], ', PGS: ', image[image$id==i, 'rc_size_pgs'][j]),
           pos = 2,
           offset = 0.5,
           cex = 0.7,
           srt = 90)}
    
    
    
    
    
  }
  
  
  #From the data, create a table with TSV and a few important datetimes from OTV. 
  to_graph <- TSV %>% 
    select('id','dt', variable) %>%
    left_join(OTV %>% 
                select ('id','lsw','pres','mls5dt','surgdt') ) %>% 
    drop_na(variable) 
  
  
  ### Time formatting
  dt_cols <- c('dt','lsw','pres','mls5dt','surgdt')
  for(i in dt_cols[-1]){
    to_graph[,i] <- ymd_hms(to_graph[,i], truncated = 3)}
  to_graph <- to_graph[which(to_graph$dt < to_graph$lsw+(7*24*3600)), ] 
  
  
  
  
  ### Osmotic df
  hts23 <-TSV %>% drop_na(hts23) %>% arrange(dt) %>% select('id','dt') %>% rename(hts23_dt=dt) %>% left_join(distinct(to_graph[,c('id','lsw')]))
  hts23$hts23_dt <- ymd_hms(hts23$hts23_dt, truncated = 3)
  
  hts3 <-TSV %>% drop_na(hts3) %>% arrange(dt) %>% select('id','dt') %>% rename(hts3_dt=dt) %>% left_join(distinct(to_graph[,c('id','lsw')]))
  hts3$hts3_dt <- ymd_hms(hts3$hts3_dt, truncated = 3)
  
  mannitol <-TSV %>% drop_na(mannitol) %>% arrange(dt) %>% select('id','dt') %>% rename(mannitol_dt=dt) %>% left_join(distinct(to_graph[,c('id','lsw')]))
  mannitol$mannitol_dt <- ymd_hms(mannitol$mannitol_dt, truncated = 3)
  
  
  ### Imaging df
  image <- TSV %>% drop_na(imagetype) %>% select('id','dt','imagetype','rc_mls','rc_size_mls','rc_pgs','rc_size_pgs')
  image$dt <- ymd_hms(image$dt)
  
  min_graph <- image[,c('id','dt')] %>% full_join(to_graph[,c('id','dt')]) %>% group_by(id) %>% slice_min(dt) %>% rename(min_dt=dt)
  max_graph <- to_graph[,c('id','dt')] %>% group_by(id) %>% slice_max(dt) %>% rename(max_dt=dt)
  
  last_pup <- to_graph %>% group_by(id) %>% slice_max(dt) %>% rename(last_pup=dt)
  image <- left_join(image, last_pup) %>% filter(dt < last_pup)
  
  min_max_graph <- full_join(min_graph, max_graph) %>% distinct() 
  
  #Derive the time from last seen well for each timestamp. We will use this for graphing.
  to_graph$tf_lsw <- difftime(to_graph$dt, to_graph$lsw, units = 'hours')
  
  
  ## Table with ID Pupil Measurements, Hours followed, Average time between pupils, At least 3 pupils before 18h (* pupils), MLS >=5 at time before admission (12/30)
  sum_dat <- to_graph %>%
    group_by(id) %>%
    summarise(pup_count=n(),
              hrs_followed=round(difftime(tail(dt,1),head(dt,1),units = 'hours'), 0),
              iqr_25 = paste0(round(quantile(measure_difftime(unique(id), variable, to_graph), c(.25), na.rm=T), 2), ' hours'),
              avg_delta = round(hrs_followed/(pup_count-1), 2),
              iqr_75 = paste0(round(quantile(measure_difftime(unique(id), variable, to_graph), c(.75), na.rm=T), 2), ' hours'),
              mls5_at_pres = ifelse(unique(difftime(mls5dt, pres))<0, 'YES', 'NO')
    )
  
  max_img <- TSV %>% group_by(id) %>% summarise(
    max_mls = ifelse(max(rc_size_mls, na.rm=TRUE)<0, NA, max(rc_size_mls, na.rm=TRUE)),
    max_pgs = ifelse(max(rc_size_pgs, na.rm=TRUE)<0, NA, max(rc_size_pgs, na.rm=TRUE))
  )
  
  sec_outcomes <- OTV %>% select(id, surg, surgdt, deathByDischarge) %>% 
    left_join(TSV %>% drop_na(variable) %>% filter(id %in% OTV[OTV$death==1,]$id) %>%
                select(id, dt) %>%
                group_by(id) %>% 
                slice_max(dt) %>%  
                rename(withdrawaldt=dt)) %>% select(-surg, -deathByDischarge)
  
  sum_dat <- sum_dat %>% left_join(max_img) %>% left_join(sec_outcomes)
  colnames(sum_dat) <- c('ID', 'Number of Measurements', 'Hours Followed','Measurement \u0394 (Q1)','Measurement \u0394 (Mean)','Measurement \u0394 (Q3)', 'MLS \u2265 5 at Presentation', 'Max MLS', 'Max PGS', 'Hemicraniectomy', 'Withdrawal of Treatment')
  
  
  var_plot(pt, desc = F, var=variable)
  
}