rm(list=ls())

pack <- c("tidyverse", "stringr", "ggplot2", "ocedata", "gstat", "sp", 
          "rgdal", "maptools", "rworldmap", "scales","matlab","mosaic", "gpclib")
lapply(pack, library, character.only = TRUE) 

mat <- matrix(0, 12, 12)
mat[1:12  , 1:12 ] <- 1

############################
#
# APRIL
#
###########################

setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/OUTPUT/")
load("data_select_mn_APR.Rdata")
load("dclust_APR-MAY_APR_mn.Rdata")
df <- data_cluster[,-2:-10]
names(df) <- c("id_ind", "group")

dp_daily_2$id_ind <- as.numeric(as.character(dp_daily_2$id_ind))
df$id_ind <- as.numeric(as.character(df$id_ind))
full_data <- full_join(dp_daily_2,df, by="id_ind")
date <- dp_daily_2$date[1:182]

month <- "APRIL"
group     <- c(8,5,4,3,2)
col_group <- c("#BD0026","#BD0026","#BD0026","#BD0026","#08306B") 

col_group_max <- "#FD8D3C" 
nb_group_max  <- 1

date_init <- c("2006-04-08", "2006-04-15", "2006-04-22", "2006-05-01")
date_end  <- c("2006-04-01", "2006-04-08", "2006-04-15", "2006-04-22")

for (d in 1:length(date_init)) {
  select_data <- filter(full_data, date   <  date_init[d])
  select_data <- filter(select_data, date >= date_end[d] )
  
  init_date <- range(select_data$date)[1]
  end_date  <- range(select_data$date)[2]
  
  layout(mat)
  par(oma=c(1, 1, 1, 1))
  par(mar=c(1, 1, 1, 1))
  
  data("coastlineWorldFine")
  plot(coastlineWorldFine, clongitude=-63.5, clatitude=48.5, span=1000, xlab="", ylab="", axes=FALSE, xaxt="n")
  axis(2, at= seq(44,52,2) ,  las=1, labels=c("44°N", "46°N","48°N", "50°N", "52°N"), mgp=c(1.5,0.5,0), cex.axis=1.2)   
  axis(1, at=seq(-75,-50,5),  las=1, labels=c("75°W","70°W","65°W","60°W", "55°W", "50°W"), mgp=c(1.5,0.5,0), cex.axis=1.2)     
  box()
  
  j <- c(as.numeric(as.character(select_data$id_ind[select_data$group==nb_group_max[1] & select_data$date==init_date])))
  
  for(k in j){
    lines(select_data$lon[select_data$id_ind == k],
          select_data$lat[select_data$id_ind == k ],
          col=col_group_max[1], lwd=0.3)
  }
  
  # AUTRES TRAITS
  g <- 0
  
  for(i in group){
    g <- g + 1 
    points(select_data$lon[select_data$date == init_date & select_data$group==i], 
           select_data$lat[select_data$date == init_date & select_data$group==i], 
           col = col_group[g], 
           pch = 1, cex = 0.2)
    text(-70, 51, labels = month, cex = 2, font = 2  )
    j <- c(as.numeric(as.character(select_data$id_ind[select_data$group==i & select_data$date==init_date])))
    
    for(k in j){
      lines(select_data$lon[select_data$id_ind == k],
            select_data$lat[select_data$id_ind == k ],
            col=col_group[g])
    }
  }
  
  setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/GRAPH/")
  dev.print(tiff,filename=paste0('APR_',d,'_mn.tiff'), width=30, height=20,units="cm",res = 400)
  dev.off()
}

############################
#
# MAY
#
###########################

## ADD DATA 
setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/OUTPUT/")
load("data_select_mn_MAY.Rdata")
load("dclust_MAY-JUN_MAY_mn.Rdata")

df <- data_cluster[,-2:-10]
names(df) <- c("id_ind", "group")
dp_daily_2$id_ind <- as.numeric(as.character(dp_daily_2$id_ind))
df$id_ind <- as.numeric(as.character(df$id_ind))
full_data <- full_join(dp_daily_2,df, by="id_ind")

month <- "MAY"
group <- c(7,6,9,5,8,4,1,2)
col_group <- c("#BD0026","#BD0026","#BD0026","#BD0026","#BD0026","#BD0026","#08306B","#08306B")

col_group_max <- "#FD8D3C" 
nb_group_max  <- 3

date_init <- c("2006-05-08", "2006-05-15", "2006-05-22", "2006-06-01")
date_end  <- c("2006-05-01", "2006-05-08", "2006-05-15", "2006-05-22")

## MODIF TO DO 
for(d in 1 : length(date_init)){
  
  select_data <- filter(full_data, date < date_init[d])
  select_data <- filter(select_data, date >= date_end[d])
  
  init_date <- range(select_data$date)[1]
  end_date  <- range(select_data$date)[2]
  
  # AUTRES 
  
  layout(mat)
  par(oma=c(1, 1, 1, 1))
  par(mar=c(1, 1, 1, 1))
  
  data("coastlineWorldFine")
  plot(coastlineWorldFine, clongitude=-63.5, clatitude=48.5, span=1000, xlab="", ylab="", axes=FALSE, xaxt="n")
  axis(2, at= seq(44,52,2),  las=1, labels=c("44°N", "46°N","48°N", "50°N", "52°N"), mgp=c(1.5,0.5,0), cex.axis=1.2)   
  axis(1, at=seq(-75,-50,5),  las=1, labels=c("75°W","70°W","65°W","60°W", "55°W", "50°W"), mgp=c(1.5,0.5,0), cex.axis=1.2)     
  box()
  
  j <- c(as.numeric(as.character(select_data$id_ind[select_data$group==nb_group_max & select_data$date==init_date])))
  
  for(k in j){
    lines(select_data$lon[select_data$id_ind == k],
          select_data$lat[select_data$id_ind == k ],
          col=col_group_max, lwd=0.3)
  }
  
  g <- 0
  
  for(i in group){
    g <- g + 1 
    points(select_data$lon[select_data$date == init_date & select_data$group==i], 
           select_data$lat[select_data$date == init_date & select_data$group==i], 
           col = col_group[g], 
           pch = 1, cex = 0.2)
    text(-70, 51, labels = month, cex = 2, font = 2  )
    j <- c(as.numeric(as.character(select_data$id_ind[select_data$group==i & select_data$date==init_date])))
    
    for(k in j){
      lines(select_data$lon[select_data$id_ind == k],
            select_data$lat[select_data$id_ind == k ],
            col=col_group[g])
    }
  }
  
  setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/GRAPH/")
  dev.print(tiff,filename=paste0('MAY_',d,'_mn.tiff'), width=30, height=20,units="cm",res = 400)
  dev.off()
}

############################
#
# JUNE
#
###########################

## ADD DATA 
setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/OUTPUT/")
load("data_select_mn_JUN.Rdata")
load("dclust_JUN-JUL_JUN_mn.Rdata")

df <- data_cluster[,-2:-10]
names(df) <- c("id_ind", "group")
dp_daily_2$id_ind <- as.numeric(as.character(dp_daily_2$id_ind))
df$id_ind <- as.numeric(as.character(df$id_ind))
full_data <- full_join(dp_daily_2,df, by="id_ind")
date <- dp_daily_2$date[1:182]

month <- "JUNE"
group     <- c(6,12,9,10,3,4,2)
col_group <- c("#BD0026", "#BD0026", "#BD0026", "#BD0026", "#BD0026", "#BD0026", "#08306B")

col_group_max <- rgb(8,48,107, maxColorValue=255, alpha = 120) 
nb_group_max  <- 1


date_init <- c("2006-06-08", "2006-06-15", "2006-06-22", "2006-07-01")
date_end  <- c("2006-06-01", "2006-06-08", "2006-06-15", "2006-06-22")

## MODIF TO DO 
for(d in 1 : length(date_init)){
  select_data <- filter(full_data, date < date_init[d])
  select_data <- filter(select_data, date >= date_end[d])
  
  init_date <- range(select_data$date)[1]
  end_date  <- range(select_data$date)[2]
  
  layout(mat)
  par(oma=c(1, 1, 1, 1))
  par(mar=c(1, 1, 1, 1))
  
  data("coastlineWorldFine")
  plot(coastlineWorldFine, clongitude=-63.5, clatitude=48.5, span=1000, xlab="", ylab="", axes=FALSE, xaxt="n")
  axis(2, at= seq(44,52,2),  las=1, labels=c("44°N", "46°N","48°N", "50°N", "52°N"), mgp=c(1.5,0.5,0), cex.axis=1.2)   
  axis(1, at=seq(-75,-50,5),  las=1, labels=c("75°W","70°W","65°W","60°W", "55°W", "50°W"), mgp=c(1.5,0.5,0), cex.axis=1.2)     
  box()
  
  j <- c(as.numeric(as.character(select_data$id_ind[select_data$group==nb_group_max & select_data$date==init_date])))
  
  for(k in j){
    lines(select_data$lon[select_data$id_ind == k],
          select_data$lat[select_data$id_ind == k ],
          col=col_group_max, lwd=0.3)
  }
  
  g <- 0
  
  for(i in group){
    g <- g + 1 
    points(select_data$lon[select_data$date == init_date & select_data$group==i], 
           select_data$lat[select_data$date == init_date & select_data$group==i], 
           col = col_group[g], 
           pch = 1, cex = 0.2)
    text(-70, 51, labels = month, cex = 2, font = 2  )
    j <- c(as.numeric(as.character(select_data$id_ind[select_data$group==i & select_data$date==init_date])))
    
    for(k in j){
      lines(select_data$lon[select_data$id_ind == k],
            select_data$lat[select_data$id_ind == k ],
            col=col_group[g])
    }
  }
  
  setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/GRAPH/")
  dev.print(tiff,filename=paste0('JUN_',d,'_mn.tiff'), width=30, height=20,units="cm",res = 400)
  dev.off()
}

############################
#
# JULY
#
###########################

## ADD DATA 
setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/OUTPUT/")
load("data_select_mn_JUL.Rdata")
load("dclust_JUL-AUG_JUL_mn.Rdata")

df <- data_cluster[,-2:-10]
names(df) <- c("id_ind", "group")
dp_daily_2$id_ind <- as.numeric(as.character(dp_daily_2$id_ind))
df$id_ind <- as.numeric(as.character(df$id_ind))
full_data <- full_join(dp_daily_2,df, by="id_ind")
date <- dp_daily_2$date[1:182]

month <- "JULY"
group     <- c(7,11,6,4,3,2)
col_group <- c("#BD0026", "#BD0026", "#BD0026", "#BD0026", "#08306B","#08306B")

col_group_max <- rgb(8,48,107, maxColorValue=255, alpha = 120) 
nb_group_max  <- 1

date_init <- c("2006-07-08", "2006-07-15", "2006-07-22", "2006-08-01")
date_end  <- c("2006-07-01", "2006-07-08", "2006-07-15", "2006-07-22")

## MODIF TO DO 
for(d in 1 : length(date_init)){
  
  select_data <- filter(full_data, date < date_init[d])
  select_data <- filter(select_data, date >= date_end[d])
  
  init_date <- range(select_data$date)[1]
  end_date  <- range(select_data$date)[2]
  
  layout(mat)
  par(oma=c(1, 1, 1, 1))
  par(mar=c(1, 1, 1, 1))
  
  data("coastlineWorldFine")
  plot(coastlineWorldFine, clongitude=-63.5, clatitude=48.5, span=1000, xlab="", ylab="", axes=FALSE, xaxt="n")
  axis(2, at= seq(44,52,2),  las=1, labels=c("44°N", "46°N","48°N", "50°N", "52°N"), mgp=c(1.5,0.5,0), cex.axis=1.2)   
  axis(1, at=seq(-75,-50,5),  las=1, labels=c("75°W","70°W","65°W","60°W", "55°W", "50°W"), mgp=c(1.5,0.5,0), cex.axis=1.2)     
  box()
  
  j <- c(as.numeric(as.character(select_data$id_ind[select_data$group==nb_group_max & select_data$date==init_date])))
  
  for(k in j){
    lines(select_data$lon[select_data$id_ind == k],
          select_data$lat[select_data$id_ind == k ],
          col=col_group_max, lwd=0.3)
  }
  
  g <- 0
  
  for(i in group){
    g <- g + 1 
    points(select_data$lon[select_data$date == init_date & select_data$group==i], 
           select_data$lat[select_data$date == init_date & select_data$group==i], 
           col = col_group[g], 
           pch = 1, cex = 0.2)
    text(-70, 51, labels = month, cex = 2, font = 2  )
    j <- c(as.numeric(as.character(select_data$id_ind[select_data$group==i & select_data$date==init_date])))
    
    for(k in j){
      lines(select_data$lon[select_data$id_ind == k],
            select_data$lat[select_data$id_ind == k ],
            col=col_group[g])
    }
  }
  
  setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/GRAPH/")
  dev.print(tiff,filename=paste0('JUL_',d,'_mn.tiff'), width=30, height=20,units="cm",res = 400)
  dev.off()
}

############################
#
# AUGUST
#
###########################

setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/OUTPUT/")
load("data_select_mn_AUG.Rdata")
load("dclust_AUG-SEP_AUG_mn.Rdata")

df <- data_cluster[,-2:-10]
names(df) <- c("id_ind", "group")

dp_daily_2$id_ind <- as.numeric(as.character(dp_daily_2$id_ind))
df$id_ind <- as.numeric(as.character(df$id_ind))

full_data <- full_join(dp_daily_2,df, by="id_ind")

date <- dp_daily_2$date[1:182]

month <- "AUGUST"
group     <- c(11,5,7,4,8,2,1)
col_group <- c(rgb(189,0,38, maxColorValue=255),
               rgb(189,0,38, maxColorValue=255),
               rgb(189,0,38, maxColorValue=255),
               rgb(189,0,38, maxColorValue=255),
               rgb(8,48,107, maxColorValue=255),
               rgb(8,48,107, maxColorValue=255),
               rgb(8,48,107, maxColorValue=255))

col_group_max <- rgb(8,48,107, maxColorValue=255, alpha = 120) 
nb_group_max  <- 3

date_init <- c("2006-08-08", "2006-08-15", "2006-08-22", "2006-09-01")
date_end  <- c("2006-08-01", "2006-08-08", "2006-08-15", "2006-08-22")

for(d in 1 : length(date_init)){
  
  select_data <- filter(full_data, date < date_init[d])
  select_data <- filter(select_data, date >= date_end[d])
  
  init_date <- range(select_data$date)[1]
  end_date  <- range(select_data$date)[2]
  
  layout(mat)
  par(oma=c(1, 1, 1, 1))
  par(mar=c(1, 1, 1, 1))  
  
  data("coastlineWorldFine")
  plot(coastlineWorldFine, clongitude=-63.5, clatitude=48.5, span=1000, xlab="", ylab="", axes=FALSE, xaxt="n")
  axis(2, at= seq(44,52,2),  las=1, labels=c("44°N", "46°N","48°N", "50°N", "52°N"), mgp=c(1.5,0.5,0), cex.axis=1.2)   
  axis(1, at=seq(-75,-50,5),  las=1, labels=c("75°W","70°W","65°W","60°W", "55°W", "50°W"), mgp=c(1.5,0.5,0), cex.axis=1.2)     
  box()
  
  j <- c(as.numeric(as.character(select_data$id_ind[select_data$group==nb_group_max & select_data$date==init_date])))
  
  for(k in j){
    lines(select_data$lon[select_data$id_ind == k],
          select_data$lat[select_data$id_ind == k ],
          col=col_group_max, lwd=0.3)
  }
  
  g <- 0
  
  for(i in group){
    g <- g + 1 
    points(select_data$lon[select_data$date == init_date & select_data$group==i], 
           select_data$lat[select_data$date == init_date & select_data$group==i], 
           col = col_group[g], 
           pch = 1, cex = 0.2)
    text(-70, 51, labels = month, cex = 2, font = 2  )
    j <- c(as.numeric(as.character(select_data$id_ind[select_data$group==i & select_data$date==init_date])))
    
    for(k in j){
      lines(select_data$lon[select_data$id_ind == k],
            select_data$lat[select_data$id_ind == k ],
            col=col_group[g])
    }
  }
  
  setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/GRAPH/")
  dev.print(tiff,filename=paste0('AUG_',d,'_mn.tiff'), width=30, height=20,units="cm",res = 400)
  dev.off()
}
