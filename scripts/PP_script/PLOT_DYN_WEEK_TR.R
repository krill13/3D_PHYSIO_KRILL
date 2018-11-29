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
load("data_select_tr_APR.Rdata")
load("dclust_APR-MAY_APR_tr.Rdata")
df <- data_cluster[,-2:-10]
names(df) <- c("id_ind", "group")

dp_daily_2$id_ind <- as.numeric(as.character(dp_daily_2$id_ind))
df$id_ind <- as.numeric(as.character(df$id_ind))
full_data <- full_join(dp_daily_2,df, by="id_ind")
date <- dp_daily_2$date[1:182]

month <- "APRIL"
group     <- c(7,6,14,5,13,4,10,12,15,9,2,8,11)
col_group <- c("#BD0026", "#BD0026", "#BD0026", "#BD0026", "#BD0026", "#BD0026", "#FD8D3C", "#FD8D3C",
               "#FD8D3C", "#FD8D3C", "#FD8D3C","#08306B","#08306B")


col_group_max <- c(rgb(253,141,60, maxColorValue=255, alpha = 120),
                   rgb(253,141,60, maxColorValue=255, alpha = 120))
nb_group_max  <- c(3,1)

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
  
  j <- c(as.numeric(as.character(select_data$id_ind[select_data$group==nb_group_max[2] & select_data$date==init_date])))
  
  for(k in j){
    lines(select_data$lon[select_data$id_ind == k],
          select_data$lat[select_data$id_ind == k],
          col=col_group_max[2], lwd=0.3)
  }
  # AUTRES TRAITS
  g <- 0
  arr_group     <- c(2,9,15,12,10,4,13,5,14,6,7,8,11)
  arr_col_group <- c("#FD8D3C", "#FD8D3C", "#FD8D3C", "#FD8D3C", "#FD8D3C","#BD0026", 
                     "#BD0026", "#BD0026", "#BD0026", "#BD0026", "#BD0026", "#08306B","#08306B")

     for(z in arr_group){
      g <- g + 1 
      points(select_data$lon[select_data$date == init_date & select_data$group==z], 
             select_data$lat[select_data$date == init_date & select_data$group==z], 
             col = arr_col_group[g], 
             pch = 1, cex = 0.2)
      text(-70, 51, labels = month, cex = 2, font = 2  )
      j <- c(as.numeric(as.character(select_data$id_ind[select_data$group==z & select_data$date==init_date])))
      
      for(k in j){
        lines(select_data$lon[select_data$id_ind == k],
              select_data$lat[select_data$id_ind == k ],
              col=arr_col_group[g])
      }
    }
    
  setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/GRAPH/")
  dev.print(tiff,filename=paste0('APR_',d,'_tr.tiff'), width=30, height=20,units="cm",res = 400)
  dev.off()
}

############################
#
# MAY
#
###########################

## ADD DATA 
setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/OUTPUT/")
load("data_select_tr_MAY.Rdata")
load("dclust_MAY-JUN_MAY_tr.Rdata")

df <- data_cluster[,-2:-10]
names(df) <- c("id_ind", "group")
dp_daily_2$id_ind <- as.numeric(as.character(dp_daily_2$id_ind))
df$id_ind <- as.numeric(as.character(df$id_ind))
full_data <- full_join(dp_daily_2,df, by="id_ind")

month <- "MAY"
group     <- c(6,5,9,4,8,3,2,7)
col_group <- c("#BD0026", "#BD0026", "#BD0026", "#BD0026", "#BD0026", "#BD0026", "#FD8D3C", "#08306B")

col_group_max <- rgb(253,141,60, maxColorValue=255, alpha = 120) 
nb_group_max  <- 1

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
  arr_group     <- c(2,6,5,9,4,8,3,7)
  arr_col_group <- c( "#FD8D3C","#BD0026", "#BD0026", "#BD0026", "#BD0026", "#BD0026", "#BD0026", "#08306B")
  
  for(z in arr_group){
    g <- g + 1 
    points(select_data$lon[select_data$date == init_date & select_data$group==z], 
           select_data$lat[select_data$date == init_date & select_data$group==z], 
           col = arr_col_group[g], 
           pch = 1, cex = 0.2)
    text(-70, 51, labels = month, cex = 2, font = 2  )
    j <- c(as.numeric(as.character(select_data$id_ind[select_data$group==z & select_data$date==init_date])))
    
    for(k in j){
      lines(select_data$lon[select_data$id_ind == k],
            select_data$lat[select_data$id_ind == k ],
            col=arr_col_group[g])
    }
  }
  
  setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/GRAPH/")
  dev.print(tiff,filename=paste0('MAY_',d,'_tr.tiff'), width=30, height=20,units="cm",res = 400)
  dev.off()
}

############################
#
# JUNE
#
###########################

## ADD DATA 
setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/OUTPUT/")
load("data_select_tr_JUN.Rdata")
load("dclust_JUN-JUL_JUN_tr.Rdata")

df <- data_cluster[,-2:-10]
names(df) <- c("id_ind", "group")
dp_daily_2$id_ind <- as.numeric(as.character(dp_daily_2$id_ind))
df$id_ind <- as.numeric(as.character(df$id_ind))
full_data <- full_join(dp_daily_2,df, by="id_ind")
date <- dp_daily_2$date[1:182]

month <- "JUNE"
group     <- c(5,12,10,3,2)
col_group <- c("#BD0026", "#BD0026", "#BD0026", "#BD0026", "#BD0026")

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
  dev.print(tiff,filename=paste0('JUN_',d,'_tr.tiff'), width=30, height=20,units="cm",res = 400)
  dev.off()
}

############################
#
# JULY
#
###########################

## ADD DATA 
setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/OUTPUT/")
load("data_select_tr_JUL.Rdata")
load("dclust_JUL-AUG_JUL_tr.Rdata")

df <- data_cluster[,-2:-10]
names(df) <- c("id_ind", "group")
dp_daily_2$id_ind <- as.numeric(as.character(dp_daily_2$id_ind))
df$id_ind <- as.numeric(as.character(df$id_ind))
full_data <- full_join(dp_daily_2,df, by="id_ind")
date <- dp_daily_2$date[1:182]

month <- "JULY"
group     <- c(5,3,2)
col_group <- c(rgb(189,0,38, maxColorValue=255),
               rgb(189,0,38, maxColorValue=255),
               rgb(189,0,38, maxColorValue=255))

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
  dev.print(tiff,filename=paste0('JUL_',d,'_tr.tiff'), width=30, height=20,units="cm",res = 400)
  dev.off()
}

############################
#
# AUGUST
#
###########################

setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/OUTPUT/")
load("data_select_tr_AUG.Rdata")
load("dclust_AUG-SEP_AUG_tr.Rdata")

df <- data_cluster[,-2:-10]
names(df) <- c("id_ind", "group")

dp_daily_2$id_ind <- as.numeric(as.character(dp_daily_2$id_ind))
df$id_ind <- as.numeric(as.character(df$id_ind))

full_data <- full_join(dp_daily_2,df, by="id_ind")

date <- dp_daily_2$date[1:182]

month <- "AUGUST"
group     <- c(4,3,2)
col_group <- c(rgb(189,0,38, maxColorValue=255),
               rgb(189,0,38, maxColorValue=255),
               rgb(8,48,107, maxColorValue=255))

col_group_max <- rgb(8,48,107, maxColorValue=255, alpha = 120) 
nb_group_max  <- 1

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
  arr_group     <- c(2,3,4)
  arr_col_group <- c(rgb(8,48,107, maxColorValue=255),
                     rgb(189,0,38, maxColorValue=255),
                     rgb(189,0,38, maxColorValue=255))
  
  for(z in arr_group){
    g <- g + 1 
    points(select_data$lon[select_data$date == init_date & select_data$group==z], 
           select_data$lat[select_data$date == init_date & select_data$group==z], 
           col = arr_col_group[g], 
           pch = 1, cex = 0.2)
    text(-70, 51, labels = month, cex = 2, font = 2  )
    j <- c(as.numeric(as.character(select_data$id_ind[select_data$group==z & select_data$date==init_date])))
    
    for(k in j){
      lines(select_data$lon[select_data$id_ind == k],
            select_data$lat[select_data$id_ind == k ],
            col=arr_col_group[g])
    }
  }
  
  setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/GRAPH/")
  dev.print(tiff,filename=paste0('AUG_',d,'_tr.tiff'), width=30, height=20,units="cm",res = 400)
  dev.off()
}
