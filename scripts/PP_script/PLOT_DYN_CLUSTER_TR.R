rm(list=ls())

pack <- c("tidyverse", "stringr", "ggplot2", "ocedata", "gstat", "sp", 
          "rgdal", "maptools", "rworldmap", "scales","matlab","mosaic", "gpclib")
lapply(pack, library, character.only = TRUE) 


mat <- matrix(0, 28, 27)
mat[1:18 , 1:18 ] <- 1
mat[1:18 , 19:24] <- 2
mat[19:26 , 3:13] <- 3
mat[19:26 , 15:25] <- 4

############################
#
# APRIL
#
###########################

setwd("/Users/deborah/Documents/Doctorat/3D MODEL/OUTPUT/")
load("data_select_tr_APR.Rdata")
load("dclust_APR-MAY_APR_tr.Rdata")
df <- data_cluster[,-2:-10]
names(df) <- c("id_ind", "group")

dp_daily_2$id_ind <- as.numeric(as.character(dp_daily_2$id_ind))
df$id_ind <- as.numeric(as.character(df$id_ind))
full_data <- full_join(dp_daily_2,df, by="id_ind")
date <- dp_daily_2$date[1:182]

boxplot(data_cluster$cumul_grow ~ data_cluster$group)
abline(h=quantile(data_cluster$cumul_grow, c(0.9)), col="red")
abline(h=0, col="blue")

month <- "APRIL"
group     <- c(7,6,14,5,13,4,10,12,15,9,2,8,11)
col_group <- c("#BD0026", "#BD0026", "#BD0026", "#BD0026", "#BD0026", "#BD0026", "#FD8D3C", "#FD8D3C",
               "#FD8D3C", "#FD8D3C", "#FD8D3C","#08306B","#08306B")


col_group_max <- c(rgb(253,141,60, maxColorValue=255, alpha = 120),
                   rgb(253,141,60, maxColorValue=255, alpha = 120))
nb_group_max  <- c(3,1)

tot_group     <- c(7,6,14,5,13,4,10,12,1,3,15,2,9,8,11)
tot_col_group <- c("#BD0026", "#BD0026", "#BD0026", "#BD0026", "#BD0026", "#BD0026", "#FD8D3C", "#FD8D3C",
               "#FD8D3C", "#FD8D3C", "#FD8D3C", "#FD8D3C", "#FD8D3C","#08306B","#08306B")

date_init <- c("2006-05-01")
date_end  <- c("2006-04-01")
  
  select_data <- filter(full_data, date < date_init[1])
  select_data <- filter(select_data, date >= date_end[1])
  
  init_date <- range(select_data$date)[1]
  end_date  <- range(select_data$date)[2]
  
  layout(mat)
  data("coastlineWorldFine")
  plot(coastlineWorldFine, clongitude=-63.5, clatitude=48.5, span=1000, xlab="", ylab="", axes=FALSE, xaxt="n")
  axis(2, at= seq(44,52,2),  las=1, labels=c("44°N", "46°N","48°N", "50°N", "52°N"), mgp=c(1.5,0.5,0), cex.axis=1.2)   
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
  # AUTRES TRAITS DU MOIS
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
  
  par(mar = c(1,1,1,1))
  length_tot <- length(tot_group) * 2 + 1
  loc_rect <- length_tot - 3
  
  plot(c(0,0.5), c(0,1), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='')
  text(x=0.125, y=(length_tot/length_tot), labels = "GROUP", cex=1.6, font=2)
  
  for (i in 1:length(tot_group)) {
    rect(0,(loc_rect/length_tot),0.125, (loc_rect/length_tot)+(1/length_tot), col=tot_col_group[i], border=NA)
    text(x=0.25, y=((loc_rect + 0.5)/length_tot), labels = tot_group[i], cex=1.6, font=2)
    loc_rect <- loc_rect - 2 
  }
  
  hist(data_cluster$cumul_grow, xlab="", ylab="",xaxt="n", yaxt="n", cex.axis=2, axes=FALSE, main="", ylim=c(0,2600))
  axis(2, ylim=c(0,3500), las=1, labels=TRUE, mgp=c(1.5,0.5,0),cex.axis=1.5)     # Axis caracteristics : mgp = position of values and labels, cex.lab = labels size, cex.axis = values size
  axis(1, xlim=c(-5.5, 19), cex.axis=1.5)
  mtext("Cumulated mass (mg C)", side=1, line = 2.3, cex=1.1)
  mtext("Frequency", side=2, line = 3.5, cex=1.1)
  abline(v=quantile(data_cluster$cumul_grow, c(0.9)), col="red", lwd=2)
  abline(v=0, col="black", lwd=2, lty=2)
  box()
  
  for(i in tot_group){
    points(x=median(data_cluster$cumul_grow[data_cluster$group==i]), y=2400, pch=20)
    text(x=median(data_cluster$cumul_grow[data_cluster$group==i]),y=2550, labels=i, cex=1.1, font=2)
    
  }
  
  plot(x=select_data$date[select_data$id_ind=="1"],
       y=tapply(select_data$mass[select_data$group==tot_group[1]],
                select_data$date[select_data$group==tot_group[1]], median), 
       type="l", col=tot_col_group[1],  xlab="", ylab="",xaxt="n", yaxt="n", cex.axis=2, axes=FALSE,  ylim=c(0,30), lwd = 2)
  
  for(i in 2:length(tot_group)){
    lines(x=select_data$date[select_data$id_ind=="1"],
          y=tapply(select_data$mass[select_data$group==tot_group[i]],
                   select_data$date[select_data$group==tot_group[i]], median), 
          type="l", col=tot_col_group[i], lwd = 2)
  }
  axis(2, las=1, labels=TRUE, mgp=c(1.5,0.5,0),cex.axis=1.5)     # Axis caracteristics : mgp = position of values and labels, cex.lab = labels size, cex.axis = values size
  axis.Date(1, at = seq(select_data$date[1], select_data$date[length(select_data$date)], by = "day"), 
            format = "%b%d", labels=TRUE, cex.axis=1.5)
  mtext("Mass (mg C)", cex = 1.1, side=2, line=3)
  mtext("Time (month)", cex = 1.1, side=1, line=2.3)
  box()
  
  abline(h=15, lwd = 2, lty=2)
  
  
  setwd("/Users/deborah/Documents/Doctorat/3D MODEL/GRAPH_PAPER_2/")
  dev.print(tiff,filename='FIG_11_CHAP_2.tiff', width=24, height=16,units="cm",res = 400)
  dev.off()


############################
#
# MAY
#
###########################

## ADD DATA 
setwd("/Users/deborah/Documents/Doctorat/3D MODEL/OUTPUT/")
load("data_select_tr_MAY.Rdata")
load("dclust_MAY-JUN_MAY_tr.Rdata")
month <- "MAY"

df <- data_cluster[,-2:-10]
names(df) <- c("id_ind", "group")
dp_daily_2$id_ind <- as.numeric(as.character(dp_daily_2$id_ind))
df$id_ind <- as.numeric(as.character(df$id_ind))
full_data <- full_join(dp_daily_2,df, by="id_ind")

boxplot(data_cluster$cumul_grow ~ data_cluster$group)
abline(h=quantile(data_cluster$cumul_grow, c(0.9)), col="red")
abline(h=0, col="blue")

group     <- c(6,5,9,4,8,3,2,7)
col_group <- c("#BD0026", "#BD0026", "#BD0026", "#BD0026", "#BD0026", "#BD0026", "#FD8D3C", "#08306B")

col_group_max <- rgb(253,141,60, maxColorValue=255, alpha = 80) 
nb_group_max  <- 1

tot_group     <- c(6,5,9,4,8,3,2,1,7)
tot_col_group <- c("#BD0026", "#BD0026", "#BD0026", "#BD0026", "#BD0026", "#BD0026", "#FD8D3C", "#FD8D3C", "#08306B")

date_init <- c("2006-06-01")
date_end  <- c("2006-05-01")

## MODIF TO DO 
  
select_data <- filter(full_data, date < date_init[1])
select_data <- filter(select_data, date >= date_end[1])

init_date <- range(select_data$date)[1]
end_date  <- range(select_data$date)[2]

layout(mat)
data("coastlineWorldFine")
plot(coastlineWorldFine, clongitude=-63.5, clatitude=48.5, span=1000, xlab="", ylab="", axes=FALSE, xaxt="n")
axis(2, at= seq(44,52,2),  las=1, labels=c("44°N", "46°N","48°N", "50°N", "52°N"), mgp=c(1.5,0.5,0), cex.axis=1.2)   
axis(1, at=seq(-75,-50,5),  las=1, labels=c("75°W","70°W","65°W","60°W", "55°W", "50°W"), mgp=c(1.5,0.5,0), cex.axis=1.2)     
box()

j <- c(as.numeric(as.character(select_data$id_ind[select_data$group==nb_group_max[1] & select_data$date==init_date])))

for(k in j){
  lines(select_data$lon[select_data$id_ind == k],
        select_data$lat[select_data$id_ind == k ],
        col=col_group_max[1], lwd=0.3)
}

# AUTRES TRAITS DU MOIS
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

par(mar = c(1,1,1,1))
length_tot <- length(tot_group) * 2 + 1
loc_rect <- length_tot - 3

plot(c(0,0.5), c(0,1), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='')
text(x=0.125, y=(length_tot/length_tot), labels = "GROUP", cex=1.6, font=2)

for (i in 1:length(tot_group)) {
  rect(0,(loc_rect/length_tot),0.125, (loc_rect/length_tot)+(1/length_tot), col=tot_col_group[i], border=NA)
  text(x=0.25, y=((loc_rect + 0.5)/length_tot), labels = tot_group[i], cex=1.6, font=2)
  loc_rect <- loc_rect - 2 
}

hist(data_cluster$cumul_grow, xlab="", ylab="",xaxt="n", yaxt="n", cex.axis=2, axes=FALSE, main="", ylim=c(0,3600))
axis(2, ylim=c(0,3500), las=1, labels=TRUE, mgp=c(1.5,0.5,0),cex.axis=1.5)     # Axis caracteristics : mgp = position of values and labels, cex.lab = labels size, cex.axis = values size
axis(1, xlim=c(-5.5, 19), cex.axis=1.5)
mtext("Cumulated mass (mg C)", side=1, line = 2.3, cex=1.1)
mtext("Frequency", side=2, line = 3.5, cex=1.1)
abline(v=quantile(data_cluster$cumul_grow, c(0.9)), col="red", lwd=2)
abline(v=0, col="black", lwd=2, lty=2)
box()

for(i in tot_group){
  points(x=median(data_cluster$cumul_grow[data_cluster$group==i]), y=3200, pch=20)
  text(x=median(data_cluster$cumul_grow[data_cluster$group==i]),y=3500, labels=i, cex=1.5, font=2)
  
}

plot(x=select_data$date[select_data$id_ind=="1"],
     y=tapply(select_data$mass[select_data$group==tot_group[1]],
              select_data$date[select_data$group==tot_group[1]], median), 
     type="l", col=tot_col_group[1],  xlab="", ylab="",xaxt="n", yaxt="n", cex.axis=2, axes=FALSE,  ylim=c(0,30), lwd = 2)

for(i in 2:length(tot_group)){
  lines(x=select_data$date[select_data$id_ind=="1"],
        y=tapply(select_data$mass[select_data$group==tot_group[i]],
                 select_data$date[select_data$group==tot_group[i]], median), 
        type="l", col=tot_col_group[i], lwd = 2)
}
axis(2, las=1, labels=TRUE, mgp=c(1.5,0.5,0),cex.axis=1.5)     # Axis caracteristics : mgp = position of values and labels, cex.lab = labels size, cex.axis = values size
axis.Date(1, at = seq(select_data$date[1], select_data$date[length(select_data$date)], by = "day"), 
          format = "%b%d", labels=TRUE, cex.axis=1.5)
mtext("Mass (mg C)", cex = 1.1, side=2, line=3)
mtext("Time (month)", cex = 1.1, side=1, line=2.3)
box()

abline(h=15, lwd = 2, lty=2)


setwd("/Users/deborah/Documents/Doctorat/3D MODEL/GRAPH_PAPER_2/")
dev.print(tiff,filename='TOT_CLUSTER_ADV_MAY_TR.tiff', width=24, height=16,units="cm",res = 400)
dev.off()

############################
#
# JUNE
#
###########################

## ADD DATA 
setwd("/Users/deborah/Documents/Doctorat/3D MODEL/OUTPUT/")
load("data_select_tr_JUN.Rdata")
load("dclust_JUN-JUL_JUN_tr.Rdata")
month <- "JUNE"

df <- data_cluster[,-2:-10]
names(df) <- c("id_ind", "group")
dp_daily_2$id_ind <- as.numeric(as.character(dp_daily_2$id_ind))
df$id_ind <- as.numeric(as.character(df$id_ind))
full_data <- full_join(dp_daily_2,df, by="id_ind")

boxplot(data_cluster$cumul_grow ~ data_cluster$group)
abline(h=quantile(data_cluster$cumul_grow, c(0.9)), col="red")
abline(h=0, col="blue")

group     <- c(5,12,10,3,2)
col_group <- c("#BD0026", "#BD0026", "#BD0026", "#BD0026", "#BD0026")

col_group_max <- rgb(8,48,107, maxColorValue=255, alpha = 80) 
nb_group_max  <- 1

tot_group <- c(5,12,10,3,2,1)
tot_col_group <- c("#BD0026", "#BD0026", "#BD0026", "#BD0026", "#BD0026","#08306B")

date_init <- c("2006-07-01")
date_end  <- c("2006-06-01")

## MODIF TO DO 

select_data <- filter(full_data, date < date_init[1])
select_data <- filter(select_data, date >= date_end[1])

init_date <- range(select_data$date)[1]
end_date  <- range(select_data$date)[2]

layout(mat)
data("coastlineWorldFine")
plot(coastlineWorldFine, clongitude=-63.5, clatitude=48.5, span=1000, xlab="", ylab="", axes=FALSE, xaxt="n")
axis(2, at= seq(44,52,2),  las=1, labels=c("44°N", "46°N","48°N", "50°N", "52°N"), mgp=c(1.5,0.5,0), cex.axis=1.2)   
axis(1, at=seq(-75,-50,5),  las=1, labels=c("75°W","70°W","65°W","60°W", "55°W", "50°W"), mgp=c(1.5,0.5,0), cex.axis=1.2)     
box()

j <- c(as.numeric(as.character(select_data$id_ind[select_data$group==nb_group_max[1] & select_data$date==init_date])))

for(k in j){
  lines(select_data$lon[select_data$id_ind == k],
        select_data$lat[select_data$id_ind == k ],
        col=col_group_max[1], lwd=0.3)
}

# AUTRES TRAITS DU MOIS
g <- 0

for(z in group){
  g <- g + 1 
  points(select_data$lon[select_data$date == init_date & select_data$group==z], 
         select_data$lat[select_data$date == init_date & select_data$group==z], 
         col = col_group[g], 
         pch = 1, cex = 0.2)
  text(-70, 51, labels = month, cex = 2, font = 2  )
  j <- c(as.numeric(as.character(select_data$id_ind[select_data$group==z & select_data$date==init_date])))
  
  for(k in j){
    lines(select_data$lon[select_data$id_ind == k],
          select_data$lat[select_data$id_ind == k ],
          col=col_group[g])
  }
}

par(mar = c(1,1,1,1))
length_tot <- length(tot_group) * 2 + 1
loc_rect <- length_tot - 3

plot(c(0,0.5), c(0,1), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='')
text(x=0.125, y=(length_tot/length_tot), labels = "GROUP", cex=1.6, font=2)

for (i in 1:length(tot_group)) {
  rect(0,(loc_rect/length_tot),0.125, (loc_rect/length_tot)+(1/length_tot), col=tot_col_group[i], border=NA)
  text(x=0.25, y=((loc_rect + 0.5)/length_tot), labels = tot_group[i], cex=1.6, font=2)
  loc_rect <- loc_rect - 2 
}

hist(data_cluster$cumul_grow, xlab="", ylab="",xaxt="n", yaxt="n", cex.axis=2, axes=FALSE, main="", ylim=c(0,6500))
axis(2, ylim=c(0,3500), las=1, labels=TRUE, mgp=c(1.5,0.5,0),cex.axis=1.5)     # Axis caracteristics : mgp = position of values and labels, cex.lab = labels size, cex.axis = values size
axis(1, xlim=c(-5.5, 19), cex.axis=1.5)
mtext("Cumulated mass (mg C)", side=1, line = 2.3, cex=1.1)
mtext("Frequency", side=2, line = 3.5, cex=1.1)
abline(v=quantile(data_cluster$cumul_grow, c(0.9)), col="red", lwd=2)
abline(v=0, col="black", lwd=2, lty=2)
box()

for(i in tot_group){
  points(x=median(data_cluster$cumul_grow[data_cluster$group==i]), y=6000, pch=20)
  text(x=median(data_cluster$cumul_grow[data_cluster$group==i]),y=6400, labels=i, cex=1.5, font=2)
  
}

plot(x=select_data$date[select_data$id_ind=="1"],
     y=tapply(select_data$mass[select_data$group==tot_group[1]],
              select_data$date[select_data$group==tot_group[1]], median), 
     type="l", col=tot_col_group[1],  xlab="", ylab="",xaxt="n", yaxt="n", cex.axis=2, axes=FALSE,  ylim=c(0,30), lwd = 2)

for(i in 2:length(tot_group)){
  lines(x=select_data$date[select_data$id_ind=="1"],
        y=tapply(select_data$mass[select_data$group==tot_group[i]],
                 select_data$date[select_data$group==tot_group[i]], median), 
        type="l", col=tot_col_group[i], lwd = 2)
}
axis(2, las=1, labels=TRUE, mgp=c(1.5,0.5,0),cex.axis=1.5)     # Axis caracteristics : mgp = position of values and labels, cex.lab = labels size, cex.axis = values size
axis.Date(1, at = seq(select_data$date[1], select_data$date[length(select_data$date)], by = "day"), 
          format = "%b%d", labels=TRUE, cex.axis=1.5)
mtext("Mass (mg C)", cex = 1.1, side=2, line=3)
mtext("Time (month)", cex = 1.1, side=1, line=2.3)
box()

abline(h=15, lwd = 2, lty=2)

setwd("/Users/deborah/Documents/Doctorat/3D MODEL/GRAPH_PAPER_2/")
dev.print(tiff,filename='FIG_12_CHAP_2.tiff', width=24, height=16,units="cm",res = 400)

############################
#
# JULY
#
###########################

## ADD DATA 
setwd("/Users/deborah/Documents/Doctorat/3D MODEL/OUTPUT/")
load("data_select_tr_JUL.Rdata")
load("dclust_JUL-AUG_JUL_tr.Rdata")
month <- "JULY"

df <- data_cluster[,-2:-10]
names(df) <- c("id_ind", "group")
dp_daily_2$id_ind <- as.numeric(as.character(dp_daily_2$id_ind))
df$id_ind <- as.numeric(as.character(df$id_ind))
full_data <- full_join(dp_daily_2,df, by="id_ind")

boxplot(data_cluster$cumul_grow ~ data_cluster$group)
abline(h=quantile(data_cluster$cumul_grow, c(0.9)), col="red")
abline(h=0, col="blue")

group     <- c(5,3,2)
col_group <- c(rgb(189,0,38, maxColorValue=255),
               rgb(189,0,38, maxColorValue=255),
               rgb(189,0,38, maxColorValue=255))

col_group_max <- rgb(8,48,107, maxColorValue=255, alpha = 20) 
nb_group_max  <- 1

tot_group <- c(2,1)
tot_col_group <- c("#BD0026","#08306B")

date_init <- c("2006-08-01")
date_end  <- c("2006-07-01")

## MODIF TO DO 

select_data <- filter(full_data, date < date_init[1])
select_data <- filter(select_data, date >= date_end[1])

init_date <- range(select_data$date)[1]
end_date  <- range(select_data$date)[2]

# AUTRES TRAITS DU MOIS
layout(mat)
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

par(mar = c(1,1,1,1))
length_tot <- length(levels(select_data$group)) * 2 + 1
loc_rect <- length_tot - 3

plot(c(0,0.5), c(0,1), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='')
text(x=0.125, y=(length_tot/length_tot), labels = "GROUP", cex=1, font=2)

for (i in 1:length(levels(select_data$group))) {
  rect(0,(loc_rect/length_tot),0.125, (loc_rect/length_tot)+(1/length_tot), col=tot_col_group[i], border=NA)
  text(x=0.25, y=((loc_rect + 0.5)/length_tot), labels = tot_group[i], cex=1, font=2)
  loc_rect <- loc_rect - 2 
}

hist(data_cluster$cumul_grow, ylab="", xlab="", main="", xlim=c(-0.5, 3), ylim=c(0,6300))
mtext("Cumulated mass in mg C", side=1, line = 2, cex=0.8)
mtext("Frequency", side=2, line = 2, cex=1)
for(i in tot_group){
  points(x=median(data_cluster$cumul_grow[data_cluster$group==i]), y=6100, pch=20)
  text(x=median(data_cluster$cumul_grow[data_cluster$group==i]),y=6200, labels=i, cex=0.7)
  
}
abline(v=quantile(data_cluster$cumul_grow, c(0.9)), col="red", lwd=2)
box()


plot(x=select_data$date[select_data$id_ind=="1"],
     y=tapply(select_data$mass[select_data$group==group[1]],
              select_data$date[select_data$group==group[1]], median), 
     type="l", col=col_group[1],  ylab="", xlab= "",  ylim=c(0,40), lwd = 2)

for(i in 2:length(tot_group)){
  lines(x=select_data$date[select_data$id_ind=="1"],
        y=tapply(select_data$mass[select_data$group==tot_group[i]],
                 select_data$date[select_data$group==tot_group[i]], median), 
        type="l", col=tot_col_group[i], lwd = 2)
}
mtext("Mass in mg C", side=2, line = 2, cex=1)
mtext("Time", side=1, line = 2, cex=1)

abline(h=15, lwd = 2, lty=2)

setwd("/Users/deborah/Documents/Doctorat/3D MODEL/GRAPH_PAPER_2/")
dev.print(tiff,filename='TOT_CLUSTER_ADV_JUNE_TR.tiff', width=24, height=16,units="cm",res = 400)


############################
#
# AUGUST
#
###########################

## ADD DATA 
setwd("/Users/deborah/Documents/Doctorat/3D MODEL/OUTPUT/")
load("data_select_tr_AUG.Rdata")
load("dclust_AUG-SEP_AUG_tr.Rdata")
month <- "AUGUST"

df <- data_cluster[,-2:-10]
names(df) <- c("id_ind", "group")
dp_daily_2$id_ind <- as.numeric(as.character(dp_daily_2$id_ind))
df$id_ind <- as.numeric(as.character(df$id_ind))
full_data <- full_join(dp_daily_2,df, by="id_ind")

boxplot(data_cluster$cumul_grow ~ data_cluster$group)
abline(h=quantile(data_cluster$cumul_grow, c(0.9)), col="red")
abline(h=0, col="blue")

group     <- c(4,3,2)
col_group <- c(rgb(189,0,38, maxColorValue=255),
               rgb(189,0,38, maxColorValue=255),
               rgb(8,48,107, maxColorValue=255))

col_group_max <- rgb(8,48,107, maxColorValue=255, alpha = 80) 
nb_group_max  <- 1

tot_group <- c(4,3,2,1)
tot_col_group <- c("#BD0026", "#BD0026", "#08306B", "#08306B")

date_init <- c("2006-09-01")
date_end  <- c("2006-08-01")

## MODIF TO DO 

select_data <- filter(full_data, date < date_init[1])
select_data <- filter(select_data, date >= date_end[1])

init_date <- range(select_data$date)[1]
end_date  <- range(select_data$date)[2]

layout(mat)
data("coastlineWorldFine")
plot(coastlineWorldFine, clongitude=-63.5, clatitude=48.5, span=1000, xlab="", ylab="", axes=FALSE, xaxt="n")
axis(2, at= seq(44,52,2),  las=1, labels=c("44°N", "46°N","48°N", "50°N", "52°N"), mgp=c(1.5,0.5,0), cex.axis=1.2)   
axis(1, at=seq(-75,-50,5),  las=1, labels=c("75°W","70°W","65°W","60°W", "55°W", "50°W"), mgp=c(1.5,0.5,0), cex.axis=1.2)     
box()

j <- c(as.numeric(as.character(select_data$id_ind[select_data$group==nb_group_max[1] & select_data$date==init_date])))

for(k in j){
  lines(select_data$lon[select_data$id_ind == k],
        select_data$lat[select_data$id_ind == k ],
        col=col_group_max[1], lwd=0.3)
}

# AUTRES TRAITS DU MOIS
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

par(mar = c(1,1,1,1))
length_tot <- length(tot_group) * 2 + 1
loc_rect <- length_tot - 3

plot(c(0,0.5), c(0,1), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='')
text(x=0.125, y=(length_tot/length_tot), labels = "GROUP", cex=1.6, font=2)

for (i in 1:length(tot_group)) {
  rect(0,(loc_rect/length_tot),0.125, (loc_rect/length_tot)+(1/length_tot), col=tot_col_group[i], border=NA)
  text(x=0.25, y=((loc_rect + 0.5)/length_tot), labels = tot_group[i], cex=1.6, font=2)
  loc_rect <- loc_rect - 2 
}

hist(data_cluster$cumul_grow, xlab="", ylab="",xaxt="n", yaxt="n", cex.axis=2, axes=FALSE, main="", ylim=c(0,6500))
axis(2, ylim=c(0,3500), las=1, labels=TRUE, mgp=c(1.5,0.5,0),cex.axis=1.5)     # Axis caracteristics : mgp = position of values and labels, cex.lab = labels size, cex.axis = values size
axis(1, xlim=c(-5.5, 19), cex.axis=1.5)
mtext("Cumulated mass (mg C)", side=1, line = 2.3, cex=1.1)
mtext("Frequency", side=2, line = 3.5, cex=1.1)
abline(v=quantile(data_cluster$cumul_grow, c(0.9)), col="red", lwd=2)
abline(v=0, col="black", lwd=2, lty=2)
box()

for(i in tot_group){
  points(x=median(data_cluster$cumul_grow[data_cluster$group==i]), y=6000, pch=20)
  text(x=median(data_cluster$cumul_grow[data_cluster$group==i]),y=6400, labels=i, cex=1.5, font=2)
  
}

plot(x=select_data$date[select_data$id_ind=="1"],
     y=tapply(select_data$mass[select_data$group==tot_group[1]],
              select_data$date[select_data$group==tot_group[1]], median), 
     type="l", col=tot_col_group[1],  xlab="", ylab="",xaxt="n", yaxt="n", cex.axis=2, axes=FALSE,  ylim=c(0,30), lwd = 2)

for(i in 2:length(tot_group)){
  lines(x=select_data$date[select_data$id_ind=="1"],
        y=tapply(select_data$mass[select_data$group==tot_group[i]],
                 select_data$date[select_data$group==tot_group[i]], median), 
        type="l", col=tot_col_group[i], lwd = 2)
}
axis(2, las=1, labels=TRUE, mgp=c(1.5,0.5,0),cex.axis=1.5)     # Axis caracteristics : mgp = position of values and labels, cex.lab = labels size, cex.axis = values size
axis.Date(1, at = seq(select_data$date[1], select_data$date[length(select_data$date)], by = "day"), 
          format = "%b%d", labels=TRUE, cex.axis=1.5)
mtext("Mass (mg C)", cex = 1.1, side=2, line=3)
mtext("Time (month)", cex = 1.1, side=1, line=2.3)
box()

abline(h=15, lwd = 2, lty=2)

setwd("/Users/deborah/Documents/Doctorat/3D MODEL/GRAPH_PAPER_2/")
dev.print(tiff,filename='FIG_13_CHAP_2.tiff', width=24, height=16,units="cm",res = 400)
