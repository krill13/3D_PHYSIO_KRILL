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

setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/OUTPUT/")
load("data_select_mn_APR.Rdata")
load("dclust_APR-MAY_APR_mn.Rdata")
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
group     <- c(8,5,4,3,2)
col_group <- c("#BD0026","#BD0026","#BD0026","#BD0026","#08306B") 

col_group_max <- "#FD8D3C" 
nb_group_max  <- 1

tot_group     <- c(8,5,4,3,1,2)
tot_col_group <- c("#BD0026","#BD0026","#BD0026","#BD0026","#FD8D3C","#08306B")
  
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

hist(data_cluster$cumul_grow, xlab="", ylab="",xaxt="n", yaxt="n", cex.axis=2, axes=FALSE, main="", ylim=c(0,3500))
axis(2, ylim=c(0,3500), las=1, labels=TRUE, mgp=c(1.5,0.5,0),cex.axis=1.5)     # Axis caracteristics : mgp = position of values and labels, cex.lab = labels size, cex.axis = values size
axis(1, xlim=c(-5.5, 19), cex.axis=1.5)
mtext("Cumulated mass (mg C)", side=1, line = 2.3, cex=1.1)
mtext("Frequency", side=2, line = 3.5, cex=1.1)
abline(v=quantile(data_cluster$cumul_grow, c(0.9)), col="red", lwd=2)
abline(v=0, col="black", lwd=2, lty=2)
box()

for(i in tot_group){
  points(x=median(data_cluster$cumul_grow[data_cluster$group==i]), y=3100, pch=20)
  text(x=median(data_cluster$cumul_grow[data_cluster$group==i]),y=3400, labels=i, cex=1.5, font=2)
  
}

plot(x=select_data$date[select_data$id_ind=="1"],
     y=tapply(select_data$mass[select_data$group==tot_group[1]],
              select_data$date[select_data$group==tot_group[1]], median), 
     type="l", col=tot_col_group[1],  xlab="", ylab="",xaxt="n", yaxt="n", cex.axis=2, axes=FALSE,  ylim=c(40,260), lwd = 2)

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

abline(h=123, lwd = 2, lty=2)


setwd("/Users/deborah/Documents/Doctorat/3D MODEL/GRAPH_PAPER_2/")
dev.print(tiff,filename='FIG_8_CHAP_2.tiff', width=24, height=16,units="cm",res = 400)
dev.off()


############################
#
# MAY
#
###########################

## ADD DATA 
setwd("/Users/deborah/Documents/Doctorat/3D MODEL/OUTPUT/")
load("data_select_mn_MAY.Rdata")
load("dclust_MAY-JUN_MAY_mn.Rdata")
month <- "MAY"

df <- data_cluster[,-2:-10]
names(df) <- c("id_ind", "group")
dp_daily_2$id_ind <- as.numeric(as.character(dp_daily_2$id_ind))
df$id_ind <- as.numeric(as.character(df$id_ind))
full_data <- full_join(dp_daily_2,df, by="id_ind")

boxplot(data_cluster$cumul_grow ~ data_cluster$group)
abline(h=quantile(data_cluster$cumul_grow, c(0.9)), col="red")
abline(h=0, col="blue")

group <- c(7,6,9,5,8,4,1,2)
col_group <- c("#BD0026","#BD0026","#BD0026","#BD0026","#BD0026","#BD0026","#08306B","#08306B")

col_group_max <- "#FD8D3C" 
nb_group_max  <- 3

tot_group     <- c(7,6,9,5,8,4,3,1,2)
tot_col_group <- c("#BD0026", "#BD0026", "#BD0026", "#BD0026", "#BD0026", "#BD0026", "#FD8D3C", "#08306B","#08306B")

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

for (i in length(tot_group)) {
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

plot(x=select_data$date[select_data$id_ind=="10000"],
     y=tapply(select_data$mass[select_data$group==tot_group[1]],
              select_data$date[select_data$group==tot_group[1]], median), 
     type="l", col=tot_col_group[1],  xlab="", ylab="",xaxt="n", yaxt="n", cex.axis=2, axes=FALSE,  ylim=c(40,260), lwd = 2)

for(i in 2:length(tot_group)){
  lines(x=select_data$date[select_data$id_ind=="10000"],
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

abline(h=123, lwd = 2, lty=2)

setwd("/Users/deborah/Documents/Doctorat/3D MODEL/GRAPH_PAPER_2/")
dev.print(tiff,filename='TOT_CLUSTER_ADV_MAY_MN.tiff', width=24, height=16,units="cm",res = 400)
dev.off()


############################
#
# JUNE
#
###########################

## ADD DATA 
setwd("/Users/deborah/Documents/Doctorat/3D MODEL/OUTPUT/")
load("data_select_mn_JUN.Rdata")
load("dclust_JUN-JUL_JUN_mn.Rdata")
month <- "JUNE"

df <- data_cluster[,-2:-10]
names(df) <- c("id_ind", "group")
dp_daily_2$id_ind <- as.numeric(as.character(dp_daily_2$id_ind))
df$id_ind <- as.numeric(as.character(df$id_ind))
full_data <- full_join(dp_daily_2,df, by="id_ind")

boxplot(data_cluster$cumul_grow ~ data_cluster$group)
abline(h=quantile(data_cluster$cumul_grow, c(0.9)), col="red")
abline(h=0, col="blue")

group     <- c(6,12,9,10,3,4,2)
col_group <- c("#BD0026", "#BD0026", "#BD0026", "#BD0026", "#BD0026", "#BD0026", "#08306B")

col_group_max <- rgb(8,48,107, maxColorValue=255, alpha = 80) 
nb_group_max  <- 1

tot_group     <- c(6,12,9,10,3,4,1,2)
tot_col_group <- c("#BD0026", "#BD0026", "#BD0026", "#BD0026", "#BD0026", "#BD0026","#08306B","#08306B")

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

hist(data_cluster$cumul_grow, xlab="", ylab="",xaxt="n", yaxt="n", cex.axis=2, axes=FALSE, main="", ylim=c(0,5000))
axis(2, ylim=c(0,5000), las=1, labels=TRUE, mgp=c(1.5,0.5,0),cex.axis=1.5)     # Axis caracteristics : mgp = position of values and labels, cex.lab = labels size, cex.axis = values size
axis(1, xlim=c(-5.5, 19), cex.axis=1.5)
mtext("Cumulated mass (mg C)", side=1, line = 2.3, cex=1.1)
mtext("Frequency", side=2, line = 3.5, cex=1.1)
abline(v=quantile(data_cluster$cumul_grow, c(0.9)), col="red", lwd=2)
abline(v=0, col="black", lwd=2, lty=2)
box()

for(i in tot_group){
  points(x=median(data_cluster$cumul_grow[data_cluster$group==i]), y=4400, pch=20)
  text(x=median(data_cluster$cumul_grow[data_cluster$group==i]),y=4800, labels=i, cex=1.5, font=2)
  
}

plot(x=select_data$date[select_data$id_ind=="13153"],
     y=tapply(select_data$mass[select_data$group==tot_group[1]],
              select_data$date[select_data$group==tot_group[1]], median), 
     type="l", col=tot_col_group[1],  xlab="", ylab="",xaxt="n", yaxt="n", cex.axis=2, axes=FALSE,  ylim=c(40,260), lwd = 2)

for(i in 2:length(tot_group)){
  lines(x=select_data$date[select_data$id_ind=="13153"],
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

abline(h=123, lwd = 2, lty=2)


setwd("/Users/deborah/Documents/Doctorat/3D MODEL/GRAPH_PAPER_2/")
dev.print(tiff,filename='FIG_9_CHAP_2.tiff', width=24, height=16,units="cm",res = 400)
dev.off()

############################
#
# JULY
#
###########################

## ADD DATA 
setwd("/Users/deborah/Documents/Doctorat/3D MODEL/OUTPUT/")
load("data_select_mn_JUL.Rdata")
load("dclust_JUL-AUG_JUL_mn.Rdata")
month <- "JULY"

df <- data_cluster[,-2:-10]
names(df) <- c("id_ind", "group")
dp_daily_2$id_ind <- as.numeric(as.character(dp_daily_2$id_ind))
df$id_ind <- as.numeric(as.character(df$id_ind))
full_data <- full_join(dp_daily_2,df, by="id_ind")

boxplot(data_cluster$cumul_grow ~ data_cluster$group)
abline(h=quantile(data_cluster$cumul_grow, c(0.9)), col="red")
abline(h=0, col="blue")

group     <- c(7,11,6,4,3,2)
col_group <- c("#BD0026", "#BD0026", "#BD0026", "#BD0026", "#08306B","#08306B")

col_group_max <- rgb(8,48,107, maxColorValue=255, alpha = 80) 
nb_group_max  <- 1

tot_group     <- c(7,11,6,4,3,1,2)
tot_col_group <- c("#BD0026", "#BD0026", "#BD0026", "#BD0026","#08306B","#08306B","#08306B")

date_init <- c("2006-08-01")
date_end  <- c("2006-07-01")

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
arr_group     <-  c(3,2,7,11,6,4)
arr_col_group <- c("#08306B","#08306B","#BD0026", "#BD0026", "#BD0026", "#BD0026")

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

hist(data_cluster$cumul_grow, xlab="", ylab="",xaxt="n", yaxt="n", cex.axis=2, axes=FALSE, main="", ylim=c(0,4500))
axis(2, ylim=c(0,5000), las=1, labels=TRUE, mgp=c(1.5,0.5,0),cex.axis=1.5)     # Axis caracteristics : mgp = position of values and labels, cex.lab = labels size, cex.axis = values size
axis(1, xlim=c(-5.5, 19), cex.axis=1.5)
mtext("Cumulated mass (mg C)", side=1, line = 2.3, cex=1.1)
mtext("Frequency", side=2, line = 3.5, cex=1.1)
abline(v=quantile(data_cluster$cumul_grow, c(0.9)), col="red", lwd=2)
abline(v=0, col="black", lwd=2, lty=2)
box()

for(i in tot_group){
  points(x=median(data_cluster$cumul_grow[data_cluster$group==i]), y=4100, pch=20)
  text(x=median(data_cluster$cumul_grow[data_cluster$group==i]),y=4400, labels=i, cex=1.5, font=2)
  
}

plot(x=select_data$date[select_data$id_ind=="19744"],
     y=tapply(select_data$mass[select_data$group==tot_group[1]],
              select_data$date[select_data$group==tot_group[1]], median), 
     type="l", col=tot_col_group[1],  xlab="", ylab="",xaxt="n", yaxt="n", cex.axis=2, axes=FALSE,  ylim=c(40,260), lwd = 2)

for(i in 2:length(tot_group)){
  lines(x=select_data$date[select_data$id_ind=="19744"],
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

abline(h=123, lwd = 2, lty=2)


setwd("/Users/deborah/Documents/Doctorat/3D MODEL/GRAPH_PAPER_2/")
dev.print(tiff,filename='TOT_CLUSTER_ADV_JULY_MN.tiff', width=24, height=16,units="cm",res = 400)
dev.off()

############################
#
# AUGUST
#
###########################

## ADD DATA 
setwd("/Users/deborah/Documents/Doctorat/3D MODEL/OUTPUT/")
load("data_select_mn_AUG.Rdata")
load("dclust_AUG-SEP_AUG_mn.Rdata")
month <- "AUGUST"

df <- data_cluster[,-2:-10]
names(df) <- c("id_ind", "group")
dp_daily_2$id_ind <- as.numeric(as.character(dp_daily_2$id_ind))
df$id_ind <- as.numeric(as.character(df$id_ind))
full_data <- full_join(dp_daily_2,df, by="id_ind")

boxplot(data_cluster$cumul_grow ~ data_cluster$group)
abline(h=quantile(data_cluster$cumul_grow, c(0.9)), col="red")
abline(h=0, col="blue")

group     <- c(11,5,7,4,8,2,1)
col_group <- c(rgb(189,0,38, maxColorValue=255),
               rgb(189,0,38, maxColorValue=255),
               rgb(189,0,38, maxColorValue=255),
               rgb(189,0,38, maxColorValue=255),
               rgb(8,48,107, maxColorValue=255),
               rgb(8,48,107, maxColorValue=255),
               rgb(8,48,107, maxColorValue=255))

col_group_max <- rgb(8,48,107, maxColorValue=255, alpha = 80) 
nb_group_max  <- 3

tot_group <- c(11,5,7,4,8,3,2,1)
tot_col_group <- c("#BD0026", "#BD0026", "#BD0026", "#BD0026", "#08306B", "#08306B", "#08306B", "#08306B")

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
arr_group     <- c(8,2,1,11,5,7,4)
arr_col_group <- c("#08306B", "#08306B", "#08306B","#BD0026", "#BD0026", "#BD0026", "#BD0026")

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

hist(data_cluster$cumul_grow, xlab="", ylab="",xaxt="n", yaxt="n", cex.axis=2, axes=FALSE, main="", ylim=c(0,6300))
axis(2, ylim=c(0,5000), las=1, labels=TRUE, mgp=c(1.5,0.5,0),cex.axis=1.5)     # Axis caracteristics : mgp = position of values and labels, cex.lab = labels size, cex.axis = values size
axis(1, xlim=c(-5.5, 19), cex.axis=1.5)
mtext("Cumulated mass (mg C)", side=1, line = 2.3, cex=1.1)
mtext("Frequency", side=2, line = 3.5, cex=1.1)
abline(v=quantile(data_cluster$cumul_grow, c(0.9)), col="red", lwd=2)
abline(v=0, col="black", lwd=2, lty=2)
box()

for(i in tot_group){
  points(x=median(data_cluster$cumul_grow[data_cluster$group==i]), y=5700, pch=20)
  text(x=median(data_cluster$cumul_grow[data_cluster$group==i]),y=6100, labels=i, cex=1.5, font=2)
  
}

plot(x=select_data$date[select_data$id_ind=="28579"],
     y=tapply(select_data$mass[select_data$group==tot_group[1]],
              select_data$date[select_data$group==tot_group[1]], median), 
     type="l", col=tot_col_group[1],  xlab="", ylab="",xaxt="n", yaxt="n", cex.axis=2, axes=FALSE,  ylim=c(40,260), lwd = 2)

for(i in 2:length(tot_group)){
  lines(x=select_data$date[select_data$id_ind=="28579"],
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

abline(h=123, lwd = 2, lty=2)


setwd("/Users/deborah/Documents/Doctorat/3D MODEL/GRAPH_PAPER_2/")
dev.print(tiff,filename='FIG_10_CHAP_2.tiff', width=24, height=16,units="cm",res = 400)
dev.off()
