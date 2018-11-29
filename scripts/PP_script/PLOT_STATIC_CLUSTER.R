rm(list=ls())

pack <- c("tidyverse", "stringr", "ggplot2", "ocedata", "gstat", "sp", 
          "rgdal", "maptools", "rworldmap", "scales","matlab","mosaic", "gpclib","Hmisc")
lapply(pack, library, character.only = TRUE) 

species <- "MN"

#####################################
## PLOT CLUSTER 
#####################################
if(species=="MN"){
  setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/OUTPUT/")
  load("data_cluster_group_mn.Rdata")
  load("data_daily_mn.Rdata")
  
  #dp_daily$egg[is.na(dp_daily$egg)] <- 0
  #dp_daily <- mutate(dp_daily, egg_prop = (egg * 1.5 * 10^-3) / mass * 100)
  #dp_daily$egg_prop[is.na(dp_daily$egg_prop)] <- 0
  
  df_clust <- data_cluster[-2:-6]
  df_clust <- df_clust[-4:-5]
  
  df_full <- full_join(dp_daily, df_clust, by="id_ind") 
  
  select_group <- filter(df_full,group_12 =="1" | group_12 =="2" | group_12 =="3" | group_12 =="4" |
                           group_12 =="5" | group_12 =="6" | group_12 =="7" |
                           group_12 =="10" | group_12 == "11")
  
  median_group <- sort(median(select_group$cumul_grow ~ select_group$group_12))
  order_group <- c('10','1','2','3','4','7','5','6','11')
  
  select_group$group_12 <- factor(select_group$group_12, levels = order_group)
  
} else {
  setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/OUTPUT/")
  load("data_cluster_group_tr.Rdata")
  load("data_daily_tr.Rdata")
  
  #dp_daily <- mutate(dp_daily, egg_prop = (egg * 1.2 * 10^-3) / mass * 100)
  
  df_clust <- data_cluster[-2:-6]
  df_clust <- df_clust[-4:-5]
  
  df_full <- full_join(dp_daily, df_clust, by="id_ind") 
  
  select_group <- filter(df_full, group_15 =="1" | group_15 =="2" | group_15 =="3" | group_15 =="4" |
                           group_15 =="5" | group_15 =="6" | group_15 =="7" | group_15 =="8" |
                           group_15 =="9" | group_15 == "12")
  
  median_group <- sort(median(select_group$cumul_grow ~ select_group$group_15))
  order_group <- c('2','3','1','4','5','6','7','9','8', '12')
  
  select_group$group_15 <- factor(select_group$group_15, levels = order_group)
  
  
}

mat <- matrix(0, 45, 27)
mat[1:18 , 1:18 ] <- 1
mat[1:18 , 19:24] <- 2
mat[20:29 , 2:12] <- 3
mat[20:29 , 14:24] <- 4
mat[31:45, 2:26] <- 5


if(species=="MN"){
  color_cluster   <- c(rgb(140,107,177, maxColorValue=255),  #gr10 --   31  violet
                       rgb(107,174,214, maxColorValue=255),#gr1 --   945  bleu clair
                       rgb(8,48,107, maxColorValue=255),#gr2 --   4222  bleu foncé
                       rgb(116,196,118, maxColorValue=255),  #gr3  0   872   vert moyen
                       rgb(255,237,160, maxColorValue=255), #gr4  ++   114  jaune
                       rgb(254,217,118, maxColorValue=255),   #gr7 ++   98  jaune foncé
                       rgb(253,141,60, maxColorValue=255),  #gr5 ++   91  orange
                       rgb(227,26,28, maxColorValue=255),  #gr6 ++   67  rouge
                       rgb(189,0,38, maxColorValue=255))  #gr11 ++   18  rouge foncé
} else{
  color_cluster   <- c(rgb(107,174,214, maxColorValue=255),  #gr2   138  blue clair
                       rgb(8,48,107, maxColorValue=255), #gr3   4760   blue foncé
                       rgb(65,171,93, maxColorValue=255), #gr1   1005  vert moyen
                       rgb(255,237,160, maxColorValue=255), #gr4    157  jaune clair
                       rgb(254,217,118, maxColorValue=255),  #gr5   146   jaune foncé
                       rgb(253,141,60, maxColorValue=255),  #gr6   106   orange
                       rgb(252,78,42, maxColorValue=255),   #gr7   86     orange foncé
                       rgb(227,26,28, maxColorValue=255),  #gr9   81    rouge
                       rgb(189,0,38, maxColorValue=255), #gr8   28   rouge foncé
                       rgb(128,0,38, maxColorValue=255))  #gr12   19   rouge très foncé
}



par(mar = c(1,1,1,1))
data("coastlineWorldFine")
cex_text <- 0.8

layout(mat)
plot(coastlineWorldFine, clongitude=-63.5, clatitude=48.5, span=1000, xlab="", ylab="", axes=FALSE, xaxt="n")
box()
axis(2, at= seq(44,52,2),  las=1, labels=c("44°N", "46°N","48°N", "50°N", "52°N"), mgp=c(1.5,0.5,0), cex.axis=1.2)   
axis(1, at=seq(-75,-50,5),  las=1, labels=c("75°W","70°W","65°W","60°W", "55°W", "50°W"), mgp=c(1.5,0.5,0), cex.axis=1.2)     

k=0
for(i in order_group){
  k <- k+1
  points(data_cluster$lon[data_cluster$group_12==i], 
         data_cluster$lat[data_cluster$group_12==i], col = color_cluster[k], 
         pch = 19, cex = 0.4)}


par(mar = c(1,1,1,1))
length_tot <- length(levels(select_group$group_12)) * 2 + 1
loc_rect <- length_tot - 3

plot(c(0,0.5), c(0,1), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='')
text(x=0.125, y=(length_tot/length_tot), labels = "GROUP", cex=1.6, font=2)

for (i in 1:length(levels(select_group$group_12))) {
  rect(0,(loc_rect/length_tot),0.125, (loc_rect/length_tot)+(1/length_tot), col=color_cluster[i], border=NA)
  text(x=0.25, y=((loc_rect + 0.5)/length_tot), labels = order_group[i], cex=1.6, font=2)
  loc_rect <- loc_rect - 2 
}


par(mar = c(3.5,4,0,0))  
boxplot(select_group$cumul_grow~select_group$group_12, col=color_cluster, 
        xlab="", ylab="",xaxt="n", yaxt="n", cex.axis=2, axes=FALSE)
axis(2, las=1, labels=TRUE, mgp=c(1.5,0.5,0),cex.axis=1.5)     # Axis caracteristics : mgp = position of values and labels, cex.lab = labels size, cex.axis = values size
axis(1, at = seq(1,9,1), labels = c(10, 1, 2, 3, 4, 7, 5, 6, 11), cex.axis=1.5)
mtext("Cumul growth (mg C)", cex = 1.1, side=2, line=2.7)
mtext("Clustering group", cex = 1.1, side=1, line=2.7)
box()
abline(h=0,lty=2)

par(mar = c(3.5,4,0,0))  
boxplot(select_group$egg/100~select_group$group_12, col=color_cluster, 
        xlab="", ylab="",xaxt="n", yaxt="n", cex.axis=2, axes=FALSE, na.rm=TRUE)
axis(2, las=1, labels=TRUE, mgp=c(1.5,0.5,0),cex.axis=1.5)     # Axis caracteristics : mgp = position of values and labels, cex.lab = labels size, cex.axis = values size
axis(1, at = seq(1,9,1), labels = c(10, 1, 2, 3, 4, 7, 5, 6, 11), cex.axis=1.5)
mtext(expression(paste("Cumul egg (ind"^"-1",")")), cex = 1.1, side=2, line=2.7)
mtext("Clustering group", cex = 1.1, side=1, line=2.7)
box()


plot(x=select_group$date[select_group$id_ind=="1"],
     y=tapply(select_group$mass[select_group$group_12==order_group[1]],
              select_group$date[select_group$group_12==order_group[1]], median), 
     type="l", col=color_cluster[1],  ylab="", xlab= "", 
     xaxt="n", yaxt="n", cex.axis=2, axes=FALSE, lwd = 2, ylim=c(0,400))
axis(2, las=1, ylim=c(-10,20), labels=TRUE, mgp=c(1.5,0.5,0),cex.axis=1.5)     # Axis caracteristics : mgp = position of values and labels, cex.lab = labels size, cex.axis = values size
axis.Date(1, at = seq(select_group$date[1], select_group$date[1175356], by = "month"), 
          format = "%b", labels=TRUE, cex.axis=1.5)
mtext("Mass (mg C)", cex = 1.1, side=2, line=2.7)
mtext("Time (month)", cex = 1.1, side=1, line=2.3)
box()

for(i in 1:length(order_group)){
  lines(x=select_group$date[select_group$id_ind=="1"],
        y=tapply(select_group$mass[select_group$group_12==order_group[i]],
                 select_group$date[select_group$group_12==order_group[i]], median), 
        type="l", col=color_cluster[i], lwd = 2, ylab="Mass in mg C", xlab= "Time",  ylim=c(0,400))
  
}
abline(h=123, lty=2)

setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/GRAPH/FIGURE_PAPER_2/")
dev.print(tiff,filename='FIG_6_CHAP_2_TEST.tiff', width=22, height=25,units="cm",res = 400)
dev.off()  


##### T. raschii
layout(mat)

par(mar = c(1,1,1,1))
data("coastlineWorldFine")
cex_text <- 0.8

plot(coastlineWorldFine, clongitude=-63.5, clatitude=48.5, span=1000, xlab="", ylab="", axes=FALSE, xaxt="n")
box()
axis(2, at= seq(44,52,2),  las=1, labels=c("44°N", "46°N","48°N", "50°N", "52°N"), mgp=c(1.5,0.5,0), cex.axis=1.2)   
axis(1, at=seq(-75,-50,5),  las=1, labels=c("75°W","70°W","65°W","60°W", "55°W", "50°W"), mgp=c(1.5,0.5,0), cex.axis=1.2)     

k=0
for(i in order_group){
  k <- k+1
  points(data_cluster$lon[data_cluster$group_15==i], 
         data_cluster$lat[data_cluster$group_15==i], col = color_cluster[k], 
         pch = 19, cex = 0.4)}


par(mar = c(1,1,1,1))
length_tot <- length(levels(select_group$group_15)) * 2 + 1
loc_rect <- length_tot - 3

plot(c(0,0.5), c(0,1), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='')
text(x=0.125, y=(length_tot/length_tot), labels = "GROUP", cex=1.6, font=2)

for (i in 1:length(levels(select_group$group_15))) {
  rect(0,(loc_rect/length_tot),0.125, (loc_rect/length_tot)+(1/length_tot), col=color_cluster[i], border=NA)
  text(x=0.25, y=((loc_rect + 0.5)/length_tot), labels = order_group[i], cex=1.6, font=2)
  loc_rect <- loc_rect - 2 
}

par(mar = c(3.5,4,0,0))  
boxplot(select_group$cumul_grow~select_group$group_15, col=color_cluster, 
        xlab="", ylab="",xaxt="n", yaxt="n", cex.axis=2, axes=FALSE)
axis(2, las=1, labels=TRUE, mgp=c(1.5,0.5,0),cex.axis=1.5)     # Axis caracteristics : mgp = position of values and labels, cex.lab = labels size, cex.axis = values size
axis(1, at = seq(1,10,1), labels = c(2, 3, 1, 4, 5, 6, 7, 9, 8, 12), cex.axis=1.5)
mtext("Cumul growth (mg C)", cex = 1.1, side=2, line=2.7)
mtext("Clustering group", cex = 1.1, side=1, line=2.7)
box()
abline(h=0,lty=2)

par(mar = c(3.5,4,0,0))  
boxplot(select_group$egg/100~select_group$group_15, col=color_cluster, 
        xlab="", ylab="",xaxt="n", yaxt="n", cex.axis=2, axes=FALSE, na.rm=TRUE)
axis(2, las=1, labels=TRUE, mgp=c(1.5,0.5,0),cex.axis=1.5)     # Axis caracteristics : mgp = position of values and labels, cex.lab = labels size, cex.axis = values size
axis(1, at = seq(1,10,1), labels = c(2, 3, 1, 4, 5, 6, 7, 9, 8, 12), cex.axis=1.5)
mtext(expression(paste("Cumul egg (ind"^"-1",")")), cex = 1.1, side=2, line=2.7)
mtext("Clustering group", cex = 1.1, side=1, line=2.7)
box()

plot(x=select_group$date[select_group$id_ind=="1"],
     y=tapply(select_group$mass[select_group$group_15==order_group[1]],
              select_group$date[select_group$group_15==order_group[1]], median), 
     type="l", col=color_cluster[1],  ylab="", xlab= "",  ylim=c(0,65), 
     xaxt="n", yaxt="n", cex.axis=2, axes=FALSE, lwd = 2)
axis(2, las=1, labels=TRUE, mgp=c(1.5,0.5,0),cex.axis=1.5)     # Axis caracteristics : mgp = position of values and labels, cex.lab = labels size, cex.axis = values size
axis.Date(1, at = seq(select_group$date[1], select_group$date[1175356], by = "month"), 
          format = "%b", labels=TRUE, cex.axis=1.5)
mtext("Mass (mg C)", cex = 1.1, side=2, line=2.7)
mtext("Time (month)", cex = 1.1, side=1, line=2.3)
box()

for(i in 2:length(order_group)){
  lines(x=select_group$date[select_group$id_ind=="1"],
        y=tapply(select_group$mass[select_group$group_15==order_group[i]],
                 select_group$date[select_group$group_15==order_group[i]], median), 
        type="l", col=color_cluster[i], lwd = 2, ylab="Mass in mg C", xlab= "Time",  ylim=c(0,100))}

abline(h=15, lty=2)

setwd("/Users/deborah/Documents/Doctorat/3D MODEL/GRAPH_PAPER_2/")
dev.print(tiff,filename='FIG_7_CHAP_2.tiff', width=22, height=25,units="cm",res = 400)
dev.off()

test2 <- rbind((pos_month_mn$sum * 100) / 6576, (pos_month_tr$sum * 100) / 6576)

barplot(test2, beside=T, ylim = c(0,100), col=c("blue", "pink"), ylab="Prop. of GSL with positive growth")
legend(15,100, legend = c("M. norvegica", "T.raschii"), col = c("blue", "pink"))
box()
