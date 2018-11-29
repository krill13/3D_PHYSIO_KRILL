rm(list=ls())

pack <- c("tidyverse", "stringr", "ggplot2", "ocedata", "gstat", "sp", 
          "rgdal", "maptools", "rworldmap", "scales","matlab","mosaic", "gpclib","Hmisc")
lapply(pack, library, character.only = TRUE) 


# Choose species 
species <- "MN"

# Load dataframe
if (species == "MN"){
  setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/OUTPUT/")
  load("month_sum_mn.Rdata")
  month_sum_mn <- month_sum
  
  grow_factor <- c("(-0.5,-0.2]","(-0.2,-0.1]", "(-0.1,0]", "(0,0.1]", "(0.1,0.2]", 
                   "(0.2,0.3]", "(0.3,0.4]","(0.4,2]") 
  
  label_color_bar <- c("inf.","-0.2","-0.1", "0", "0.1", "0.2", "0.3", "0.4", "sup.")
  
  
  color_grow  <- c(rgb(64,0,75, maxColorValue=255), rgb(118,42,131, maxColorValue=255),rgb(194,165,207, maxColorValue=255),
                   rgb(229,245,224, maxColorValue=255),rgb(166,219,160, maxColorValue=255), 
                   rgb(90,174,97, maxColorValue=255),
                   rgb(27,120,55, maxColorValue=255), rgb(0,68,27, maxColorValue=255), rgb(0,40,28, maxColorValue=255))
  
  egg_factor  <- c("(1, 1000]", "(1000, 2000]", "(2000, 3000]", "(3000, 4000]", 
                   "(4000, 5000]", "(> 5000]") 
  
  color_egg   <- c(rgb(252,197,192, maxColorValue=255),rgb(250,159,181, maxColorValue=255),
                   rgb(247,104,161, maxColorValue=255),rgb(221,52,151, maxColorValue=255),
                   rgb(174,1,126, maxColorValue=255),  rgb(122,1,119, maxColorValue=255))
  
  label_color_bar_egg <- c("0","1000","2000", "3000", "4000", "5000", "sup.")
  
  
} else{
  setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/OUTPUT/")
  load("month_sum_tr.Rdata")
  
  month_sum_tr <- month_sum
  
  grow_factor <- c("(-0.5,-0.05]","(-0.05,-0.025]", "(-0.025,-0.0125]", "(-0.0125,0]", "(0,0.0125]", 
                   "(0.0125,0.025]", "(0.025,0.05]","(0.05,0.075]", "(0.075,0.5]") 
  
  label_color_bar <- c("inf.","-0.05","-0.025", "-0.0125", "0", "0.0125", "0.025", "0.05","sup.")
  
  color_grow  <- c(rgb(45,0,75, maxColorValue=255), rgb(64,0,75, maxColorValue=255), rgb(118,42,131, maxColorValue=255),
                   rgb(194,165,207, maxColorValue=255), rgb(166,219,160, maxColorValue=255), rgb(90,174,97, maxColorValue=255),
                   rgb(27,120,55, maxColorValue=255), rgb(0,68,27, maxColorValue=255))
    
    #c("darkmagenta","magenta","darkblue","blue","palegreen","darkgreen","orange", "red2", "red4")
  
  
  egg_factor  <- c("(1, 100]","(100, 200]", "(200, 300]", "(300, 400]", "(400, 500]", 
                   "(> 500]") 
  
  label_color_bar_egg <- c("0","100","200", "300", "400", "500", "sup.")
  
  color_egg   <- c(rgb(252,197,192, maxColorValue=255),rgb(250,159,181, maxColorValue=255),
                   rgb(247,104,161, maxColorValue=255),rgb(221,52,151, maxColorValue=255),
                   rgb(174,1,126, maxColorValue=255),  rgb(122,1,119, maxColorValue=255))
}

# Custom color bar function
colorBar <- function(colRamp, xmin, xmax, ymin, ymax, labelBar, nb_group) {
  plot(c(xmin,xmax), c(ymin,ymax), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='')
  for (i in 1:(length(colRamp))) {
    y = i / nb_group 
    y_1 = xmax
    j = labelBar[i]
    rect(y,0,y+0.1,0.35, col=colRamp[i], border=NA)
    text(x=y, y=0.45, labels = j, cex=0.8, font=2)
  }
  y = i / 10 + 0.1
  j = labelBar[i+1]
  text(x=y, y=0.45, labels = j, cex=0.8, font=2)
}

# Layout
mat <- matrix(0, 28, 36)
mat[1:12 , 1:12 ] <- 1
mat[1:12 , 13:24] <- 2
mat[1:12 , 25:36] <- 3
mat[13:24, 1:12 ] <- 4
mat[13:24, 13:24] <- 5
mat[13:24, 25:36] <- 6
mat[25:28, 9:28 ] <- 7

data("coastlineWorldFine")
month <- c("April", "May", "June", "July", "August", "September") 

#layout(matrix(c(1,2,3,4,5,6,7,7,7),3,3, byrow = TRUE), width=c(3,3,3), height=c(3,3,2))
#par(oma=c(0,0,0,0))
layout(mat)
par(mar = c(0,0,0,0))

l=0
for(j in 4:9) {
  if(species == "MN") { month_sum <- month_sum_mn
  } else {
    month_sum <- month_data_tr}
  
  l=l+1
  k=0
  plot(coastlineWorldFine, clongitude=-63.5, clatitude=48.5, span=1250, xlab="", ylab="", axes=FALSE, xaxt="n")
  box()
  axis(2, at= seq(44,52,2),  las=1, labels=c("44°N", "46°N","48°N", "50°N", "52°N"), mgp=c(1.5,0.5,0), cex.axis=1.2)   
  axis(1, at=seq(-75,-50,5),  las=1, labels=c("75°W","70°W","65°W","60°W", "55°W", "50°W"), mgp=c(1.5,0.5,0), cex.axis=1.2)     
  text(x=-67.7, y=51.8,   labels = month[l], cex=1.2, font = 2)
  #text(x=-57.5, y=48.5,   labels = "NF", cex=0.5, font = 2)
  #text(x=-59.5, y=51.5,   labels = "Strait of Belle-Isle", cex=0.5, font = 2)
  #text(x=-64, y=50.6,   labels = "Jacques-Cartier Strait", cex=0.5, font = 2)
  #text(x=-66, y=48.8,   labels = "Gaspe", cex=0.5, font = 2)
  #text(x=-68.1, y=49.7, labels = "Lower Estuary (LSLE)", cex=0.5, font = 2, srt=38)
  #text(x=-63, y=49.5,   labels = "Anticosti", cex=0.5, font = 2, srt=335)
  #map.scale(x=-59.5, y=45.5, ratio=TRUE, relwidth=0.4, metric = TRUE, cex=0.5)
  for(i in grow_factor) {
    k = k+1
    points(month_sum$lon[month_sum$group_way==i & month_sum$month==j], 
           month_sum$lat[month_sum$group_way==i & month_sum$month==j], col = color_grow[k], 
           pch = 19, cex = 0.2)}}

if (species == "MN"){

    colorBar(color_grow, 0,1,0, 0.5, label_color_bar, 10)
  
  setwd("/Users/deborah/Documents/Doctorat/3D MODEL/GRAPH_PAPER_2/")
  dev.print(tiff,filename='GROW_POT_MN.tiff', width=20, height=13,units="cm",res = 400)
  dev.off()
  
} else {

  colorBar(color_grow, 0,1,0, 0.5,  label_color_bar, 10)
  
  setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/GRAPH/FIGURE_COMPL/")
  dev.print(tiff,filename='GROW_POT_100_TR.tiff', width=20, height=13,units="cm",res = 400)
  dev.off()
}



data("coastlineWorldFine")
month <- c("April", "May", "June", "July", "August", "September") 

l=0
layout(mat)
par(mar = c(0,0,0,0))
#layout(matrix(c(1,2,3,4,5,6,7,7,7),3,3, byrow = TRUE), width=c(3,3,3), height=c(3,3,2))
#par(oma=c(0,0,0,0))


for(j in 4:9) {
  if(species == "MN") { month_sum <- month_sum_mn
  } else {
    month_sum <- month_data_tr}
  l=l+1
  k=0
  plot(coastlineWorldFine, clongitude=-63.5, clatitude=48.5, span=1300, xlab="", ylab="", axes=FALSE, xaxt="n")
  box()
  axis(2, at= seq(44,52,2),  las=1, labels=c("44°N", "46°N","48°N", "50°N", "52°N"), mgp=c(1.5,0.5,0), cex.axis=1.2)   
  axis(1, at=seq(-75,-50,5),  las=1, labels=c("75°W","70°W","65°W","60°W", "55°W", "50°W"), mgp=c(1.5,0.5,0), cex.axis=1.2)     
  text(x=-67.7, y=51.8,   labels = month[l], cex=1.2, font = 2)
  #text(x=-57.5, y=48.5,   labels = "NF", cex=0.5, font = 2)
  #text(x=-59.5, y=51.5,   labels = "Strait of Belle-Isle", cex=0.5, font = 2)
  #text(x=-64, y=50.6,   labels = "Jacques-Cartier Strait", cex=0.5, font = 2)
  #text(x=-66, y=48.8,   labels = "Gaspe", cex=0.5, font = 2)
  #text(x=-68.1, y=49.7,     labels = "Lower Estuary (LSLE)", cex=0.5, font = 2, srt=38)
  #text(x=-63, y=49.5,   labels = "Anticosti", cex=0.5, font = 2, srt=335)
  #map.scale(x=-59.5, y=45.5, ratio=TRUE, relwidth=0.4, metric = TRUE, cex=0.5)
  for(i in egg_factor) {
    k = k+1
    points(month_sum$lon[month_sum$group_egg==i & month_sum$month==j], 
           month_sum$lat[month_sum$group_egg==i & month_sum$month==j], col = color_egg[k], 
           pch = 19, cex = 0.4)}}

if (species == "MN"){
  
  colorBar(color_egg, 0,1,0, 0.5,  label_color_bar_egg, 10)
  
  setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/GRAPH/FIGURE_COMPL/")
  dev.print(tiff,filename='EGG_POT_MN.tiff', width=20, height=13,units="cm",res = 400)
  dev.off()
  
} else {
  
  colorBar(color_egg, 0,1,0, 0.5,  label_color_bar_egg, 10)
  
  setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/GRAPH/FIGURE_COMPL/")
  dev.print(tiff,filename='EGG_POT_100_TR.tiff', width=20, height=13,units="cm",res = 400)
  dev.off()
}

################################################################################################################
##### PLOT EGG
################################################################################################################
if(species=="MN"){
  setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/OUTPUT/")
  load("tot_spawn_mn.Rdata")
  load("month_sum_mn.Rdata")
  
  tot_spawn_mn  <- tot_spawn
  month_data_mn <- group_by(month_sum, month)
  month_data_mn <- mutate(month_data_mn, species = "MN")
  
  mean_month_mn <- summarise( month_data_mn, mean=mean(egg, na.rm=TRUE), SE=sd(egg,na.rm=TRUE)/sqrt(length(egg)) )
  pos_month_mn  <- summarise(month_data_mn, sum = length(id_ind[grow_way > 0]))
  pos_month_mn  <- mutate(pos_month_mn, species = "MN")
    
  month_sum$egg[is.na(month_sum$egg)] <- 0
  
  
} else {
  setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/OUTPUT/")
  load("tot_spawn_tr.Rdata")
  load("month_sum_tr.Rdata")
  
  tot_spawn_tr <- tot_spawn
  month_data_tr <- group_by(month_sum, month)
  month_data_tr <- mutate(month_data_tr, species = "TR")
  
  mean_month_tr <- summarise( month_data_tr, mean=mean(egg, na.rm=TRUE), SE=sd(egg,na.rm=TRUE)/sqrt(length(egg)) )
  pos_month_tr  <- summarise(month_data_tr, sum = length(id_ind[grow_way > 0]))
  pos_month_tr  <- mutate(pos_month_tr, species = "TR")
  
  month_sum$egg[is.na(month_sum$egg)] <- 0
}


j = 0
if(species=="MN"){
  month = c("May", "June", "July", "August", "September") 
  
  nb_ind <- matrix(NA, ncol= 2, nrow = 5)
  j=0
  
  for(i in 5:9){
    j=j+1
    select_ind <- filter(month_sum_mn, month == i  & egg > 0)
    nb_ind[j,1] <- i
    nb_ind[j,2] <- length(select_ind$id_ind)
  }  
  
  nb_ind <- as.data.frame(nb_ind)
  names(nb_ind) <- c("month", "nb_ind")
  mean_month_mn <- left_join(mean_month_mn, nb_ind, by = "month")
} else {
  month = c("April", "May", "June", "July", "August", "September") 
  
  nb_ind <- matrix(NA, ncol= 2, nrow = 6)
  j=0
  
  for(i in 4:9){
    j=j+1
    select_ind <- filter(month_sum_tr, month == i  & egg > 0)
    nb_ind[j,1] <- i
    nb_ind[j,2] <- length(select_ind$id_ind)
  }  
  
  nb_ind <- as.data.frame(nb_ind)
  names(nb_ind) <- c("month", "nb_ind")
  mean_month_tr <- left_join(mean_month_tr, nb_ind, by = "month")
}

if(species == "MN") {
  
  for(i in 5 : 9){
    j = j + 1 
    hist(month_data$egg[month_data$month==i], main = month[j], ylim=c(0,2000),xlab = "Number of egg release", breaks = 10)}
  
setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/GRAPH/FIGURE_COMPL/")
dev.print(tiff,filename='HIST_EGG_MN.tiff', width=18, height=18,units="cm",res = 400)
dev.off()
} else{
  
  for(i in 4 : 9){
    j = j + 1 
    hist(month_data$egg[month_data$month==i], main = month[j], ylim=c(0,2500),xlab = "Number of egg release", breaks = 10)}
  
  setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/GRAPH/FIGURE_COMPL/")
  dev.print(tiff,filename='HIST_EGG_100_TR.tiff', width=18, height=18,units="cm",res = 400)
  dev.off()
}


mat <- matrix(0, 16, 24)
mat[1:8 , 1:12 ] <- 1
mat[1:8 , 13:24] <- 2
mat[9:16 , 1:12 ] <- 3
mat[9:16 , 13:24] <- 4

layout(mat)
par(mar=c(4.1,5.1,2.1,5.1))

egg_plot <- barplot((tot_spawn_mn * 100) / 6576, width = 0.85, 
                    xlab="Spawning events", las=1,
                    ylim=c(0, 50), main="Meganyctiphanes norvegica", font.main=3,cex.main=1.5,  
                    ylab = "% of individual", cex.lab = 1.4, font.lab=1, col="#CC99CC")
box()

egg_plot <- barplot((tot_spawn_tr * 100) / 6576, width = 0.85, 
                    xlab="Spawning events", las=1,
                    ylim=c(0, 50), main="Thysanoessa raschii", font.main=3,cex.main=1.5, 
                    ylab = "% of individual", cex.lab = 1.4, font.lab=1, col = "#99CC99")
box()

#lab <- as.character(tot_spawn)
## Add text at top of bars
#text(x = egg_plot, y = (tot_spawn * 100) / 6576, label = lab, pos = 3, col = "black")
par(mar=c(4.1,5.1,2.1,5.1))

bp <- barplot((mean_month_mn$nb_ind*100)/6576, col = rgb(189,189,189, maxColorValue=255, alpha=200), border = "NA", ylab = "% of spawning individuals", 
            ylim = c(0,50), font.lab = 2, xlim=c(0.2,12), cex.lab=1.1)

barplot((mean_month_mn$nb_ind*100)/6576, col = "#CC99CC", border = "NA", ylab = "% of spawning individuals", xlab="Month",
        ylim = c(0,70), las = 1, font.lab = 1, cex.lab = 1.4)

lines(bp,mean_month_mn$mean/100, pch=1,col="black", xaxt = "n", xlab="", ylab="", cex =1)
points(bp,mean_month_mn$mean/100, pch=1,col="black", xaxt = "n", xlab="", ylab="", cex =1)
errbar(bp ,mean_month_mn$mean/100, (mean_month_mn$mean+mean_month_mn$SE)/100, (mean_month_mn$mean-mean_month_mn$SE)/100, add= TRUE,pch=19, col="black", errbar.col="black", lwd=2, cex=1)

axis(4,at=c(0,10,20,30,40,50,60), labels=c("0","1000","2000","3000","4000","5000", "6000"), cex.lab=1.1, font.lab=2, las=1)
mtext("Mean number of eggs released", side = 4, line = 3, font = 1, cex = 0.9)
axis(1, at=bp, labels=c("Apr.", "May", "Jun.", "Jul.", "Aug.", "Sep."), cex = 1, font = 1)
box()


#lab <- as.character(tot_spawn)
## Add text at top of bars
#text(x = egg_plot, y = (tot_spawn * 100) / 6576, label = lab, pos = 3, col = "black")
par(mar=c(4.1,5.1,2.1,5.1))

#bp <- barplot((mean_month$nb_ind*100)/6576, col = rgb(252,197,192, maxColorValue=255, alpha=200), border = "NA", ylab = "% of individual spawning egg", 
#              ylim = c(0,50), font.lab = 2, xlim=c(0.2,12), cex.lab=1.1)

barplot((mean_month_tr$nb_ind*100)/6576, col = "#99CC99", border = "NA", ylab = "% of individual spawning egg", xlab="Month",
        ylim = c(0,70), font.lab = 1, cex.lab = 1.4, las=1)

lines(bp,mean_month_tr$mean/10, pch=1,col="black", xaxt = "n", xlab="", ylab="", cex =1)
points(bp,mean_month_tr$mean/10, pch=1,col="black", xaxt = "n", xlab="", ylab="", cex =1)
errbar(bp ,mean_month_tr$mean/10, (mean_month_tr$mean+mean_month_tr$SE)/10, (mean_month_tr$mean-mean_month_tr$SE)/10, add= TRUE,pch=19, col="black", errbar.col="black", lwd=2, cex=1)

axis(4,at=c(0,10,20,30,40,50,60), labels=c("0","100","200","300","400", "500", "600"), cex.lab=1.1, font.lab=2, las =1)
mtext("Mean number of eggs released", side = 4, line = 3, font = 1, cex = 0.9)
axis(1, at=bp, labels=c("Apr.", "May", "Jun.", "Jul.", "Aug.", "Sep."), cex = 1, font = 1)
box()

setwd("/Users/deborah/Documents/Doctorat/3D MODEL/GRAPH_PAPER_2/")
dev.print(tiff,filename='FIG_5_CHAP_2.tiff', width=18, height=18,units="cm",res = 400)
dev.off()



vioplot_df <- rbind(month_data_mn,month_data_tr)
vioplot_df$species <- as.factor(vioplot_df$species)
vioplot_df$month <- as.factor(vioplot_df$month)

pos_month_df <- rbind(pos_month_mn,pos_month_tr)
pos_month_df$species <- as.factor(pos_month_df$species)
pos_month_df$month <- as.factor(pos_month_df$month)


#violinplot <- 
  
violinplot<- ggplot(vioplot_df, aes(x=factor(month), y=grow_way/mass *100)) +
  scale_y_continuous(limits = c(-0.3, 0.3)) +
  #theme(panel.border=element_rect(colour="black",size=1)) +
  theme(legend.position="none")+
  geom_hline(yintercept = 0, colour="red", linetype="dashed") +
  xlab("Month") +
  ylab("Growth potential (% of the body mass)") +
  scale_fill_manual("Species", values = c("#CC99CC","#99CC99"), labels = c("M. norvegica", "T. raschii")) +
  scale_x_discrete(breaks=c("4","5","6","7","8","9"),labels=c("APR", "MAY", "JUN","JUL","AUG","SEP"))+
  geom_violin(aes(fill=species))

pos_month_plot <- ggplot(pos_month_df, aes(x=factor(month),y=sum/length(levels(month_data_mn$id_ind)) *100, fill=species)) +
  geom_bar(stat="identity",position=position_dodge())+
  theme(legend.position="none")+
  xlab("Month") +
  ylab("Proportion of gulf area with positive growth potential") +
  scale_fill_manual("Species", values = c("#CC99CC","#99CC99"), labels = c("M. norvegica", "T. raschii"))+ 
  theme(legend.text = element_text(face = "italic"))+
  scale_x_discrete(labels=c("APR", "MAY", "JUN","JUL","AUG","SEP"))
  
mat <- matrix(0, 15, 20)
mat[3:12 , 1:10 ] <- 1
mat[3:12 , 11:20] <- 2
mat[13:15 , 10:14] <- 3

layout(mat)
layout.show(3)

library(cowplot)
legend <- get_legend(pos_month_plot)
p <- ggdraw(legend)

setwd("/Users/deborah/Documents/Doctorat/3D MODEL/SCRIPTS/")
source("multiplot_ggplot_function.R")

multiplot(violinplot,pos_month_plot, p, layout=mat)


