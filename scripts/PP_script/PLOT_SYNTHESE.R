rm(list=ls())


pack <- c("tidyverse", "stringr", "ggplot2", "ocedata", "gstat", "sp", 
          "rgdal", "maptools", "rworldmap", "scales","matlab","mosaic", "gpclib","ggmap", "maps")
lapply(pack, library, character.only = TRUE) 

par(oma=c(1, 1, 1, 1))
par(mar=c(1, 1, 1, 1))

color_month <- c("#ff7f00","#4daf4a48","#e41a1c64","#377eb880","#984ea396")



##################################################
##  Observed Data
#################################################

setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/INPUT/")
agg_df <- read.csv("SPERA_Plankton_5dB.csv")
# Assumes a dB to biomass conversion factor of -70 dB g^-1 (McQuinn et al. 2013)
# ( 1 dB = 10*log10(x) )
B = agg_df$Sa / 10^(-70/10) # g m^-2

# Ensuite tu choisis seulement les valeurs qui dépassent un seuil; j’avais défini dans mon papier FTLE de 2015 100 g m^-2 :
LonX = agg_df$Lon[ B>=100 ]
LatX = agg_df$Lat[ B>=100 ]

############################
# APRIL
###########################
setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/OUTPUT/")
load("data_select_mn_APR.Rdata")
load("dclust_APR-MAY_APR_mn.Rdata")
df <- data_cluster[,-2:-10]
names(df) <- c("id_ind", "group")

dp_daily_2$id_ind <- as.numeric(as.character(dp_daily_2$id_ind))
df$id_ind <- as.numeric(as.character(df$id_ind))
full_data_apr <- full_join(dp_daily_2,df, by="id_ind")
date <- dp_daily_2$date[1:182]

group_apr     <- c(8,5,4,3)

date_init <- c("2006-05-01")
date_end  <- c("2006-04-01")

select_data_apr <- filter(full_data_apr, date < date_init[1])
select_data_apr <- filter(select_data_apr, date >= date_end[1])

init_date_apr <- range(select_data_apr$date)[1]
end_date_apr  <- range(select_data_apr$date)[2]

############################
# MAY
###########################
setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/OUTPUT/")
load("data_select_mn_MAY.Rdata")
load("dclust_MAY-JUN_MAY_mn.Rdata")
month <- "MAY"

df <- data_cluster[,-2:-10]
names(df) <- c("id_ind", "group")
dp_daily_2$id_ind <- as.numeric(as.character(dp_daily_2$id_ind))
df$id_ind <- as.numeric(as.character(df$id_ind))
full_data_may <- full_join(dp_daily_2,df, by="id_ind")

group_may <- c(7,6,9,5,8,4)

date_init <- c("2006-06-01")
date_end  <- c("2006-05-01")

select_data_may <- filter(full_data_may, date < date_init[1])
select_data_may <- filter(select_data_may, date >= date_end[1])

init_date_may <- range(select_data_may$date)[1]
end_date_may  <- range(select_data_may$date)[2]

############################
# JUNE
###########################
setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/OUTPUT/")
load("data_select_mn_JUN.Rdata")
load("dclust_JUN-JUL_JUN_mn.Rdata")
month <- "JUNE"

df <- data_cluster[,-2:-10]
names(df) <- c("id_ind", "group")
dp_daily_2$id_ind <- as.numeric(as.character(dp_daily_2$id_ind))
df$id_ind <- as.numeric(as.character(df$id_ind))
full_data_jun <- full_join(dp_daily_2,df, by="id_ind")

group_jun     <- c(6,12,9,10,3,4)

date_init <- c("2006-07-01")
date_end  <- c("2006-06-01")

select_data_jun <- filter(full_data_jun, date < date_init[1])
select_data_jun <- filter(select_data_jun, date >= date_end[1])

init_date_jun <- range(select_data_jun$date)[1]
end_date_jun  <- range(select_data_jun$date)[2]

############################
# JULY
###########################
setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/OUTPUT/")
load("data_select_mn_JUL.Rdata")
load("dclust_JUL-AUG_JUL_mn.Rdata")
month <- "JULY"

df <- data_cluster[,-2:-10]
names(df) <- c("id_ind", "group")
dp_daily_2$id_ind <- as.numeric(as.character(dp_daily_2$id_ind))
df$id_ind <- as.numeric(as.character(df$id_ind))
full_data_jul <- full_join(dp_daily_2,df, by="id_ind")

group_jul     <- c(7,11,6,4)

date_init <- c("2006-08-01")
date_end  <- c("2006-07-01")

select_data_jul <- filter(full_data_jul, date < date_init[1])
select_data_jul <- filter(select_data_jul, date >= date_end[1])

init_date_jul <- range(select_data_jul$date)[1]
end_date_jul  <- range(select_data_jul$date)[2]

############################
# AUGUST
###########################

## ADD DATA 
setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/OUTPUT/")
load("data_select_mn_AUG.Rdata")
load("dclust_AUG-SEP_AUG_mn.Rdata")
month <- "AUGUST"

df <- data_cluster[,-2:-10]
names(df) <- c("id_ind", "group")
dp_daily_2$id_ind <- as.numeric(as.character(dp_daily_2$id_ind))
df$id_ind <- as.numeric(as.character(df$id_ind))
full_data_aug <- full_join(dp_daily_2,df, by="id_ind")

group_aug <- c(11,5,7,4)

date_init <- c("2006-09-01")
date_end  <- c("2006-08-01")

select_data_aug <- filter(full_data_aug, date < date_init[1])
select_data_aug <- filter(select_data_aug, date >= date_end[1])

init_date_aug <- range(select_data_aug$date)[1]
end_date_aug  <- range(select_data_aug$date)[2]

########################################
##  PLOT
########################################

mat <- matrix(0, 40, 10)
mat[1:20 , 1:10 ] <- 1
mat[21:40 , 1:10 ] <- 2

layout(mat)

data("coastlineWorldFine")
plot(coastlineWorldFine, clongitude=-63.5, clatitude=48.5, span=1000, xlab="", ylab="", axes=FALSE, xaxt="n")
axis(2, at= seq(44,52,2),  las=1, labels=c("44°N", "46°N","48°N", "50°N", "52°N"), mgp=c(1.5,0.5,0), cex.axis=1.2)   
axis(1, at=seq(-75,-50,5),  las=1, labels=c("75°W","70°W","65°W","60°W", "55°W", "50°W"), mgp=c(1.5,0.5,0), cex.axis=1.2)     
box()

g <- 1
for(z in group_apr){
  points(select_data_apr$lon[select_data_apr$date == init_date_apr & select_data_apr$group==z], 
         select_data_apr$lat[select_data_apr$date == init_date_apr & select_data_apr$group==z], 
         col = color_month[g], 
         pch = 1, cex = 0.2)
  j <- c(as.numeric(as.character(select_data_apr$id_ind[select_data_apr$group==z & select_data_apr$date==init_date_apr])))
  
  for(k in j){
    lines(select_data_apr$lon[select_data_apr$id_ind == k],
          select_data_apr$lat[select_data_apr$id_ind == k ],
          col=color_month[g])
  }
}

g <- g + 1
for(z in group_may){
  points(select_data_may$lon[select_data_may$date == init_date_may & select_data_may$group==z], 
         select_data_may$lat[select_data_may$date == init_date_may & select_data_may$group==z], 
         col = color_month[g], 
         pch = 1, cex = 0.2)
  j <- c(as.numeric(as.character(select_data_may$id_ind[select_data_may$group==z & select_data_may$date==init_date_may])))
  
  for(k in j){
    lines(select_data_may$lon[select_data_may$id_ind == k],
          select_data_may$lat[select_data_may$id_ind == k ],
          col=color_month[g])
  }
}

g <- g + 1
for(z in group_jun){
  points(select_data_jun$lon[select_data_jun$date == init_date_jun & select_data_jun$group==z], 
         select_data_jun$lat[select_data_jun$date == init_date_jun & select_data_jun$group==z], 
         col = color_month[g], 
         pch = 1, cex = 0.2)
  j <- c(as.numeric(as.character(select_data_jun$id_ind[select_data_jun$group==z & select_data_jun$date==init_date_jun])))
  
  for(k in j){
    lines(select_data_jun$lon[select_data_jun$id_ind == k],
          select_data_jun$lat[select_data_jun$id_ind == k ],
          col=color_month[g])
  }
}

g <- g + 1
for(z in group_jul){
  points(select_data_jul$lon[select_data_jul$date == init_date_jul & select_data_jul$group==z], 
         select_data_jul$lat[select_data_jul$date == init_date_jul & select_data_jul$group==z], 
         col = color_month[g], 
         pch = 1, cex = 0.2)
  j <- c(as.numeric(as.character(select_data_jul$id_ind[select_data_jul$group==z & select_data_jul$date==init_date_jul])))
  
  for(k in j){
    lines(select_data_jul$lon[select_data_jul$id_ind == k],
          select_data_jul$lat[select_data_jul$id_ind == k ],
          col=color_month[g])
  }
}

g <- g + 1
for(z in group_aug){
  points(select_data_aug$lon[select_data_aug$date == init_date_aug & select_data_aug$group==z], 
         select_data_aug$lat[select_data_aug$date == init_date_aug & select_data_aug$group==z], 
         col = color_month[g], 
         pch = 1, cex = 0.2)
  j <- c(as.numeric(as.character(select_data_aug$id_ind[select_data_aug$group==z & select_data_aug$date==init_date_aug])))
  
  for(k in j){
    lines(select_data_aug$lon[select_data_aug$id_ind == k],
          select_data_aug$lat[select_data_aug$id_ind == k ],
          col=color_month[g])
  }
}

points(x=LonX, y=LatX, pch=19)

legend(-73.8,52,legend=c("April","May", "June", "July", "August", "Obs.Data"), 
       col=c("#ff7f00","#4daf4a","#e41a1c","#377eb8","#984ea3", "black"),
       lty=c(1,1,1,1,1,NA), pch=c(NA,NA,NA,NA,NA,19), lwd=c(2,2,2,2,2,NA), cex=1, bty = "n", pt.cex=1.2, 
       y.intersp=0.4, x.intersp = 0.25, seg.len=0.8)

text(x=-57.3, y=48.3,   labels = "Newfoundland", cex=0.75)
text(x=-58, y=51.6,   labels = "Strait of Belle-Isle", cex=0.75)
text(x=-62, y=50.5,   labels = "North Shore", cex=0.75)
text(x=-65.7, y=48.8,   labels = "Gaspe \nPeninsula", cex=0.75)
text(x=-68.5, y=49.3,     labels = "St. Lawrence Estuary", cex=0.75, srt=38)
text(x=-63, y=49.55,   labels = "Anticosti Island", cex=0.75, srt=335)
text(x=-70.1, y=48.6,   labels = "Saguenay \nRiver", cex=0.75, srt=335)

############################
# APRIL
###########################
setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/OUTPUT/")
load("data_select_tr_APR.Rdata")
load("dclust_APR-MAY_APR_tr.Rdata")
df <- data_cluster[,-2:-10]
names(df) <- c("id_ind", "group")

dp_daily_2$id_ind <- as.numeric(as.character(dp_daily_2$id_ind))
df$id_ind <- as.numeric(as.character(df$id_ind))
full_data_apr <- full_join(dp_daily_2,df, by="id_ind")
date <- dp_daily_2$date[1:182]

group_apr     <- c(7,6,14,5,13,4,10,12,15,9,2)

date_init <- c("2006-05-01")
date_end  <- c("2006-04-01")

select_data_apr <- filter(full_data_apr, date < date_init[1])
select_data_apr <- filter(select_data_apr, date >= date_end[1])

init_date_apr <- range(select_data_apr$date)[1]
end_date_apr  <- range(select_data_apr$date)[2]

############################
# MAY
###########################
setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/OUTPUT/")
load("data_select_tr_MAY.Rdata")
load("dclust_MAY-JUN_MAY_tr.Rdata")
month <- "MAY"

df <- data_cluster[,-2:-10]
names(df) <- c("id_ind", "group")
dp_daily_2$id_ind <- as.numeric(as.character(dp_daily_2$id_ind))
df$id_ind <- as.numeric(as.character(df$id_ind))
full_data_may <- full_join(dp_daily_2,df, by="id_ind")

group_may     <- c(6,5,9,4,8,3,2)

date_init <- c("2006-06-01")
date_end  <- c("2006-05-01")

select_data_may <- filter(full_data_may, date < date_init[1])
select_data_may <- filter(select_data_may, date >= date_end[1])

init_date_may <- range(select_data_may$date)[1]
end_date_may  <- range(select_data_may$date)[2]

############################
# JUNE
###########################
setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/OUTPUT/")
load("data_select_tr_JUN.Rdata")
load("dclust_JUN-JUL_JUN_tr.Rdata")
month <- "JUNE"

df <- data_cluster[,-2:-10]
names(df) <- c("id_ind", "group")
dp_daily_2$id_ind <- as.numeric(as.character(dp_daily_2$id_ind))
df$id_ind <- as.numeric(as.character(df$id_ind))
full_data_jun <- full_join(dp_daily_2,df, by="id_ind")

group_jun     <- c(5,12,10,3,2)

date_init <- c("2006-07-01")
date_end  <- c("2006-06-01")

select_data_jun <- filter(full_data_jun, date < date_init[1])
select_data_jun <- filter(select_data_jun, date >= date_end[1])

init_date_jun <- range(select_data_jun$date)[1]
end_date_jun  <- range(select_data_jun$date)[2]

############################
# JULY
###########################
setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/OUTPUT/")
load("data_select_tr_JUL.Rdata")
load("dclust_JUL-AUG_JUL_tr.Rdata")
month <- "JULY"

df <- data_cluster[,-2:-10]
names(df) <- c("id_ind", "group")
dp_daily_2$id_ind <- as.numeric(as.character(dp_daily_2$id_ind))
df$id_ind <- as.numeric(as.character(df$id_ind))
full_data_jul <- full_join(dp_daily_2,df, by="id_ind")

group_jul     <- c(5,3,2)

date_init <- c("2006-08-01")
date_end  <- c("2006-07-01")

select_data_jul <- filter(full_data_jul, date < date_init[1])
select_data_jul <- filter(select_data_jul, date >= date_end[1])

init_date_jul <- range(select_data_jul$date)[1]
end_date_jul  <- range(select_data_jul$date)[2]

############################
# AUGUST
###########################
setwd("/Users/deborah/Documents/Doctorat/CHAP_2_3D/OUTPUT/")
load("data_select_tr_AUG.Rdata")
load("dclust_AUG-SEP_AUG_tr.Rdata")
month <- "AUGUST"

df <- data_cluster[,-2:-10]
names(df) <- c("id_ind", "group")
dp_daily_2$id_ind <- as.numeric(as.character(dp_daily_2$id_ind))
df$id_ind <- as.numeric(as.character(df$id_ind))
full_data_aug <- full_join(dp_daily_2,df, by="id_ind")

group_aug <- c(4,3)

date_init <- c("2006-09-01")
date_end  <- c("2006-08-01")

select_data_aug <- filter(full_data_aug, date < date_init[1])
select_data_aug <- filter(select_data_aug, date >= date_end[1])

init_date_aug <- range(select_data_aug$date)[1]
end_date_aug  <- range(select_data_aug$date)[2]

##############################################
##  PLOT
#############################################

data("coastlineWorldFine")
plot(coastlineWorldFine, clongitude=-63.5, clatitude=48.5, span=1000, xlab="", ylab="", axes=FALSE, xaxt="n")
axis(2, at= seq(44,52,2),  las=1, labels=c("44°N", "46°N","48°N", "50°N", "52°N"), mgp=c(1.5,0.5,0), cex.axis=1.2)   
axis(1, at=seq(-75,-50,5),  las=1, labels=c("75°W","70°W","65°W","60°W", "55°W", "50°W"), mgp=c(1.5,0.5,0), cex.axis=1.2)     
box()

g <- 1
for(z in group_apr){
  points(select_data_apr$lon[select_data_apr$date == init_date_apr & select_data_apr$group==z], 
         select_data_apr$lat[select_data_apr$date == init_date_apr & select_data_apr$group==z], 
         col = color_month[g], 
         pch = 1, cex = 0.2)
  j <- c(as.numeric(as.character(select_data_apr$id_ind[select_data_apr$group==z & select_data_apr$date==init_date_apr])))
  
  for(k in j){
    lines(select_data_apr$lon[select_data_apr$id_ind == k],
          select_data_apr$lat[select_data_apr$id_ind == k ],
          col=color_month[g])
  }
}

g <- g + 1
for(z in group_may){
  points(select_data_may$lon[select_data_may$date == init_date_may & select_data_may$group==z], 
         select_data_may$lat[select_data_may$date == init_date_may & select_data_may$group==z], 
         col = color_month[g], 
         pch = 1, cex = 0.2)
  j <- c(as.numeric(as.character(select_data_may$id_ind[select_data_may$group==z & select_data_may$date==init_date_may])))
  
  for(k in j){
    lines(select_data_may$lon[select_data_may$id_ind == k],
          select_data_may$lat[select_data_may$id_ind == k ],
          col=color_month[g])
  }
}

g <- g + 1
for(z in group_jun){
  points(select_data_jun$lon[select_data_jun$date == init_date_jun & select_data_jun$group==z], 
         select_data_jun$lat[select_data_jun$date == init_date_jun & select_data_jun$group==z], 
         col = color_month[g], 
         pch = 1, cex = 0.2)
  j <- c(as.numeric(as.character(select_data_jun$id_ind[select_data_jun$group==z & select_data_jun$date==init_date_jun])))
  
  for(k in j){
    lines(select_data_jun$lon[select_data_jun$id_ind == k],
          select_data_jun$lat[select_data_jun$id_ind == k ],
          col=color_month[g])
  }
}

g <- g + 1
for(z in group_jul){
  points(select_data_jul$lon[select_data_jul$date == init_date_jul & select_data_jul$group==z], 
         select_data_jul$lat[select_data_jul$date == init_date_jul & select_data_jul$group==z], 
         col = color_month[g], 
         pch = 1, cex = 0.2)
  j <- c(as.numeric(as.character(select_data_jul$id_ind[select_data_jul$group==z & select_data_jul$date==init_date_jul])))
  
  for(k in j){
    lines(select_data_jul$lon[select_data_jul$id_ind == k],
          select_data_jul$lat[select_data_jul$id_ind == k ],
          col=color_month[g])
  }
}

g <- g + 1
for(z in group_aug){
  points(select_data_aug$lon[select_data_aug$date == init_date_aug & select_data_aug$group==z], 
         select_data_aug$lat[select_data_aug$date == init_date_aug & select_data_aug$group==z], 
         col = color_month[g], 
         pch = 1, cex = 0.2)
  j <- c(as.numeric(as.character(select_data_aug$id_ind[select_data_aug$group==z & select_data_aug$date==init_date_aug])))
  
  for(k in j){
    lines(select_data_aug$lon[select_data_aug$id_ind == k],
          select_data_aug$lat[select_data_aug$id_ind == k ],
          col=color_month[g])
  }
}

points(x=LonX, y=LatX, pch=19)

legend(-73.8,52,legend=c("April","May", "June", "July", "August", "Obs.Data"), 
       col=c("#ff7f00","#4daf4a","#e41a1c","#377eb8","#984ea3", "black"),
       lty=c(1,1,1,1,1,NA), pch=c(NA,NA,NA,NA,NA,19), lwd=c(2,2,2,2,2,NA), cex=1, bty = "n", pt.cex=1.2, 
       y.intersp=0.4, x.intersp = 0.25, seg.len=0.8)

text(x=-57.3, y=48.3,   labels = "Newfoundland", cex=0.75)
text(x=-58, y=51.6,   labels = "Strait of Belle-Isle", cex=0.75)
text(x=-62, y=50.5,   labels = "North Shore", cex=0.75)
text(x=-65.7, y=48.8,   labels = "Gaspe \nPeninsula", cex=0.75)
text(x=-68.5, y=49.3,     labels = "St. Lawrence Estuary", cex=0.75, srt=38)
text(x=-63, y=49.55,   labels = "Anticosti Island", cex=0.75, srt=335)
text(x=-70.1, y=48.6,   labels = "Saguenay \nRiver", cex=0.75, srt=335)

setwd("/Users/deborah/Documents/Doctorat/3D MODEL/GRAPH_PAPER_2/")
dev.print(tiff,filename='FIG_14_CHAP_2_test.tiff', width=16, height=16,units="cm",res = 400)
#dev.off()