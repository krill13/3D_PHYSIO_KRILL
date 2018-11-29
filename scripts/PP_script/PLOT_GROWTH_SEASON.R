rm(list=ls())
pack <- c("tidyverse", "stringr", "ggplot2", "ocedata", "gstat", "sp", 
          "rgdal", "maptools", "rworldmap", "scales","matlab","mosaic", "gpclib")
lapply(pack, library, character.only = TRUE) 


setwd("/Users/deborah/Documents/Doctorat/3D MODEL")
load("month_sum_mn.Rdata")
month_sum_mn <- month_sum

pos_growth_mn <- filter(month_sum_mn, grow_way >=0)
pos_growth_mn <- mutate(pos_growth_mn, prop_grow = (grow_way*100)/mass)
  
spring_season_mn <- filter(pos_growth_mn, month =="4" | month =="5")
spring_season_mn <- group_by(spring_season_mn, id_ind)
spring_season_mn <- summarise(spring_season_mn, lat = mean(lat), lon=mean(lon), 
                              mass=mean(mass), grow_way = mean(grow_way), prop_grow = mean(prop_grow))

summer_season_mn <- filter(pos_growth_mn, month =="6" | month =="7")
summer_season_mn <- group_by(summer_season_mn, id_ind)
summer_season_mn <- summarise(summer_season_mn, lat = mean(lat), lon=mean(lon), 
                              mass=mean(mass), grow_way = mean(grow_way), prop_grow = mean(prop_grow))

autumn_season_mn <- filter(pos_growth_mn, month == "8" | month =="9")
autumn_season_mn <- group_by(autumn_season_mn, id_ind)
autumn_season_mn <- summarise(autumn_season_mn, lat = mean(lat), lon=mean(lon), 
                              mass=mean(mass), grow_way = mean(grow_way), prop_grow = mean(prop_grow))

setwd("/Users/deborah/Documents/Doctorat/3D MODEL")
load("month_sum_tr.Rdata")
month_sum_tr <- month_sum

pos_growth_tr <- filter(month_sum_tr, grow_way >=0)
pos_growth_tr <- mutate(pos_growth_tr, prop_grow = (grow_way*100)/mass)

spring_season_tr <- filter(pos_growth_tr, month =="4" | month =="5")
spring_season_tr <- group_by(spring_season_tr, id_ind)
spring_season_tr <- summarise(spring_season_tr, lat = mean(lat), lon=mean(lon), 
                              mass=mean(mass), grow_way = mean(grow_way), prop_grow = mean(prop_grow))

summer_season_tr <- filter(pos_growth_tr, month =="6" | month =="7")
summer_season_tr <- group_by(summer_season_tr, id_ind)
summer_season_tr <- summarise(summer_season_tr, lat = mean(lat), lon=mean(lon), 
                              mass=mean(mass), grow_way = mean(grow_way), prop_grow = mean(prop_grow))

autumn_season_tr <- filter(pos_growth_tr, month == "8" | month =="9")
autumn_season_tr <- group_by(autumn_season_tr, id_ind)
autumn_season_tr <- summarise(autumn_season_tr, lat = mean(lat), lon=mean(lon), 
                              mass=mean(mass), grow_way = mean(grow_way), prop_grow = mean(prop_grow))

# Layout
mat <- matrix(0, 34, 36)
mat[4:15 , 1:12 ] <- 1
mat[4:15 , 13:24] <- 2
mat[4:15 , 25:36] <- 3
mat[19:30, 1:12 ] <- 4
mat[19:30, 13:24] <- 5
mat[19:30, 25:36] <- 6
mat[31:34, 1:36 ] <- 7

data("coastlineWorldFine")
month <- c("Spring", "Summer", "Autumn") 

layout(mat)

world <- rworldmap::getMap(resolution = "high") 
fortify(world)

spring_mn <- ggplot(world, aes(long, lat, group=group)) +
  geom_point(data=spring_season_mn, aes(x=lon, y=lat,color=prop_grow), inherit.aes = FALSE) +
  scale_color_gradient(low=rgb(255,245,240,maxColorValue=255), high=rgb(203,24,29,maxColorValue=255), limits=c(0,0.25)) +
  geom_polygon(fill = 'grey', color = "black") +
  xlab("") + # Change x-axis title
  ylab("") +
  scale_x_continuous(breaks = seq(-75,-50,5), labels= c("75°W","70°W","65°W","60°W", "55°W", "50°W")) +
  scale_y_continuous(breaks = seq(44,52,2), labels= c("44°N", "46°N","48°N", "50°N", "52°N")) +
  labs(color = "Prop. of body mass") + # Title of the color legend
  theme(legend.text = element_text(size =9)) +
  theme(legend.title = element_text(size =12))+
  theme(axis.title.x = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12)) +
  theme(axis.text.x = element_text(size=12)) +
  theme(axis.text.y = element_text(size=12)) +
  theme_bw() +
  theme(legend.direction = "horizontal", legend.position = "bottom") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) +
  annotate("text", x=-70, y=51.5, label= "SPRING", color='black', cex=4) + 
  theme(legend.position="none") + 
  coord_cartesian(xlim = c(-57, -71), ylim = c(45.7, 51.8))

spring_tr <- ggplot(world, aes(long, lat, group=group)) +
  geom_point(data=spring_season_tr, aes(x=lon, y=lat,color=prop_grow), inherit.aes = FALSE) +
  scale_color_gradient(low=rgb(255,245,240,maxColorValue=255), high=rgb(203,24,29,maxColorValue=255), limits=c(0,0.25)) +
  geom_polygon(fill = 'grey', color = "black") +
  xlab("") + # Change x-axis title
  ylab("") +
  scale_x_continuous(breaks = seq(-75,-50,5), labels= c("75°W","70°W","65°W","60°W", "55°W", "50°W")) +
  scale_y_continuous(breaks = seq(44,52,2), labels= c("44°N", "46°N","48°N", "50°N", "52°N")) +
  labs(color = "Prop. of body mass") + # Title of the color legend
  theme(legend.text = element_text(size =9)) +
  theme(legend.title = element_text(size =12))+
  theme(axis.title.x = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12)) +
  theme(axis.text.x = element_text(size=12)) +
  theme(axis.text.y = element_text(size=12)) +
  theme_bw() +
  theme(legend.direction = "horizontal", legend.position = "bottom") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) +
  annotate("text", x=-70, y=51.5, label= "SPRING", color='black', cex=4) + 
  theme(legend.position="none") + 
  coord_cartesian(xlim = c(-57, -71), ylim = c(45.7, 51.8))

summer_mn <- ggplot(world, aes(long, lat, group=group)) +
  geom_point(data=summer_season_mn, aes(x=lon, y=lat,color=prop_grow), inherit.aes = FALSE) +
  scale_color_gradient(low=rgb(254,224,210,maxColorValue=255), high=rgb(203,24,29,maxColorValue=255), limits=c(0,0.25)) +
  geom_polygon(fill = 'grey', color = "black") +
  xlab("") + # Change x-axis title
  ylab("") +
  scale_x_continuous(breaks = seq(-75,-50,5), labels= c("75°W","70°W","65°W","60°W", "55°W", "50°W")) +
  scale_y_continuous(breaks = seq(44,52,2), labels= c("44°N", "46°N","48°N", "50°N", "52°N")) +
  labs(color = "Prop. of body mass") + # Title of the color legend
  theme(legend.text = element_text(size =9)) +
  theme(legend.title = element_text(size =12))+
  theme(axis.title.x = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12)) +
  theme(axis.text.x = element_text(size=12)) +
  theme(axis.text.y = element_text(size=12)) +
  theme_bw() +
  theme(legend.direction = "horizontal", legend.position = "bottom") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) +
  annotate("text", x=-68, y=51.5, label= "EARLY SUMMER", color='black', cex=4) + 
  theme(legend.position="none") + 
  coord_cartesian(xlim = c(-57, -71), ylim = c(45.7, 51.8))  
  
summer_tr <- ggplot(world, aes(long, lat, group=group)) +
  geom_point(data=summer_season_tr, aes(x=lon, y=lat,color=prop_grow), inherit.aes = FALSE) +
  scale_color_gradient(low=rgb(254,224,210,maxColorValue=255), high=rgb(203,24,29,maxColorValue=255), limits=c(0,0.2)) +
  geom_polygon(fill = 'grey', color = "black") +
  xlab("") + # Change x-axis title
  ylab("") +
  scale_x_continuous(breaks = seq(-75,-50,5), labels= c("75°W","70°W","65°W","60°W", "55°W", "50°W")) +
  scale_y_continuous(breaks = seq(44,52,2), labels= c("44°N", "46°N","48°N", "50°N", "52°N")) +
  labs(color = "Prop. of body mass") + # Title of the color legend
  theme(legend.text = element_text(size =9)) +
  theme(legend.title = element_text(size =12))+
  theme(axis.title.x = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12)) +
  theme(axis.text.x = element_text(size=12)) +
  theme(axis.text.y = element_text(size=12)) +
  theme_bw() +
  theme(legend.direction = "horizontal", legend.position = "bottom") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) +
  annotate("text", x=-68, y=51.5, label= "EARLY SUMMER", color='black', cex=4) + 
  theme(legend.position="none") + 
  coord_cartesian(xlim = c(-57, -71), ylim = c(45.7, 51.8))
  
autumn_mn <- ggplot(world, aes(long, lat, group=group)) +
  geom_point(data=autumn_season_mn, aes(x=lon, y=lat,color=prop_grow), inherit.aes = FALSE) +
  scale_color_gradient(low=rgb(254,224,210,maxColorValue=255), high=rgb(203,24,29,maxColorValue=255), limits=c(0,0.25)) +
  geom_polygon(fill = 'grey', color = "black") +
  xlab("") + # Change x-axis title
  ylab("") +
  scale_x_continuous(breaks = seq(-75,-50,5), labels= c("75°W","70°W","65°W","60°W", "55°W", "50°W")) +
  scale_y_continuous(breaks = seq(44,52,2), labels= c("44°N", "46°N","48°N", "50°N", "52°N")) +
  labs(color = "Prop. of body mass") + # Title of the color legend
  theme(legend.text = element_text(size =9)) +
  theme(legend.title = element_text(size =12))+
  theme(axis.title.x = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12)) +
  theme(axis.text.x = element_text(size=12)) +
  theme(axis.text.y = element_text(size=12)) +
  theme_bw() +
  theme(legend.direction = "horizontal", legend.position = "bottom") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) +
  annotate("text", x=-68, y=51.5, label= "LATE SUMMER", color='black', cex=4) + 
  theme(legend.position="none") + 
  coord_cartesian(xlim = c(-57, -71), ylim = c(45.7, 51.8))

autumn_tr <- ggplot(world, aes(long, lat, group=group)) +
  geom_point(data=autumn_season_tr, aes(x=lon, y=lat,color=prop_grow), inherit.aes = FALSE) +
  scale_color_gradient(low=rgb(254,224,210,maxColorValue=255), high=rgb(203,24,29,maxColorValue=255), limits=c(0,0.25)) +
  geom_polygon(fill = 'grey', color = "black") +
  xlab("") + # Change x-axis title
  ylab("") +
  scale_x_continuous(breaks = seq(-75,-50,5), labels= c("75°W","70°W","65°W","60°W", "55°W", "50°W")) +
  scale_y_continuous(breaks = seq(44,52,2), labels= c("44°N", "46°N","48°N", "50°N", "52°N")) +
  labs(color = "Prop. of body mass") + # Title of the color legend
  theme(legend.text = element_text(size =9)) +
  theme(legend.title = element_text(size =12))+
  theme(axis.title.x = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12)) +
  theme(axis.text.x = element_text(size=12)) +
  theme(axis.text.y = element_text(size=12)) +
  theme_bw() +
  theme(legend.direction = "horizontal", legend.position = "bottom") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) +
  annotate("text", x=-68, y=51.5, label= "LATE SUMMER", color='black', cex=4) + 
  theme(legend.position="none") + 
  coord_cartesian(xlim = c(-57, -71), ylim = c(45.7, 51.8))

library(cowplot)
legend <- get_legend(autumn_tr)
p <- ggdraw(legend)


setwd("/Users/deborah/Documents/Doctorat/3D MODEL/SCRIPTS/")
source("multiplot_ggplot_function.R")

multiplot(spring_mn, summer_mn, autumn_mn, spring_tr, summer_tr, autumn_tr, p, layout=mat)
# Dimension to save the image : 11 * 7 