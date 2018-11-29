rm(list=ls())
setwd("/Users/deborah/Documents/Doctorat/3D MODEL")

pack <- c("sf", "raster", "tidyverse", "viridis", "akima")
lapply(pack, library, character.only = TRUE) 

xo = seq(-73, -53, length.out = 1000)
yo = seq(45, 53, length.out = 1000)

load("data_surface_forcing.Rdata")

data_surface  <- filter(data_surface, lat > 45 & lon < -56)
select_ind    <- filter(data_surface, phy_apr >= 0)
df            <- filter(data_surface, id %in% select_ind$id)
df[is.na(df)] <- 0

df[, 10:15]   <- (df[, 10:15] * 6.76 * 14) 
df[, 16:21]   <- (df[, 16:21] * 7 * 14) 

wm <- rworldmap::getMap(resolution = "high")

res1 <- interp(x = df$lon, y = df$lat, z = df$tp_apr, xo = xo, yo = yo)
dat1 <- expand.grid(res1$x, res1$y)
dat1 <- data_frame(lon = dat1$Var1, lat = dat1$Var2, temp = as.vector(res1$z))
dat1 <- filter(dat1,temp != 0)

res2 <- interp(x = df$lon, y = df$lat, z = df$tp_may, xo = xo, yo = yo)
dat2 <- expand.grid(res2$x, res2$y)
dat2 <- data_frame(lon = dat2$Var1, lat = dat2$Var2, temp = as.vector(res2$z))
dat2 <- filter(dat2,temp != 0)

res3 <- interp(x = df$lon, y = df$lat, z = df$tp_jun, xo = xo, yo = yo)
dat3 <- expand.grid(res3$x, res3$y)
dat3 <- data_frame(lon = dat3$Var1, lat = dat3$Var2, temp = as.vector(res3$z))
dat3 <- filter(dat3,temp != 0)

res4 <- interp(x = df$lon, y = df$lat, z = df$tp_jul, xo = xo, yo = yo)
dat4 <- expand.grid(res4$x, res4$y)
dat4 <- data_frame(lon = dat4$Var1, lat = dat4$Var2, temp = as.vector(res4$z))
dat4 <- filter(dat4,temp != 0)

res5 <- interp(x = df$lon, y = df$lat, z = df$tp_aug, xo = xo, yo = yo)
dat5 <- expand.grid(res3$x, res3$y)
dat5 <- data_frame(lon = dat5$Var1, lat = dat5$Var2, temp = as.vector(res5$z))
dat5 <- filter(dat5,temp != 0)

res6 <- interp(x = df$lon, y = df$lat, z = df$tp_sep, xo = xo, yo = yo)
dat6 <- expand.grid(res6$x, res6$y)
dat6 <- data_frame(lon = dat6$Var1, lat = dat6$Var2, temp = as.vector(res6$z))
dat6 <- filter(dat6,temp != 0)

t_apr <- ggplot(dat1, aes(x = lon, y = lat, fill = temp)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradientn(colours = rev(rainbow(15))) +
  theme(legend.direction = "horizontal", legend.position = "bottom") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) +
  xlab("") + # Change x-axis title
  ylab("") +
  scale_x_continuous(breaks = seq(-75,-50,5), labels= c("75°W","70°W","65°W","60°W", "55°W", "50°W")) +
  scale_y_continuous(breaks = seq(44,52,2), labels= c("44°N", "46°N","48°N", "50°N", "52°N")) +
  labs(fill = "Temperature (°C)") + # Title of the color legend
  theme(legend.text = element_text(size =9)) +
  theme(legend.title = element_text(size =12))+
  theme(axis.title.x = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12)) +
  theme(axis.text.x = element_text(size=12)) +
  theme(axis.text.y = element_text(size=12)) +
  geom_polygon(data = wm, aes(x = long, y = lat, group = group), inherit.aes = FALSE) +
  coord_cartesian(xlim = c(-57, -71), ylim = c(45.7, 51.8))

t_may <- ggplot(dat2, aes(x = lon, y = lat, fill = temp)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradientn(colours = rev(rainbow(15))) +
  theme(legend.direction = "horizontal", legend.position = "bottom") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) +
  xlab("") + # Change x-axis title
  ylab("") +
  scale_x_continuous(breaks = seq(-75,-50,5), labels= c("75°W","70°W","65°W","60°W", "55°W", "50°W")) +
  scale_y_continuous(breaks = seq(44,52,2), labels= c("44°N", "46°N","48°N", "50°N", "52°N")) +
  labs(fill = "Temperature (°C)") + # Title of the color legend
  theme(legend.text = element_text(size =9)) +
  theme(legend.title = element_text(size =12))+
  theme(axis.title.x = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12)) +
  theme(axis.text.x = element_text(size=12)) +
  theme(axis.text.y = element_text(size=12)) +
  geom_polygon(data = wm, aes(x = long, y = lat, group = group), inherit.aes = FALSE) +
  coord_cartesian(xlim = c(-57, -71), ylim = c(45.7, 51.8))

t_jun <- ggplot(dat3, aes(x = lon, y = lat, fill = temp)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradientn(colours = rev(rainbow(15))) +
  theme(legend.direction = "horizontal", legend.position = "bottom") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) +
  xlab("") + # Change x-axis title
  ylab("") +
  scale_x_continuous(breaks = seq(-75,-50,5), labels= c("75°W","70°W","65°W","60°W", "55°W", "50°W")) +
  scale_y_continuous(breaks = seq(44,52,2), labels= c("44°N", "46°N","48°N", "50°N", "52°N")) +
  labs(fill = "Temperature (°C)") + # Title of the color legend
  theme(legend.text = element_text(size =9)) +
  theme(legend.title = element_text(size =12))+
  theme(axis.title.x = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12)) +
  theme(axis.text.x = element_text(size=12)) +
  theme(axis.text.y = element_text(size=12)) +
  geom_polygon(data = wm, aes(x = long, y = lat, group = group), inherit.aes = FALSE) +
  coord_cartesian(xlim = c(-57, -71), ylim = c(45.7, 51.8))

t_jul <- ggplot(dat4, aes(x = lon, y = lat, fill = temp)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradientn(colours = rev(rainbow(15))) +
  theme(legend.direction = "horizontal", legend.position = "bottom") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) +
  xlab("") + # Change x-axis title
  ylab("") +
  scale_x_continuous(breaks = seq(-75,-50,5), labels= c("75°W","70°W","65°W","60°W", "55°W", "50°W")) +
  scale_y_continuous(breaks = seq(44,52,2), labels= c("44°N", "46°N","48°N", "50°N", "52°N")) +
  labs(fill = "Temperature (°C)") + # Title of the color legend
  theme(legend.text = element_text(size =9)) +
  theme(legend.title = element_text(size =12))+
  theme(axis.title.x = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12)) +
  theme(axis.text.x = element_text(size=12)) +
  theme(axis.text.y = element_text(size=12)) +
  geom_polygon(data = wm, aes(x = long, y = lat, group = group), inherit.aes = FALSE) +
  coord_cartesian(xlim = c(-57, -71), ylim = c(45.7, 51.8))

t_aug <- ggplot(dat5, aes(x = lon, y = lat, fill = temp)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradientn(colours = rev(rainbow(15))) +
  theme(legend.direction = "horizontal", legend.position = "bottom") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) +
  xlab("") + # Change x-axis title
  ylab("") +
  scale_x_continuous(breaks = seq(-75,-50,5), labels= c("75°W","70°W","65°W","60°W", "55°W", "50°W")) +
  scale_y_continuous(breaks = seq(44,52,2), labels= c("44°N", "46°N","48°N", "50°N", "52°N")) +
  labs(fill = "Temperature (°C)") + # Title of the color legend
  theme(legend.text = element_text(size =9)) +
  theme(legend.title = element_text(size =12))+
  theme(axis.title.x = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12)) +
  theme(axis.text.x = element_text(size=12)) +
  theme(axis.text.y = element_text(size=12)) +
  geom_polygon(data = wm, aes(x = long, y = lat, group = group), inherit.aes = FALSE) +
  coord_cartesian(xlim = c(-57, -71), ylim = c(45.7, 51.8))

t_sep <- ggplot(dat6, aes(x = lon, y = lat, fill = temp)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradientn(colours = rev(rainbow(15))) +
  theme(legend.direction = "horizontal", legend.position = "bottom") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) +
  xlab("") + # Change x-axis title
  ylab("") +
  scale_x_continuous(breaks = seq(-75,-50,5), labels= c("75°W","70°W","65°W","60°W", "55°W", "50°W")) +
  scale_y_continuous(breaks = seq(44,52,2), labels= c("44°N", "46°N","48°N", "50°N", "52°N")) +
  labs(fill = "Temperature (°C)") + # Title of the color legend
  theme(legend.text = element_text(size =9)) +
  theme(legend.title = element_text(size =12))+
  theme(axis.title.x = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12)) +
  theme(axis.text.x = element_text(size=12)) +
  theme(axis.text.y = element_text(size=12)) +
  geom_polygon(data = wm, aes(x = long, y = lat, group = group), inherit.aes = FALSE) +
  coord_cartesian(xlim = c(-57, -71), ylim = c(45.7, 51.8))

res7 <- interp(x = df$lon, y = df$lat, z = df$phy_apr, xo = xo, yo = yo)
dat7 <- expand.grid(res7$x, res7$y)
dat7 <- data_frame(lon = dat7$Var1, lat = dat7$Var2, phy = as.vector(res7$z))
dat7 <- filter(dat7,phy != 0)

res8 <- interp(x = df$lon, y = df$lat, z = df$phy_may, xo = xo, yo = yo)
dat8 <- expand.grid(res8$x, res8$y)
dat8 <- data_frame(lon = dat8$Var1, lat = dat8$Var2, phy = as.vector(res8$z))
dat8 <- filter(dat8,phy != 0)

res9 <- interp(x = df$lon, y = df$lat, z = df$phy_jun, xo = xo, yo = yo)
dat9 <- expand.grid(res9$x, res9$y)
dat9 <- data_frame(lon = dat9$Var1, lat = dat9$Var2, phy = as.vector(res9$z))
dat9 <- filter(dat9,phy != 0)

res10 <- interp(x = df$lon, y = df$lat, z = df$phy_jul, xo = xo, yo = yo)
dat10 <- expand.grid(res10$x, res10$y)
dat10 <- data_frame(lon = dat10$Var1, lat = dat10$Var2, phy = as.vector(res10$z))
dat10 <- filter(dat10,phy != 0)

res11 <- interp(x = df$lon, y = df$lat, z = df$phy_aug, xo = xo, yo = yo)
dat11 <- expand.grid(res11$x, res11$y)
dat11 <- data_frame(lon = dat11$Var1, lat = dat11$Var2, phy = as.vector(res11$z))
dat11 <- filter(dat11,phy != 0)

res12 <- interp(x = df$lon, y = df$lat, z = df$phy_sep, xo = xo, yo = yo)
dat12 <- expand.grid(res12$x, res12$y)
dat12 <- data_frame(lon = dat12$Var1, lat = dat12$Var2, phy = as.vector(res12$z))
dat12 <- filter(dat12,phy != 0)

phy_apr <- ggplot(dat7, aes(x = lon, y = lat, fill = phy)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradientn(colours = rev(viridis(10))) +
  theme(legend.direction = "horizontal", legend.position = "bottom") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) +
  xlab("") + # Change x-axis title
  ylab("") +
  scale_x_continuous(breaks = seq(-75,-50,5), labels= c("75°W","70°W","65°W","60°W", "55°W", "50°W")) +
  scale_y_continuous(breaks = seq(44,52,2), labels= c("44°N", "46°N","48°N", "50°N", "52°N")) +
  labs(fill = "Phytoplankton (mg C.m-3)") + # Title of the color legend
  theme(legend.text = element_text(size =9)) +
  theme(legend.title = element_text(size =12))+
  theme(axis.title.x = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12)) +
  theme(axis.text.x = element_text(size=12)) +
  theme(axis.text.y = element_text(size=12)) +
  geom_polygon(data = wm, aes(x = long, y = lat, group = group), inherit.aes = FALSE) +
  coord_cartesian(xlim = c(-57, -71), ylim = c(45.7, 51.8))

phy_may <- ggplot(dat8, aes(x = lon, y = lat, fill = phy)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradientn(colours = rev(viridis(10))) +
  theme(legend.direction = "horizontal", legend.position = "bottom") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) +
  xlab("") + # Change x-axis title
  ylab("") +
  scale_x_continuous(breaks = seq(-75,-50,5), labels= c("75°W","70°W","65°W","60°W", "55°W", "50°W")) +
  scale_y_continuous(breaks = seq(44,52,2), labels= c("44°N", "46°N","48°N", "50°N", "52°N")) +
  labs(fill = "Phytoplankton (mg C.m-3)") + # Title of the color legend
  theme(legend.text = element_text(size =9)) +
  theme(legend.title = element_text(size =12))+
  theme(axis.title.x = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12)) +
  theme(axis.text.x = element_text(size=12)) +
  theme(axis.text.y = element_text(size=12)) +
  geom_polygon(data = wm, aes(x = long, y = lat, group = group), inherit.aes = FALSE) +
  coord_cartesian(xlim = c(-57, -71), ylim = c(45.7, 51.8))

phy_jun <- ggplot(dat9, aes(x = lon, y = lat, fill = phy)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradientn(colours = rev(viridis(10))) +
  theme(legend.direction = "horizontal", legend.position = "bottom") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) +
  xlab("") + # Change x-axis title
  ylab("") +
  scale_x_continuous(breaks = seq(-75,-50,5), labels= c("75°W","70°W","65°W","60°W", "55°W", "50°W")) +
  scale_y_continuous(breaks = seq(44,52,2), labels= c("44°N", "46°N","48°N", "50°N", "52°N")) +
  labs(fill = "Phytoplankton (mg C.m-3)") + # Title of the color legend
  theme(legend.text = element_text(size =9)) +
  theme(legend.title = element_text(size =12))+
  theme(axis.title.x = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12)) +
  theme(axis.text.x = element_text(size=12)) +
  theme(axis.text.y = element_text(size=12)) +
  geom_polygon(data = wm, aes(x = long, y = lat, group = group), inherit.aes = FALSE) +
  coord_cartesian(xlim = c(-57, -71), ylim = c(45.7, 51.8))

phy_jul <- ggplot(dat10, aes(x = lon, y = lat, fill = phy)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradientn(colours = rev(viridis(10))) +
  theme(legend.direction = "horizontal", legend.position = "bottom") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) +
  xlab("") + # Change x-axis title
  ylab("") +
  scale_x_continuous(breaks = seq(-75,-50,5), labels= c("75°W","70°W","65°W","60°W", "55°W", "50°W")) +
  scale_y_continuous(breaks = seq(44,52,2), labels= c("44°N", "46°N","48°N", "50°N", "52°N")) +
  labs(fill = "Phytoplankton (mg C.m-3)") + # Title of the color legend
  theme(legend.text = element_text(size =9)) +
  theme(legend.title = element_text(size =12))+
  theme(axis.title.x = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12)) +
  theme(axis.text.x = element_text(size=12)) +
  theme(axis.text.y = element_text(size=12)) +
  geom_polygon(data = wm, aes(x = long, y = lat, group = group), inherit.aes = FALSE) +
  coord_cartesian(xlim = c(-57, -71), ylim = c(45.7, 51.8))

phy_aug <- ggplot(dat11, aes(x = lon, y = lat, fill = phy)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradientn(colours = rev(viridis(10))) +
  theme(legend.direction = "horizontal", legend.position = "bottom") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) +
  xlab("") + # Change x-axis title
  ylab("") +
  scale_x_continuous(breaks = seq(-75,-50,5), labels= c("75°W","70°W","65°W","60°W", "55°W", "50°W")) +
  scale_y_continuous(breaks = seq(44,52,2), labels= c("44°N", "46°N","48°N", "50°N", "52°N")) +
  labs(fill = "Phytoplankton (mg C.m-3)") + # Title of the color legend
  theme(legend.text = element_text(size =9)) +
  theme(legend.title = element_text(size =12))+
  theme(axis.title.x = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12)) +
  theme(axis.text.x = element_text(size=12)) +
  theme(axis.text.y = element_text(size=12)) +
  geom_polygon(data = wm, aes(x = long, y = lat, group = group), inherit.aes = FALSE) +
  coord_cartesian(xlim = c(-57, -71), ylim = c(45.7, 51.8))

phy_sep <- ggplot(dat12, aes(x = lon, y = lat, fill = phy)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradientn(colours = rev(viridis(10))) +
  theme(legend.direction = "horizontal", legend.position = "bottom") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) +
  xlab("") + # Change x-axis title
  ylab("") +
  scale_x_continuous(breaks = seq(-75,-50,5), labels= c("75°W","70°W","65°W","60°W", "55°W", "50°W")) +
  scale_y_continuous(breaks = seq(44,52,2), labels= c("44°N", "46°N","48°N", "50°N", "52°N")) +
  labs(fill = "Phytoplankton (mg C.m-3)") + # Title of the color legend
  theme(legend.text = element_text(size =9)) +
  theme(legend.title = element_text(size =12))+
  theme(axis.title.x = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12)) +
  theme(axis.text.x = element_text(size=12)) +
  theme(axis.text.y = element_text(size=12)) +
  geom_polygon(data = wm, aes(x = long, y = lat, group = group), inherit.aes = FALSE) +
  coord_cartesian(xlim = c(-57, -71), ylim = c(45.7, 51.8))

res13 <- interp(x = df$lon, y = df$lat, z = df$zoo_apr, xo = xo, yo = yo)
dat13 <- expand.grid(res13$x, res13$y)
dat13 <- data_frame(lon = dat13$Var1, lat = dat13$Var2, zoo = as.vector(res13$z))
dat13 <- filter(dat13,zoo != 0)

res14 <- interp(x = df$lon, y = df$lat, z = df$zoo_may, xo = xo, yo = yo)
dat14 <- expand.grid(res14$x, res14$y)
dat14 <- data_frame(lon = dat14$Var1, lat = dat14$Var2, zoo = as.vector(res14$z))
dat14 <- filter(dat14,zoo != 0)

res15 <- interp(x = df$lon, y = df$lat, z = df$zoo_jun, xo = xo, yo = yo)
dat15 <- expand.grid(res15$x, res15$y)
dat15 <- data_frame(lon = dat15$Var1, lat = dat15$Var2, zoo = as.vector(res15$z))
dat15 <- filter(dat15,zoo != 0)

res16 <- interp(x = df$lon, y = df$lat, z = df$zoo_jul, xo = xo, yo = yo)
dat16 <- expand.grid(res16$x, res16$y)
dat16 <- data_frame(lon = dat16$Var1, lat = dat16$Var2, zoo = as.vector(res16$z))
dat16 <- filter(dat16,zoo != 0)

res17 <- interp(x = df$lon, y = df$lat, z = df$zoo_aug, xo = xo, yo = yo)
dat17 <- expand.grid(res17$x, res17$y)
dat17 <- data_frame(lon = dat17$Var1, lat = dat17$Var2, zoo = as.vector(res17$z))
dat17 <- filter(dat17,zoo != 0)

res18 <- interp(x = df$lon, y = df$lat, z = df$zoo_sep, xo = xo, yo = yo)
dat18 <- expand.grid(res18$x, res18$y)
dat18 <- data_frame(lon = dat18$Var1, lat = dat18$Var2, zoo = as.vector(res18$z))
dat18 <- filter(dat18,zoo != 0)

zoo_apr <- ggplot(dat13, aes(x = lon, y = lat, fill = zoo)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradientn(colours = rev(heat.colors(10))) +
  theme(legend.direction = "horizontal", legend.position = "bottom") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) +
  xlab("") + # Change x-axis title
  ylab("") +
  scale_x_continuous(breaks = seq(-75,-50,5), labels= c("75°W","70°W","65°W","60°W", "55°W", "50°W")) +
  scale_y_continuous(breaks = seq(44,52,2), labels= c("44°N", "46°N","48°N", "50°N", "52°N")) +
  labs(fill = "Zooplankton (mg C.m-3)") + # Title of the color legend
  theme(legend.text = element_text(size =9)) +
  theme(legend.title = element_text(size =12))+
  theme(axis.title.x = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12)) +
  theme(axis.text.x = element_text(size=12)) +
  theme(axis.text.y = element_text(size=12)) +
  geom_polygon(data = wm, aes(x = long, y = lat, group = group), inherit.aes = FALSE) +
  coord_cartesian(xlim = c(-57, -71), ylim = c(45.7, 51.8))

zoo_may <- ggplot(dat14, aes(x = lon, y = lat, fill = zoo)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradientn(colours = rev(heat.colors(10))) +
  theme(legend.direction = "horizontal", legend.position = "bottom") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) +
  xlab("") + # Change x-axis title
  ylab("") +
  scale_x_continuous(breaks = seq(-75,-50,5), labels= c("75°W","70°W","65°W","60°W", "55°W", "50°W")) +
  scale_y_continuous(breaks = seq(44,52,2), labels= c("44°N", "46°N","48°N", "50°N", "52°N")) +
  labs(fill = "Zooplankton (mg C.m-3)") + # Title of the color legend
  theme(legend.text = element_text(size =9)) +
  theme(legend.title = element_text(size =12))+
  theme(axis.title.x = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12)) +
  theme(axis.text.x = element_text(size=12)) +
  theme(axis.text.y = element_text(size=12)) +
  geom_polygon(data = wm, aes(x = long, y = lat, group = group), inherit.aes = FALSE) +
  coord_cartesian(xlim = c(-57, -71), ylim = c(45.7, 51.8))

zoo_jun <- ggplot(dat15, aes(x = lon, y = lat, fill = zoo)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradientn(colours = rev(heat.colors(10))) +
  theme(legend.direction = "horizontal", legend.position = "bottom") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) +
  xlab("") + # Change x-axis title
  ylab("") +
  scale_x_continuous(breaks = seq(-75,-50,5), labels= c("75°W","70°W","65°W","60°W", "55°W", "50°W")) +
  scale_y_continuous(breaks = seq(44,52,2), labels= c("44°N", "46°N","48°N", "50°N", "52°N")) +
  labs(fill = "Zooplankton (mg C.m-3)") + # Title of the color legend
  theme(legend.text = element_text(size =9)) +
  theme(legend.title = element_text(size =12))+
  theme(axis.title.x = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12)) +
  theme(axis.text.x = element_text(size=12)) +
  theme(axis.text.y = element_text(size=12)) +
  geom_polygon(data = wm, aes(x = long, y = lat, group = group), inherit.aes = FALSE) +
  coord_cartesian(xlim = c(-57, -71), ylim = c(45.7, 51.8))

zoo_jul <- ggplot(dat16, aes(x = lon, y = lat, fill = zoo)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradientn(colours = rev(heat.colors(10))) +
  theme(legend.direction = "horizontal", legend.position = "bottom") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) +
  xlab("") + # Change x-axis title
  ylab("") +
  scale_x_continuous(breaks = seq(-75,-50,5), labels= c("75°W","70°W","65°W","60°W", "55°W", "50°W")) +
  scale_y_continuous(breaks = seq(44,52,2), labels= c("44°N", "46°N","48°N", "50°N", "52°N")) +
  labs(fill = "Zooplankton (mg C.m-3)") + # Title of the color legend
  theme(legend.text = element_text(size =9)) +
  theme(legend.title = element_text(size =12))+
  theme(axis.title.x = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12)) +
  theme(axis.text.x = element_text(size=12)) +
  theme(axis.text.y = element_text(size=12)) +
  geom_polygon(data = wm, aes(x = long, y = lat, group = group), inherit.aes = FALSE) +
  coord_cartesian(xlim = c(-57, -71), ylim = c(45.7, 51.8))

zoo_aug <- ggplot(dat17, aes(x = lon, y = lat, fill = zoo)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradientn(colours = rev(heat.colors(10))) +
  theme(legend.direction = "horizontal", legend.position = "bottom") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) +
  xlab("") + # Change x-axis title
  ylab("") +
  scale_x_continuous(breaks = seq(-75,-50,5), labels= c("75°W","70°W","65°W","60°W", "55°W", "50°W")) +
  scale_y_continuous(breaks = seq(44,52,2), labels= c("44°N", "46°N","48°N", "50°N", "52°N")) +
  labs(fill = "Zooplankton (mg C.m-3)") + # Title of the color legend
  theme(legend.text = element_text(size =9)) +
  theme(legend.title = element_text(size =12))+
  theme(axis.title.x = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12)) +
  theme(axis.text.x = element_text(size=12)) +
  theme(axis.text.y = element_text(size=12)) +
  geom_polygon(data = wm, aes(x = long, y = lat, group = group), inherit.aes = FALSE) +
  coord_cartesian(xlim = c(-57, -71), ylim = c(45.7, 51.8))

zoo_sep <- ggplot(dat18, aes(x = lon, y = lat, fill = zoo)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradientn(colours = rev(heat.colors(10))) +
  theme(legend.direction = "horizontal", legend.position = "bottom") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) +
  xlab("") + # Change x-axis title
  ylab("") +
  scale_x_continuous(breaks = seq(-75,-50,5), labels= c("75°W","70°W","65°W","60°W", "55°W", "50°W")) +
  scale_y_continuous(breaks = seq(44,52,2), labels= c("44°N", "46°N","48°N", "50°N", "52°N")) +
  labs(fill = "Zooplankton (mg C.m-3)") + # Title of the color legend
  theme(legend.text = element_text(size =9)) +
  theme(legend.title = element_text(size =12))+
  theme(axis.title.x = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12)) +
  theme(axis.text.x = element_text(size=12)) +
  theme(axis.text.y = element_text(size=12)) +
  geom_polygon(data = wm, aes(x = long, y = lat, group = group), inherit.aes = FALSE) +
  coord_cartesian(xlim = c(-57, -71), ylim = c(45.7, 51.8))


mat <- matrix(0, 30, 30)

mat[1:10  , 1:10] <- 1
mat[11:20 , 1:10] <- 2
mat[21:30 , 1:10] <- 3

mat[1:10  , 11:20] <- 4
mat[11:20 , 11:20] <- 5
mat[21:30 , 11:20] <- 6

mat[1:10  , 21:30 ] <- 7
mat[11:20 , 21:30 ] <- 8
mat[21:30 , 21:30 ] <- 9

layout(mat)
layout.show(9)

par(mar = c(0,0,0,0))

setwd("/Users/deborah/Documents/Doctorat/3D MODEL/SCRIPTS/")
source('multiplot_ggplot_function.R')

multiplot(t_apr, t_jun, t_aug, phy_apr, phy_jun, phy_aug, zoo_apr, zoo_jun, zoo_aug, layout=mat)


mat <- matrix(0, 10, 30)

mat[1:10 ,  1:10] <- 1
mat[1:10 , 11:20] <- 2
mat[1:10 , 21:30] <- 3

layout(mat)
layout.show(3)

par(mar = c(0,0,0,0))
multiplot(t_apr, phy_apr, zoo_apr, layout=mat)
