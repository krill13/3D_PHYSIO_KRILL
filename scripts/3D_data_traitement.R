library(ncdf4)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ocedata)
library(gstat)
library(sp)
##library(rgdal)
library(maptools)
library(rworldmap)
library(scales)

#Load NetCDF data file

name_data_file <- 'STATIC_1P_MN_traj.nc'
path_data_file <- '/home/dbenkort/Documents/MODELS/git_krill/NEMO_GSL/trunk/KrillModelNETCDF/'
ncid <- nc_open(paste(path_data_file,name_data_file, sep = ''))
#print(ncid)

# Define dimention counter 
nb_dim   <- ncid$ndims

start_count_time   <- 1
end_count_time     <- ncid$dim$time$len

start_count_seq    <- 1
end_count_seq      <- ncid$dim$sequence$len

start_count_krill  <- 1
end_count_krill    <- ncid$dim$krill$len
 

# General counter for data extract
seq <- 1 
starter <- c(start_count_time,seq, start_count_krill)
counter <- c(end_count_time, seq, end_count_krill)

# Extract variable 
# nb_var <- ncid$nvars

tstep      <- ncvar_get(ncid, varid = 'time')*86400
tstep      <- as.POSIXct(tstep, origin = "2006-JAN-01", 
                 format = "%Y-%B-%d", tz="GMT")
tstep      <- as.Date(tstep, origin = "2006-JAN-01", 
                    format = "%Y-%B-%d", tz="GMT")

date   <- rep(tstep,(end_count_krill-1))
id_ind <- as.factor(seq(1, (ncid$dim$krill$len-1), 1))

nb_var   <- ncid$nvars
var_name <- rep(NA,nb_var)
for (i in 1:nb_var){
  var_name[i]  <- names(ncid$var[i]) 
}

var_name_select <- c(var_name[20],var_name[21],var_name[24],var_name[25],
                     var_name[26],var_name[31],var_name[33],var_name[34],
                     var_name[35], var_name[40], var_name[42],var_name[43],
                     var_name[44], var_name[45], var_name[46], var_name[47], var_name[48])

data_physio <- matrix(NA, ncol= length(var_name_select), nrow = length(tstep)*(ncid$dim$krill$len-1))

for(i in 1:length(var_name_select)){
  
  j = var_name_select[i]
  
  data            <- t(ncvar_get(ncid, varid = j))
  data            <- data[,-1]
  data            <- as.data.frame(data, header=TRUE)
  names(data)     <- id_ind
  data            <- gather(data, id_ind)
  valeurs         <- data$value
  data_physio[,i] <- valeurs
  v_id_ind        <- as.factor(data$id_ind)
  
}

data_physio        <- as.data.frame(data_physio)
data_physio        <- cbind(v_id_ind, data_physio)
names(data_physio) <- c("id_ind", "lon", "lat", "zpo", "size", "mass", "grow_way", 
                        "ingestion_phy", "ingestion_zoo", "breath", "nb_egg", 
                        "gonad", "max_gonad", "tp", "diat", "flag", "micz","mesz" )

df                 <- data.frame(date  = date, 
                                 year  = as.numeric(format(date, format = "%Y")),
                                 month = as.numeric(format(date, format = "%m")),
                                 day   = as.numeric(format(date, format = "%d")))

data_physio        <- cbind(df,data_physio)
data_physio$month  <- as.factor(data_physio$month)
data_id_month      <- group_by(data_physio, id_ind, month)

data_physio_mean_month  <- summarise(data_id_month, lon = mean(lon, na.rm=TRUE), lat = mean(lat, na.rm=TRUE),
                                   zpo = mean(zpo, na.rm=TRUE), size = mean(size, na.rm=TRUE), mass = mean(mass, na.rm=TRUE),
                                   grow_way = mean(grow_way, na.rm=TRUE), ing_phy = mean(ingestion_phy, na.rm=TRUE), 
                                   ing_zoo = mean(ingestion_zoo, na.rm=TRUE), breath = mean(breath, na.rm=TRUE), 
                                   nb_egg = mean(nb_egg, na.rm=TRUE), gonad = mean(gonad, na.rm=TRUE),
                                   max_gonad = mean(max_gonad, na.rm=TRUE), tp = mean(tp, na.rm=TRUE), diat = mean(diat, na.rm=TRUE),
                                   flag = mean(flag, na.rm=TRUE), micz = mean(micz, na.rm=TRUE), mesz = mean(mesz, na.rm=TRUE))


save(data_physio_mean_day, file="data_month_mn.Rdata")

origin_data <- data_physio_mean_month

data_MN    <- origin_data
data_MN$x  <-  data_MN$lon
data_MN$y  <-  data_MN$lat
coordinates(data_MN) = ~x + y

date_wanted <- c( "2006-04-02", "2006-05-02", "2006-06-02", "2006-07-02", "2006-08-02","2006-09-02","2006-10-02")
month <- c( "April", "May", "June", "July", "August", "September", "October")

world <- rworldmap::getMap(resolution = "high") 
fortify(world)

setwd("../")

for (i in 1: length(date_wanted)){
  
  j = month[i]
  i = date_wanted[i]
  
  x.range <- as.numeric(c(min(data_MN[data_MN$date==i,]$x), max(data_MN[data_MN$date==i,]$x)))
  y.range <- as.numeric(c(min(data_MN[data_MN$date==i,]$y), max(data_MN[data_MN$date==i,]$y)))
  grd     <- expand.grid(x = seq(from=x.range[1], to = x.range[2], by =0.1), 
                         y = seq(from=y.range[1], to = y.range[2], by =0.1))
  
  coordinates(grd) <- ~x + y
  gridded(grd) <- TRUE
  
  interp    <- idw(formula = data_MN[data_MN$day==i,]$grow_way ~ 1, locations = data_MN[data_MN$day==i,], newdata = grd)
  interp_output = as.data.frame(interp)
  names(interp_output)[1:3] <- c("long", "lat", "grow")
  
  
  p<-ggplot(world, aes(long, lat, group = group)) +
    #theme_black() +
    theme_classic() +
    geom_raster(data=interp_output, alpha = 0.8, aes(x=long, y=lat, fill=grow) , 
                inherit.aes = FALSE) +
    scale_fill_gradient2(low = "darkcyan", mid = "white", high = "orangered2") + #(colours=c("blue", "white", "red"), values=reshape(c(0.1, 0, -0.1))) +
    geom_polygon(fill = 'black', color = "white") +
    labs(fill = "Monthly mean\nof daily growth\npotential") + # Title of the color legend
    xlab("Longitude") + # Change x-axis title
    ylab("Latitude") +
    theme(legend.text = element_text(size =14)) +
    theme(legend.title = element_text(size =14))+
    theme(axis.title.x = element_text(size=14)) +
    theme(axis.title.y = element_text(size=14)) +
    theme(axis.text.x = element_text(size=14)) +
    theme(axis.text.y = element_text(size=14)) +
    coord_cartesian(xlim = c(-57.5, -71), ylim = c(46, 53)) +
    annotate("text", x=-69, y=52, label= j, color='white', cex=6.5) +
    annotate("text", x=-57.5, y=48.5, label= "NF", color='white', cex=4) + 
    annotate("text", x=-59, y=51.8, label= "Strait of Belle Isle", color='white', cex=4) + 
    annotate("text", x=-64, y=50.6, label= "Jacques-Cartier Strait", color='white', cex=4) + 
    annotate("text", x=-66, y=48.8, label= "Gaspe", color='white', cex=4) +  
    annotate("text", x=-68.8, y=49.5, label= "Lower Estuary", color='white', srt=38, cex=4) +  
    annotate("text", x=-63, y=49.5, label= "Anticosti", color='white', srt=335, cex=4)   
  
  p
  
  ggsave(paste("MN_STATIC_3D_", j, ".jpeg"), plot=p, device = 'jpeg', 
         width = 18, height = 12, units = "cm", dpi = 1000)
  
  
}


ggplot(origin_data,aes(x=date, y=mean_day_nb_egg), fill=id_ind) +
  geom_line()


