#---------------------------------#
#   Forcing Rimouski Station      #
#---------------------------------#

# Use NEMO model outputs in NetCDF format
# Load package RNetCDF
require(ncdf4)

ncid  <- nc_open("/home/FORCING/bgcm_out2h_20060101_20061231_ave_TSUV.nc")   # open the forcing file

depth <- ncvar_get(ncid, varid="depthu")                  # get the depth axis values at the center of the grid cells

torig <- unname( unlist(ncatt_get(ncid, varid="time_counter", attname="time_origin")[2]) ) # get the origin of the time axis
tstep <- ncvar_get(ncid, varid="time_counter")                                             # get the time values. Seconds since the time origin (2005-05-01:00)
tstep <- as.POSIXct(tstep, origin= torig, format = "%Y-%B-%d", tz="GMT")                  # convert tstep into an R format

i <- which( tstep==strptime("2006-01-01", format="%Y-%m-%d", tz="GMT") ) # choose the starting date for data sampling

start <- c(43, 183, 1, 1) # rimouski location : starting time fo forcing profile
count <- c(1, 1, 20, 4380) # define the extent of sampling



#--- Extract temperature, phyto and zooplankton food concentration
temp <- ncvar_get(ncid, varid="votemper", start, count) # get a 2D vertical profile of temperature

nc_close(ncid)


ncid_food  <- nc_open("/home/FORCING/bgcm_out2h_20060101_20061231_bgcm_02.nc")   # open the forcing food concentration file

diat  <- ncvar_get(ncid_food, varid="diat" , start, count)                 # get the diatom concentration axis values at the center of the grid cells
flag  <- ncvar_get(ncid_food, varid="flag" , start, count)                 # get the flagellates concentration axis values at the center of the grid cells
micro <- ncvar_get(ncid_food, varid="micro", start, count)                 # get the microzooplankton concentration axis values at the center of the grid cells
meso  <- ncvar_get(ncid_food, varid="meso" , start, count)                 # get the mesozooplankton concentration axis values at the center of the grid cells

nc_close(ncid_food)

# remove missing values. Use gradients for that.
j <- max( which( diff(temp[,1])!=0 ) )

temp[(j+1):dim(temp)[1],]   <- NA

diat[(j+1):dim(diat)[1],]   <- NA
flag[(j+1):dim(flag)[1],]   <- NA
micro[(j+1):dim(micro)[1],] <- NA
meso[(j+1):dim(meso)[1],]   <- NA

ZOO <-meso
ZOO <- ZOO[1:16,]

PHY <- diat + flag + micro
PHY <- PHY[1:16,]

temp <- temp[1:16,]

matplot(ZOO)

library(fields)
image.plot(matrix((data=ZOO), ncol=4380, nrow=20))

write.csv2(ZOO, file='zoo_rimouski_forcing')


# ----- Define a function for plotting a matrix ----- #
myImagePlot <- function(x, ...){
  min <- min(x)
  max <- max(x)
  yLabels <- rownames(x)
  xLabels <- colnames(x)
  title <-c()
  # check for additional function arguments
  if( length(list(...)) ){
    Lst <- list(...)
    if( !is.null(Lst$zlim) ){
      min <- Lst$zlim[1]
      max <- Lst$zlim[2]
    }
    if( !is.null(Lst$yLabels) ){
      yLabels <- c(Lst$yLabels)
    }
    if( !is.null(Lst$xLabels) ){
      xLabels <- c(Lst$xLabels)
    }
    if( !is.null(Lst$title) ){
      title <- Lst$title
    }
  }
  # check for null values
  if( is.null(xLabels) ){
    xLabels <- c(1:ncol(x))
  }
  if( is.null(yLabels) ){
    yLabels <- c(1:nrow(x))
  }
  
  layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(4,1), heights=c(1,1))
  
  # Red and green range from 0 to 1 while Blue ranges from 1 to 0
  ColorRamp <- rgb( seq(0,1,length=256),  # Red
                    seq(0,1,length=256),  # Green
                    seq(1,0,length=256))  # Blue
  ColorLevels <- seq(min, max, length=length(ColorRamp))
  
  # Reverse Y axis
  reverse <- nrow(x) : 1
  yLabels <- yLabels[reverse]
  x <- x[reverse,]
  
  # Data Map
  par(mar = c(3,5,2.5,2))
  image(1:length(xLabels), 1:length(yLabels), t(x), col=ColorRamp, xlab="",
        ylab="", axes=FALSE, zlim=c(min,max))
  if( !is.null(title) ){
    title(main=title)
  }
  axis(BELOW<-1, at=1:length(xLabels), labels=xLabels, cex.axis=0.7)
  axis(LEFT <-2, at=1:length(yLabels), labels=yLabels, las= HORIZONTAL<-1,
       cex.axis=0.7)
  
  # Color Scale
  par(mar = c(3,2.5,2.5,2))
  image(1, ColorLevels,
        matrix(data=ColorLevels, ncol=length(ColorLevels),nrow=1),
        col=ColorRamp,
        xlab="",ylab="",
        xaxt="n")
  
  layout(1)
}
# ----- END plot function ----- #





depth <-  seq(1,20,1)
date  <-  seq(1,4380,1)

myImagePlot(ZOO)
myImagePlot(PHY)
myImagePlot(temp)

#depth <- c(-7.5, -17.5, -27.5, -37.5, -47.5, -65, -85, -112.5, -142.5, -175, -215, -270, -370, -495, -720)
#date  <- c("2000","2001", "2002", "2003", "2004", "2005","2006","2007","2008","2009","2010","2011" )

max(ZOO)
