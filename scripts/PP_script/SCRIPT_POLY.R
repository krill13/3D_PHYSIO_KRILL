library(mapview)
library(mapedit)
library(sf)
library(tidyverse)

## Ouvrir map interactive
 map <- mapview() %>%
   editMap()
 saveRDS(map, "zone6.rds")

map <- readRDS("zone4.rds")
mapview(map$finished)

poly <- map$finished


# Open points -------------------------------------------------------------

st_crs(poly)

my_points <- read_csv("~/Desktop/df.csv")

my_points <- st_as_sf(my_points, coords = c("lon", "lat"), crs = st_crs(poly))

# Get intersection --------------------------------------------------------

i <- sapply(st_intersects(my_points, poly), function(x) {
  length(x) == 0
})

plot(my_points)
axis(1)
axis(2)
plot(poly, add = T, color = "red")

my_points %>% 
  slice(which(i)) %>%
  plot()
axis(1)
axis(2)

## Nombre de points à l'extérieur
sum(i)

