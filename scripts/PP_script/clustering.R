require(dplyr)
require(tidyverse)
require(stringr)
require(ggplot2)
require(tidyverse)
require(vegan)
require(ade4) 

setwd("/home/dbenkort/Documents/MODELS/git_krill/NEMO_GSL/trunk/DATA_ANALYSES/TR/")
load("data_JUL_tr.Rdata")

data_physio        <-data_JUL
data_physio$egg[is.na(data_physio$egg)] <- 0

cum_grow           <- group_by(data_physio, id_ind)
cum_grow           <- summarise(cum_grow, cumul_grow = sum(grow_way[date >= "2006-07-01" & date <= "2006-08-01"]))

cum_egg            <- group_by(data_physio, id_ind)
cum_egg            <- summarise(cum_egg, cumul_egg = sum(egg[date >= "2006-07-01" & date <= "2006-08-01"]))

data_id_day        <- group_by(data_physio, id_ind, date)
data_mean_day      <- summarise(data_id_day, lon = mean(lon, na.rm=TRUE), 
                                lat = mean(lat, na.rm=TRUE),
                                size = mean(length, na.rm=TRUE), 
                                mass = mean(mass, na.rm=TRUE))

init_position      <- filter(data_mean_day, date == "2006-07-01")
init_position      <- init_position[,-2]
init_position      <- init_position[,-4:-5]

colnames(init_position) <- c("id_ind", "init_lon", "init_lat")

data_cluster       <- filter(data_mean_day, date == "2006-08-01")
data_cluster       <- left_join(data_cluster, cum_grow, by="id_ind")
data_cluster       <- left_join(data_cluster, cum_egg, by="id_ind")
data_cluster       <- left_join(data_cluster, init_position, by="id_ind")
data_cluster       <- filter(data_cluster, cumul_grow != 0)
#data_cluster       <- select(data_cluster, size, mass, cumul_grow, cumul_egg)

nam_row <- as.character(seq(1,length(data_cluster$id_ind),1))
rownames(data_cluster) <- nam_row


#install.packages("factoextra")
#install.packages("cluster")
#install.packages("magrittr")
#install.packages("NbClust")

library("cluster")
library("factoextra")
library("magrittr")
    
# data standardization
medians = apply(data_cluster[,-c(1,2,3,8,9)],2,median)
mads = apply(data_cluster[,-c(1,2,3,8,9)],2,mad)
test_cluster = scale(data_cluster[,-c(1,2,3,8,9)],center=medians,scale=mads)

cluster.dist = dist(test_cluster, method = "euclidean")
hdw=hclust(cluster.dist,method="ward.D2")
hds=hclust(cluster.dist,method="single")
hdc=hclust(cluster.dist,method="complete")
hda=hclust(cluster.dist,method="average")

par(mfrow=c(1,1))
plot(hdw, hang=-1, main="Ward")
plot(hds, hang=-1, main="Lien simple")
plot(hdc, hang=-1, main="Lien complet")
plot(hda, hang=-1, main="Lien moyen")
#plot(cluster.hclust)

dcow=cophenetic(hdw)
dcos=cophenetic(hds)
dcoc=cophenetic(hdc)
dcoa=cophenetic(hda)

rw=round(cor(cluster.dist, dcow),2)
rs=round(cor(cluster.dist, dcos),2)
rc=round(cor(cluster.dist, dcoc),2)
ra=round(cor(cluster.dist, dcoa),2)

res <- cascadeKM(dcoa, 2, 15, iter = 30, criterion = 'calinski')

res
d <- plot(res)
save(res, file= "res_KM_JUL-AUG_JUL_tr.Rdata")

group_11 <- cutree(hda,11)
table(group_11)
save(group_4, file="group_JUL-AUG_JUL_tr.Rdata")

data_cluster <- data.frame(data_cluster,group_11)
data_cluster$group_11 <- as.factor(data_cluster$group_11)

save(data_cluster, file="dclust_JUL-AUG_JUL_tr.Rdata")

ggplot(data_cluster, aes(init_lon, init_lat), fill=group_11) +
  geom_point(aes(colour = factor(group_11)))

ggplot(data_cluster, aes(lon, lat), fill=group_11) +
  geom_point(aes(colour = factor(group_11)))


ggplot(data_cluster, aes(x=cumul_grow)) +
  geom_histogram() +
  facet_wrap(~group_15)

ggplot(data_cluster, aes(x=1,y=cumul_egg)) +
  geom_histogram() +
  facet_wrap(~group_8)

ggplot(data_cluster, aes(x=1,y=cumul_grow, fill=group_12)) +
  geom_boxplot() 


ggplot(data_cluster, aes(x=size)) +
  geom_histogram() +
  facet_wrap(~group_8)

ggplot(data_cluster, aes(x=mass)) +
  geom_histogram() +
  facet_wrap(~group_8)

#ggsave("cluster_init_pos_tr.jpeg", plot=p, device = 'jpeg', 
#       width = 18, height = 9, units = "cm", dpi = 400)


#### ------------ PCA --------------##

library(ggfortify)
library(cluster)


pca <- prcomp(data_cluster[, 4:7])

screeplot(pca)
autoplot(pca, data=data_cluster, loadings = TRUE, loadings.colour="blue",
         loadings.label = TRUE, loadings.label.size = 3)

classif <- agnes(scale(data_cluster[,1:4]), method = "ward")
