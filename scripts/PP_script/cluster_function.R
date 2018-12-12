  library(tidyverse)
  require(vegan)
  require(ade4) 
  
  setwd("/home/dbenkort/Documents/MODELS/git_krill/NEMO_GSL/trunk/CHAPTER_3/OUTPUT/DYNAMIC/")
  load('DAILY_2003_DYNAMIC.Rdata')
  
  data_physio <- data_physio_daily
    
  cum_grow           <- group_by(data_physio, id_ind)
  cum_grow           <- summarise(cum_grow, cumul_grow = sum(grow_way))

  cum_egg            <- group_by(data_physio, id_ind)
  cum_egg            <- summarise(cum_egg, cumul_egg = sum(nb_egg, na.rm=TRUE))

  data_id_day        <- group_by(data_physio, id_ind, date)
  data_mean_day      <- summarise(data_id_day, lon = mean(lon, na.rm=TRUE), 
                                  lat = mean(lat, na.rm=TRUE),
                                  size = mean(size, na.rm=TRUE), 
                                  mass = mean(mass, na.rm=TRUE))
  
  init_position      <- filter(data_physio, date == "2003-06-01")
  init_position      <- init_position[,-2]
  init_position      <- init_position[,-4:-19]
  
  colnames(init_position) <- c("id_ind", "init_lon", "init_lat")
  
  data_cluster       <- filter(data_mean_day, date == "2003-06-30")
  data_cluster       <- left_join(data_cluster, cum_grow, by="id_ind")
  data_cluster       <- left_join(data_cluster, cum_egg,  by="id_ind")
  data_cluster       <- left_join(data_cluster, init_position, by="id_ind")

  data_cluster       <- filter(data_cluster, cumul_grow != 0)

  nam_row <- as.character(seq(1,length(data_cluster$id_ind),1))
  rownames(data_cluster) <- nam_row

  library("cluster")
  library("factoextra")
  library("magrittr")
  
  # data standardization
  medians = apply(data_cluster[,-c(1,2,3,4,9,10)],2,median)
  mads = apply(data_cluster[,-c(1,2,3,4,9,10)],2,mad)
  test_cluster = scale(data_cluster[,-c(1,2,3,4,9,10)],center=medians,scale=mads)
  
  cluster.dist = dist(test_cluster, method = "euclidean")
  
  hdw=hclust(cluster.dist,method="ward.D2")
  hds=hclust(cluster.dist,method="single")
  hdc=hclust(cluster.dist,method="complete")
  hda=hclust(cluster.dist,method="average")
  
  #par(mfrow=c(1,1))
  #plot(hdw, hang=-1, main="Ward")
  #plot(hds, hang=-1, main="Lien simple")
  #plot(hdc, hang=-1, main="Lien complet")
  #plot(hda, hang=-1, main="Lien moyen")
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
  plot(res)
  dev.print(tiff,file="res_dynamic_2003.tiff", width=10, height=10, units="cm", res=100)
  dev.off()
  
  group <- cutree(hda,13)
  table(group)
  
  data_cluster <- data.frame(data_cluster,group)
  data_cluster$group <- as.factor(data_cluster$group)
  
save(data_cluster, file = "data_cluster_dynamic_2003.Rdata")
