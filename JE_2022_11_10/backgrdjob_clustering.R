library(sf)
library(tidyverse)
library(spatstat)
library(data.table)

# reading data
liste_patterns_snap <- readRDS("data_for_bckgrd/jewellers_list.rds")
list_of_networks <- readRDS("data_for_bckgrd/network_list.rds")


#### clustering ####
cluster_matrice_observed <- list()

for (i in 1:length(liste_patterns_snap)) {
  observed_lpp <- lpp(X = as.ppp(X = liste_patterns_snap[[i]] %>% select(geometry)), # transformation du sf en Planar point pattern (ppp)
                      L = maptools::as.linnet.SpatialLines(X = as(list_of_networks[[i]], "Spatial"))) # transofrmation en Linear network (Spatial lines)
  
  observed_dist <- pairdist(X = observed_lpp)
  
  cluster_matrice_observed[[i]] <- hclust(d = as.dist(observed_dist), method = "average")
  
}

## liste de sf contenant les clustering
# sachant que l'étude des clusters : on notait que c'était intéressant de faire une découpe vers 650 m
dist_param <- 650 # découpe à 650 m

sf_cutree <- list()

for (i in 1:length(cluster_matrice_observed)) {
  cut_tree <- cutree(cluster_matrice_observed[[i]], h = dist_param)
  
  snap_revu <- liste_patterns_snap[[i]] %>%
    mutate(cluster = factor(cut_tree, levels = 1:dist_param))
  
  sf_cutree[[i]] <- snap_revu
  
}

st_write(obj = sf_cutree %>% rbindlist() %>% st_as_sf(), 
         dsn = "data_for_bckgrd//jewellers_data_sf_clustering_init.gpkg")
