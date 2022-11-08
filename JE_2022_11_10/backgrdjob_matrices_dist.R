library(sf)
library(tidyverse)
library(spatstat)
library(data.table)

# reading data
liste_patterns_snap <- readRDS("data_for_bckgrd/jewellers_list.rds")
list_of_networks <- readRDS("data_for_bckgrd/network_list.rds")

#### distances matrices

#### random points on network ####
# generate points on a linear network
liste_rss_csr <- list()
nsimulation <- 10

for (i in 1:length(liste_patterns_snap)) {
  # run génération de points aléatoires
  liste_calcul <- runiflpp(n = nrow(liste_patterns_snap[[i]]), 
                           L = maptools::as.linnet.SpatialLines(X = as(list_of_networks[[i]], "Spatial")), 
                           nsim = nsimulation) # nombre de simulation choisies
  
  liste_rss_csr[[i]] <- liste_calcul
  
}


### calculs des shortest past
# calcul des shortest past pour toutes les simulations
liste_dist_pi_p <- list()

for (i in 1:length(liste_rss_csr)) {
  
  liste_rss_csr_extract <- lapply(liste_rss_csr[[i]], `[`, c())
  
  ma_liste_simulation <- list()
  
  for (j in 1:length(liste_rss_csr_extract)) {
    dist_pi_p <- pairdist(X = liste_rss_csr_extract[[j]])
    
    dist_pi_p[upper.tri(x = dist_pi_p, diag = TRUE)] <- NA
    
    ma_liste_simulation[[j]] <- dist_pi_p %>%
      as_tibble() %>%
      rowid_to_column(var = "Pi") %>%
      pivot_longer(cols = -Pi, names_to = "P", values_to = "dist_pi_p") %>%
      filter(!is.na(dist_pi_p)) %>%
      mutate(P = str_replace_all(string = P, pattern = "V", replacement = "")) %>%
      mutate(sim = j) %>% 
      mutate(source = liste_patterns_snap[[i]]$source[1]) %>%
      mutate(source_annee = liste_patterns_snap[[i]]$source_annee[1]) 
    
  }
  
  liste_dist_pi_p[[i]] <- ma_liste_simulation
  
}

liste_dist_pi_p[[1]]


### observed distributions
liste_observed_dist <- list()

for (i in 1:length(liste_patterns_snap)) {
  observed_lpp <- lpp(X = as.ppp(X = liste_patterns_snap[[i]] %>% select(geometry)), # transformation du sf en Planar point pattern (ppp)
                      L = maptools::as.linnet.SpatialLines(X = as(list_of_networks[[i]], "Spatial"))) # transofrmation en Linear network (Spatial lines)
  
  tableau_observed_dist_pi_p <- pairdist(X = observed_lpp)
  
  tableau_observed_dist_pi_p[upper.tri(x = tableau_observed_dist_pi_p, diag = TRUE)] <- NA
  
  liste_observed_dist[[i]] <- tableau_observed_dist_pi_p %>%
    as_tibble() %>%
    rowid_to_column(var = "Pi") %>%
    pivot_longer(cols = -Pi, names_to = "P", values_to = "dist_pi_p") %>%
    filter(!is.na(dist_pi_p)) %>%
    mutate(P = str_replace_all(string = P, pattern = "V", replacement = "")) %>%
    mutate(sim = NA) %>%
    mutate(source = liste_patterns_snap[[i]]$source[1]) %>%
    mutate(source_annee = liste_patterns_snap[[i]]$source_annee[1])
  
}

### tableaux pour plot des shortest path ####
# simulation et observations
# en tibble
tableau_plot <- list()

for (i in 1:length(liste_dist_pi_p)) {
  tableau_init <- rbindlist(l = liste_dist_pi_p[[i]]) %>% 
    as_tibble() %>%
    bind_rows(liste_observed_dist[[i]])
  
  
  tableau_plot[[i]] <- tableau_init
}

# saving
saveRDS(object = tableau_plot, file = "data_for_bckgrd/tableau_plot_grocers.rds")