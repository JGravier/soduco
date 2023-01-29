#### functions for ranking analysis ####

#### rank turnover as defined in Iniguez et al 2022 and adapted to analyse diff from diverse N0/N
# @param: N0_sequence > the sequence 2 -> N (complete or choosen), every k
# @param: tableau_entry > the init dataframe 
# @param: snapshot_time > the time period of observations/simulations in tableau_entry$snapshot_time
# @param: id_unique > the identifier choosen to compare elements over time in tableau_entry$identifier
# (ex: names of chess players or Insee ID for french municipalities)
# @param: rank > the rank of elements over time in tableau_entry$rank
# @return: a dataframe composed by time_t (snapshot as 1 to ... n), Nt (see Iniguez) and N0
rank_turnover <- function(N0_sequence, tableau_entry, snapshot_time, id_unique, rank){
  
  # output tibble of rank turnover
  tableau_turnover <- tibble(time_t = numeric(), Nt = numeric(), N0 = numeric())
  
  # loop to create rank turnover considering diverse ranking sizes
  # filtering init tibble by country and creation of list of tibbles by periods t analysed
  tableau <- tableau_entry %>%
    group_by(snapshot_time, .add = TRUE) %>%
    group_split(snapshot_time)
  
  for (i in 1:length(x = N0_sequence)) {
    
    liste_filtre_N0 <- list()
    
    # filtering data by ranking size for a time t
    for (j in 1:length(tableau)) {
      
      liste_filtre_N0[[j]] <- tableau[[j]] %>%
        filter(rank <= N0_sequence[i]) %>%
        mutate(time_t = j)
      
    }
    
    tableau_filtre_N0 <- data.table::rbindlist(l = liste_filtre_N0) %>% # binding all tibbles in list in one tibble
      as_tibble()
    
    tabletableau_Nt_N0 <- tibble(time_t = numeric(), Nt = numeric(), N0 = numeric())
    
    for (j in 1:length(liste_filtre_N0)) {
      # calculate rank turnover for each time t periods
      tableau_intermediaire <- tableau_filtre_N0 %>%
        filter(time_t <= j) %>%
        bind_rows(
          tableau_filtre_N0 %>%
            filter(time_t == j)
        ) %>%
        summarise(Nt = n_distinct(id_unique)) %>%
        mutate(time_t = j,
               N0 = N0_sequence[i])
      
      tabletableau_Nt_N0 <- tabletableau_Nt_N0 %>%
        bind_rows(tableau_intermediaire)
    }
    
    print(i)
    
    tableau_turnover <- tableau_turnover %>%
      bind_rows(tabletableau_Nt_N0)
    
  }
  
  return(tableau_turnover)
}



#### rank flux Ft as defined in Iniguez et al 2022 and adapted to analyse diff from diverse N0/N
# @param: N0_sequence > the sequence 2 -> N (complete or choosen), every k
# @param: tableau_entry > the init dataframe 
# @param: snapshot_time > the time period of observations/simulations in tableau_entry$snapshot_time
# @param: id_unique > the identifier choosen to compare elements over time in tableau_entry$identifier
# (ex: names of chess players or Insee ID for french municipalities)
# @return: a dataframe composed by Ft (snapshot as 1 to ... n), time_t and t+1 (see Iniguez), N0
rank_flux <- function(N0_sequence, tableau_entry, snapshot_time, id_unique){
  
  # output tibble of rank flux
  tableau_flux <- tibble(Ft = numeric(), time_t = numeric(), time_t1 = numeric(), N0 = numeric())
  
  # loop to create rank turnover considering diverse ranking sizes
  # filtering init tibble by country and creation of list of tibbles by periods t analysed
  tableau <- tableau_entry %>%
    group_by(snapshot_time, .add = TRUE) %>%
    group_split(snapshot_time)
  
  # loop to create rank flux considering diverse ranking sizes
  for (i in 1:length(x = N0)) {
    
    liste_filtre_N0 <- list()
    
    # filtering data by ranking size for a time t
    for (j in 1:length(tableau)) {
      
      liste_filtre_N0[[j]] <- tableau[[j]] %>%
        filter(rank <= N0[i]) %>%
        mutate(time_t = j)
      
    }
    
    tableau_filtre_N0 <- data.table::rbindlist(l = liste_filtre_N0) %>% # binding all tibbles in list in one tibble
      as_tibble()
    
    tabletableau_Ft_N0 <- tibble(Ft = numeric(), time_t = numeric(), time_t1 = numeric(), N0 = numeric())
    
    for (j in 1:(length(liste_filtre_N0)-1)) {
      # calculate rank turnover for each time t periods
      tableau_intermediaire <- tableau_filtre_N0 %>%
        filter(time_t == j) %>%
        bind_rows(
          tableau_filtre_N0 %>%
            filter(time_t == j + 1)
        ) %>%
        select(id_unique) %>%
        unique() %>% 
        count() %>%
        rename(Ft = n) %>%
        mutate(time_t = j,
               time_t1 = j+1,
               N0 = N0[i])
      
      tabletableau_Ft_N0 <- tabletableau_Ft_N0 %>%
        bind_rows(tableau_intermediaire)
    }
    
    print(i)
    
    tableau_flux <- tableau_flux %>%
      bind_rows(tabletableau_Ft_N0)
    
  }
  
  return(tableau_flux)
}
