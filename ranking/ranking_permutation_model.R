library(tidyverse)
library(latex2exp)
library(data.table)
source(file = "permutation_function.R")

# with N0 is constant over time
# with diffusion a proba alpha
# jump = 1 - alpha
# permutation t-1, t = a(1-a)
diff_proba <- seq(0.01, 0.99, 0.05)
jump_proba <- 1-diff_proba
perm_proba <- diff_proba*jump_proba

#### system of size 1000 on 10 times
N0 <- seq(1, 1000, 1)
timest <- seq(1,10,1)

#### only jump model ####
model_list <- list()

for (p in 1:length(perm_proba)) {
  # create a list of rank
  permutation_proba <- list()
  permutation_proba[[1]] <- N0
  
  ### for t times periods
  # including path dependency of the system
  for (t in 2:length(timest)) {
    permutation_proba[[t]] <- jump_diff_permutation(vector = permutation_proba[[t-1]], prob = perm_proba[p]*length(N0))
  }
  
  model_list[[p]] <- unlist(permutation_proba) %>%
    as_tibble() %>%
    rename(rank = value) %>%
    mutate(proba = perm_proba[p]) %>%
    mutate(timest = rep(x = timest, each = length(N0)))
}


### Rank turnover cals: Ot ####
# loop to create rank turnover considering diverse ranking sizes (<=> N0)

model_list_turnover <- list()

for (l in 1:length(model_list)) {
  
  tableau <- model_list[[l]] %>%
    group_by(timest) %>%
    mutate(rowid = row_number()) %>%
    ungroup() %>%
    group_split(timest)
  
  # output tibble of rank turnover
  tableau_turnover <- tibble(time_t = numeric(), Nt = numeric(), N0 = numeric(), proba = numeric())
  
  for (i in 2:length(x = N0)) {
    
    liste_filtre_N0 <- list()
    
    # filtering data by ranking size for a time t
    for (j in 1:length(tableau)) {
      
      liste_filtre_N0[[j]] <- tableau[[j]] %>%
        filter(rank <= N0[i]) %>%
        mutate(time_t = j)
      
    }
    
    tableau_filtre_N0 <- data.table::rbindlist(l = liste_filtre_N0) %>% # binding all tibbles in list in one tibble
      as_tibble()
    
    tabletableau_Nt_N0 <- tibble(time_t = numeric(), Nt = numeric(), N0 = numeric(), proba = numeric())
    
    for (j in 1:length(liste_filtre_N0)) {
      # calculate rank turnover for each time t periods
      tableau_intermediaire <- tableau_filtre_N0 %>%
        filter(time_t <= j) %>%
        bind_rows(
          tableau_filtre_N0 %>%
            filter(time_t == j)
        ) %>%
        summarise(Nt = n_distinct(rowid)) %>%
        mutate(time_t = j,
               N0 = N0[i])
      
      tabletableau_Nt_N0 <- tabletableau_Nt_N0 %>%
        bind_rows(tableau_intermediaire)
    }
    
    print(l)
    print(i)
    
    tableau_turnover <- tableau_turnover %>%
      bind_rows(tabletableau_Nt_N0) %>%
      mutate(proba = model_list[[l]]$proba[1])
    
  }
  
  model_list_turnover[[l]] <- tableau_turnover
  
}

# model output
write.csv(x = rbindlist(model_list_turnover) %>%
            as_tibble(x = .), 
          file = "o_t_jumpdiffmodel_size1000_t10_p20.csv", 
          row.names = FALSE)


#### visual analytics ####
rbindlist(model_list_turnover) %>%
  as_tibble(x = .) %>%
  mutate(Ot = Nt/N0) %>%
  # filter(N0 %in% seq(2, 1002, 25)) %>%
  ggplot(aes(x = time_t, y = Ot, color = N0, group = N0)) +
  scale_color_viridis_c(option = "C") +
  geom_line() +
  theme_bw() +
  scale_x_continuous(name = TeX(r"($t$)"), breaks = seq(1,10,1)) +
  scale_y_continuous(name = TeX(r"($O_{t}$)"), breaks = seq(1,10,1)) +
  labs(caption = "J. Gravier, 2022", 
       title = "Rank turnover", subtitle = "N=1000, t=10") +
  facet_wrap(~proba)
  









