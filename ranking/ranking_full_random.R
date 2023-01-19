library(tidyverse)
library(latex2exp)
library(data.table)
source(file = "permutation_function.R")

#### full random ####
#### random list replacement with system of size 1000 on 10 time t 

N0 <- seq(1, 1000, 1)
t <- 10

tibble_output <- tibble(value = numeric(), sim = numeric())

for (j in 1:t) {
    
    tableau_init <- sample(x = N0, size = length(N0), replace = FALSE) %>% 
      as_tibble() %>%
      mutate(sim = j)
    
    print(j)
    
    tibble_output <- tibble_output %>%
      bind_rows(tableau_init)
}


#### rank turnover Ot
tibble_output_rowid <- tibble_output %>%
  group_by(sim) %>%
  mutate(rowid = row_number())

# output tibble of rank turnover, sim are as a time t
tableau_turnover <- tibble(time_t = numeric(), Nt = numeric(), N0 = numeric())

# loop to create rank turnover considering diverse ranking sizes
tableau <- tibble_output_rowid %>%
  ungroup() %>%
  group_split(sim)

for (i in 2:length(x = N0)) {
  
  liste_filtre_N0 <- list()
  
  # filtering data by ranking size for a time t
  for (j in 1:length(tableau)) {
    
    liste_filtre_N0[[j]] <- tableau[[j]] %>%
      filter(value <= N0[i]) %>%
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
      summarise(Nt = n_distinct(rowid)) %>%
      mutate(time_t = j,
             N0 = N0[i])
    
    tabletableau_Nt_N0 <- tabletableau_Nt_N0 %>%
      bind_rows(tableau_intermediaire)
  }
  
  print(i)
  
  tableau_turnover <- tableau_turnover %>%
    bind_rows(tabletableau_Nt_N0)
  
}

# output data calculations
write.csv(x = tableau_turnover, file = "outputs_data/o_point_random_size1000_t10.csv", 
          row.names = FALSE)


# input data from model
tableau_turnover <- read.csv(file = "outputs_data/o_point_random_size1000_t10.csv")

#### plotting diverse Ot on N0 
tableau_turnover %>%
  mutate(Ot = Nt/N0,
         N=1000) %>%
  ggplot(aes(x = time_t, y = Ot, color = N0/N, group = N0/N)) +
  scale_color_viridis_c(option = "C") +
  geom_line() +
  theme_bw() +
  scale_x_continuous(name = TeX(r"($t$)"), breaks = seq(1,10,1)) +
  scale_y_continuous(name = TeX(r"($O_{t}$)"), breaks = seq(1,10,1)) +
  labs(caption = "J. Gravier, 2022", 
       title = "Turnover rank: random situation", subtitle = "N=1000, t=10")

ggsave(filename = "rank_turnover_ot_random.png", plot = last_plot(), 
       width = 18, height = 12, units = 'cm')

#### plotting dot O 
tableau_turnover %>%
  mutate(Ot = Nt/N0) %>%
  filter(time_t %in% c(1, 10)) %>%
  mutate(time_t = paste0("t_", time_t)) %>%
  pivot_wider(id_cols = c(N0), names_from = time_t, values_from = Ot) %>%
  mutate(o_point = (t_10 - t_1)/10) %>%
  ggplot(aes(x = N0/1000, y = o_point)) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(name = TeX(r"($N_{0}/N$)")) +
  ylab(TeX(r"($\dot{O}$)")) +
  labs(caption = "J. Gravier, 2022", 
       title = "Mean turnover rank rate: random situation", subtitle = "N=1000, t=10")

ggsave(filename = "rank_turnover_rate_normalized_random.png", plot = last_plot(), 
       width = 18, height = 12, units = 'cm')


#### only jump model ####
# including path dependecy of the system
# times periods t
diff_proba <- seq(0.01, 0.99, 0.01)
jump_proba <- 1-diff_proba
perm_proba <- diff_proba*jump_proba
max(perm_proba)

N0 <- seq(1, 1000, 1)
t <- seq(1,10,1)

permutation_proba <- list()
permutation_proba[[1]] <- N0

### for t times periods
for (t in 2:length(t)) {
  
  init <- permutation_proba[[t-1]]
  
  permutation_proba[[t]] <- jump_permutation(vector = permutation_proba[[t-1]], prob = max(perm_proba))
  
}

df_permutation_proba <- tibble(value = numeric(), time = numeric())

for (i in 1:length(permutation_proba)) {
  
  df_permutation_proba <- df_permutation_proba %>%
    bind_rows(permutation_proba[[i]] %>%
                as_tibble() %>%
                mutate(time = i))

  }

df_permutation_proba

#### rank turnover Ot ####
tibble_output_rowid <- df_permutation_proba %>%
  group_by(time) %>%
  mutate(rowid = row_number())

# output tibble of rank turnover
tableau_turnover <- tibble(time_t = numeric(), Nt = numeric(), N0 = numeric())

# loop to create rank turnover considering diverse ranking sizes
tableau <- tibble_output_rowid %>%
  ungroup() %>%
  group_split(time)

for (i in 2:length(x = N0)) {
  
  liste_filtre_N0 <- list()
  
  # filtering data by ranking size for a time t
  for (j in 1:length(tableau)) {
    
    liste_filtre_N0[[j]] <- tableau[[j]] %>%
      filter(value <= N0[i]) %>%
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
      summarise(Nt = n_distinct(rowid)) %>%
      mutate(time_t = j,
             N0 = N0[i])
    
    tabletableau_Nt_N0 <- tabletableau_Nt_N0 %>%
      bind_rows(tableau_intermediaire)
  }
  
  print(i)
  
  tableau_turnover <- tableau_turnover %>%
    bind_rows(tabletableau_Nt_N0)
  
}

tableau_turnover

# output data calculations
write.csv(x = tableau_turnover, file = "outputs_data/o_permutation_size1000_t10.csv", 
          row.names = FALSE)

