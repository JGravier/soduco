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

tableau_turnover <- read.csv(file = "outputs_data/o_point_random_size1000_t10.csv")

#### plotting diverse Ot on N0 
tableau_turnover %>%
  mutate(Ot = Nt/N0) %>%
  filter(N0 %in% seq(2, 1002, 50)) %>%
  ggplot(aes(x = time_t, y = Ot, color = N0, group = N0)) +
  scale_color_viridis_c(option = "C") +
  geom_line() +
  theme_bw() +
  scale_x_continuous(name = TeX(r"($t$)"), breaks = seq(1,10,1)) +
  scale_y_continuous(name = TeX(r"($O_{t}$)"), breaks = seq(1,10,1)) +
  labs(caption = "J. Gravier, 2022", 
       title = "Turnover rank: random situation", subtitle = "N=1000, t=10")

ggsave(filename = "ranking_exploration/rank_turnover_ot_random.png", plot = last_plot(), 
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



#### proba diffusion/jump model ####
# with N0 is constant over time
# with diffusion a proba alpha
# jump = 1 - alpha
# permutation t-1, t = a(1-a)

diff_proba <- seq(0.01, 0.99, 0.01)
jump_proba <- 1-diff_proba
perm_proba <- diff_proba*jump_proba
max(perm_proba)

#### system of size 1000 on 10 times
N0 <- seq(1, 1000, 1)

### permutation of N0 elements in t times with diverse permutation probabilities
# for permutation, see for a example:
# https://github.com/JGravier/soduco/blob/main/ranking/permutation_demo/permutation.qmd

tibble_output <- tibble(value = numeric(), proba = numeric(), sim_in_p = numeric())

### for a probability p
for (p in 1:length(perm_proba)) {
  
  vec <- N0
  
  # sampling:
  vecsamp <- sample(x = vec, size = length(vec)*perm_proba[p])
  # second sampling with same proba, but on initial vector vec without the result of vecsamp
  vec2 <- setdiff(x = vec, y = vecsamp) # element of x not in y
  # resampling:
  vecsamp2 <- sample(x = vec2, size = length(vec)*perm_proba[p])
  
  # final vector permutation
  finalvec <- vec
  
  for (i in 1:length(vec)) {
    # is vec[i] is equal to vecsamp[i] ? then replace
    for (j in 1:length(vecsamp)) {
      
      if (vec[i] == vecsamp[j]) {
        finalvec[i] <- vecsamp2[j]
      } else if (vec[i] == vecsamp2[j]) {
        finalvec[i] <- vecsamp[j]
      }
      
    }
    
  }
  
  tibble_output <- tibble_output %>%
    bind_rows(
      finalvec %>%
        as_tibble() %>%
        mutate(proba = perm_proba[p]) %>%
        mutate(sim_in_p = p)
    )
  
  print(p)
  
}

tibble_output


#### only jump model ####
# including path dependecy of the system
# times periods t

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



#### analysis of time model

#### rank turnover Ot
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


#### jump and diffusion model ####
# including path dependecy of the system
# times periods t

N0 <- seq(1, 1000, 1)
t <- seq(1,10,1)

permutation_proba <- list()
permutation_proba[[1]] <- N0

### for t times periods
for (t in 2:length(t)) {
  
  # init <- permutation_proba[[t-1]]
  
  permutation_proba[[t]] <- jump_diff_permutation(vector = permutation_proba[[t-1]], prob = max(perm_proba))
  
}

df_permutation_proba <- tibble(value = numeric(), time = numeric())

for (i in 1:length(permutation_proba)) {
  
  df_permutation_proba <- df_permutation_proba %>%
    bind_rows(permutation_proba[[i]] %>%
                as_tibble() %>%
                mutate(time = i))
  
}

df_permutation_proba



#### analysis of time model

#### rank turnover Ot
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








#### plotting diverse models ####
tableau_turnover <- read.csv(file = "outputs_data/o_permutation_size1000_t10.csv") %>%
  as_tibble()

tableau_turnover_m1 <- read.csv(file = "outputs_data/o_point_random_size1000_t10.csv") %>%
  as_tibble()

tableau_turnover <- tableau_turnover %>%
  mutate(model = "Permutation (p=0.25),\nwith path dependency") %>%
  bind_rows(tableau_turnover_m1 %>% 
              mutate(model = "Full random"))

#### plotting diverse Ot on N0 
tableau_turnover %>%
  mutate(Ot = Nt/N0) %>%
  filter(N0 %in% seq(2, 1002, 25)) %>%
  ggplot(aes(x = time_t, y = Ot, color = N0, group = N0)) +
  scale_color_viridis_c(option = "C") +
  geom_line() +
  theme_bw() +
  scale_x_continuous(name = TeX(r"($t$)"), breaks = seq(1,10,1)) +
  scale_y_continuous(name = TeX(r"($O_{t}$)"), breaks = seq(1,10,1)) +
  labs(caption = "J. Gravier, 2022", 
       title = "Rank turnover", subtitle = "N=1000, t=10") +
  facet_wrap(~ model)

ggsave(filename = "rank_turnover_models.png", plot = last_plot(), 
       width = 18, height = 12, units = 'cm')

#### plotting dot O 
tableau_turnover %>%
  mutate(Ot = Nt/N0) %>%
  filter(time_t %in% c(1, 10)) %>%
  mutate(time_t = paste0("t_", time_t)) %>%
  pivot_wider(id_cols = c(N0, model), names_from = time_t, values_from = Ot) %>%
  mutate(o_point = (t_10 - t_1)/10) %>%
  ggplot(aes(x = N0/1000, y = o_point)) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(name = TeX(r"($N_{0}/N$)")) +
  ylab(TeX(r"($\dot{O}$)")) +
  labs(caption = "J. Gravier, 2022", 
       title = "Mean turnover rank rate", subtitle = "N=1000, t=10") +
  facet_wrap(~ model)

ggsave(filename = "rank_turnover_rate_normalized_models.png", plot = last_plot(), 
       width = 18, height = 12, units = 'cm')







# notes
vec <- seq(1,100,1)
# names(vec) <- vec
#### sampling the vector with a probability
## probability is linked to permutation as:
## proba of diffusion = alpha
## proba of jump = 1-alpha
## Permutation t-1,t = alpha(1-alpha) ; max proba is 0.25

# sampling 1:
vecsamp <- sample(x = vec, size = length(vec)*0.25)

# vec for sampling 2: with same proba, but on initial vector vec without the result of vecsamp
vec2 <- setdiff(x = vec, y = vecsamp) # element of x not in y

# sampling 2:
vecsamp2 <- sample(x = vec2, size = length(vec)*0.25)
vecsamp2 <- sort(x = vecsamp2, decreasing = FALSE) # need reordering elements
# names(vecsamp2) <- vecsamp2

vecsamp
vecsamp2

# final vector complete permutation
finalvec <- vec

for (i in 1:length(vec)) {
  
  for (j in 1:length(vecsamp2)) {
    
    if (vec[i] == vecsamp2[j]) { # is vec[i] is equal to vecsamp2[j] ? then replace
      finalvec[i] <- vecsamp[j]
    } else if (vecsamp[j] > vecsamp2[j] & vec[i] >= vecsamp2[j] & vec[i] < vecsamp2[j+1]) { # case of descending in ranking for diffusion
      finalvec[i] <- vec[i-1]
    } else if (vecsamp[j] < vecsamp2[j] & vec[i] <= vecsamp2[j] & vec[i] >= vecsamp[j]) { # case of ascending in ranking for diffusion
      finalvec[i] <- vec[i+1]
    }
    
  }
  
}

finalvec
vecsamp
vecsamp2


# en deux temps avec du filtering ?
vecsamp
vecsamp2

vecsampsup <- vecsamp[vecsamp > vecsamp2]
vecsamp2inf <- vecsamp2[vecsamp2 < vecsamp]

# first match
finalvec <- vec

for (i in 1:length(vec)) {
  
  for (j in 1:length(vecsamp2inf)) {
    
    if (vec[i] == vecsamp2inf[j]) { # is vec[i] is equal to vecsamp2[j] ? then replace
      finalvec[i] <- vecsampsup[j]
    } else if (vec[i] > vecsamp2inf[j] & vec[i] <= vecsamp2inf[j+1]) { 
      # case of descending in ranking for diffusion
      finalvec[i] <- vec[i-1]
    } else if (vec[i] > max(vecsamp2inf) & vec[i] <= tail(x = vecsampsup, n = 1)) { 
      # case of descending in ranking for diffusion
      finalvec[i] <- vec[i-1]
    }
  }
  
}

finalvec
vecsampsup
vecsamp2inf


#### qvec une logique differente

vec <- seq(1,100,1)
vecsamp <- sample(x = vec, size = length(vec)*0.25)
vec2 <- setdiff(x = vec, y = vecsamp) # element of x not in y
vecsamp2 <- sample(x = vec2, size = length(vec)*0.25)
vecsamp2 <- sort(x = vecsamp2, decreasing = FALSE) 

vec
vecsamp
vecsamp2

finalvec <- vec
finalvec[vecsamp2] <- vecsamp

finalvec[-vecsamp2] < vecsamp2[2]

split(x = finalvec)

splitAt <- function(x, pos) {
  unname(split(x, findInterval(x, pos)))
}

blob <- splitAt(x = finalvec, pos = vecsamp2)

unlist(blob)


# make it appen!
vec <- seq(1,100,1)
vecsamp <- sample(x = vec, size = length(vec)*0.25)
vec2 <- setdiff(x = vec, y = vecsamp)
vecsamp2 <- sample(x = vec2, size = length(vec)*0.25)
vecsamp2 <- sort(x = vecsamp2, decreasing = FALSE)

# en sup et en inf
vecsamp2sup <- vecsamp2
vecsamp2sup[vecsamp2sup < vecsamp] <- 0

vecsamp2inf <- vecsamp2
vecsamp2inf[vecsamp2inf > vecsamp] <- 0

vecsamp2inf
vecsamp2sup

# split function
splitAt <- function(x, pos) {
  unname(split(x, findInterval(x, pos)))
}

listsplit <- splitAt(x = vec, pos = vecsamp2)
finalvec <- list()

for (i in 1:length(listsplit)) {
  
  # length of 26 and ascending elements (i.e. vecsamp > than vecsamp 2)
  if (length(listsplit) == 26 & listsplit[i+1][[1]][1] == vecsamp2inf[i] 
      & length(listsplit[i+1][[1]]) != 1 & vecsamp[i] > vecsamp2[i]) {
    datar <- c(listsplit[[i+1]][1:length(listsplit[[i+1]])])
    datar2 <- datar + i + 1
    finalvec[[i]] <- c(vecsamp[i], datar)
    
  } else if (length(listsplit) == 26 & listsplit[i+1][[1]][1] == vecsamp2inf[i] & 
             length(listsplit[i+1][[1]]) == 1 & vecsamp[i] > vecsamp2[i]) {
    finalvec[[i]] <- vecsamp[i]
    
  } else if (length(listsplit) == 26 & listsplit[i+1][[1]][1] == vecsamp2sup[i] & 
             length(listsplit[i+1][[1]]) != 1 & vecsamp[i] > vecsamp2[i]) {
    datar2 <- c(listsplit[[i+1]][1:length(listsplit[[i+1]])])
    finalvec[[i]] <- c(vecsamp[i], datar2)
    
  } else if (length(listsplit) == 26 & listsplit[i+1][[1]][1] == vecsamp2sup[i] & 
             length(listsplit[i+1][[1]] & vecsamp[i] > vecsamp2[i]) == 1) {
    finalvec[[i]] <- vecsamp[i]
  }
  
}

vec
vecsamp
vecsamp2
unlist(finalvec)

