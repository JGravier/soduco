library(tidyverse)
library(readxl)
library(data.table)
library(latex2exp)
library(patchwork)

#### tradeve database ####
tradeve <- read_excel(path = "TRADEVE_database/TRADEVE_UrbanAreas_Data.xlsx", sheet = "UrbanAreas_Data")

# creation of ranking by countries
dk_countries_tradeve <- tradeve %>%
  group_by(Country) %>%
  mutate(rang_1961 = rank(desc(Pop_1961)),
         rang_1971 = rank(desc(Pop_1971)),
         rang_1981 = rank(desc(Pop_1981)),
         rang_1991 = rank(desc(Pop_1991)),
         rang_2001 = rank(desc(Pop_2001)),
         rang_2011 = rank(desc(Pop_2011)))

#### rank turnover calculations ####
# as defined in Iniguez et al. 2022
# G. Iñiguez, C. Pineda, C. Gershenson, et A.-L. Barabási, 
# « Dynamics of ranking », Nat Commun, vol. 13, nᵒ 1, Art. nᵒ 1, 2022, doi: 10.1038/s41467-022-29256-x

# list of countries studied
liste_pays <- c("DE", "CZ", "ES", "FR", "UK", "IT", "NL", "PL", "RO")
# test of diverse ranking sizes
N0 <- seq(2, 768, 1)
# output tibble of rank turnover
tableau_turnover <- tibble(Country = character(), time_t = numeric(), Nt = numeric(), N0 = numeric())

# loop to create rank turnover by countries, considering diverse ranking sizes
for (k in 1:length(liste_pays)) {
  
  # filtering init tibble by country and creation of list of tibbles by periods t analysed
  tableau <- dk_countries_tradeve %>%
    filter(Country == liste_pays[k]) %>%
    rowid_to_column() %>%
    select(Country, rowid, rang_1961:rang_2011) %>%
    pivot_longer(cols = rang_1961:rang_2011, names_to = "periods", values_to = "rank") %>%
    ungroup() %>%
    group_split(periods)
  
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
    
    tabletableau_Nt_N0 <- tibble(Country = character(), time_t = numeric(), Nt = numeric(), N0 = numeric())
    
    for (j in 1:(length(liste_filtre_N0))) {
      # calculate rank turnover for each time t periods
      tableau_intermediaire <- tableau_filtre_N0 %>%
        filter(time_t <= j) %>%
        bind_rows(
          tableau_filtre_N0 %>%
            filter(time_t == j)
        ) %>%
        group_by(Country) %>%
        summarise(Nt = n_distinct(rowid)) %>%
        mutate(time_t = j,
               N0 = N0[i])
      
      tabletableau_Nt_N0 <- tabletableau_Nt_N0 %>%
        bind_rows(tableau_intermediaire)
    }
    
    print(k)
    print(N0[i])
    
    tableau_turnover <- tableau_turnover %>%
      bind_rows(tabletableau_Nt_N0)
    
  }
  
}


#### tradeve plots ####
# size of each country system
no_max_countries <- dk_countries_tradeve %>%
  filter(Country %in% c("DE", "CZ", "ES", "FR", "UK", "IT", "NL", "PL", "RO"))  %>%
  summarise(N = n())

# filtering rank turnover considering the size N of country system
tableau_turnover <- tableau_turnover %>%
  left_join(x = ., y = no_max_countries, by = "Country") %>%
  filter(N0 <= N)

# a visualisation
tableau_turnover %>%
  filter(N0 == 52) %>%
  mutate(Ot = Nt/N0) %>%
  ggplot(aes(x = time_t, y = Ot, color = Country, group = Country)) +
  geom_line(show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  ggthemes::scale_color_tableau(palette = "Tableau 10") +
  theme_bw() +
  xlab(expression("t")) +
  ylab(TeX(r"($O_{t}$)")) +
  labs(subtitle = TeX(r"($N_{0} = 52$)"), title = "Rank turnover") +
  tableau_turnover %>%
  mutate(Ot = Nt/N0) %>%
  group_by(Country, N0, N) %>%
  summarise(meanot = mean(Ot), sdot = sd(Ot)) %>%
  ggplot(aes(x = N0/N, y = meanot, color = Country, group = Country)) +
  geom_line() +
  ggthemes::scale_color_tableau(palette = "Tableau 10") +
  theme_bw() +
  scale_x_continuous(name = TeX(r"($N_{0}/N$)")) +
  ylab(TeX(r"($\bar{O_{t}}$)")) +
  labs(caption = "J. Gravier, 2022. Data: TRADEVE DB", subtitle = "Open systems")

ggsave(filename = "ranking_exploration/rank_turnover_tradeve.png", plot = last_plot(), 
       width = 26, height = 12, units = 'cm')


# visualization of o point as defined by iniguez et al.
tableau_turnover %>%
  mutate(Ot = Nt/N0) %>%
  filter(time_t %in% c(1, 6)) %>%
  mutate(time_t = paste0("t_", time_t)) %>%
  pivot_wider(id_cols = c(Country, N0), names_from = time_t, values_from = Ot) %>%
  mutate(o_point = (t_6 - t_1)/6) %>%
  ggplot(aes(x = N0, y = o_point, color = Country, group = Country)) +
  geom_line() +
  ggthemes::scale_color_tableau(palette = "Tableau 10") +
  theme_bw() +
  scale_x_continuous(name = TeX(r"($N_{0}$)")) +
  ylab(TeX(r"($\dot{O}$)")) +
  labs(caption = "J. Gravier, 2022. Data: TRADEVE DB", title = "Mean turnover rank rate")

ggsave(filename = "ranking_exploration/rank_turnover_rate_tradeve.png", plot = last_plot(), 
       width = 18, height = 12, units = 'cm')

# visualization of o point as defined by iniguez et al. zith N0 normalized
tableau_turnover %>%
  mutate(Ot = Nt/N0) %>%
  filter(time_t %in% c(1, 6)) %>%
  mutate(time_t = paste0("t_", time_t)) %>%
  pivot_wider(id_cols = c(Country, N0, N), names_from = time_t, values_from = Ot) %>%
  mutate(o_point = (t_6 - t_1)/6) %>%
  ggplot(aes(x = N0/N, y = o_point, color = Country, group = Country)) +
  geom_line() +
  ggthemes::scale_color_tableau(palette = "Tableau 10") +
  theme_bw() +
  scale_x_continuous(name = TeX(r"($N_{0}/N$)")) +
  ylab(TeX(r"($\dot{O}$)")) +
  labs(caption = "J. Gravier, 2022. Data: TRADEVE DB", title = "Mean turnover rank rate")

ggsave(filename = "ranking_exploration/rank_turnover_rate_normalized_tradeve.png", plot = last_plot(), 
       width = 18, height = 12, units = 'cm')

# outputs turnover
tableau_turnover <- tableau_turnover %>%
  mutate(Ot = Nt/N0) %>%
  filter(time_t %in% c(1, 6)) %>%
  mutate(time_t = paste0("t_", time_t)) %>%
  pivot_wider(id_cols = c(Country, N0, N), names_from = time_t, values_from = Ot) %>%
  mutate(o_point = (t_6 - t_1)/6)

write.csv(x = tableau_turnover, file = "outputs_data/o_point_tradeve.csv", row.names = FALSE)

#### shangai dataset 100 ####
shangai <- read.csv2(file = "shangairanking/shanghai-world-university-ranking.csv") %>%
  as_tibble()

shangai_ranked <- shangai %>%
  mutate(rank = as.numeric(World.rank)) %>%
  filter(rank <= 100) %>%
  select(University, Year, rank)

#### 100 rank turnover calculations ####

# test of diverse ranking sizes
N0 <- seq(2, 100, 1)
# output tibble of rank turnover
tableau_turnover <- tibble(time_t = numeric(), Nt = numeric(), N0 = numeric())

# loop to create rank turnover considering diverse ranking sizes
# filtering init tibble by country and creation of list of tibbles by periods t analysed
tableau <- shangai_ranked %>%
    group_split(Year)
  
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
    
  tabletableau_Nt_N0 <- tibble(time_t = numeric(), Nt = numeric(), N0 = numeric())
    
  for (j in 1:length(liste_filtre_N0)) {
      # calculate rank turnover for each time t periods
      tableau_intermediaire <- tableau_filtre_N0 %>%
        filter(time_t <= j) %>%
        bind_rows(
          tableau_filtre_N0 %>%
            filter(time_t == j)
        ) %>%
        summarise(Nt = n_distinct(University)) %>%
        mutate(time_t = j,
               N0 = N0[i])
      
      tabletableau_Nt_N0 <- tabletableau_Nt_N0 %>%
        bind_rows(tableau_intermediaire)
    }
    
    print(i)
    
  tableau_turnover <- tableau_turnover %>%
    bind_rows(tabletableau_Nt_N0)
    
}

#### shangai plots ####
# how many diverse universities?
shangai_ranked %>% 
  select(University) %>% 
  unique() %>% 
  count()

tableau_turnover %>%
  mutate(Ot = Nt/N0) %>%
  filter(time_t %in% c(1, 14)) %>%
  mutate(time_t = paste0("t_", time_t)) %>%
  pivot_wider(id_cols = c(N0), names_from = time_t, values_from = Ot) %>%
  mutate(o_point = (t_14 - t_1)/14) %>%
  ggplot(aes(x = N0/147, y = o_point)) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(name = TeX(r"($N_{0}/N$)")) +
  ylab(TeX(r"($\dot{O}$)")) +
  labs(caption = "J. Gravier, 2022. Data: Shangai World University Ranking", 
       title = "Mean turnover rank rate - top 100")

ggsave(filename = "ranking_exploration/rank_turnover_rate_normalized_shangai.png", plot = last_plot(), 
       width = 18, height = 12, units = 'cm')

# outputs turnover
tableau_turnover <- tableau_turnover %>%
  mutate(Ot = Nt/N0) %>%
  filter(time_t %in% c(1, 14)) %>%
  mutate(time_t = paste0("t_", time_t), N = 147) %>%
  pivot_wider(id_cols = c(N0, N), names_from = time_t, values_from = Ot) %>%
  mutate(o_point = (t_14 - t_1)/14)

write.csv(x = tableau_turnover, file = "ranking_exploration/outputs_data/o_point_shangai.csv", row.names = FALSE)


#### shangai all ####
shangai <- read.csv2(file = "ranking_exploration/shangairanking/shanghai-world-university-ranking.csv") %>%
  as_tibble()

shangai_ranked <- shangai %>%
  select(University, Year, World.rank.integer) %>%
  rename(rank = World.rank.integer)

#### all rank turnover calculations ####

# test of diverse ranking sizes
N0 <- c(2, 50, 100, 150, 200, 300, 400, 500)
# output tibble of rank turnover
tableau_turnover <- tibble(time_t = numeric(), Nt = numeric(), N0 = numeric())

# loop to create rank turnover considering diverse ranking sizes
# filtering init tibble by country and creation of list of tibbles by periods t analysed
tableau <- shangai_ranked %>%
  group_split(Year)

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
  
  tabletableau_Nt_N0 <- tibble(time_t = numeric(), Nt = numeric(), N0 = numeric())
  
  for (j in 1:length(liste_filtre_N0)) {
    # calculate rank turnover for each time t periods
    tableau_intermediaire <- tableau_filtre_N0 %>%
      filter(time_t <= j) %>%
      bind_rows(
        tableau_filtre_N0 %>%
          filter(time_t == j)
      ) %>%
      summarise(Nt = n_distinct(University)) %>%
      mutate(time_t = j,
             N0 = N0[i])
    
    tabletableau_Nt_N0 <- tabletableau_Nt_N0 %>%
      bind_rows(tableau_intermediaire)
  }
  
  print(i)
  
  tableau_turnover <- tableau_turnover %>%
    bind_rows(tabletableau_Nt_N0)
  
}

#### shangai plots ####
# how many diverse universities?
shangai_ranked %>% 
  select(University) %>% 
  unique() %>% 
  count()

tableau_turnover %>%
  mutate(Ot = Nt/N0) %>%
  filter(time_t %in% c(1, 14)) %>%
  mutate(time_t = paste0("t_", time_t)) %>%
  pivot_wider(id_cols = c(N0), names_from = time_t, values_from = Ot) %>%
  mutate(o_point = (t_14 - t_1)/14) %>%
  ggplot(aes(x = N0/726, y = o_point)) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(name = TeX(r"($N_{0}/N$)")) +
  ylab(TeX(r"($\dot{O}$)")) +
  labs(caption = "J. Gravier, 2022. Data: Shangai World University Ranking", 
       title = "Mean turnover rank rate - top 500")

ggsave(filename = "ranking_exploration/rank_turnover_rate_normalized_shangaiall.png", plot = last_plot(), 
       width = 18, height = 12, units = 'cm')

# outputs turnover
tableau_turnover <- tableau_turnover %>%
  mutate(Ot = Nt/N0) %>%
  filter(time_t %in% c(1, 14)) %>%
  mutate(time_t = paste0("t_", time_t), N = 147) %>%
  pivot_wider(id_cols = c(N0, N), names_from = time_t, values_from = Ot) %>%
  mutate(o_point = (t_14 - t_1)/14)

write.csv(x = tableau_turnover, file = "ranking_exploration/outputs_data/o_point_shangai_500.csv", row.names = FALSE)


#### Fortune500 ####
companies <- read.csv(file = "fortune500/fortune500_1955_2019.csv") %>%
  as_tibble()

harmonization <- read.csv(file = "fortune500/fortune500_1955_2019_uniquenames_review.csv") %>%
  as_tibble()

harmonization2 <- harmonization %>%
  filter(!is.na(x = grouping)) %>%
  group_by(grouping) %>%
  arrange() %>%
  filter(row_number() == 1) %>%
  rename(company2=company) %>%
  select(-any_continuity)

companies <- companies %>%
  left_join(y = harmonization, by = "company") %>%
  left_join(y = harmonization2, by = "grouping") %>%
  mutate(company = if_else(condition = !is.na(grouping), company2, company)) %>%
  select(company, rank, date)

#### rank turnover calculations ####

# test of diverse ranking sizes
N0 <- seq(2, 500, 1)
# output tibble of rank turnover
tableau_turnover <- tibble(time_t = numeric(), Nt = numeric(), N0 = numeric())

# loop to create rank turnover considering diverse ranking sizes
# filtering init tibble by country and creation of list of tibbles by periods t analysed
tableau <- companies %>%
  group_split(date)

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
  
  tabletableau_Nt_N0 <- tibble(time_t = numeric(), Nt = numeric(), N0 = numeric())
  
  for (j in 1:length(liste_filtre_N0)) {
    # calculate rank turnover for each time t periods
    tableau_intermediaire <- tableau_filtre_N0 %>%
      filter(time_t <= j) %>%
      bind_rows(
        tableau_filtre_N0 %>%
          filter(time_t == j)
      ) %>%
      summarise(Nt = n_distinct(company)) %>%
      mutate(time_t = j,
             N0 = N0[i])
    
    tabletableau_Nt_N0 <- tabletableau_Nt_N0 %>%
      bind_rows(tableau_intermediaire)
  }
  
  print(i)
  
  tableau_turnover <- tableau_turnover %>%
    bind_rows(tabletableau_Nt_N0)
  
}

#### fortune500 plots ####
# how many diverse universities?
companies %>% 
  select(company) %>% 
  unique() %>% 
  count()
# 2285

tableau_turnover %>%
  mutate(Ot = Nt/N0) %>%
  filter(time_t %in% c(1, 65)) %>%
  mutate(time_t = paste0("t_", time_t)) %>%
  pivot_wider(id_cols = c(N0), names_from = time_t, values_from = Ot) %>%
  mutate(o_point = (t_65 - t_1)/65) %>%
  ggplot(aes(x = N0/2285, y = o_point)) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(name = TeX(r"($N_{0}/N$)")) +
  ylab(TeX(r"($\dot{O}$)")) +
  labs(caption = "J. Gravier, 2022. Data: Fortune 500", 
       title = "Mean turnover rank rate - top 100")

ggsave(filename = "rank_turnover_rate_normalized_fortune500.png", plot = last_plot(), 
       width = 18, height = 12, units = 'cm')

# outputs turnover
tableau_turnover <- tableau_turnover %>%
  mutate(Ot = Nt/N0) %>%
  filter(time_t %in% c(1, 65)) %>%
  mutate(time_t = paste0("t_", time_t), N = 2285) %>%
  pivot_wider(id_cols = c(N0, N), names_from = time_t, values_from = Ot) %>%
  mutate(o_point = (t_65 - t_1)/65)

write.csv(x = tableau_turnover, file = "outputs_data/o_point_fortune500.csv", row.names = FALSE)


