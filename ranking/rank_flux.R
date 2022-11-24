library(tidyverse)
library(readxl)
library(latex2exp)
library(data.table)

#### tradeve database ####
tradeve <- read_excel(path = "ranking_exploration/TRADEVE_database/TRADEVE_UrbanAreas_Data.xlsx", sheet = "UrbanAreas_Data")

# creation of ranking by countries
dk_countries_tradeve <- tradeve %>%
  group_by(Country) %>%
  mutate(rang_1961 = rank(desc(Pop_1961)),
         rang_1971 = rank(desc(Pop_1971)),
         rang_1981 = rank(desc(Pop_1981)),
         rang_1991 = rank(desc(Pop_1991)),
         rang_2001 = rank(desc(Pop_2001)),
         rang_2011 = rank(desc(Pop_2011)))

#### rank flux calculations ####
# as defined in Iniguez et al. 2022
# G. Iñiguez, C. Pineda, C. Gershenson, et A.-L. Barabási, 
# « Dynamics of ranking », Nat Commun, vol. 13, nᵒ 1, Art. nᵒ 1, 2022, doi: 10.1038/s41467-022-29256-x

# list of countries studied
liste_pays <- c("DE", "CZ", "ES", "FR", "UK", "IT", "NL", "PL", "RO")
# test of diverse ranking sizes
N0 <- seq(2, 768, 5)
# output tibble of rank flux
tableau_flux <- tibble(Ft = numeric(), Country = character(), time_t = numeric(), time_t1 = numeric(), N0 = numeric())

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
    
    tabletableau_Ft_N0 <- tibble(Ft = numeric(), Country = character(), time_t = numeric(), time_t1 = numeric(), N0 = numeric())
    
    for (j in 2:(length(liste_filtre_N0)-1)) {
      # calculate rank turnover for each time t periods
      tableau_intermediaire <- tableau_filtre_N0 %>%
        filter(time_t == j) %>%
        bind_rows(
          tableau_filtre_N0 %>%
            filter(time_t == j + 1)
        ) %>%
        select(rowid) %>%
        unique() %>% 
        count() %>%
        rename(Ft = n) %>%
        mutate(Country = tableau_filtre_N0$Country[1],
               time_t = j,
               time_t1 = j+1,
               N0 = N0[i])
      
      tabletableau_Ft_N0 <- tabletableau_Ft_N0 %>%
        bind_rows(tableau_intermediaire)
    }
    
    print(k)
    print(N0[i])
    
    tableau_flux <- tableau_flux %>%
      bind_rows(tabletableau_Ft_N0)
    
  }
  
}


# size of each country system
no_max_countries <- dk_countries_tradeve %>%
  filter(Country %in% c("DE", "CZ", "ES", "FR", "UK", "IT", "NL", "PL", "RO"))  %>%
  summarise(N = n())

# filtering rank flux considering the size N of country system
tableau_flux <- tableau_flux %>%
  left_join(x = ., y = no_max_countries, by = "Country") %>%
  filter(N0 <= N)

tableau_flux %>%
  mutate(Ft_proba = (Ft-N0)/N0) %>%
  mutate(t_on_T = time_t/6) %>%
  mutate(N0 = paste0("N0 = ", N0)) %>%
  ggplot(aes(x = t_on_T, y = Ft_proba, color = Country, group = Country)) +
  geom_line() +
  ggthemes::scale_color_tableau(palette = "Tableau 10") +
  theme_bw() +
  scale_x_continuous(name = TeX(r"($t/T$)")) +
  ylab(TeX(r"($F_{t}$)")) +
  labs(caption = "J. Gravier, 2022. Data: TRADEVE DB", title = "Rank flux") +
  facet_wrap(~N0)


# visu of F as mean of Ft
tableau_flux %>%
  mutate(Ft_proba = (Ft-N0)/N0) %>%
  group_by(Country, N0, N) %>%
  summarise(Ft_proba = mean(Ft_proba)) %>%
  ggplot(aes(x = N0/N, y = Ft_proba, color = Country, group = Country)) +
  geom_line() +
  ggthemes::scale_color_tableau(palette = "Tableau 10") +
  theme_bw() +
  scale_x_continuous(name = TeX(r"($N_{0}/N$)")) +
  ylab(TeX(r"($F$)")) +
  labs(caption = "J. Gravier, 2022. Data: TRADEVE DB", title = "Mean rank flux")

ggsave(filename = "ranking_exploration/rank_flux_tradeve.png", plot = last_plot(), 
       width = 18, height = 12, units = 'cm')
