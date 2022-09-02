library(tidyverse)
library(readxl)
# data from: Gravier, Julie (2018): Areas and populations of urban settlements. figshare. Dataset. https://doi.org/10.6084/m9.figshare.5632015.v3 

nord_france_data <- read_excel(path = "ranking_exploration/Data_EtmjPop_V2.xls", sheet = "Donnees")

nord_france_data <- nord_france_data %>%
  mutate(rang_1821 = dense_rank(desc(POP1821)),
         rang_1836 = dense_rank(desc(POP1836)),
         rang_1866 = dense_rank(desc(POP1866)))

di_t <- function(xt, xtmoins1){
  x <- abs(xt - xtmoins1)
  return(x)
}

`%ni%` <- Negate(`%in%`)


nord_france_data <- nord_france_data %>%
  mutate(di_1821_1836 = di_t(xt = rang_1836, xtmoins1 = rang_1821),
         di_1836_1866 = di_t(xt = rang_1866, xtmoins1 = rang_1836),
         no_abs_1821_1836 = rang_1821-rang_1836,
         no_abs_1836_1866 = rang_1836-rang_1866)


mean_dt <- nord_france_data %>%
  summarise(dt_1821_1836 = mean(di_1821_1836), dt_1836_1866 = mean(di_1836_1866),
            sd1 = sd(di_1821_1836), sd2 = sd(di_1836_1866))


pivot_nord_france_data <- nord_france_data %>%
  pivot_longer(data = ., cols = di_1821_1836:di_1836_1866, names_to = "periods", values_to = "di(t)") %>%
  mutate(periods = str_sub(string = periods, start = 4, end = 12)) 

mean_dt_pivot <- pivot_nord_france_data %>%
  summarise(mean = mean(`di(t)`), sd = sd(`di(t)`))
  
pivot_nord_france_data %>%
  mutate(categ = case_when(
    `di(t)` <= (mean_dt_pivot$mean) ~ "<=mean",
    TRUE  ~ ">mean"
  )) %>%
  ggplot(mapping = aes(x = periods, y = `di(t)`)) +
  geom_point() +
  geom_line(aes(group = NOM_COMM, color = NOM_COMM), show.legend = FALSE) +
  facet_wrap(~ categ)


pivot_nord_france_data %>%
  ggplot(mapping = aes(x = periods, y = `di(t)`)) +
  geom_point() +
  geom_line(aes(group = NOM_COMM, color = NOM_COMM), show.legend = FALSE) +
  facet_wrap(~ TYPE_LIEU)


nord_france_data %>%
  filter(di_1821_1836 > (mean_dt$dt_1821_1836 + mean_dt$sd1)) %>%
  # filter(di_1836_1866 > (mean_dt$dt_1836_1866 + mean_dt$dt_1836_1866)) %>%
  pivot_longer(data = ., cols = di_1821_1836:di_1836_1866, names_to = "periods", values_to = "di(t)") %>%
  mutate(periods = str_sub(string = periods, start = 4, end = 12)) %>%
  ggplot(mapping = aes(x = periods, y = `di(t)`)) +
  geom_point() +
  geom_line(aes(group = NOM_COMM, color = NOM_COMM), show.legend = TRUE)

nord_france_data %>%
  filter(di_1821_1836 > (mean_dt$dt_1821_1836)) %>%
  filter(di_1836_1866 > (mean_dt$dt_1836_1866)) %>%
  filter(TYPE_LIEU == "Ville") %>%
  mutate(evo = case_when(
    no_abs_1821_1836 < 0 & no_abs_1836_1866 < 0 ~ "decreased",
    no_abs_1821_1836 > 0 & no_abs_1836_1866 > 0 ~ "increased",
    TRUE ~ "both",
  )) %>%
  pivot_longer(data = ., cols = no_abs_1821_1836:no_abs_1836_1866, names_to = "periods", values_to = "di(t)") %>%
  mutate(periods = str_sub(string = periods, start = 8, end = 16)) %>%
  ggplot(mapping = aes(x = periods, y = `di(t)`, group = NOM_COMM, color = NOM_COMM), show.legend = TRUE) +
  geom_point() +
  geom_line() +
  ggthemes::scale_color_tableau(palette = "Tableau 20") +
  theme_bw() +
  facet_grid(evo ~ TYPE_LIEU)

# rank diversity
rank_diversity <- nord_france_data %>%
  select(NOM_COMM, rang_1821:rang_1866) %>%
  pivot_longer(cols = rang_1821:rang_1866, names_to = "date", values_to = "rang") %>%
  group_by(rang) %>%
  summarise(n_dist = n_distinct(NOM_COMM),
    `d(k)` = n_distinct(NOM_COMM)/3)

rank_diversity %>%
  ggplot(mapping = aes(x = rang, y = `d(k)`)) +
  geom_point() +
  geom_line()
  
#### tradeve database ####
tradeve <- read_excel(path = "ranking_exploration/TRADEVE_database/TRADEVE_UrbanAreas_Data.xlsx", sheet = "UrbanAreas_Data")

#### dt de Batty 2006 ####
dt_tradeve <- dk_countries_tradeve %>%
  mutate(di_1961_1971 = di_t(xt = rang_1971, xtmoins1 = rang_1961),
         di_1971_1981 = di_t(xt = rang_1981, xtmoins1 = rang_1971),
         di_1981_1991 = di_t(xt = rang_1991, xtmoins1 = rang_1981),
         di_1991_2001 = di_t(xt = rang_2001, xtmoins1 = rang_1991),
         di_2001_2011 = di_t(xt = rang_2011, xtmoins1 = rang_2001))


mean_dt_country <- dt_tradeve %>%
  ungroup() %>%
  pivot_longer(cols = di_1961_1971:di_2001_2011, names_to = "periods", values_to = "di") %>%
  group_by(periods, Country) %>%
  summarise(dt = mean(di), dt_sd = sd(di), d = mean(di)/6, n = n())

mean_dt_country %>%
  filter(n > 100) %>%
  mutate(periods = str_sub(string = periods, start = 4, end = 12),
         Country = paste(Country, " n =", n)) %>%
  ggplot(aes(x = periods, y = dt, group = Country, color = Country)) +
  geom_point() +
  geom_line() +
  ggthemes::scale_color_tableau(palette = "Tableau 10") +
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  ylab("d(t)") +
  labs(caption = "J. Gravier, 2022. Data: TRADEVE DB",
       title = "Rank shift of European Cities",
       subtitle = "Systems composed of more than 100 cities")

ggsave(filename = "ranking_exploration/dt_european_cities.png", plot = last_plot(), width = 19, height = 13, units = 'cm')


#### dk ####
tradevetwo <- tradeve %>%
  select(Name, Country, Pop_1961:Pop_2011) %>%
  rowid_to_column() %>%
  mutate(estnull = if_else(
    if_any(Pop_1961:Pop_2011, ~ is.na(.)), TRUE, FALSE
  )) %>%
  filter(estnull == FALSE) %>%
  mutate(rang_1961 = rank(desc(Pop_1961)),
         rang_1971 = rank(desc(Pop_1971)),
         rang_1981 = rank(desc(Pop_1981)),
         rang_1991 = rank(desc(Pop_1991)),
         rang_2001 = rank(desc(Pop_2001)),
         rang_2011 = rank(desc(Pop_2011)))

rank_diversity <- tradevetwo %>%
  select(rowid, rang_1961:rang_2011) %>%
  pivot_longer(cols = rang_1961:rang_2011, names_to = "date", values_to = "rang") %>%
  group_by(rang) %>%
  summarise(`d(k)` = n_distinct(rowid)/6,
            distinction = n_distinct(rowid))

rank_diversity %>%
  ggplot(mapping = aes(x = rang, y = `d(k)`)) +
  geom_line(color = "grey25") +
  geom_point(size = 0.7) +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_continuous(trans = "log10", name = "k") +
  theme_bw() +
  labs(caption = "J. Gravier, 2022. Data: TRADEVE DB",
       subtitle = "Rank diversity of European Cities: 1961-2011")

ggsave(filename = "ranking_exploration/dk_european_cities.png", plot = last_plot(), width = 15, height = 12, units = 'cm')



#  par pays et non uniquement à l'échelle européenne
dk_countries_tradeve <- tradeve %>%
  group_by(Country) %>%
  mutate(rang_1961 = rank(desc(Pop_1961)),
         rang_1971 = rank(desc(Pop_1971)),
         rang_1981 = rank(desc(Pop_1981)),
         rang_1991 = rank(desc(Pop_1991)),
         rang_2001 = rank(desc(Pop_2001)),
         rang_2011 = rank(desc(Pop_2011)))


dk_countries_tradeve %>%
  rowid_to_column() %>%
  select(rowid, Country, rang_1961:rang_2011) %>%
  ungroup() %>%
  pivot_longer(cols = rang_1961:rang_2011, names_to = "date", values_to = "rang") %>%
  group_by(Country, rang) %>%
  summarise(`d(k)` = n_distinct(rowid)/6,
            distinction = n_distinct(rowid)) %>%
  filter(nchar(x = Country) < 3) %>%
  filter(Country %ni% c("CH", "MT", "LU", "CH")) %>%
  ggplot(mapping = aes(x = rang, y = `d(k)`)) +
  geom_line(color = "grey25") +
  geom_point(size = 0.2) +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_continuous(trans = "log10", name = "k") +
  theme_bw() +
  theme(axis.text = element_text(size = 5)) +
  labs(caption = "J. Gravier, 2022. Data: TRADEVE DB",
       subtitle = "Rank diversity of European Cities: 1961-2011") +
  facet_wrap(~Country)

ggsave(filename = "ranking_exploration/dk_maincountries_european_cities.png", plot = last_plot(), width = 21, height = 15, units = 'cm')

#### rank change C ####
list_elements <- tradevetwo %>%
  select(-Name:-estnull) %>%
  pivot_longer(cols = rang_1961:rang_2011, names_to = "periods", values_to = "rang") %>%
  arrange(rang, periods) %>%
  group_by(rang) %>%
  mutate(id_davant = lag(x = rowid)) %>%
  mutate(ctt1 = if_else(condition = rowid == id_davant, 0, 1)) %>%
  mutate(ctt1 = if_else(is.na(ctt1), 0, ctt1))
  
list_elements %>%
  rename(k = rang) %>%
  summarise(C = sum(ctt1)/5) %>%
  ggplot(mapping = aes(x = k/nrow(tradevetwo), y = C)) +
  geom_line(color = "grey25") +
  geom_point(size = 0.7) +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw() +
  labs(caption = "J. Gravier, 2022. Data: TRADEVE DB",
       subtitle = "...") 


# par pays
list_elements <- dk_countries_tradeve %>%
  rowid_to_column() %>%
  select(rowid, Country, rang_1961:rang_2011) %>%
  ungroup() %>%
  filter(nchar(x = Country) < 3) %>%
  filter(Country %ni% c("CH", "MT", "LU", "CH")) %>%
  pivot_longer(cols = rang_1961:rang_2011, names_to = "periods", values_to = "rang") %>%
  arrange(Country, rang, periods) %>%
  group_by(Country, rang) %>%
  mutate(id_davant = lag(x = rowid)) %>%
  mutate(ctt1 = if_else(condition = rowid == id_davant, 0, 1)) %>%
  mutate(ctt1 = if_else(is.na(ctt1), 0, ctt1))


list_elements %>%
  rename(k = rang) %>%
  summarise(C = sum(ctt1)/5) %>%
  mutate(est_max = max(k)) %>%
  ggplot(mapping = aes(x = k/est_max, y = C)) +
  geom_line(color = "grey25") +
  geom_point(size = 0.7) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  scale_x_continuous(breaks = c(0, 0.5, 1), name = "k/N_0") +
  theme_bw() +
  labs(caption = "J. Gravier, 2022. Data: TRADEVE DB",
       subtitle = "Rank change in ranking European cities by country: 1961-2011") +
  facet_wrap(~ Country)

ggsave(filename = "ranking_exploration/C_maincountries_european_cities.png", plot = last_plot(), width = 21, height = 15, units = 'cm')


    