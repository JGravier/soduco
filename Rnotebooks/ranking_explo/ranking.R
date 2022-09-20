library(tidyverse)
library(readxl)
library(ggpmisc)
library(sf)
library(ggfortify)
library(patchwork)

# function
di_t <- function(xt, xtmoins1){
  x <- abs(xt - xtmoins1)
  return(x)
}

`%ni%` <- Negate(`%in%`)

  
#### tradeve database ####
tradeve <- read_excel(path = "ranking_exploration/TRADEVE_database/TRADEVE_UrbanAreas_Data.xlsx", sheet = "UrbanAreas_Data")

dk_countries_tradeve <- tradeve %>%
  group_by(Country) %>%
  mutate(rang_1961 = rank(desc(Pop_1961)),
         rang_1971 = rank(desc(Pop_1971)),
         rang_1981 = rank(desc(Pop_1981)),
         rang_1991 = rank(desc(Pop_1991)),
         rang_2001 = rank(desc(Pop_2001)),
         rang_2011 = rank(desc(Pop_2011)))

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
  scale_y_continuous(limits = c(0,1), name = expression(d(k))) +
  scale_x_continuous(trans = "log10", name = expression(k)) +
  theme_bw() +
  labs(caption = "J. Gravier, 2022. Data: TRADEVE DB",
       subtitle = "Rank diversity of European Cities: 1961-2011")

ggsave(filename = "ranking_exploration/dk_european_cities.png", plot = last_plot(), width = 15, height = 12, units = 'cm')



#  par pays et non uniquement à l'échelle européenne
dk_countries_tradeve %>%
  rowid_to_column() %>%
  select(rowid, Country, rang_1961:rang_2011) %>%
  ungroup() %>%
  pivot_longer(cols = rang_1961:rang_2011, names_to = "date", values_to = "rang") %>%
  group_by(Country, rang) %>%
  summarise(`d(k)` = n_distinct(rowid)/6,
            distinction = n_distinct(rowid)) %>%
  filter(Country %in% c("DE", "CZ", "ES", "FR", "UK", "IT", "NL", "PL", "RO")) %>%
  ggplot(mapping = aes(x = rang, y = `d(k)`)) +
  geom_line(color = "grey25") +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_continuous(trans = "log10", name = "k") +
  theme_bw() +
  theme(axis.text = element_text(size = 5)) +
  labs(caption = "J. Gravier, 2022. Data: TRADEVE DB",
       subtitle = "Rank diversity of European Cities: 1961-2011") +
  facet_wrap(~Country, scales = "free")

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
  scale_y_continuous(breaks = c(0, 0.5, 1), limits = c(0,1)) +
  scale_x_continuous(breaks = c(0, 0.5, 1), name = "k/N0") +
  theme_bw() +
  labs(caption = "J. Gravier, 2022. Data: TRADEVE DB",
       subtitle = "Rank change in ranking European cities: 1961-2011") 

ggsave(filename = "ranking_exploration/C_european_cities.png", plot = last_plot(), width = 15, height = 12, units = 'cm')


# par pays
list_elements <- dk_countries_tradeve %>%
  rowid_to_column() %>%
  select(rowid, Country, rang_1961:rang_2011) %>%
  ungroup() %>%
  filter(Country %in% c("DE", "CZ", "ES", "FR", "UK", "IT", "NL", "PL", "RO")) %>%
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
  # geom_point(size = 0.7) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  scale_x_continuous(breaks = c(0, 0.5, 1), name = expression(frac(k, N0))) +
  theme_bw() +
  labs(caption = "J. Gravier, 2022. Data: TRADEVE DB",
       subtitle = "Rank change in ranking European cities by country: 1961-2011") +
  facet_wrap(~ Country)

ggsave(filename = "ranking_exploration/C_maincountries_european_cities.png", plot = last_plot(), width = 21, height = 15, units = 'cm')

#### Blumm: x & delta x ####
x_normalized  <- dk_countries_tradeve %>%
  ungroup() %>%
  rowid_to_column() %>%
  mutate(
    x_normalized_1961 = Pop_1961/sum(Pop_1961, na.rm = TRUE),
    x_normalized_1971 = Pop_1971/sum(Pop_1971, na.rm = TRUE),
    x_normalized_1981 = Pop_1981/sum(Pop_1981, na.rm = TRUE),
    x_normalized_1991 = Pop_1991/sum(Pop_1991, na.rm = TRUE),
    x_normalized_2001 = Pop_2001/sum(Pop_2001, na.rm = TRUE),
    x_normalized_2011 = Pop_2011/sum(Pop_2011, na.rm = TRUE)) %>%
  select(rowid, Name, Country, x_normalized_1961:x_normalized_2011) %>%
  pivot_longer(cols = x_normalized_1961:x_normalized_2011, names_to = "periods", values_to = "xnormalized")

x_normalized %>%
  ggplot(mapping = aes(x = xnormalized)) +
  geom_histogram(bins = 100) +
  scale_x_continuous(trans = 'log10', name = expression(paste(x, (i), t, '...',tn))) +
  theme_bw() +
  labs(caption = "J. Gravier, 2022. Data: TRADEVE DB",
       subtitle = "European cities: 1961-2011")

ggsave(filename = "ranking_exploration/distrib_xi.png", plot = last_plot(), width = 16, height = 14, units = 'cm')

blumm_deltax <- dk_countries_tradeve %>%
  ungroup() %>%
  rowid_to_column() %>%
  mutate(delta_1961_1971 = Pop_1971/sum(Pop_1971, na.rm = TRUE) - Pop_1961/sum(Pop_1961, na.rm = TRUE),
         delta_1971_1981 = Pop_1981/sum(Pop_1981, na.rm = TRUE) - Pop_1971/sum(Pop_1971, na.rm = TRUE),
         delta_1981_1991 = Pop_1991/sum(Pop_1991, na.rm = TRUE) - Pop_1981/sum(Pop_1981, na.rm = TRUE),
         delta_1991_2001 = Pop_2001/sum(Pop_2001, na.rm = TRUE) - Pop_1991/sum(Pop_1991, na.rm = TRUE),
         delta_2001_2011 = Pop_2011/sum(Pop_2011, na.rm = TRUE) - Pop_2001/sum(Pop_2001, na.rm = TRUE)) %>%
  pivot_longer(cols = delta_1961_1971:delta_2001_2011, names_to = "periods_intervals", values_to = "deltablumm") %>%
  select(rowid, Name, Country, periods_intervals, deltablumm)


# lier les xit et les delta x
# le truc qui n est pas clair est le lien xit et delta x (faire 2 possibilites de join, en considerant le temps ou non)
all_periods_deltax <-  x_normalized %>%
  left_join(x = ., y = blumm_deltax, by = c("rowid", "Name", "Country"))

classcuting <- classInt::classIntervals(var = log10(all_periods_deltax$xnormalized), n = 100, style = "equal")

all_periods_deltax_summarised <- all_periods_deltax %>%
  mutate(lgxnormalized = log10(xnormalized)) %>%
  mutate(classeslg = cut(x = lgxnormalized, classcuting$brks, include.lowest = TRUE)) %>%
  group_by(classeslg) %>%
  summarise(meanx = mean(xnormalized, na.rm = TRUE),
            sddeltax = sd(deltablumm, na.rm = TRUE),
            vardeltax = var(deltablumm))


# visualisation
all_periods_deltax_summarised %>%
  ggplot(aes(x = meanx, y = sddeltax)) +
  geom_point(alpha = 0.7, color = 'darkorange') +
  scale_x_continuous(trans = 'log10', name = 'x') +
  scale_y_continuous(trans = 'log10', name = expression(paste(sigma, Delta, "x"))) +
  ggpmisc::stat_poly_line(color = "grey40", size = 0.4) +
  ggpmisc::stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                          after_stat(rr.label), sep = "*\", \"*")),
                        size = 2, label.x = "right") +
  theme_bw() +
  labs(caption = "J. Gravier, 2022. Data: TRADEVE DB",
       title = "European cities: 1961-2011", 
       subtitle = bquote("Where" ~ sigma[paste(Delta, 'x|x')] ~ "based on Blumm et al. 2012"))

ggsave(filename = "ranking_exploration/distrib_xit_deltaxt.png", plot = last_plot(),  width = 16, height = 14, units = 'cm')

#### same by country ####
# ce sont les classifications qui doivent etre differentes selon les pays + un filtre initial evidemment
all_periods_deltax_countries <- all_periods_deltax %>%
  filter(Country %in% c("DE", "CZ", "ES", "FR", "UK", "IT", "NL", "PL", "RO"))


all_periods_deltax_c_summarised <- tibble(
  classeslg = factor(),
  meanx = numeric(),
  sddeltax = numeric(),
  vardeltax = numeric(),
  country = character()
)

paysliste <- all_periods_deltax_countries %>%
  select(Country) %>%
  unique()

for (i in 1:nrow(x = paysliste)) {
  
  payslisteextract <- paysliste$Country[i]
  
  datapays <- all_periods_deltax_countries %>%
    filter(Country == payslisteextract)
  
  classcuting <- classInt::classIntervals(var = log10(datapays$xnormalized), n = 100, style = "equal")
  
  all_periods_deltax_c_summarised <- all_periods_deltax_c_summarised %>%
  bind_rows(all_periods_deltax %>%
              mutate(lgxnormalized = log10(xnormalized)) %>%
              mutate(classeslg = cut(x = lgxnormalized, classcuting$brks, include.lowest = TRUE)) %>%
              group_by(classeslg) %>%
              summarise(meanx = mean(xnormalized, na.rm = TRUE),
                        sddeltax = sd(deltablumm, na.rm = TRUE),
                        vardeltax = var(deltablumm)) %>%
              mutate(country = paysliste$Country[i]))
  
}

# visualisation
all_periods_deltax_c_summarised %>%
  ggplot(aes(x = meanx, y = sddeltax)) +
  geom_point(size = 0.5, alpha = 0.7, color = 'darkorange') +
  scale_x_continuous(trans = 'log10', name = 'x') +
  scale_y_continuous(trans = 'log10', name = expression(paste(sigma, Delta, "x"))) +
  ggpmisc::stat_poly_line(color = "grey40", size = 0.4, se = FALSE) +
  ggpmisc::stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                          after_stat(rr.label), sep = "*\", \"*")),
                        size = 2, label.x = "right") +
  theme_bw() +
  labs(caption = "J. Gravier, 2022. Data: TRADEVE DB",
       title = "European cities: 1961-2011", 
       subtitle = bquote("Where" ~ sigma[paste(Delta, 'x|x')] ~ "based on Blumm et al. 2012")) +
  facet_wrap(~ country)

ggsave(filename = "ranking_exploration/distrib_xit_deltaxt-countries.png", plot = last_plot(),
       width = 16, height = 14, units = 'cm')

#### surface plots ####
# je pense qu'ils font varier les bins ici justement

all_periods_deltax %>%
  ggplot(mapping = aes(x = xnormalized, y = deltablumm)) +
  geom_bin2d(bins = 100) +
  scale_fill_viridis_b(option = "magma", direction = -1, n.breaks = 10) +
  scale_x_continuous(trans = "log10", name = "x") +
  ylab(label = expression(paste(Delta, 'x'))) +
  theme_bw() +
  theme(legend.text = element_text(size = 5)) +
  labs(caption = "J. Gravier, 2022. Data: TRADEVE DB",
       title = "European cities: 1961-2011")

ggsave(filename = "ranking_exploration/distrib_xit_deltaxt_surfaceplot.png", plot = last_plot(),
       width = 16, height = 14, units = 'cm')

all_periods_deltax_countries %>%
  ggplot(mapping = aes(x = xnormalized, y = deltablumm)) +
  geom_bin2d(bins = 100, drop = TRUE) +
  scale_fill_viridis_b(option = "magma", direction = -1, n.breaks = 10) +
  scale_x_continuous(trans = "log10", name = "x") +
  ylab(label = expression(paste(Delta, 'x'))) +
  theme_bw() +
  theme(legend.text = element_text(size = 5)) +
  labs(caption = "J. Gravier, 2022. Data: TRADEVE DB",
       title = "European cities: 1961-2011") +
  facet_wrap(~ Country, scales = "free_x")


#### delta x et delta r ####
# par pays
dx_dr_countries <- dk_countries_tradeve %>%
  rowid_to_column() %>%
  mutate(diff_1961_1971 = Pop_1971/sum(Pop_1971, na.rm = TRUE) - Pop_1961/sum(Pop_1961, na.rm = TRUE),
         diff_1971_1981 = Pop_1981/sum(Pop_1981, na.rm = TRUE) - Pop_1971/sum(Pop_1971, na.rm = TRUE),
         diff_1981_1991 = Pop_1991/sum(Pop_1991, na.rm = TRUE) - Pop_1981/sum(Pop_1981, na.rm = TRUE),
         diff_1991_2001 = Pop_2001/sum(Pop_2001, na.rm = TRUE) - Pop_1991/sum(Pop_1991, na.rm = TRUE),
         diff_2001_2011 = Pop_2011/sum(Pop_2011, na.rm = TRUE) - Pop_2001/sum(Pop_2001, na.rm = TRUE)) %>%
  pivot_longer(cols = diff_1961_1971:diff_2001_2011, names_to = "periods", values_to = "deltablumm") %>%
  select(rowid, Name, Country, periods, deltablumm) %>%
  left_join(x = ., y = dk_countries_tradeve %>%
              rowid_to_column() %>%
              mutate(ncities = n()) %>%
              ungroup() %>%
              mutate(diff_1961_1971 = (rang_1971 - rang_1961)/(ncities-1),
                     diff_1971_1981 = (rang_1981 - rang_1971)/(ncities-1),
                     diff_1981_1991 = (rang_1991 - rang_1981)/(ncities-1),
                     diff_1991_2001 = (rang_2001 - rang_1991)/(ncities-1),
                     diff_2001_2011 = (rang_2011 - rang_2001)/(ncities-1)) %>%
              select(rowid, diff_1961_1971:diff_2001_2011) %>%
              pivot_longer(cols = diff_1961_1971:diff_2001_2011, names_to = "periods", values_to = "rangdiff"), 
            by = c("periods", "rowid")) %>%
  mutate(periods = str_sub(string = periods, start = 6, end = 14)) %>%
  filter(Country %in% c("DE", "CZ", "ES", "FR", "UK", "IT", "NL", "PL", "RO"))

dx_dr_countries %>%
  filter(Name != 'Geesthacht') %>%
  ggplot(aes(x = deltablumm, y = rangdiff)) +
  geom_point(size = 0.3, alpha = 0.7, color = 'darkorange') +
  scale_x_continuous(name = expression(paste(Delta, "x"))) +
  scale_y_continuous(name = expression(paste(Delta, "r"))) +
  # ggpmisc::stat_poly_line(color = "grey40", size = 0.2) +
  # ggpmisc::stat_poly_eq(aes(label = paste(after_stat(eq.label),
  #                                         after_stat(rr.label), sep = "*\", \"*")),
  #                       size = 2, label.x = "right") +
  theme_bw() +
  theme(axis.text = element_text(size = 6)) +
  labs(caption = "J. Gravier, 2022. Data: TRADEVE DB") +
  facet_grid(periods~Country, scales = "free")

ggsave(filename = "ranking_exploration/deltax-deltar-countries.png", plot = last_plot(),
       width = 24, height = 21, units = 'cm')

#### quadrants ####
dx_dr_countries <- dx_dr_countries %>%
  mutate(reference = case_when(
    deltablumm > 0 & rangdiff > 0 ~ "Quadrant 1",
    deltablumm > 0 & rangdiff < 0 ~ "Quadrant 2",
    deltablumm < 0 & rangdiff < 0 ~ "Quadrant 3",
    TRUE ~ "Quadrant 4"
  ))

dx_dr_countries %>%
  group_by(Country, periods, reference) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  ggplot(mapping = aes(y = freq, x = periods, fill = reference)) +
  geom_bar(stat = "identity") +
  ggthemes::scale_fill_tableau(palette = "Tableau 10") +
  ylab("Freauency") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 5), axis.title.x = element_blank()) +
  facet_wrap(~Country) +
  labs(caption = "J. Gravier, 2022. Data: TRADEVE DB",
       title = bquote("Distribution of cities according to" ~ paste(Delta, x) ~ 'and' ~ paste(Delta, r)))

ggsave(filename = "ranking_exploration/deltax-deltar-countries-distribution.png", plot = last_plot(),
       width = 21, height = 18, units = 'cm')



