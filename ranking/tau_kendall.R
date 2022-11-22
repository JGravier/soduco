library(tidyverse)
library(readxl)
library(latex2exp)
library(patchwork)

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

tau_kendall_countries <- dk_countries_tradeve %>%
  filter(Country %in% c("DE", "CZ", "ES", "FR", "UK", "IT", "NL", "PL", "RO")) %>%
  mutate(
    tau_1961_1971 = VGAM::kendall.tau(x = rang_1961, y = rang_1971, exact = TRUE),
    tau_1971_1981 = VGAM::kendall.tau(x = rang_1971, y = rang_1981, exact = TRUE),
    tau_1981_1991 = VGAM::kendall.tau(x = rang_1981, y = rang_1991, exact = TRUE),
    tau_1991_2001 = VGAM::kendall.tau(x = rang_1991, y = rang_2001, exact = TRUE),
    tau_2001_2011 = VGAM::kendall.tau(x = rang_2001, y = rang_2011, exact = TRUE)
  ) %>%
  select(Country, tau_1961_1971:tau_2001_2011) %>%
  unique()

tau_kendall_countries %>%
  pivot_longer(cols = tau_1961_1971:tau_2001_2011, names_to = "periods", values_to = "kendall") %>%
  mutate(periods = str_sub(string = periods, start = 5)) %>%
  ggplot(aes(x = periods, y = kendall, color= Country, group= Country)) +
  geom_point() +
  geom_line() +
  ggthemes::scale_color_tableau(palette = "Tableau 10") +
  theme_bw() +
  # theme(axis.title.x = element_blank()) +
  ylab(expression(tau)) +
  labs(caption = "J. Gravier, 2022. Data: TRADEVE DB",
       title = "Kendall rank correlation coefficient", subtitle = "Complete closed systems")


#### Kendall tau metric considering N0 ####
kendall_output <- tibble(Country = character(), n = numeric(), tau = numeric(), sdtau = numeric())

N0 <- seq(2, 768, 1)

for (i in 1:length(x = N0)) {
  tableau <- dk_countries_tradeve %>%
    filter(Country %in% c("DE", "CZ", "ES", "FR", "UK", "IT", "NL", "PL", "RO")) %>%
    select(Country, rang_1961:rang_2011)
  
  # period 1
  calcul_kendall1 <- tableau %>%
    filter(rang_1961 <= N0[i]) %>%
    mutate(tau_1961_1971 = VGAM::kendall.tau(x = rang_1961, y = rang_1971, exact = TRUE)) %>%
    select(Country, tau_1961_1971) %>%
    unique()
  # period 2
  calcul_kendall2 <- tableau %>%
    filter(rang_1971 <= N0[i]) %>%
    mutate(tau_1971_1981 = VGAM::kendall.tau(x = rang_1971, y = rang_1981, exact = TRUE)) %>%
    select(Country, tau_1971_1981) %>%
    unique()
  # period 3
  calcul_kendall3 <- tableau %>%
    filter(rang_1981 <= N0[i]) %>%
    mutate(tau_1981_1991 = VGAM::kendall.tau(x = rang_1981, y = rang_1991, exact = TRUE)) %>%
    select(Country, tau_1981_1991) %>%
    unique()
  # period 4
  calcul_kendall4 <- tableau %>%
    filter(rang_1991 <= N0[i]) %>%
    mutate(tau_1991_2001 = VGAM::kendall.tau(x = rang_1991, y = rang_2001, exact = TRUE)) %>%
    select(Country, tau_1991_2001) %>%
    unique()
  # period 5
  calcul_kendall5 <- tableau %>%
    filter(rang_2001 <= N0[i]) %>%
    mutate(tau_2001_2011 = VGAM::kendall.tau(x = rang_2001, y = rang_2011, exact = TRUE)) %>%
    select(Country, tau_2001_2011) %>%
    unique()
  
  tableau_bind_col <- calcul_kendall1 %>%
    left_join(calcul_kendall2, by = "Country") %>%
    left_join(calcul_kendall3, by = "Country") %>%
    left_join(calcul_kendall4, by = "Country") %>%
    left_join(calcul_kendall5, by = "Country") %>%
    mutate(n = N0[i]) %>%
    pivot_longer(cols = tau_1961_1971:tau_2001_2011, names_to = "periods", values_to = "kendall") %>%
    mutate(periods = str_sub(string = periods, start = 5)) %>%
    group_by(Country, n) %>%
    summarise(tau = mean(kendall, na.rm = TRUE), sdtau = sd(kendall, na.rm = TRUE))
  
  print(i)
  print(N0[i])
  
  kendall_output <- kendall_output %>%
    bind_rows(tableau_bind_col)
  
}

no_max_countries <- dk_countries_tradeve %>%
  filter(Country %in% c("DE", "CZ", "ES", "FR", "UK", "IT", "NL", "PL", "RO"))  %>%
  summarise(N = n())

kendall_output %>%
  left_join(x = ., y = no_max_countries, by = "Country") %>%
  filter(n <= N) %>%
  ggplot(aes(n, tau, group = Country, color = Country)) +
  geom_line() +
  geom_point(size = 0.8, alpha = 0.5) +
  ggthemes::scale_color_tableau(palette = "Tableau 10") +
  theme_bw() +
  scale_x_continuous(name = TeX(r"($N_{0}$)"), trans = "log10") +
  ylab(TeX(r"($\bar{\tau}$)")) +
  labs(caption = "J. Gravier, 2022. Data: TRADEVE DB")

# sortie rapide
tau_kendall_countries %>%
  pivot_longer(cols = tau_1961_1971:tau_2001_2011, names_to = "periods", values_to = "kendall") %>%
  mutate(periods = str_sub(string = periods, start = 5)) %>%
  ggplot(aes(x = periods, y = kendall, color= Country, group= Country)) +
  geom_point(show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  ggthemes::scale_color_tableau(palette = "Tableau 10") +
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  ylab(expression(tau)) +
  labs(title = "Kendall rank correlation coefficient", subtitle = "Complete closed systems") +
  kendall_output %>%
  left_join(x = ., y = no_max_countries, by = "Country") %>%
  filter(n <= N) %>%
  ggplot(aes(n, tau, group = Country, color = Country)) +
  geom_line() +
  ggthemes::scale_color_tableau(palette = "Tableau 10") +
  theme_bw() +
  scale_x_continuous(name = TeX(r"($N_{0}$)"), trans = "log10") +
  ylab(TeX(r"($\bar{\tau}$)")) +
  labs(caption = "J. Gravier, 2022. Data: TRADEVE DB", subtitle = "Open systems")

ggsave(filename = "ranking_exploration/kendall_tradeve.png", plot = last_plot(), 
       width = 26, height = 12, units = 'cm')
