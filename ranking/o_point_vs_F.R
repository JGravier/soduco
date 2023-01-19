library(tidyverse)
library(latex2exp)
library(ggpubr)
library(ggrepel)


#### tradeve DB ####
o_point_tradeve <- read.csv(file = "outputs_data/o_point_tradeve.csv", stringsAsFactors = FALSE) %>%
  as_tibble()

flux_proba_tradeve <- read.csv(file = "outputs_data/Flux_mean_t_proba_tradeve.csv", stringsAsFactors = FALSE) %>%
  as_tibble()

o_f_data_tradeve <- o_point_tradeve %>%
  left_join(x = ., y = flux_proba_tradeve, by = c("Country", "N0", "N")) %>% mutate(`N0/N` = N0/N)

# visualization O and F with N0/N by countries
o_f_data_tradeve %>%
  # filter(Ft_proba < 0.10) %>%
  ggplot(aes(x = Ft_proba, y = o_point, color = N0/N, group = N0/N)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_c() +
  theme_bw() +
  xlab(TeX(r"($F$)")) +
  ylab(TeX(r"($\dot{O}$)")) +
  labs(caption = "J. Gravier, 2022. Data: TRADEVE DB") +
  facet_wrap(~Country, scales = "free")

ggscatter(data = o_f_data_tradeve, 
          x = "Ft_proba", y = "o_point", color = "N0/N", alpha = 0.5, add = "reg.line") +
  stat_cor(label.y = 0.045, size = 2) +
  stat_regline_equation(label.y = 0.05, size = 2) +
  geom_smooth(se = FALSE, method = "lm", color = "grey40") +
  scale_color_viridis_c() +
  theme_bw() +
  xlab(TeX(r"($F$)")) +
  ylab(TeX(r"($\dot{O}$)")) +
  labs(caption = "J. Gravier, 2022. Data: TRADEVE DB") +
  facet_wrap(~Country, scales = "free")

ggsave(filename = "F_O_point_N0onN_countries.png", plot = last_plot(),
       width = 23, height = 20, units = 'cm')


# visu considering random point selection
ggscatter(data = o_f_data_tradeve %>% sample_frac(size = 0.68), 
          x = "Ft_proba", y = "o_point", color = "N0/N", alpha = 0.5, add = "reg.line") +
  stat_cor(label.y = 0.07, size = 2) +
  stat_regline_equation(label.y = 0.06, size = 2) +
  geom_smooth(se = FALSE, method = "lm", color = "grey40") +
  scale_color_viridis_c() +
  theme_bw() +
  xlab(TeX(r"($F$)")) +
  ylab(TeX(r"($\dot{O}$)")) +
  labs(caption = "J. Gravier, 2022. Data: TRADEVE DB")

ggsave(filename = "F_O_point_N0onN_countries_sample.png", plot = last_plot(),
       width = 18, height = 15, units = 'cm')

# maximisation of dot o
o_f_data_tradeve %>%
  group_by(Country) %>%
  mutate(maximisation = max(o_point)) %>%
  mutate(estmax = if_else(maximisation == o_point, TRUE, FALSE)) %>%
  filter(estmax == TRUE) %>%
  ungroup() %>%
  ggplot() +
  geom_point(aes(x = Ft_proba, y = o_point, color = Country, size = `N0/N`)) +
  geom_text(aes(x = Ft_proba, y = o_point, label = round(`N0/N`, digits = 2)), hjust = 0, vjust = 1, size = 2.5) +
  ggthemes::scale_color_tableau(palette = "Tableau 10") +
  theme_bw() +
  xlab(TeX(r"($F$)")) +
  scale_y_continuous(TeX(r"($\dot{O}$)"), limits = c(0, 0.20)) +
  labs(caption = "J. Gravier, 2022. Data: TRADEVE DB")

ggsave(filename = "F_O_point_N0onN_countries_maximisationdoto.png", plot = last_plot(),
       width = 18, height = 15, units = 'cm')

# maximisation of dot o 2nd version
o_f_data_tradeve %>%
  filter(`N0/N` > 0.10) %>%
  group_by(Country) %>%
  mutate(maximisation = max(o_point)) %>%
  mutate(estmax = if_else(maximisation == o_point, TRUE, FALSE)) %>%
  filter(estmax == TRUE) %>%
  ungroup() %>%
  ggplot() +
  geom_point(aes(x = Ft_proba, y = o_point, color = Country, size = `N0/N`)) +
  geom_text(aes(x = Ft_proba, y = o_point, label = round(`N0/N`, digits = 2)), hjust = 0, vjust = 1, size = 2.5) +
  ggthemes::scale_color_tableau(palette = "Tableau 10") +
  theme_bw() +
  xlab(TeX(r"($F$)")) +
  scale_y_continuous(TeX(r"($\dot{O}$)"), limits = c(0, 0.075)) +
  labs(caption = "J. Gravier, 2022. Data: TRADEVE DB")

ggsave(filename = "F_O_point_N0onN_countries_maximisationdoto_v2.png", plot = last_plot(),
       width = 18, height = 15, units = 'cm')


#### shangai DB ####
o_point_shangai <- read.csv(file = "ranking_exploration/outputs_data/o_point_shangai.csv", stringsAsFactors = FALSE) %>%
  as_tibble()

flux_proba_shangai <- read.csv(file = "ranking_exploration/outputs_data/ft_proba_shangai.csv", stringsAsFactors = FALSE) %>%
  as_tibble()

o_f_data_shangai <- o_point_shangai %>%
  left_join(x = ., y = flux_proba_shangai, by = c("N0", "N")) %>%
  mutate(`N0/N` = N0/N)

# visualization of F and o dot in scatter reg
ggscatter(data = o_f_data_shangai, 
          x = "Ft_proba", y = "o_point", color = "N0/N", alpha = 0.5, add = "reg.line") +
  stat_cor(label.y = 0.045, size = 2) +
  stat_regline_equation(label.y = 0.05, size = 2) +
  geom_smooth(se = FALSE, method = "lm", color = "grey40") +
  scale_color_viridis_c() +
  theme_bw() +
  xlab(TeX(r"($F$)")) +
  ylab(TeX(r"($\dot{O}$)")) +
  labs(caption = "J. Gravier, 2022. Data: Shangai World University Ranking")

ggsave(filename = "ranking_exploration/F_O_point_N0onN_shangai.png", plot = last_plot(),
       width = 18, height = 15, units = 'cm')

#### comparing: O vs F ####
o_f_datasets <- o_f_data_tradeve %>%
  select(-t_1, -t_6) %>%
  bind_rows(o_f_data_shangai %>%
              select(-t_1, -t_14)) %>%
  mutate(Country = if_else(condition = is.na(Country), true = "University", false = Country))


ggscatter(data = o_f_datasets, 
          x = "Ft_proba", y = "o_point", color = "N0/N", alpha = 0.5, add = "reg.line") +
  stat_cor(label.y = 0.045, size = 2) +
  stat_regline_equation(label.y = 0.05, size = 2) +
  geom_smooth(se = FALSE, method = "lm", color = "grey40") +
  scale_color_viridis_c() +
  theme_bw() +
  xlab(TeX(r"($F$)")) +
  ylab(TeX(r"($\dot{O}$)")) +
  labs(caption = "J. Gravier, 2022. Data: TRADEVE DB") +
  facet_wrap(~Country, scales = "free")

ggsave(filename = "ranking_exploration/F_O_point_N0onN_comparing.png", plot = last_plot(),
       width = 23, height = 20, units = 'cm')


#### comparing: maximisation ####
o_f_datasets %>%
  filter(`N0/N` > 0.25) %>%
  group_by(Country) %>%
  mutate(maximisation = max(o_point)) %>%
  mutate(estmax = if_else(maximisation == o_point, TRUE, FALSE)) %>%
  filter(estmax == TRUE) %>%
  ungroup() %>%
  ggplot() +
  geom_point(aes(x = Ft_proba, y = o_point, color = Country, size = `N0/N`)) +
  geom_text(aes(x = Ft_proba, y = o_point, label = round(`N0/N`, digits = 3)), hjust = 0, vjust = 1, size = 2.5) +
  ggthemes::scale_color_tableau(palette = "Color Blind") +
  theme_bw() +
  xlab(TeX(r"($F$)")) +
  scale_y_continuous(TeX(r"($\dot{O}$)"), limits = c(0, 0.075)) +
  labs(caption = "J. Gravier, 2022.")

ggsave(filename = "ranking_exploration/F_O_point_N0onN_countries_maximisationdoto_datasets.png", plot = last_plot(),
       width = 18, height = 15, units = 'cm')



