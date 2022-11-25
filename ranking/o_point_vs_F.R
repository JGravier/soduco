library(tidyverse)
library(latex2exp)
library(ggpubr)

o_point_tradeve <- read.csv(file = "ranking_exploration/outputs_data/o_point_tradeve.csv", stringsAsFactors = FALSE) %>%
  as_tibble()

flux_proba_tradeve <- read.csv(file = "ranking_exploration/outputs_data/ft_proba_tradeve.csv", stringsAsFactors = FALSE) %>%
  as_tibble()

o_f_data <- o_point_tradeve %>%
  left_join(x = ., y = flux_proba_tradeve, by = c("Country", "N0", "N"))

# visualization O and F with N0/N by countries
o_f_data %>%
  # filter(Ft_proba < 0.10) %>%
  ggplot(aes(x = Ft_proba, y = o_point, color = N0/N, group = N0/N)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_c() +
  theme_bw() +
  xlab(TeX(r"($F$)")) +
  ylab(TeX(r"($\dot{O}$)")) +
  labs(caption = "J. Gravier, 2022. Data: TRADEVE DB") +
  facet_wrap(~Country, scales = "free")

o_f_data <- o_f_data %>% mutate(N0_N = N0/N)

ggscatter(data = o_f_data %>% filter(Ft_proba < 0.10), 
          x = "Ft_proba", y = "o_point", color = "N0_N", alpha = 0.5, add = "reg.line") +
  stat_cor(label.y = 0.045, size = 2) +
  stat_regline_equation(label.y = 0.05, size = 2) +
  scale_color_viridis_c() +
  theme_bw() +
  xlab(TeX(r"($F$)")) +
  ylab(TeX(r"($\dot{O}$)")) +
  labs(caption = "J. Gravier, 2022. Data: TRADEVE DB") +
  facet_wrap(~Country, scales = "free")
