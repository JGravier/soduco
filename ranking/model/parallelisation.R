library(tidyverse)
library(data.table)
library(tidyfast)
library(tidytable) # need to be after tidyverse to maj functions with same name
library(latex2exp)
library(doParallel)
library(foreach)

### begin times
begin <- Sys.time()

# source of functions
source(file = "jump_function.R")


vector_init <- seq(1, 100, 1)
size_choice <- seq(1, 100, 1)
times <- seq(1, 499, 1)

# how many simulation ?
Nsimu <- 5

nb_core <- parallel::detectCores() - 2
clustersdo <- parallel::makeCluster(nb_core)
doParallel::registerDoParallel(clustersdo)

jump_parallelise <- foreach(i = seq_len(Nsimu), .combine = "rbind", .packages = "tidyverse", .multicombine = TRUE) %dopar% 
  {data_jump_result(vec = vector_init, size_c = size_choice, timesT = times)}

stopCluster(clustersdo)

# adding simulation name
simulation <- nrow(jump_parallelise)/Nsimu
jump_parallelise$simulation <- c(0, rep(1:(nrow(jump_parallelise)-1)%/%simulation))

# ending time
ending <- Sys.time()
ending - begin


#### calculs of Ft and F ####
rank_flux_timest <- jump_parallelise %>%
  ungroup() %>%
  mutate(nf = if_else(condition = rank <= size_N0_N & id_unique > size_N0_N, true = 1, false = 0) |
           if_else(condition = rank > size_N0_N & id_unique <= size_N0_N, true = 1, false = 0)) %>%
  mutate(nf = if_else(condition = nf == TRUE, true = 1, false = 0)) %>%
  group_by(simulation, size_N0_N, snapshot_time) %>%
  summarise(nf = sum(nf)) %>%
  mutate(Ft = nf/100)

rank_flux_F <- rank_flux_timest %>%
  filter(snapshot_time != 1) %>% # it is still group by simulation and size
  summarise(F_flux = mean(Ft)) %>%
  mutate(N=100) %>%
  mutate(model = "numerics")


##### visualisation
probagenerale <- 2*vector_init/100*(1-vector_init/100)
probagenerale <- probagenerale %>% as_tibble() %>% mutate(size_N0_N = vector_init)

rank_flux_F %>%
  ungroup() %>%
  bind_rows(probagenerale %>% rename(F_flux = value) %>% mutate(model="analitical", N=100)) %>%
  ggplot(aes(x=size_N0_N/N, y = F_flux, color = model, group=simulation)) +
  geom_line() +
  theme_bw() +
  theme(legend.text = element_text(size = 7)) +
  scale_x_continuous(name = TeX(r"($N_{0}/N$)")) +
  ylab(TeX(r"($F$)")) +
  ggtitle(label = "ps=1, N=100, T=500, Nsim=5")


# no distinction of simulation
rank_flux_F <- rank_flux_timest %>%
  group_by(size_N0_N) %>%
  filter(snapshot_time != 1) %>%
  summarise(F_flux = mean(Ft)) %>%
  mutate(N=100) %>%
  mutate(model = "numerics")

rank_flux_F %>%
  ungroup() %>%
  bind_rows(probagenerale %>% rename(F_flux = value) %>% mutate(model="analitical", N=100)) %>%
  ggplot(aes(x=size_N0_N/N, y = F_flux, color = model)) +
  geom_line() +
  theme_bw() +
  theme(legend.text = element_text(size = 7)) +
  scale_x_continuous(name = TeX(r"($N_{0}/N$)")) +
  ylab(TeX(r"($F$)")) +
  ggtitle(label = "ps=1, N=100, T=500, Nsim=5")