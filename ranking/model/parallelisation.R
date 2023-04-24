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
  {data_jump_result(vec = vector_init, size_c = size_choice, timesT = times, ps = 1, pd = 0)}

stopCluster(clustersdo)

# adding simulation name
simulation <- nrow(jump_parallelise)/Nsimu
jump_parallelise$simulation <- c(0, rep(1:(nrow(jump_parallelise)-1)%/%simulation))

# ending time
ending <- Sys.time()
ending - begin
