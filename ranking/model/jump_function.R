library(purrr)

jump_function <- function(vector_entry, sizeN, ps, pd) {
  
  # probabilities of jumping or diffusion
  ps_size <- ps*sizeN
  pd_size <- pd*sizeN
  
  list_vectors <- list()
  list_vectors[[1]] <- vector_entry
  
  ## sampling jumping
  s_init <- sample(x = list_vectors[[1]], size = ps_size, replace = FALSE)
  s_jump_output <- sample(x = list_vectors[[1]], size = ps_size, replace = FALSE)
  ## sampling diffusion
  s_init_pd <- sample(x = list_vectors[[1]], size = pd_size, replace = FALSE)
  
  
  ### Jumping re-ranking:
  if (purrr::is_empty(s_init)) {
    ### do nothing
  } else {
    for (i in 1:length(s_init)) {
      
      vector_init <- list_vectors[[i]]
      rank_init <- list_vectors[[i]]
      vector_output <- list_vectors[[i]]
      posinvec_init <- which(vector_init %in% s_init[i])
      posinvec_ouput <- which(vector_init %in% s_jump_output[i])
      
      if (s_init[i] == s_jump_output[i]) {
        ############### do nothing
      } else if (which(vector_init %in% s_jump_output[i]) - which(vector_init %in% s_init[i]) == 1) { # if position output - input = 1
        # is the case when next one to each other but output is > than input
        # do > permutation
        vector_init[posinvec_ouput] <- s_init[i]
        vector_init[posinvec_init] <- s_jump_output[i]
        
      } else if (which(vector_init %in% s_init[i]) - which(vector_init %in% s_jump_output[i]) == 1) {# inverse
        # do > permutation
        vector_init[posinvec_ouput] <- s_init[i]
        vector_init[posinvec_init] <- s_jump_output[i]
        
      } else {
        mininvec <- min(posinvec_init, posinvec_ouput)
        maxinvec <- max(posinvec_init, posinvec_ouput)
        
        # update rank for elements with rank between sampling init and output
        if (posinvec_ouput < posinvec_init) {
          maxinvecrevu <- maxinvec-1
          j <- vector_init[mininvec:maxinvecrevu]
          minvec2 <- mininvec+1
          maxinvecrevu2 <- maxinvecrevu+1
          vector_output[minvec2:maxinvecrevu2] <- j 
        } else {
          mininvecrevu <- mininvec+1
          j <- vector_init[mininvecrevu:maxinvec]
          mininvecrevu2 <- mininvecrevu-1
          maxinvec2 <- maxinvec-1
          vector_output[mininvecrevu2:maxinvec2] <- j 
        }
        
        rank_init[which(rank_init %in% s_init[i])] <- s_jump_output[i]
        vector_init[which(vector_init %in% s_jump_output[i])] <- s_init[i]
        
        # update rank list
        if (posinvec_ouput < posinvec_init) {
          mininvecrevu <- mininvec+1
          vector_init[mininvecrevu:maxinvec] <- vector_output[mininvecrevu:maxinvec]
        } else {
          maxinvecrevu <- maxinvec-1
          vector_init[mininvec:maxinvecrevu] <- vector_output[mininvec:maxinvecrevu]
          
        }
        
      }
      
      list_vectors[[i+1]] <- vector_init
      print(paste0("sample jump init: ", s_init[i], "   ----   sample jump output: ", s_jump_output[i]))
      
    }
  }
  
  vector_final_jumping <- list_vectors[[length(s_init)+1]]
  
  
  ### Diffusion re-ranking:
  list_vectors <- list()
  list_vectors[[1]] <- vector_final_jumping
  
  if (purrr::is_empty(s_init_pd)) {
    ### do nothing
  } else {
    for (i in 1:length(s_init_pd)) {
      
      vector_init <- list_vectors[[i]]
      posinvec_init <- which(vector_init %in% s_init_pd[i])
      
      # two possibilities of outputs: + or - 1
      posinvec_ouput <- c(posinvec_init-1, posinvec_init+1)
      # case when initial position in list is min or max: in those case posinvec_output[i] = NA
      if (posinvec_ouput[1] < min(vector_init)) { # case when posinvec init is the first element of the list
        posinvec_ouput <- posinvec_ouput[2]
        replacement <- vector_init[posinvec_ouput]
      } else if (posinvec_ouput[2] > max(vector_init)) { # case when posinvec init is the last element of the list
        posinvec_ouput <- posinvec_ouput[1]
        replacement <- vector_init[posinvec_ouput]
      } else {
        # proba 1/2 to select
        posinvec_ouput <- sample(x = posinvec_ouput, size = 1, replace = FALSE)
        replacement <- vector_init[posinvec_ouput] 
      }
      
      # permutation
      vector_init[posinvec_ouput] <- s_init_pd[i]
      vector_init[posinvec_init] <- replacement
      
      list_vectors[[i+1]] <- vector_init
      print(paste0("sample diffusion init: ", s_init_pd[i], "   ----   sample diffusion output: ", replacement))
      
    }
  }
  
  # output: final vector
  if (length(list_vectors) == 1) {
    vector_final <- vector_final_jumping
  } else {
    vector_final <- list_vectors[[length(s_init_pd)+1]]
  }
  
  return(vector_final)
  
}

jump_function_baser <- function(vectorbase, PS, size) {
  
  # initi
  vector_output <- vectorbase
  
  proba <- PS*size
  
  sampling_init <- sample(x = vector_init, size = proba, replace = FALSE)
  sampling_output <- sample(x = vector_init, size = proba, replace = FALSE)
  
  vector_output[sampling_output] <- vector_output[sampling_init]
  
  vector_output <- frank(x = vector_output, ties.method = "last")
  
  return(vector_output)
}

data_jump_result <- function(vec, size_c, ps, pd, timesT){
  
  vector_init <- vec
  size_choice <- size_c
  times <- timesT
  new_ps <- ps
  new_pd <- pd
  
  liste_size_choicing <- list()
  
  for (s in 1:length(size_choice)) {
    
    liste_outputs <- list()
    liste_outputs[[1]] <- vector_init
    
    for (l in 1:length(times)) {
      liste_outputs[[l+1]] <- jump_function(vector_entry = liste_outputs[[l]], sizeN = size_choice[s], ps = new_ps, pd = new_pd)
    }
    
    liste_size_choicing[[s]] <- liste_outputs
    
  }
  
  # output as tibble
  tible_result <- tibble(id_unique = numeric(), probaS = numeric(), snapshot_time = numeric())
  
  vreplace <- length(times) + 1
  vreplace <- paste0("V", vreplace)
  
  for (l in 1:length(x = liste_size_choicing)) {
    
    tible_result_inter <- as.data.frame(do.call(cbind, liste_size_choicing[[l]])) %>%
      as_tibble() %>%
      pivot_longer(cols = V1:all_of(vreplace), names_to = "V", values_to = "id_unique") %>%
      arrange(V) %>%
      select(-V)
    
    tible_result_inter$snapshot_time <- c(0, rep(1:(nrow(tible_result_inter)-1) %/% length(vector_init))) + 1
    
    tible_result <- tible_result %>%
      bind_rows(
        tible_result_inter %>% 
          mutate(size_N0_N = size_choice[l])
      )
    
  }
  
  tible_result <- tible_result %>%
    group_by(snapshot_time, size_N0_N) %>%
    mutate(rank = row_number())
  
  rank_flux_timest <- tible_result %>%
    ungroup() %>%
    mutate(nf = if_else(condition = rank <= size_N0_N & id_unique > size_N0_N, true = 1, false = 0) |
             if_else(condition = rank > size_N0_N & id_unique <= size_N0_N, true = 1, false = 0)) %>%
    mutate(nf = if_else(condition = nf == TRUE, true = 1, false = 0)) %>%
    group_by(size_N0_N, snapshot_time) %>%
    summarise(nf = sum(nf)) %>%
    mutate(Ft = nf/length(vector_init)) # normalized by N
  
  rank_flux_F <- rank_flux_timest %>%
    group_by(size_N0_N) %>%
    filter(snapshot_time != 1) %>%
    summarise(F_flux = mean(Ft)) %>%
    mutate(N = length(vector_init), ps = new_ps, pd = new_pd)
  
  return(rank_flux_F)
  
}
