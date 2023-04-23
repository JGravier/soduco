jump_function <- function(vector_entry, sizeN) {
  
  list_vectors <- list()
  list_vectors[[1]] <- vector_entry
  
  s_init <- sample(x = list_vectors[[1]], size = sizeN, replace = FALSE)
  s_jump_output <- sample(x = list_vectors[[1]], size = sizeN, replace = FALSE)
  
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
    print(paste0("sample init: ", s_init[i], "   ----   sample output: ", s_jump_output[i]))
    
  }
  
  vector_final <- list_vectors[[length(s_init)+1]]
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

data_jump_result <- function(vec, size_c, timesT){
  
  liste_size_choicing <- list()
  
  for (s in 1:length(size_choice)) {
    
    liste_outputs <- list()
    liste_outputs[[1]] <- vector_init
    
    for (l in 1:length(times)) {
      liste_outputs[[l+1]] <- jump_function(vector_entry = liste_outputs[[l]], sizeN = size_choice[s])
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
    
    tible_result_inter$snapshot_time <- c(0, rep(1:(nrow(tible_result_inter)-1) %/% 100)) + 1
    
    tible_result <- tible_result %>%
      bind_rows(
        tible_result_inter %>% 
          mutate(probaS = 1, 
                 size_N0_N = size_choice[l])
      )
    
  }
  
  tible_result <- tible_result %>%
    group_by(snapshot_time, size_N0_N) %>%
    mutate(rank = row_number())
  
  return(tible_result)
  
}