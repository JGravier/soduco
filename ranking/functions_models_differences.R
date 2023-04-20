library(data.table)

jump_function <- function(vector_entry, sizeN) {
  
  list_vectors <- list()
  list_vectors[[1]] <- vector_entry
  
  s_init <- sample(x = list_vectors[[1]], size = sizeN, replace = FALSE)
  s_jump_output <- sample(x = list_vectors[[1]], size = sizeN, replace = FALSE)
  
  for (i in 1:length(s_init)) {
    
    vector_init <- list_vectors[[i]]
    rank_init <- list_vectors[[i]]
    vector_output <- list_vectors[[i]]
    rank_output <- list_vectors[[i]]
    
    if (s_init[i] == s_jump_output[i]) {
      ############### do nothing
    } else if (which(vector_init %in% s_jump_output[i]) - which(vector_init %in% s_init[i]) == 1) { # if position output - input = 1
      # is the case when next one to each other but output is > than input
      posinvec_init <- which(vector_init %in% s_init[i])
      posinvec_ouput <- which(vector_init %in% s_jump_output[i])
      # do > permutation
      vector_init[posinvec_ouput] <- s_init[i]
      vector_init[posinvec_init] <- s_jump_output[i]
      
    } else if (which(vector_init %in% s_init[i]) - which(vector_init %in% s_jump_output[i]) == 1) {# inverse
      posinvec_init <- which(vector_init %in% s_init[i])
      posinvec_ouput <- which(vector_init %in% s_jump_output[i])
      # do > permutation
      vector_init[posinvec_ouput] <- s_init[i]
      vector_init[posinvec_init] <- s_jump_output[i]
      
    } else {
      posinvec_init <- which(vector_init %in% s_init[i])
      posinvec_ouput <- which(vector_init %in% s_jump_output[i])
      
      mininvec <- min(posinvec_init, posinvec_ouput)
      maxinvec <- max(posinvec_init, posinvec_ouput)
      
      # update rank for elements with rank between sampling init and output
      if (posinvec_ouput < posinvec_init) {
        maxinvecrevu <- maxinvec-1
        j <- vector_init[mininvec:maxinvecrevu]
        rank_output[mininvec:maxinvecrevu] <- j + 1
        minvec2 <- mininvec+1
        maxinvecrevu2 <- maxinvecrevu+1
        vector_output[minvec2:maxinvecrevu2] <- j 
      } else {
        mininvecrevu <- mininvec+1
        j <- vector_init[mininvecrevu:maxinvec]
        rank_output[mininvecrevu:maxinvec] <- j - 1
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
        # rank_init[vector_init[mininvecrevu:maxinvec]]
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

jump_diff_model_vectors <- function(vectorbase, PS, size) {
  
  # initi
  vector_output <- vectorbase
  
  proba <- PS*size
  
  sampling_init <- sample(x = vector_init, size = proba, replace = FALSE)
  sampling_output <- sample(x = vector_init, size = proba, replace = FALSE)
  
  vector_output[sampling_output] <- vector_output[sampling_init]
  
  vector_output <- data.rable::frank(x = vector_output, ties.method = "last")
  
  return(vector_output)
}