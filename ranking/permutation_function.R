#### functions for permutations for differents models ####

### First model: only jump in rank and permutation of elements (<=> replacement) with a probability <= 0.5
jump_permutation <- function(vector, prob){
  vec <- vector
  
  #### sampling the vector with a probability
  ## probability is linked to jump. If proba = 1, all elements moved in the ranking
  
  # sampling 1:
  vecsamp <- sample(x = vec, size = length(vec)*prob)
  
  # vec for sampling 2: with same proba, but on initial vector vec without the result of vecsamp
  vec2 <- setdiff(x = vec, y = vecsamp) # element of x not in y
  
  # sampling 2:
  vecsamp2 <- sample(x = vec2, size = length(vec)*prob)
  
  # final vector complete permutation
  finalvec <- vec
  
  for (i in 1:length(vec)) {
    
    for (j in 1:length(vecsamp)) {
      
      if (vec[i] == vecsamp[j]) { # is vec[i] is equal to vecsamp[j] ? then replace
        finalvec[i] <- vecsamp2[j]
      } else if (vec[i] == vecsamp2[j]) { # is vec[i] is equal to vecsamp2[j] ? then replace
        finalvec[i] <- vecsamp[j]
      }
      
    }
    
  }
  
  return(finalvec)
  
}

### First model: jump qnd diffusion in rank and permutation of elements with a probability <= 0.5
jump_diff_permutation <- function(vector, prob){
  vec <- vector
  
  #### sampling the vector with a probability
  ## probability is linked to permutation as:
  ## proba of diffusion = alpha
  ## proba of jump = 1-alpha
  ## Permutation t-1,t = alpha(1-alpha) ; max proba is 0.25
  
  # sampling 1:
  vecsamp <- sample(x = vec, size = length(vec)*prob)
  
  # vec for sampling 2: with same proba, but on initial vector vec without the result of vecsamp
  vec2 <- setdiff(x = vec, y = vecsamp) # element of x not in y
  
  # sampling 2:
  vecsamp2 <- sample(x = vec2, size = length(vec)*prob)
  vecsamp2 <- sort(x = vecsamp2, decreasing = FALSE) # need reordering elements
  
  # final vector complete permutation
  finalvec <- vec
  
  for (j in 1:length(vecsamp2)) {
    
    for (i in 1:length(vec)) {
      
      if (vec[i] == vecsamp2[j]) { # is vec[i] is equal to vecsamp2[j] ? then replace
        finalvec[i] <- vecsamp[j]
      } else if (vecsamp[j] > vecsamp2[j] & vec[i] >= vecsamp[j] & vec[i] < vecsamp2[j]) { # case of descending in ranking for diffusion
        finalvec[i] <- vec[i-1]
      } else if (vecsamp[j] < vecsamp2[j] & vec[i] <= vecsamp[j] & vec[i] > vecsamp2[j]) { # case of ascending in ranking for diffusion
        finalvec[i] <- vec[i+1]
      } 
      
    }
    
  }
  
  return(finalvec)
  
}
