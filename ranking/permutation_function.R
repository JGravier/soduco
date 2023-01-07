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

### Second model: jump and diffusion in rank and permutation of elements with a probability <= 0.5
jump_diff_permutation <- function(vector, prob){
  ## probability is linked to permutation as:
  ## proba of diffusion = alpha
  ## proba of jump = 1-alpha
  ## Permutation t-1,t = alpha(1-alpha) ; max proba is 0.25
  vector <- vector %>%
    as_tibble() %>%
    rowid_to_column(var = "id") %>%
    rename(rank = value)
  
  vec2 <- vector %>%
    sample_n(tbl = ., size = prob, replace = FALSE) %>%
    mutate(newvalue = sample(x = vector$rank, size = prob, replace = FALSE))
  
  result <- vector %>%
    mutate(newvalue = rank) %>%
    rows_update(x = ., y = vec2, by = "id") %>%
    mutate(newrank = rank(x = newvalue, ties.method = "last")) %>%
    arrange(newrank)
  
  finalvec <- result$id
  
  return(finalvec)
  
}
