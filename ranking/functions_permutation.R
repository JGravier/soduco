#### functions for  diverse types of permutations ####

### Jump in rank and replacement of other elements only with a max probability = 0.5
jump_replacement_full_replace <- function(vector, prob){
  vec <- vector
  
  #### sampling the vector with a probability
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

### FJump in rank and replacement of elements with prob [0 to 1]
jump_replacement_not_full_replace <- function(vector, prob){
  prob <- prob*length(vector)
  
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
    mutate(newrank = rank(x = newvalue)) %>%
    arrange(newrank) %>%
    mutate(newrank = round(x = newrank, digits = 0)) %>%
    mutate(newrank = if_else(condition = newrank > id, as.numeric(id), newrank))
  
  finalvec <- result$newrank
  
  return(finalvec)
  
}

### Jump elements with diffusion in ranking
jump_diff_permutation <- function(vector, prob){
  prob <- prob*length(vector)
  
  ## initial vector
  vector <- vector %>%
    as_tibble() %>%
    # rowid_to_column(var = "id") %>%
    rename(rank = value) %>%
    mutate(id = rank)
  
  # second vector with proba of jumping in rank list
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

### Diffusion of elements with proba 1/2 i+1 and 1/2 i-1
diff_permutation_after_jump <- function(vector, prob){
  prob <- prob*length(vector)
  
  ## initial vector
  vector <- vector %>%
    as_tibble() %>%
    # rowid_to_column(var = "id") %>%
    rename(rank = value) %>%
    mutate(id = rank)
  
  # second vector with proba of jumping in rank list
  vec2 <- vector %>%
    sample_n(tbl = ., size = prob, replace = FALSE)
  
  vec_proba_plus_1 <- sample(x = vec2$rank, size = 0.5*nrow(vec2), replace = FALSE) %>%
    as_tibble() %>%
    rename(newid = value) %>%
    mutate(newvalue = newid + 1)
  
  vec2 <- vec2 %>%
    left_join(y = vec_proba_plus_1, by = c("rank" = "newid")) %>%
    mutate(newvalue = if_else(is.na(newvalue), rank-1, newvalue))
  
  result <- vector %>%
    mutate(newvalue = rank) %>%
    rows_update(x = ., y = vec2, by = "id") %>%
    mutate(newrank = rank(x = newvalue, ties.method = "last"))
  
  finalvec <- result$newrank
  
  return(finalvec)
}

### Diffusion of elements with proba 1/2 i+1 and 1/2 i-1
diff_permutation <- function(vector, prob){
  prob <- prob*length(vector)
  
  ## initial vector
  vector <- vector %>%
    as_tibble() %>%
    # rowid_to_column(var = "id") %>%
    rename(rank = value) %>%
    mutate(id = rank)
  
  # second vector with proba of jumping in rank list
  vec2 <- vector %>%
    sample_n(tbl = ., size = prob, replace = FALSE)
  
  vec_proba_plus_1 <- sample(x = vec2$rank, size = 0.5*nrow(vec2), replace = FALSE) %>%
    as_tibble() %>%
    rename(newid = value) %>%
    mutate(newvalue = newid + 1)
  
  vec2 <- vec2 %>%
    left_join(y = vec_proba_plus_1, by = c("rank" = "newid")) %>%
    mutate(newvalue = if_else(is.na(newvalue), rank-1, newvalue))
  
  result <- vector %>%
    mutate(newvalue = rank) %>%
    rows_update(x = ., y = vec2, by = "id") %>%
    mutate(newrank = rank(x = newvalue, ties.method = "last")) %>%
    arrange(newrank)
  
  finalvec <- result$id
  
  return(finalvec)
}