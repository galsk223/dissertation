# bipmat_0 <- bipmat
fulleredges <- function(connectivity, bipmat_0){
  
  total_weeks <- rowSums(bipmat_0)
  i <- 1
  j <- 2
  for (i in 1:ncol(bipmat_0)) {
    for (j in 1:ncol(bipmat_0)) {
      
      f1 <- colnames(bipmat_0)[i]
      f2 <- colnames(bipmat_0)[j]
      
      if (i != j) {
        sum_k <- 0
        for (k in 1:nrow(bipmat_0)) {
          Wik <- bipmat_0[k, i]
          Wjk <- bipmat_0[k, j]
          Wk <- total_weeks[k]
          if (Wk > 0) {
            sum_k <- sum_k + ((Wik * Wjk) / Wk)*(Wik+Wjk)
          }
        }
        connectivity[f1, f2] <- sum_k[[1]]
      }
    }
  }
  
  out <- (connectivity / nrow(bipmat_0)) %>% 
    as.data.frame() %>%
    rownames_to_column("Fishery1") %>%
    pivot_longer(-Fishery1, names_to = "Fishery2", values_to = "Weight") %>%
    filter(Fishery1 != Fishery2) %>% 
    mutate(V1 = pmin(as.character(Fishery1), as.character(Fishery2)),
           V2 = pmax(as.character(Fishery1), as.character(Fishery2))) %>%
    as.data.table() %>% 
    distinct(V1, V2, Weight) %>%
    rename(Fishery1 = V1, Fishery2 = V2) %>% 
    arrange(Fishery1, Fishery2) 
  
  return(out)
}



fulleredges_vessel <- function(connectivity, bipmat_0, dropfishers){
  
  total_weeks <- rowSums(bipmat_0)
  i <- 1
  j <- 2
  for (i in 1:ncol(bipmat_0)) {
    for (j in 1:ncol(bipmat_0)) {
      
      f1 <- colnames(bipmat_0)[i]
      f2 <- colnames(bipmat_0)[j]
      
      if (i != j) {
        sum_k <- 0
        for (k in 1:nrow(bipmat_0)) {
          Wik <- bipmat_0[k, i]
          Wjk <- bipmat_0[k, j]
          Wk <- total_weeks[k]
          if (Wk > 0) {
            sum_k <- sum_k + ((Wik * Wjk) / Wk)*(Wik+Wjk)
          }
        }
        connectivity[f1, f2] <- sum_k[[1]]
      }
    }
  }
  
  out <- (connectivity / nrow(bipmat_0)) %>% 
    as.data.frame() %>%
    rownames_to_column("Vessel1") %>%
    pivot_longer(-Vessel1, names_to = "Vessel2", values_to = "Weight") %>%
    filter(Vessel1 != Vessel2,
           Vessel1 %in% dropfishers$Vessel_ID) %>% 
    mutate(V1 = pmin(as.character(Vessel1), as.character(Vessel2)),
           V2 = pmax(as.character(Vessel1), as.character(Vessel2))) %>%
    as.data.table() %>% 
    distinct(V1, V2, Weight) %>%
    rename(Vessel1 = V1, Vessel2 = V2) %>% 
    arrange(Vessel1, Vessel2) 
  
  return(out)
}

