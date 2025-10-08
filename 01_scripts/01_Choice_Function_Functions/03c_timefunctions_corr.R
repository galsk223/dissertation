coredges <- function(cache_dfchoice, yi){
  
  effort_wide <- bind_rows(cache_dfchoice[[yi-4]],
                           cache_dfchoice[[yi-3]],
                           cache_dfchoice[[yi-2]],
                           cache_dfchoice[[yi-1]],
                           cache_dfchoice[[yi]], .id = "source") %>%
    filter(FISHERY_ID != "Not Fishing") %>%
    group_by(FISHERY_ID, source) %>%
    tally() %>%
    ungroup() %>%
    pivot_wider(names_from = source, values_from = n, values_fill = 0) %>%
    arrange(FISHERY_ID)
  
  effort_mat <- effort_wide %>%
    select(-FISHERY_ID) %>%
    as.matrix()
  rownames(effort_mat) <- effort_wide$FISHERY_ID
  effort_mat <- effort_mat[apply(effort_mat, 1, sd) != 0, ]
  
  # Compute pairwise correlations (Pearson by default)
  cor_mat <- cor(t(effort_mat)) # transpose so fisheries are columns
  
  A_pos <- as.data.frame(as.table(cor_mat*(cor_mat > 0))) %>%
    rename(Fishery1 = Var1, Fishery2 = Var2, weight = Freq) %>%
    filter(Fishery1 != Fishery2) %>% 
    mutate(V1 = pmin(as.character(Fishery1), as.character(Fishery2)),
           V2 = pmax(as.character(Fishery1), as.character(Fishery2))) %>%
    as.data.table() %>% 
    distinct(V1, V2, weight) %>%
    rename(Fishery1 = V1, Fishery2 = V2) %>% 
    arrange(Fishery1, Fishery2) 
  
}

