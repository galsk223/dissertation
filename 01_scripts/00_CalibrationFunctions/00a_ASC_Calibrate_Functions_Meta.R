# Function Setup ----------------------------------------------------------

share_sim_j_meta <- function(delta_costs, j, prep_df_fc, asc_fc_in, asc_sc_in) {
  asc_fc_in[[j]] <- delta_costs
  out <- choices_asc_cal(start, year_ref, vessels_in, fishery_pairs, prep_df_fc, scale_t1ev,  
                         asc_sc = asc_sc_in, asc_fc = asc_fc_in)
  sim_shares <- out$fishing %>%
    select(LengthBin, RegionFishery, Shares)
  r <- left_join(prep_df_fc, sim_shares, by = join_by(LengthBin, RegionFishery)) %>%
    mutate(Shares = replace_na(Shares, 0)) %>%
    group_by(LengthBin) %>% 
    group_split()
  return(r[[j]]$Shares)
}

sim_edges_meta <- function(asc_fc_in, asc_sc_in){
  
  r <- choices_asc_cal(start, year_ref, vessels_in, fishery_pairs, prep_df_fc, scale_t1ev,  
                       asc_sc = asc_sc_in, asc_fc = asc_fc_in)
  
  return(list(ee = r$edges_ext))
  
}

asc_fc_in <- map(asc_fc_0, ~ runif(length(.x),100,1000))
asc_sc_in <- fcn_asc_sc_meta(switching_bench)$asc_sc
obj_fcn1_e_meta <- function(asc_fc_in, asc_sc_in, 
                            switching_bench, targets_lower) {
  
  sim <- sim_edges_meta(asc_fc_in, asc_sc_in) 
  targets_list <- switching_bench %>% 
    select(Fishery1, Fishery2, EdgeExp) %>% 
    filter(EdgeExp > 2) %>% 
    arrange(as.character(Fishery1), as.character(Fishery2)) 
  
  sim_edges <- sim$ee %>%
    semi_join(targets_list, by = c("Fishery1", "Fishery2")) %>% 
    arrange(as.character(Fishery1), as.character(Fishery2)) 
  
  err <- sum((sim_edges$UniqueVessels - targets_list$EdgeExp)^2)
  
  return(err)
  
}

# targets_list <- switching_bench %>% 
#   select(Fishery1, Fishery2, EdgeExp) %>% 
#   filter(EdgeExp > 2) %>% 
#   arrange(as.character(Fishery1), as.character(Fishery2)) 
# sim_edges <- sim$ee %>%
#   semi_join(targets_list, by = c("Fishery1", "Fishery2")) %>% 
#   arrange(as.character(Fishery1), as.character(Fishery2)) 
# switchcomp <- targets_list %>% 
#   left_join(sim_edges, by = c("Fishery1", "Fishery2"))
# plot(switchcomp$EdgeExp, switchcomp$UniqueVessels)
# cor(switchcomp$EdgeExp, switchcomp$UniqueVessels)

# switchcomp <- switching_bench %>% 
#   left_join(sim$ee, by = c("Fishery1", "Fishery2"))
# plot(switchcomp$UniqueVessels.x, switchcomp$UniqueVessels.y)