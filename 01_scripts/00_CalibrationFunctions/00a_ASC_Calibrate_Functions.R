# Function Setup ----------------------------------------------------------

share_sim_j <- function(f, delta_costs, j, asc_fc_in, asc_sc_in) {
  asc_fc_in[[j]] <- delta_costs
  out <- choices_asc_cal(start = start, scale_t1ev = scalet1ev, vessels_in = 200, stochastic = F,
                         region = region, year = year,
                         prep_df_fc = asc_fc_blanks$prep_df_fc, asc_fc = asc_fc_in,
                         prep_sc_mat = asc_sc_blanks$prep_sc_mat, asc_sc = asc_sc_in)
  sim_shares <- out$fishing %>%
    select(LengthBin, FISHERY_ID, Shares)
  r <- left_join(asc_fc_blanks$prep_df_fc, sim_shares, by = join_by(LengthBin, FISHERY_ID)) %>%
    mutate(Shares = replace_na(Shares, 0)) %>%
    group_by(LengthBin) %>% 
    group_split()
  return(r[[j]]$Shares)
}

sim_edges <- function(asc_fc_in, asc_sc_in){
  
  r <- choices_asc_cal(start = start, scale_t1ev = scalet1ev, vessels_in = 200, stochastic = F,
                       region = region, year = year,
                       prep_df_fc = asc_fc_blanks$prep_df_fc, asc_fc = asc_fc_in,
                       prep_sc_mat = asc_sc_blanks$prep_sc_mat, asc_sc = asc_sc_in)
  
  return(list(ef = r$edges_fuller,
              ee = r$edges_ext))
  
}

# sim_edges(asc_fc_in, asc_sc_in)

obj_fcn1_f <- function(asc_sc_in) {
  
  sim <- sim_edges(asc_fc_in, asc_sc_in) 
  err <- sum((sim$ef$Weight - targets_ef)^2)
  
  return(err)
  
}

obj_fcn1_e <- function(asc_sc_in) {
  
  sim <- sim_edges(asc_fc_in, asc_sc_in) 
  sim_lower <- sim$ee %>%
    filter(Fishery1 %in% fisherylist_use,
           Fishery2 %in% fisherylist_use) %>%
    arrange(as.character(Fishery1), as.character(Fishery2)) %>%
    pivot_wider(names_from = Fishery2, values_from = UniqueVessels) %>%
    column_to_rownames("Fishery1") %>%
    select(sort(colnames(.))[[1]], everything()) %>%
    as.matrix()
  sim_lower <- sim_lower[lower.tri(sim_lower)]
  
  err <- sum((sim_lower - targets_lower)^2)
  
  return(err)
  
}
