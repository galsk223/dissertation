adj_stock_fcn <- function(choicearray, stockstart, week, fisherylist_use){
  
  temp <- aperm(choicearray, c(2, 1, 3))  # fisheries x vessels x weeks
  dims_temp <- dim(temp)

  # Now reshape to fisheries x (vessels*weeks) and use rowSums
  dim(temp) <- c(dims_temp[1], dims_temp[2] * dims_temp[3])
  stock_sums <- rowSums(temp, na.rm = TRUE)

  # stock_sums <- apply(choicearray, 2, function(x) sum(x, na.rm = TRUE))
  stock_names <- colnames(choicearray)
  
  adj_stockrev <- data.table(FISHERY_ID = stock_names, value = stock_sums)
  adj_stockrev[, LANDING_WEEK := week]
  setkey(adj_stockrev, FISHERY_ID, LANDING_WEEK)
  
  adj_stockrev <- stockstart[adj_stockrev]
  
  # Compute stock FX
  adj_stockrev[, VesselWeeksRemaining_Sim := VesselWeeksTotal - value]
  adj_stockrev[, Stock_FX := fifelse(
    FISHERY_ID == "Not Fishing", 1,
    fifelse(VesselWeeksRemaining_Sim > 0, pmin(1,VesselWeeksRemaining_Sim / lead), 1e-5)
  )]
  
  adj_stockrev <- adj_stockrev[, .(FISHERY_ID, Stock_FX)]
  
}


week <- 1
adj_stock_fcn_meta <- function(choicearray, stockstart, week, fisherylist_use){
  
  stock_sums <- apply(choicearray, 2, function(x) sum(x, na.rm = TRUE))
  stock_names <- colnames(choicearray)
  
  adj_stockrev <- data.table(RegionFishery = stock_names, value = stock_sums)
  adj_stockrev[, LANDING_WEEK := week]
  setkey(adj_stockrev, RegionFishery, LANDING_WEEK)
  
  adj_stockrev <- stockstart[adj_stockrev]
  
  # Compute stock FX
  adj_stockrev[, VesselWeeksRemaining_Sim := VesselWeeksTotal - value]
  adj_stockrev[, Stock_FX := fifelse(
    FISHERY_ID == "Not Fishing", 1,
    fifelse(VesselWeeksRemaining_Sim > 0, pmin(1,VesselWeeksRemaining_Sim / lead), 1e-5)
  )]
  adj_stockrev[, Stock_FX := fifelse(
    is.na(Stock_FX), 1e-5, Stock_FX
  )]
  adj_stockrev[, Stock_FX := fifelse(
    RegionFishery == "Not Fishing", 1, Stock_FX
  )]
  
  adj_stockrev <- adj_stockrev[, .(RegionFishery, Stock_FX)]
  
}

adj_skill_par_fun <- function(v_set, fisherylist_use){
  
  adj_skillrev <- expand_grid(Vessel_ID = v_set$Vessel_ID,
                              FISHERY_ID = c(fisherylist_use, "Not Fishing")) %>% 
    mutate(P1 = runif(nrow(.),-1,10))
  
}

skillmax <- 2.5
adj_skill_fcn <- function(nvessels, nfisheries, v_set, choicearray, fish_levels, 
                          skillrand, adj_skill_parameters){
  
  skill_array <- rowSums(matrix(choicearray, nrow = nrow(v_set) * (nfisheries + 1), ncol = 54), 
                         na.rm = TRUE)
  
  dim(skill_array) <- c(nrow(v_set), (nfisheries + 1))
  adj_skillrev <- as.data.table(skill_array)
  setnames(adj_skillrev, fish_levels)
  adj_skillrev[, Vessel_ID := v_set$Vessel_ID]
  setcolorder(adj_skillrev, "Vessel_ID")
  
  # Step 4: Melt once (this is fast in data.table)
  adj_skillrev <- melt(adj_skillrev, id.vars = "Vessel_ID", variable.name = "FISHERY_ID", 
                       value.name = "Skill_N")
  
  # Step 5: Compute skill effect directly
  if(skillrand == TRUE){
    adj_skillrev <- merge(adj_skillrev, adj_skill_parameters,
      by = c("Vessel_ID", "FISHERY_ID"), all.x = TRUE) 
    
    adj_skillrev[, `:=`(
      Skill_FX = (P1 * Skill_N + 52) / (abs(Skill_N) + 52),
      Vessel_ID = as.numeric(Vessel_ID)
    )]
  } else {
    adj_skillrev[, `:=`(
      Skill_FX = (3 * Skill_N + 52) / (abs(Skill_N) + 52),
      Vessel_ID = as.numeric(Vessel_ID)
    )]
  }
  
  setkey(adj_skillrev, Vessel_ID, FISHERY_ID)
  
}

adj_skill_fcn_iplus <- function(choicearray, nfisheries, v_set, adj_skillrev_iprev, fisherylist_use,
                                skillrand, adj_skill_parameters){

  adj_skillrev_N <- rowSums(matrix(choicearray, nrow = nrow(v_set) * (nfisheries+1), 
                                 ncol = 53), na.rm = T) %>% 
    matrix(nrow = nrow(v_set), ncol = (nfisheries+1),
           dimnames = list(v_set$Vessel_ID, 
                           sort(c(fisherylist_use, "Not Fishing")))) %>% 
    as.data.table(keep.rownames = "Vessel_ID") %>% 
    .[, `:=` (Vessel_ID = as.numeric(Vessel_ID))] %>% 
    melt(id.vars = "Vessel_ID", variable.name = "FISHERY_ID", value.name = "W") %>% 
    merge(adj_skillrev_iprev, by = c("Vessel_ID","FISHERY_ID"), all.x = TRUE) %>% 
    .[, `:=` (AllWeeks = W + Skill_N)]
  
  if(skillrand == T){
    adj_skillrev <- adj_skillrev_N %>% 
      # merge(adj_skill_parameters,
      #       by = c("Vessel_ID", "FISHERY_ID"),all.x = TRUE) %>%
      .[, `:=` (Skill_FX = (P1*(AllWeeks)+52)/(abs(AllWeeks)+52))]
    
  } else {
    adj_skillrev <- adj_skillrev_N %>% 
      .[, `:=` (Skill_FX = (3*(AllWeeks)+52)/(abs(AllWeeks)+52))]
  }
   
  
}
