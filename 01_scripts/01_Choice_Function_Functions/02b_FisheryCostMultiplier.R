# choice <- yc$choice
fisherycostmult <- function(v_set_start, v_set, prep_df_fc, choice){
  
  if(subgraph_use != "Region"){
    pdf <- prep_df_fc %>% 
      rename(FISHERY_ID = RegionFishery)
    fishsets <- left_join(v_set_start, pdf, by = "LengthBin", relationship = "many-to-many") %>% 
      group_by(FISHERY_ID) %>% 
      summarise(NTotal = n_distinct(Vessel_ID), .groups = "drop")
  } else {
    fishsets <- left_join(v_set_start, prep_df_fc, by = "LengthBin", relationship = "many-to-many") %>% 
      group_by(FISHERY_ID) %>% 
      summarise(NTotal = n_distinct(Vessel_ID), .groups = "drop")
  }
  
  multipliers <- as.data.frame(choice) %>% 
    rowid_to_column() %>%
    filter(rowid %in% c(v_set$rowid)) %>% 
    pivot_longer(-rowid, names_to = "Week", values_to = "FISHERY_ID") %>% 
    group_by(FISHERY_ID) %>% 
    summarise(NV = n_distinct(rowid), .groups = "drop") %>% 
    left_join(fishsets, by = join_by(FISHERY_ID)) %>% 
    mutate(Portion = NV/NTotal,
           CostM = 1+3*(1-Portion)^3) %>% 
    mutate(cost_multiplier = ifelse(FISHERY_ID == "Not Fishing", 1, CostM)) %>% 
    select(FISHERY_ID, cost_multiplier) 
  
  otherfisheries <- tibble(FISHERY_ID = setdiff(fishsets$FISHERY_ID, 
                                                multipliers$FISHERY_ID),
                           cost_multiplier = 5)
  
  out <- bind_rows(multipliers, otherfisheries)
  
}


# 
# choice_long <- as.data.frame(yc$choice) %>%
#   mutate(Vessel_ID = v_set$Vessel_ID) %>%
#   pivot_longer(-Vessel_ID, names_to = "Week", values_to = "Fishery_ID")
# 
# rev_long <- as.data.frame(yc$revenue) %>%
#   mutate(Vessel_ID = v_set$Vessel_ID) %>%
#   pivot_longer(-Vessel_ID, names_to = "Week", values_to = "Revenue")
# 
# classdef <- inputs %>% 
#   filter(FISHERY_ID != "Not Fishing") %>% 
#   left_join(yc$adj_skillrev, by = c("Vessel_ID", "FISHERY_ID")) %>% 
#   group_by(LengthBin, Vessel_ID, FISHERY_ID, Skill_FX) %>% 
#   summarise(TotalRevenue = sum(ExpRev, na.rm = TRUE),
#             TotalCosts = sum(FC, na.rm = T), .groups = "drop") %>% 
#   mutate(ExpP = TotalRevenue*Skill_FX-TotalCosts) 
#   
# MaxExp <- classdef %>% 
#   distinct(LengthBin, FISHERY_ID, TotalRevenue)
#   
# revenue_summary <- left_join(choice_long, rev_long, by = c("Vessel_ID", "Week")) %>%
#   filter(Fishery_ID != "Not Fishing") %>% 
#   left_join(v_set, by = "Vessel_ID") %>% 
#   group_by(LengthBin, Fishery_ID) %>%
#   summarise(N = n_distinct(Vessel_ID),
#             MeanRevenue = sum(Revenue, na.rm = TRUE)/N, .groups = "drop") %>% 
#   left_join(MaxExp, by = c("LengthBin", "Fishery_ID" = "FISHERY_ID")) %>% 
#   mutate(P_Reenter = pmin(pmax(
#     MeanRevenue/(TotalRevenue*3) * (.8 / (1 + exp(-0.1 * (.5*N-5))))
#     ,0),1))
# 
# reenters <- classdef %>% 
#   filter(ExpP > 0)
# 
# ReturnP <- yc$adj_skillrev
