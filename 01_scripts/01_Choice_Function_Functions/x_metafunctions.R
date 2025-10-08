# ext <- graph_ext_fcn(cache_dfchoice[[yi]], fishery_pairs, yi, largestcc = F)
# ful <- graph_fuller_fcn(cache_dfchoice[[yi]], yi, fisherylist_use, largestcc = F)
# 
# extv <- graph_ext_fcn_ves(cache_dfchoice[[yi]], vessel_pairs, yi, largestcc = F)
# fulv <- graph_fuller_fcn_ves(cache_dfchoice[[yi]], yi, fisherylist_use, largestcc = F)
# 
# timetrans <- graph_time_trans(cache_dfchoice, burnin, yi, largestcc)
# timecor <- graph_time_corr(cache_dfchoice, yi, largestcc)
# 
# bip <- graph_bip_fcn(cache_dfchoice[[yi]], yi, nfisheries)
# df_choice <- cache_dfchoice[[yi]]
# 
# view(ext)
# view(ful)
# view(extv)
# view(fulv)
# view(timetrans)
# view(timecor)
# view(bip)
# 
# dc <- df_choice %>% 
#   filter(FISHERY_ID != "Not Fishing",
#          n > 0) %>% 
#   group_by(Vessel_ID) %>% 
#   summarise(NF = n_distinct(FISHERY_ID))
#   
# } else {
#   df_dist <- v_set %>%
#     distinct(LengthBin,HomeRegion) %>%
#     left_join(start$regionadjacency %>%
#                 select(-DistanceCostP) %>% 
#                 mutate(DistanceCost = ifelse(is.na(value), 1, 
#                                              (value+1)*distparameter+1-distparameter)), 
#               by = c("HomeRegion" = "Region"), relationship = "many-to-many") %>%
#     filter(!is.na(HomeRegion)) %>%
#     mutate(DistanceCost = case_when(DistanceCost == 1 ~ 1,
#                                     LengthBin == "10-20" ~ DistanceCost*runif(1,2,3),
#                                     LengthBin == "20-30" ~ DistanceCost*runif(1,1.5,2),
#                                     LengthBin == "30-40" ~ DistanceCost*runif(1,1,1.5),
#                                     LengthBin == "40-50" ~ DistanceCost*runif(1,1,1),
#                                     LengthBin == "50-60" ~ DistanceCost*runif(1,.75,1),
#                                     LengthBin == "60-70" ~ DistanceCost*runif(1,.25,.75)))
# }
# 
# { if (distrand == T) 
#   left_join(.,df_dist, by = c("LengthBin" = "LengthBin",
#                               "HomeRegion" = "HomeRegion",
#                               "Region" = "name"))
#   else 
#     left_join(.,df_dist, by = c("HomeRegion" = "Region",
#                                 "Region" = "name"))
# } %>% 
  
