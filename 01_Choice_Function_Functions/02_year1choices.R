year1choices <- function(v_set, inputs, sc_vessel, stockstart, skillrand, adj_skill_parameters,
                         nfisheries, fisherylist_use){
  
  fish_levels <- sort(c(fisherylist_use, "Not Fishing"))
  choice_levels <- factor(levels = fish_levels)
  choice <- matrix(nrow = nrow(v_set),
                   ncol = max(inputs$LANDING_WEEK))
  revenue <- matrix(nrow = nrow(v_set),
                    ncol = max(inputs$LANDING_WEEK))
  lastnf <- matrix(nrow = nrow(v_set),
                   ncol = 1)
  choicearray <- array(0, dim = c(nrow(v_set),nfisheries+1,max(inputs$LANDING_WEEK)+1),
                       dimnames = list(
                         rows = v_set$Vessel_ID,
                         cols = sort(c(fisherylist_use, "Not Fishing")),
                         slices = 1:(max(inputs$LANDING_WEEK)+1)))
  week_mat <- matrix(0, nrow = nrow(v_set), ncol = length(fish_levels))
  colnames(week_mat) <- fish_levels
  
  w1 <- inputs %>% 
    filter(LANDING_WEEK == 1) %>% 
    group_by(Vessel_ID) %>%
    filter(SubTotal == max(SubTotal, na.rm = T)) %>%
    ungroup() %>% 
    mutate(W = 1) %>% 
    arrange(Vessel_ID)
  
  if(skillrand == TRUE){
    
    m <- expand_grid(LengthBin = unique(v_set$LengthBin),
                       Fisheries = c(fisherylist_use, "Not Fishing")) %>% 
      mutate(Starting = runif(nrow(.), -100,200)) %>% 
      right_join(v_set, by = join_by(LengthBin), relationship = "many-to-many") %>% 
      { if (subgraph_use != "Region") 
        mutate(., Starting = ifelse(str_detect(Fisheries, HomeRegion), Starting, 1)) 
        else . 
      } %>% 
      select(Vessel_ID, Fisheries, Starting) %>% 
      pivot_wider(names_from = Fisheries,
                  values_from = Starting) %>% 
      select(-Vessel_ID) %>% 
      as.matrix()
    
    choicearray[,,1] <- m
    
    adj_skillrev <- adj_skill_fcn(nvessels, nfisheries, v_set, choicearray, fish_levels, 
                                  skillrand, adj_skill_parameters) 
    
    w1 <- inputs %>% 
      filter(LANDING_WEEK == 1) %>% 
      left_join(adj_skillrev, by = join_by(Vessel_ID, FISHERY_ID)) %>%
      mutate(Total = ExpRev * Skill_FX - FC + T1EV) %>% 
      group_by(Vessel_ID) %>%
      filter(Total == max(Total, na.rm = T)) %>%
      ungroup() %>% 
      mutate(W = 1) %>% 
      arrange(Vessel_ID)
    
  }
  
  revenue[,1] <- w1$SubTotal
  choice[,1] <- w1$FISHERY_ID
  lastnf <- w1$FISHERY_ID
  
  week_mat <- model.matrix(~ factor(choice[,1],
                                    levels = fish_levels) - 1)
  choicearray[,,2] <- week_mat
  
  adj_skillrev <- adj_skill_fcn(nvessels, nfisheries, v_set, choicearray, fish_levels, 
                                skillrand, adj_skill_parameters) 
  adj_stockrev <- adj_stock_fcn(choicearray[,,2:3], stockstart, 1, fisherylist_use)
  
  # s1 <- list()
  # s2 <- list()
  # s3 <- list()
  # s4 <- list()
  # s5 <- list()
  # s6 <- list()
  # # Year 1 Week 2-53
  setkey(inputs, Vessel_ID, LANDING_WEEK)
  setkey(sc_vessel, Fishery1, Fishery2)
  setkey(adj_skillrev, Vessel_ID, FISHERY_ID)
  setkey(adj_stockrev, FISHERY_ID)
  
  # inputs_by_week <- split(inputs, inputs$LANDING_WEEK)
  
  i <- 2
  # s <- Sys.time()
  for (i in 2:max(inputs$LANDING_WEEK)){
    
    # s1[[i-1]] <- Sys.time()
    # 
    # # NEW
    # vessel_order <- order(v_set$Vessel_ID)
    # dt <- data.table(Vessel_ID = v_set$Vessel_ID[vessel_order],
    #                  Prev = lastnf[vessel_order])
    # setkey(dt, Vessel_ID)
    # 
    # # Merge with week's input data
    # week_data <- inputs_by_week[[as.character(i)]]
    # dt <- dt[week_data, on = "Vessel_ID"]  # Left join
    # dt <- dt[sc_vessel, on = .(Prev = Fishery1, FISHERY_ID = Fishery2), nomatch = NA]  #
    # 
    # # Merge with skill effects
    # setkey(dt, Vessel_ID, FISHERY_ID)
    # dt <- adj_skillrev[dt]
    # 
    # # Merge with stock effects
    # setkey(dt, FISHERY_ID)
    # dt <- adj_stockrev[dt]
    # 
    # dt <- dt[order(Vessel_ID, FISHERY_ID)] %>%
      # filter(!is.na(Vessel_ID))
    
    # OLD
    dt <- data.table(Vessel_ID = sort(v_set$Vessel_ID),
                      Prev = lastnf) %>%
      merge(inputs %>%
              filter(LANDING_WEEK == i), by = "Vessel_ID", all.x = TRUE) %>%
      merge(sc_vessel, by.x = c("Prev", "FISHERY_ID"),
            by.y = c("Fishery1", "Fishery2"), all.x = TRUE) %>%
      merge(adj_skillrev, by = c("Vessel_ID","FISHERY_ID"), all.x = TRUE) %>%
      merge(adj_stockrev, by = c("FISHERY_ID"), all.x = TRUE)
    # dto <- dto[order(Vessel_ID, FISHERY_ID)]
    
    # common_cols <- intersect(colnames(dt_sorted), colnames(dto_sorted))
    # dt_sorted <- dt[, ..common_cols]
    # dto_sorted <- dto[, ..common_cols]
    # waldo::compare(dt_sorted,
    #                dto_sorted)
    
    # %>%
    #   .[, `:=`(Total = ExpRev * Skill_FX * Stock_FX - DistanceCost * FC + T1EV - SC)] %>%
    #   .[!is.na(Total)] %>%
    #   .[, .SD[which.max(Total)], by = Vessel_ID] %>%
    #   .[, W := 1] %>% .[order(Vessel_ID)]

    # s2[[i-1]] <- Sys.time()
    
    if (length(fisherylist_use) <= 8){
      dt$DistanceCost <- rep(1, nrow(dt))
    } 
    
    path_choice <- dt %>%
        .[, `:=`(Total = ExpRev * Skill_FX * Stock_FX - DistanceCost * FC + T1EV - SC)] %>%
        .[!is.na(Total)] %>%
        .[, .SD[which.max(Total)], by = Vessel_ID] %>%
        .[, W := 1] %>% .[order(Vessel_ID)]
    
    # res <- choose_best(
    #   vessel   = dt$Vessel_ID,
    #   fishery  = dt$FISHERY_ID,
    #   ExpRev   = dt$ExpRev,
    #   DistanceCost = dt$DistanceCost,
    #   Skill_FX = dt$Skill_FX,
    #   Stock_FX = dt$Stock_FX,
    #   FC       = dt$FC,
    #   T1EV     = dt$T1EV,
    #   SC       = dt$SC
    # )
    # path_choice <- data.table(
    #   Vessel_ID  = res$Vessel_ID,
    #   FISHERY_ID = res$FISHERY_ID,
    #   Total      = res$Total,
    #   W          = i
    # )[order(Vessel_ID)]

    # s3[[i-1]] <- Sys.time()
    
    revenue[,i] <- path_choice$Total
    choice[,i] <- path_choice$FISHERY_ID
    prevfish <- choice[,i]
    prevfish[prevfish == "Not Fishing"] <- lastnf[prevfish == "Not Fishing"]
    lastnf <- prevfish
    
    week_mat <- model.matrix(~ factor(choice[,i],
                                      levels = fish_levels) - 1)
    choicearray[,,i+1] <- week_mat
    
    # s4[[i-1]] <- Sys.time()
      adj_skillrev <- adj_skill_fcn(nvessels, nfisheries, v_set, choicearray, fish_levels,
                                  skillrand, adj_skill_parameters) %>%
        mutate(Year = 1)
      setkey(adj_skillrev, Vessel_ID, FISHERY_ID)
    # s5[[i-1]] <- Sys.time() 
      adj_stockrev <- adj_stock_fcn(choicearray[,,2:(i+1)], stockstart, week = i, fisherylist_use)
    # s6[[i-1]] <- Sys.time()
  }
  
  adj_skillrev_carry <- list()
  adj_skillrev_carry[[1]] <- as_tibble(adj_skillrev)
  return(list(choice = choice, lastnf = lastnf,
              revenue = revenue, adj_skillrev_carry = adj_skillrev_carry))
  }


# adj_skillrev <- rowSums(matrix(choicearray, nrow = nrow(v_set) * (nfisheries+1),
#                                ncol = max(inputs$LANDING_WEEK)), na.rm = T) %>%
#   matrix(nrow = nrow(v_set), ncol = (nfisheries+1),
#          dimnames = list(v_set$Vessel_ID,
#                          sort(c(fisherylist_use, "Not Fishing")))) %>%
#   as.data.table(keep.rownames = "Vessel_ID") %>%
#   melt(id.vars = "Vessel_ID", variable.name = "FISHERY_ID", value.name = "W") %>%
#   .[, keyby = .(Vessel_ID, FISHERY_ID),
#     .(Skill_N = sum(W, na.rm = T))] %>%
#   .[, `:=` (Skill_FX = (1.75*Skill_N+52)/(Skill_N+52),
#             Vessel_ID = as.numeric(Vessel_ID))]




# adj_stockrev2 <- apply(choicearray, 2, function(x) sum(x, na.rm = TRUE)) %>%
#   as_tibble(rownames = "FISHERY_ID") %>%
#   mutate(LANDING_WEEK = i) %>%
#   left_join(stockstart, by = c("FISHERY_ID","LANDING_WEEK")) %>%
#   mutate(VesselWeeksRemaining_Sim = VesselWeeksTotal-value,
#          Stock_FX = case_when(FISHERY_ID == "Not Fishing" ~ 1,
#                               VesselWeeksRemaining_Sim > 0 ~ VesselWeeksRemaining_Sim/lead,
#                               T ~ .00001)) %>%
#   select(FISHERY_ID, Stock_FX)