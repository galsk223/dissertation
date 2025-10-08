# adj_skillrev_carry = yc$adj_skillrev_carry

yearsichoices <- function(v_set, inputs, adj_skillrev_carry, lastnf_carry,
                          sc_vessel, stockstart, nfisheries, fisherylist_use, 
                          cost_multiplier, cost_multiplier_byf, costbyfishery,
                          skillrand, adj_skill_parameters,response, yi){
  
  fish_levels <- sort(c(fisherylist_use, "Not Fishing"))
  allskill <- adj_skillrev_carry %>% 
    lapply(\(x) as_tibble(data.table::copy(x))) %>% 
    bind_rows() %>% 
    mutate(Year = as.integer(yi)) %>% 
    distinct(Vessel_ID, Year, FISHERY_ID, .keep_all = TRUE)
  
  if(skillrand == T){
    adj_skillrev_iprev <- allskill %>%
      group_by(Vessel_ID, FISHERY_ID) %>%
      filter(Year == max(Year)) %>%
      ungroup() %>%
      mutate(Skill_FX = (P1*(Skill_N)+52)/(abs(Skill_N)+52) + response) %>% 
      select(Vessel_ID, FISHERY_ID, Skill_N, P1, Skill_FX)
  } else {
    adj_skillrev_iprev <- allskill %>%
      group_by(Vessel_ID, FISHERY_ID) %>%
      filter(Year == max(Year)) %>%
      ungroup() %>%
      mutate(Skill_FX = (3 * Skill_N + 52) / (abs(Skill_N) + 52) + response) %>%
      select(Vessel_ID, FISHERY_ID, Skill_N, Skill_FX)
  }
    
    path_choice <- lastnf_carry %>%
      as.data.table() %>% 
      merge(inputs %>%
              filter(LANDING_WEEK == 1), by = "Vessel_ID", all.x = TRUE) %>% 
      merge(sc_vessel, by.x = c("Prev", "FISHERY_ID"),
            by.y = c("Fishery1", "Fishery2"), all.x = TRUE) %>%
      merge(adj_skillrev_iprev, by = c("Vessel_ID","FISHERY_ID"), all.x = TRUE) %>%
      { if (costbyfishery == T) 
        merge(.,cost_multiplier_byf, by = c("FISHERY_ID"), all.x = TRUE) 
        else . 
      } %>% 
      .[, `:=`(Total = ExpRev * Skill_FX - FC * cost_multiplier + T1EV - SC)] %>% 
      .[!is.na(Total)] %>%
      .[, .SD[Total == max(Total)], by = Vessel_ID] %>%
      .[, W := 1] %>% .[order(Vessel_ID)]

  choice <- matrix(nrow = nrow(v_set),
                   ncol = max(inputs$LANDING_WEEK))
  revenue <- matrix(nrow = nrow(v_set),
                    ncol = max(inputs$LANDING_WEEK))
  lastnf <- matrix(nrow = nrow(v_set),
                   ncol = 1)
  choicearray <- array(0, dim = c(nrow(v_set),nfisheries+1,max(inputs$LANDING_WEEK)),
                       dimnames = list(
                         rows = v_set$Vessel_ID,
                         cols =  sort(c(fisherylist_use, "Not Fishing")),
                         slices = 1:max(inputs$LANDING_WEEK)))
  
  choice[,1] <- path_choice$FISHERY_ID
  revenue[,1] <- path_choice$Total
  lastnf <- path_choice$FISHERY_ID
  week_mat <- model.matrix(~ factor(choice[,1], 
                                    levels = sort(c(fisherylist_use, "Not Fishing"))) - 1) 
  colnames(week_mat) <- sort(c(fisherylist_use, "Not Fishing"))
  choicearray[,,1] <- week_mat
  
  adj_skillrev <- adj_skill_fcn_iplus(choicearray, nfisheries, v_set, 
                                      adj_skillrev_iprev, fisherylist_use,
                                      skillrand, adj_skill_parameters) %>% 
    mutate(Skill_FX = Skill_FX + response)
  adj_stockrev <- adj_stock_fcn(choicearray, stockstart, week = 1, fish_levels)

  i <- 2
  setkey(inputs, Vessel_ID, LANDING_WEEK)
  setkey(sc_vessel, Fishery1, Fishery2)
  setkey(adj_skillrev, Vessel_ID, FISHERY_ID)
  setkey(adj_stockrev, FISHERY_ID)
  
  inputs_by_week <- split(inputs, inputs$LANDING_WEEK)
  # s <- Sys.time()
  for (i in 2:max(inputs$LANDING_WEEK)){
      
    vessel_order <- order(v_set$Vessel_ID)
    dt <- data.table(Vessel_ID = v_set$Vessel_ID[vessel_order],
                     Prev = lastnf[vessel_order])
    setkey(dt, Vessel_ID)

    # Merge with week's input data
    week_data <- inputs_by_week[[as.character(i)]]
    dt <- dt[week_data, on = "Vessel_ID"]  # Left join
    dt <- dt[sc_vessel, on = .(Prev = Fishery1, FISHERY_ID = Fishery2), nomatch = NA]  #

    # Merge with skill effects
    setkey(dt, Vessel_ID, FISHERY_ID)
    dt <- adj_skillrev[dt]

    # Merge with stock effects
    setkey(dt, FISHERY_ID)
    dt <- adj_stockrev[dt]
    dt <- dt[order(Vessel_ID, FISHERY_ID)] %>% 
      filter(!is.na(Vessel_ID))
     
    if (costbyfishery == T){
      dt <- merge(dt,cost_multiplier_byf, by = c("FISHERY_ID"), all.x = TRUE)
    } else {
      dt$cost_multiplier <- rep(cost_multiplier, nrow(dt))
    }
    if (length(fisherylist_use) <= 8){
      dt$DistanceCost <- rep(1, nrow(dt))
    } 
    
    path_choice <- dt[
      , `:=`(Total = ExpRev * Skill_FX * Stock_FX - FC * cost_multiplier + T1EV - SC)
      ][
        !is.na(Total)
        ][
          , .SD[Total == max(Total)], by = Vessel_ID
          ][
            , W := 1][order(Vessel_ID)]
    
      # dto <- data.table(Vessel_ID = sort(v_set$Vessel_ID),
      #                           Prev = lastnf) %>%
      #   merge(inputs %>%
      #           filter(LANDING_WEEK == i), by = "Vessel_ID", all.x = TRUE) %>%
      #   merge(sc_vessel, by.x = c("Prev", "FISHERY_ID"),
      #         by.y = c("Fishery1", "Fishery2"), all.x = TRUE) %>%
      #   merge(adj_skillrev, by = c("Vessel_ID","FISHERY_ID"), all.x = TRUE) %>%
      #   merge(adj_stockrev, by = c("FISHERY_ID"), all.x = TRUE) %>%

      # if (costbyfishery == T){
      #   dto <- merge(dto,cost_multiplier_byf, by = c("FISHERY_ID"), all.x = TRUE)
      # } else {
      #   dto$cost_multiplier <- rep(cost_multiplier, nrow(dto))
      # }
      
    
      # res <- choose_best_cm(
      #   vessel   = dt$Vessel_ID,
      #   fishery  = dt$FISHERY_ID,
      #   ExpRev   = dt$ExpRev,
      #   DistanceCost = dt$DistanceCost,
      #   Skill_FX = dt$Skill_FX,
      #   Stock_FX = dt$Stock_FX,
      #   FC       = dt$FC,
      #   cost_multiplier = dt$cost_multiplier,
      #   T1EV     = dt$T1EV,
      #   SC       = dt$SC
      # )
      # path_choice <- data.table(
      #   Vessel_ID  = res$Vessel_ID,
      #   FISHERY_ID = res$FISHERY_ID,
      #   Total      = res$Total,
      #   W          = i
      # )[order(Vessel_ID)]
      
    revenue[,i] <- path_choice$Total
    choice[,i] <- path_choice$FISHERY_ID
    prevfish <- choice[,i]
    prevfish[prevfish == "Not Fishing"] <- lastnf[prevfish == "Not Fishing"]
    lastnf <- prevfish
    
    week_mat <- model.matrix(~ factor(choice[,i], 
                                      levels = sort(c(fisherylist_use, "Not Fishing"))) - 1) 
    colnames(week_mat) <- sort(c(fisherylist_use, "Not Fishing"))
    choicearray[,,i] <- week_mat
    
    adj_skillrev <- adj_skill_fcn_iplus(choicearray, nfisheries, v_set,
                                        adj_skillrev_iprev, fisherylist_use,
                                        skillrand, adj_skill_parameters) %>%
      mutate(Skill_FX = Skill_FX + response)
    setkey(adj_skillrev, Vessel_ID, FISHERY_ID)
    adj_stockrev <- adj_stock_fcn(choicearray, stockstart, week = i, fish_levels)
  }
  
  # Sys.time() - s
  
  adj_skillrev_close <- adj_skillrev %>% 
    mutate(Skill_N = Skill_N + W,
           Year = yi) %>% 
    select(-W)
  adj_skillrev_carry[[yi]] <- as_tibble(adj_skillrev_close)
  
  return(list(choice = choice, lastnf = lastnf,
              revenue = revenue, adj_skillrev_carry = adj_skillrev_carry))
  
  rm(allskill)
  rm(adj_skillrev_iprev)
}
