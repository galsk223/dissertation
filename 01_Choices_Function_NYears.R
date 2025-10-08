# rm(list = ls())
# library(tidyverse)
# library(data.table)
# library(evd)
# library(igraph)
# library(DirectedClustering)
# library(keyplayer)
# library(bipartite)
# library(tnet)
# library(statnet)
# list.files("westcoast-networks/scripts/02_Simulation/0_Port_Regions/01_Choice_Function_Functions/", 
#            full.names = TRUE) %>%
#   walk(source)
# 
# start <- read_rds("westcoast-networks/data/clean/Simulation/empiricalbenchmarks_region.rds")
#  
# regions <- c("Eureka", "Monterey", "Morro Bay")
# region <- regions[1]
# year_ref <- 2017
# nyears <- 5
# burnin <- 2
# drop <- "CA Dungeness Crab"
# e17 <- read_rds("westcoast-networks/data/Simulation/ASC_Calibration_Regional/Eureka_2017_fuller.rds")
# vessels_in <- 200
# scale_t1ev <- 500
# asc_fc <- e17$cache_fc[,ncol(e17$cache_fc)]
# asc_sc <- e17$cache_sc[,ncol(e17$cache_sc)]
# skillrand <- T
# scalerand <- F
# costbyfishery <- T
# shockpermanent <- F
closureresponse <- T

choices_asc_cal <- function(start, region, year_ref, nyears, vessels_in, burnin,
                            scale_t1ev, asc_sc, asc_fc, drop, 
                            skillrand, scalerand, costbyfishery, shockpermanent, closureresponse){
  
  # Load In 
  asc_fc_blanks <- fcn_asc_fc(start$df_filter, region, year_ref)
  prep_df_fc <- asc_fc_blanks$prep_df_fc
  
  asc_sc_blanks <- fcn_asc_sc(start$df_edges, region, year_ref, vessels_in)
  prep_sc_mat <- asc_sc_blanks$prep_sc_mat
  
  bin_df <- start$df_vsets %>% 
    filter(LANDING_YEAR == year_ref,
           Region == region) %>% 
    mutate(Total = sum(n),
           NUse = round(Prob*vessels_in),
           VUse = sum(NUse))
  nvessels <- sum(bin_df$NUse)
  nvessels_start <- nvessels
  
  v_set <- tibble(Vessel_ID = 101:(sum(bin_df$NUse)+100),
                  LengthBin = purrr::map2(bin_df$LengthBin, bin_df$NUse, rep) %>%
                    unlist())
  
  df_exprev <- start$df_exprev %>%
    filter(LANDING_YEAR == year_ref,
           Region == region)

  # if(scalerand == T){
  #   df_fc <- prep_df_fc %>% 
  #     arrange(LengthBin, FISHERY_ID) %>% 
  #     mutate(FC = unlist(asc_fc),
  #            Mult = round(runif(nrow(.), 5, 25),2),
  #            FC = ifelse(FISHERY_ID == "CA Dungeness Crab",FC*Mult,FC))
  # } else {
    df_fc <- prep_df_fc %>% 
      arrange(LengthBin, FISHERY_ID) %>% 
      mutate(FC = unlist(asc_fc))
  # }
  
  fisherylist_use <- unique(df_fc$FISHERY_ID)
  nfisheries <- length(fisherylist_use)
  
  fishery_pairs <- expand.grid(Fishery1 = fisherylist_use, Fishery2 = fisherylist_use, stringsAsFactors = FALSE) %>%
    filter(Fishery1 != Fishery2) %>% 
    arrange(Fishery1, Fishery2)  
  
  sc_mat <- matrix(Inf, nfisheries+1, nfisheries+1)
  diag(sc_mat) <- 0
  sc_mat[,nfisheries+1] <- 0
  sc_mat[nfisheries+1,] <- 0
  colnames(sc_mat) <- c(fisherylist_use, "Not Fishing")
  rownames(sc_mat) <- c(fisherylist_use, "Not Fishing")
  
  # i <- 1
  for (i in seq_len(nrow(prep_sc_mat))) {
    f1 <- prep_sc_mat$Fishery1[i]
    f2 <- prep_sc_mat$Fishery2[i]
    sc <- asc_sc[i]
    sc_mat[f1, f2] <- sc
    sc_mat[f2, f1] <- sc  # ensure symmetry
  }
  
  sc_vessel <- as.data.table(reshape2::melt(sc_mat, value.name = "SC")) %>% 
    setnames(c(1, 2, 3), c("Fishery1", "Fishery2", "SC")) %>% 
    mutate(Fishery1 = as.character(Fishery1),
           Fishery2 = as.character(Fishery2))
  
  stockstart <- start$df_stockwks %>%
    filter(LANDING_YEAR == year_ref,
           Region == region) %>%
    mutate(lead = lead(VesselWeeksRemaining))
  setDT(stockstart)
  setkey(stockstart, FISHERY_ID, LANDING_WEEK)
  
  # Choice Inputs -----------------------------------------------------------
  # print("choice inputs")
  
  inputs_fishing <- v_set %>%
    left_join(df_exprev, by = c("LengthBin"), relationship = "many-to-many") %>%
    left_join(df_fc, by = c("LengthBin", "FISHERY_ID")) %>% 
    mutate(ExpRev = ifelse(ExpRev == 0, 100, ExpRev))
  
  input_notfishing <- inputs_fishing %>% 
    distinct(Vessel_ID, LengthBin, LANDING_YEAR, Region, LANDING_WEEK) %>% 
    mutate(FISHERY_ID = "Not Fishing",
           ExpRev = 0,
           FC = 0)
  
  # Prep Year Cache
  v_set_start <- v_set
  cost_multiplier <- 1
  year_set <- rep(year_ref, nyears)
  
  cache_vessels <- list()
  cache_dfchoice <- list()
  cache_fishing <- list()
  cache_e <- list()
  cache_f <- list()
  cache_b <- list()
  v_set <- v_set_start
  response_use <- runif(1,-1,3)
  
  # nyears <- 15
  # burnin <- 4
  yi <- 2
  for(yi in 1:nyears){
    
    cat(paste("\n Year", yi,"\n"))
    
    if(
      (yi < (burnin+1)) | 
       (yi > burnin+1 & shockpermanent == F)
       ){
      
      # ALL FISHERIES
      fisherylist_use <- unique(df_fc$FISHERY_ID)
      nfisheries <- length(fisherylist_use)
      fishery_pairs <- expand.grid(Fishery1 = fisherylist_use, Fishery2 = fisherylist_use, stringsAsFactors = FALSE) %>%
        filter(Fishery1 != Fishery2) %>% 
        arrange(Fishery1, Fishery2)  
      response <- 0
      
      set.seed(Sys.time())
      inputs <- bind_rows(inputs_fishing, input_notfishing) %>%
        mutate(T1EV = rgev(n = nrow(.), loc = 0, scale = 1)*scale_t1ev,
               SubTotal = ExpRev-FC+T1EV)
      
    } else {
      
      # SHOCK AT (STARTS) BURNIN + 1 
      fisherylist_use <- setdiff(fisherylist_use, drop)
      nfisheries <- length(fisherylist_use)
      fishery_pairs <- expand.grid(Fishery1 = fisherylist_use, Fishery2 = fisherylist_use, stringsAsFactors = FALSE) %>%
        filter(Fishery1 != Fishery2) %>% 
        arrange(Fishery1, Fishery2)  
      if(closureresponse == T){
        response <- response_use
      } else {
        response <- 0
      }
      
      set.seed(Sys.time())
      inputs <- bind_rows(inputs_fishing, input_notfishing) %>%
        mutate(T1EV = rgev(n = nrow(.), loc = 0, scale = 1)*scale_t1ev,
               SubTotal = ExpRev-FC+T1EV) %>% 
        filter(FISHERY_ID %in% c(fisherylist_use, "Not Fishing"))
      
    }
    
    if(skillrand == T & yi == 1){
      adj_skill_parameters <- adj_skill_par_fun(v_set, fisherylist_use)
    }

    if(yi == 1){yc <- year1choices(v_set, inputs, sc_vessel, stockstart, 
                                   skillrand, adj_skill_parameters,
                                   nfisheries, fisherylist_use)
    } else {
      yc <- yearsichoices(v_set, inputs, adj_skillrev_carry = yc$adj_skillrev_carry, lastnf_carry,
                          sc_vessel, stockstart, 
                          nfisheries, fisherylist_use, 
                          cost_multiplier, cost_multiplier_byf, costbyfishery,
                          skillrand, adj_skill_parameters, response, yi)
      }
      
    df_choice <- df_choice_fcn(yc$choice, yc$revenue, cost_multiplier, v_set)
    cache_fishing[[yi+1]] <- fishing_fcn(df_choice, yi)

    # if(yi == (burnin) |
    #    yi == (burnin+1)){
    #   # SHOCK AT BURNIN + 1, network before and at shock
    #   cache_e[[yi+1-burnin]] <- graph_ext_fcn(df_choice, fishery_pairs, yi)
    #   cache_f[[yi+1-burnin]] <- graph_fuller_fcn(df_choice, yi, fisherylist_use)
    #   cache_b[[yi+1-burnin]] <- graph_bip_fcn(df_choice, yi, nfisheries)
    # }
    
    fished_mask <- yc$choice != "Not Fishing" & !is.na(yc$choice)
    weeks_fished <- rowSums(fished_mask)
    rev_fished <- rowSums(yc$revenue * fished_mask, na.rm = TRUE)
    v_sum <- tibble(Vessel_ID = v_set$Vessel_ID,
                    LengthBin = v_set$LengthBin,
                    Weeks_Fished = weeks_fished,
                    Revenue_Fished = rev_fished) %>% 
      rowid_to_column()
    
    cache_vessels[[yi]] <- v_sum %>% 
      mutate(Year = yi)
    
    cache_dfchoice[[yi]] <- df_choice_fcn(yc$choice, yc$revenue, cost_multiplier, v_set)
    
    if(skillrand == T){
      RevExit <- 5000
      RevEntry <- 740000
      PortionEntry <- .6
    } else {
      RevExit <- 5000
      RevEntry <- 490000
      PortionEntry <- .8
    }
    
    v_remain <- v_sum %>% 
      filter(Weeks_Fished > 5 &
            Revenue_Fished > RevExit)
      
      if(nrow(v_remain) == 0){
        print("System Collapse")
        break
      } else {
        
        cost_multiplier <- 3/(nvessels_start^2)*(nvessels_start-nrow(v_remain))^2-3/(nvessels_start^2)+1
        
        lastnf_remain <- tibble(Vessel_ID = v_set$Vessel_ID,
                                Prev = yc$lastnf) %>% 
          filter(Vessel_ID %in% v_remain$Vessel_ID)
        
        x <- mean(v_sum$Revenue_Fished)
        y <- nrow(v_remain)
        
          p_reenter <- min(max(
            (x - 10000) / (RevEntry - 10000) * (PortionEntry / (1 + exp(-0.1 * (.5*y-10))))
                               ,0),1)
         
          v_left <- v_set %>% 
            filter(!Vessel_ID %in% v_remain$Vessel_ID)
          v_canreenter <- v_set_start %>% 
            filter(!Vessel_ID %in% v_remain$Vessel_ID,
                   !Vessel_ID %in% v_left$Vessel_ID) %>% 
            slice_sample(n = round(p_reenter*(nvessels_start-nrow(v_remain))))
          
          v_set <- bind_rows(v_canreenter, v_remain) %>% 
            arrange(Vessel_ID)
          lastnf_carry <- lastnf_remain %>% 
            bind_rows(tibble(Vessel_ID = v_canreenter$Vessel_ID,
                             Prev = "Not Fishing")) %>% 
            arrange(Vessel_ID)
          
          if(costbyfishery == T){
            cost_multiplier_byf <- fisherycostmult(v_set_start, v_remain, prep_df_fc, yc$choice)
          }
          
          if(yi == (burnin+1)){
            cat(paste0("SHOCK \n", nrow(v_set), " Vessels; $",round(x),
                       "\n Exit: ", nrow(v_left)," | Entry: ", nrow(v_canreenter)))
          } else {
            cat(paste0(nrow(v_set), " Vessels; $",round(x),
                         "\n Exit: ", nrow(v_left)," | Entry: ", nrow(v_canreenter)))
          }
          
          
        }
    
    } 
  return(list(cache_vessels = cache_vessels, 
              cache_fishing = cache_fishing,
              cache_e = cache_e, cache_f = cache_f, 
              cache_b = cache_b,
              cache_dfchoice = cache_dfchoice,
              adj_skillrev_carry = yc$adj_skillrev_carry))
  }
  
# view(yc$choice)
# t2 <- bind_rows(cache_vessels) %>% 
#   group_by(Vessel_ID) %>% add_tally() %>% 
#   filter(Year %in% c(5,8)) %>% 
#   arrange(Year) %>% 
#   mutate(Dif = Revenue_Fished - lag(Revenue_Fished))
