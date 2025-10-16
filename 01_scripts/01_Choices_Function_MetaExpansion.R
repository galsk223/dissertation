# rm(list = ls())
library(tidyverse)
library(data.table)
library(evd)
library(igraph)
library(DirectedClustering)
library(keyplayer)
library(bipartite)
library(tnet)
library(statnet)

list.files("/home/gkoss/dissertation/01_scripts/01_Choice_Function_Functions/",
           full.names = TRUE) %>%
  walk(source)

startreg <- read_rds("/home/gkoss/westcoast-networks/data/clean/Simulation/empiricalbenchmarks_region.rds")
startmet <- read_rds("/home/gkoss/westcoast-networks/data/clean/Simulation/empiricalbenchmarks_meta.rds")

# year_ref <- 2017
# vessels_in <- 350
# scale_t1ev <- 500
# # nyears <- 10
# # burnin <- 5
# distparameter <- .5
# skillrand <- T
# scalerand <- T
# costbyfishery <- T
# shockpermanent <- F
# closureresponse <- F
# entry_opt <- c("portion", "sluggish", "random")
# entry <- entry_opt[[3]]
# entry <- "flat"
# dropves <- F
# cmtiming <- 2
#
# subgraph_uses <- c("Meta","Region")
# subgraph_use <- subgraph_uses[[1]]
# subgraph_use <- subgraph_uses[[2]]

# if(subgraph_use == "Region"){
#   e17 <- read_rds("westcoast-networks/data/Simulation/ASC_Calibration_Regional/Eureka_2017_fuller.rds")
#   asc_fc_reg <- e17$cache_fc[,ncol(e17$cache_fc)]
#   asc_sc_reg <- e17$cache_sc[,ncol(e17$cache_sc)]
#   start <- startreg
#   asc_sc <- asc_sc_reg
#   asc_fc <- asc_fc_reg
#   drop <- "CA Dungeness Crab"
# } else {
#   cache_all <- read_rds("~/westcoast-networks/data/Simulation/ASC_Calibration_RegionalMeta.rds")
#   asc_sc_met <- cache_all$cache_sc[,5]*2
#   asc_fc_met <- cache_all$cache_fc[,5]*2
#   start <- startmet
#   asc_sc <- asc_sc_met
#   asc_fc <- asc_fc_met
#   fisherylistuse <- unique(startmet$df_exprev$RegionFishery)
#   drop <- fisherylistuse[str_detect(fisherylistuse, "Dungeness Crab")]
#   distparameter <- 2
# }

choices_asc_meta <- function(start, subgraph_use, year_ref, nyears, vessels_in, burnin,
                            scale_t1ev, asc_sc, asc_fc, drop,
                            skillrand, costbyfishery, shockpermanent,
                            distparameter, closureresponse, entry, dropves, ds){

# Load In -----------------------------------------------------------------

  if(subgraph_use == "Region"){

    region <- "Eureka"
    asc_fc_blanks <- fcn_asc_fc(start$df_filter, region, year_ref)
    asc_sc_blanks <- fcn_asc_sc(start$df_edges, region, year_ref, vessels_in)
    prep_sc_mat <- asc_sc_blanks$prep_sc_mat
    prep_df_fc <- asc_fc_blanks$prep_df_fc

    bin_df <- start$df_vsets %>%
      filter(LANDING_YEAR == year_ref,
             Region == region) %>%
      mutate(Total = sum(n),
             NUse = round(Prob*vessels_in),
             VUse = sum(NUse))
    nvessels <- sum(bin_df$NUse)
    v_set <- tibble(Vessel_ID = 101:(sum(bin_df$NUse)+100),
                    LengthBin = purrr::map2(bin_df$LengthBin, bin_df$NUse, rep) %>%
                      unlist())

    df_exprev <- start$df_exprev %>%
      filter(LANDING_YEAR == year_ref,
             Region == region)
    df_fc <- prep_df_fc %>%
      arrange(LengthBin, FISHERY_ID) %>%
      mutate(FC = unlist(asc_fc))

    stockstart <- start$df_stockwks %>%
      filter(LANDING_YEAR == year_ref,
             Region == region) %>%
      mutate(lead = lead(VesselWeeksRemaining))

  } else {

    fleetnull <- 350
    vset <- start$df_vsets %>%
      filter(LANDING_YEAR == 2017) %>%
      mutate(SimFleet = fleetnull,
             RegionExp = RegionAll/YearAll*SimFleet,
             LengthRegionExp = RegionExp*Prob) %>%
      filter(LengthRegionExp > 5)

    df_all <- start$df_filter %>%
      filter(LANDING_YEAR %in% 2017,
             LengthBin %in% vset$LengthBin &
               Region%in% vset$Region) %>%
      mutate(RegionFishery = paste0(FISHERY_ID,", ",Region)) %>%
      distinct(LengthBin, RegionFishery)

    asc_fc_blanks <- fcn_asc_fc_meta(df_all)
    asc_sc_blanks <- fcn_asc_sc_meta(start$df_edges_meta %>%
                                       filter(LANDING_YEAR == 2017)%>%
                                       mutate(SimFleet = vessels_in,
                                              EdgeExp = SimFleet*NormUV))
    prep_sc_mat <- asc_sc_blanks$prep_sc_mat
    prep_df_fc <- asc_fc_blanks$prep_df_fc

    bin_df <- start$df_vsets %>%
      filter(LANDING_YEAR == 2017) %>%
      mutate(SimFleet = vessels_in,
             RegionExp = RegionAll/YearAll*SimFleet,
             LengthRegionExp = RegionExp*Prob) %>%
      filter(LengthRegionExp > 5)
    nvessels <- sum(ceiling(bin_df$LengthRegionExp))
    v_set <- tibble(Vessel_ID = 101:(nvessels+100),
                    HomeRegion = purrr::map2(bin_df$Region, ceiling(bin_df$LengthRegionExp), rep) %>%
                      unlist(),
                    LengthBin = purrr::map2(bin_df$LengthBin, ceiling(bin_df$LengthRegionExp), rep) %>%
                      unlist())

    df_exprev <- start$df_exprev %>%
      mutate(FISHERY_ID = RegionFishery) %>%
      select(-RegionFishery) %>%
      filter(LANDING_YEAR == year_ref)
    df_fc <- prep_df_fc %>%
      arrange(LengthBin, RegionFishery) %>%
      mutate(FC = unlist(asc_fc)) %>%
      rename(FISHERY_ID = RegionFishery)

    df_dist <- start$regionadjacency %>%
      select(-DistanceCostP) %>%
      mutate(DistanceCost = ifelse(is.na(value), 1,
                                   (value+1)*distparameter+1-distparameter))

    stockstart <- start$df_stockwks %>%
      filter(LANDING_YEAR == year_ref) %>%
      mutate(lead = lead(VesselWeeksRemaining),
             FISHERY_ID = paste0(FISHERY_ID,", ",Region)) %>%
      filter(!FISHERY_ID %in% c("Hawaii HMS Longline",
                                "High Seas HMS Longline"))

  }

  nvessels_start <- nvessels
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

  setDT(stockstart)
  setkey(stockstart, FISHERY_ID, LANDING_WEEK)
  setDT(sc_vessel)
  setDT(v_set)

  # Choice Inputs -----------------------------------------------------------
  # print("choice inputs")

  inputs_fishing <- v_set %>%
    left_join(df_exprev, by = c("LengthBin"), relationship = "many-to-many") %>%
    left_join(df_fc, by = c("LengthBin", "FISHERY_ID")) %>%
    mutate(ExpRev = ifelse(ExpRev == 0, 100, ExpRev))

  region_col <- if (subgraph_use == "Region") "Region" else "HomeRegion"

  input_notfishing <- inputs_fishing %>%
    distinct(Vessel_ID, LengthBin, LANDING_YEAR, .data[[region_col]], LANDING_WEEK) %>%
    mutate(
      FISHERY_ID = "Not Fishing",
      ExpRev = 0,
      FC = 0
    )

  # Prep Year Cache
  v_set_start <- v_set
  cost_multiplier <- 1
  # year_set <- rep(year_ref, nyears)

# Year Loops --------------------------------------------------------------

  # nyears <- 6
  # burnin <- 6
  yi <- 1
  for(yi in 1:nyears){

    if(yi == 1){
      adj_skill_parameters <- NULL
      adj_skillrev_carry <- list()
      cache_vessels <- list()
      cache_dfchoice <- list()
      cache_fishing <- list()
      cache_e <- list()
      cache_f <- list()
      cache_e_v <- list()
      cache_f_v <- list()
      cache_b <- list()
      cache_tcor <- list()
      cache_ttran <- list()
      v_set <- v_set_start
      response_use <- runif(1,-1,3)
    }

    cat(paste("\n Year", yi,"\n"))

      if(
        (yi < (burnin+1)) |
        (yi > burnin+1 & shockpermanent == F) |
        dropves == T
      ){
        # ALL FISHERIES
        fisherylist_use <- unique(df_fc$FISHERY_ID)
        nfisheries <- length(fisherylist_use)
        fishery_pairs <- expand.grid(Fishery1 = fisherylist_use,
                                     Fishery2 = fisherylist_use,
                                     stringsAsFactors = FALSE) %>%
          filter(Fishery1 != Fishery2) %>%
          arrange(Fishery1, Fishery2)
        if((closureresponse == T) &
           runif(1)<.3){
          response <- response_use
        } else {
          response <- 0
        }
      } else {
        # SHOCK AT (STARTS) BURNIN + 1
        fisherylist_use <- setdiff(fisherylist_use, drop)
        nfisheries <- length(fisherylist_use)
        fishery_pairs <- expand.grid(Fishery1 = fisherylist_use,
                                     Fishery2 = fisherylist_use,
                                     stringsAsFactors = FALSE) %>%
          filter(Fishery1 != Fishery2) %>%
          arrange(Fishery1, Fishery2)
        if(closureresponse == T){
          response <- response_use
        } else {
          response <- 0
        }
      }

    if((dropves == T) & (yi == (burnin+1))){
      dfished <- df_choice %>%
        filter(FISHERY_ID %in% drop,
               Vessel_ID %in% v_set$Vessel_ID) %>%
        distinct(Vessel_ID) %>%
        sample_frac(ds)
      v_set <- v_set %>%
        filter(!Vessel_ID %in% dfished$Vessel_ID)

        cost_multiplier <- 3/(nvessels_start^2)*(nvessels_start-nrow(v_set))^2-3/(nvessels_start^2)+1
        if(costbyfishery == T){
          cost_multiplier_byf <- fisherycostmult(v_set_start, v_set, prep_df_fc, yc$choice)
        }

      lastnf_carry <- lastnf_carry %>%
        filter(Vessel_ID %in% v_set$Vessel_ID) %>%
        arrange(Vessel_ID)
    }

      set.seed(Sys.time())
      if(subgraph_use == "Region"){

        inputs <- bind_rows(inputs_fishing, input_notfishing) %>%
          mutate(T1EV = rgev(n = nrow(.), loc = 0, scale = 1)*scale_t1ev,
                 SubTotal = ExpRev-FC+T1EV) %>%
            filter(FISHERY_ID %in% c(fisherylist_use, "Not Fishing"))
        } else {
        inputs <- bind_rows(inputs_fishing, input_notfishing) %>%
          left_join(.,df_dist, by = c("HomeRegion" = "Region",
                                        "Region" = "name")) %>%
          mutate(DistanceCost = ifelse(FISHERY_ID == "Not Fishing", 0,DistanceCost),
                 FC = DistanceCost*FC,
                 T1EV = rgev(n = nrow(.), loc = 0, scale = 1)*scale_t1ev,
                 SubTotal = ExpRev-FC+T1EV) %>%
          filter(!FISHERY_ID %in% c("Hawaii HMS Longline",
                                    "High Seas HMS Longline"),
                 FISHERY_ID %in% c(fisherylist_use, "Not Fishing"))
        }
      setDT(inputs)

    if(skillrand == T & yi == 1){
      adj_skill_parameters <- adj_skill_par_fun(v_set, fisherylist_use)
    }

    if(yi == 1){yc <- year1choices(v_set, inputs, sc_vessel, stockstart,
                                   skillrand, adj_skill_parameters,
                                   nfisheries, fisherylist_use)
    } else {
      yc <- yearsichoices(v_set, inputs, adj_skillrev_carry = asc, lastnf_carry,
                          sc_vessel, stockstart,
                          nfisheries, fisherylist_use,
                          cost_multiplier, cost_multiplier_byf, costbyfishery,
                          skillrand, adj_skill_parameters, response, yi)
    }

    asc <- yc$adj_skillrev_carry
    df_choice <- df_choice_fcn(yc$choice, yc$revenue, cost_multiplier, v_set)
    # cache_fishing[[yi+1]] <- fishing_fcn(df_choice, yi)

# Year Loops; Cache -------------------------------------------------------

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
    write_rds(list(cache_dfchoice = cache_dfchoice,
                   yi = yi,
                   fisherylist_use = fisherylist_use,
                   drop = drop), "/home/gkoss/westcoast-networks/data/choicetempcache.rds")

    largestcc <- T
    if(yi == burnin){
      cache_ttran[[yi+1-burnin]] <- graph_time_trans(cache_dfchoice, burnin, yi, nvessels, largestcc)
      cache_tcor[[yi+1-burnin]] <- graph_time_corr(cache_dfchoice, yi, nvessels, largestcc)

      # print("caches time")
    }


    if(yi == (burnin) |
       yi == (burnin+1)){
      # SHOCK AT BURNIN + 1, network before and at shock
      cache_e[[yi+1-burnin]] <- graph_ext_fcn(cache_dfchoice[[yi]], fishery_pairs, fisherylist_use, yi, largestcc)
      cache_f[[yi+1-burnin]] <- graph_fuller_fcn(cache_dfchoice[[yi]], yi, fisherylist_use, largestcc)

      # print("caches fish")

      cache_b[[yi+1-burnin]] <- graph_bip_fcn(cache_dfchoice[[yi]], yi, nfisheries)

      # print("caches b")

      vessel_pairs <- expand.grid(Vessel1 = as.character(unique(cache_dfchoice[[yi]]$Vessel_ID)),
                                  Vessel2 = as.character(unique(cache_dfchoice[[yi]]$Vessel_ID)), stringsAsFactors = FALSE) %>%
        filter(Vessel1 != Vessel2) %>%
        arrange(Vessel1, Vessel2)

      cache_e_v[[yi+1-burnin]] <- graph_ext_fcn_ves(cache_dfchoice[[yi]], fisherylist_use, vessel_pairs, yi, drop)
      cache_f_v[[yi+1-burnin]] <- graph_fuller_fcn_ves(cache_dfchoice[[yi]], yi, fisherylist_use, drop)

      # print("caches ves")
    }




# Year Loops; Next Year Prep ----------------------------------------------


    if(skillrand == T){
      RevExit <- 5000
      RevEntry <- 740000
      PortionEntry <- .6
    } else {
      RevExit <- 5000
      RevEntry <- 490000
      PortionEntry <- .8
    }
    if(subgraph_use == "Meta" &
       skillrand == F){
      RevExit <- 10000
      RevEntry <- 990000
      PortionEntry <- .4
    } else if(subgraph_use == "Meta" &
                  skillrand == T){
      RevExit <- 50000
      RevEntry <- 1240000
      PortionEntry <- .4
    }

    v_remain <- v_sum %>%
      filter(Weeks_Fished > 5 &
               Revenue_Fished > RevExit)

    if(nrow(v_remain) == 0){
      print("System Collapse")
      break
    } else {

      lastnf_remain <- tibble(Vessel_ID = v_set$Vessel_ID,
                              Prev = yc$lastnf) %>%
        filter(Vessel_ID %in% v_remain$Vessel_ID)

      x <- mean(v_sum$Revenue_Fished)
      y <- nrow(v_remain)/nvessels_start #maybe be v start

      if(entry == "portion"){
        p_reenter <- min(max(
          (x - 10000) / (RevEntry - 10000) * (PortionEntry / (1 + exp(1-10*y)))
          ,0),1)
      } else if(entry == "flat"){
        y <- nrow(v_remain)
        p_reenter <- min(max(
          (x - 10000) / (RevEntry - 10000) * (PortionEntry / (1 + exp(1-10*y)))
          ,0),1)
      } else if(entry == "sluggish"){
        p_reenter <- min(max(
          (x - 10000) / (RevEntry - 10000) * (PortionEntry / 2)
          ,0),1)
      } else if(entry == "random"){
        p_reenter <- runif(1,0,PortionEntry)
      }

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

      # OLD
      # t-1 CM
      cost_multiplier <- 3/(nvessels_start^2)*(nvessels_start-nrow(v_remain))^2-3/(nvessels_start^2)+1
      # t CM (altered)
      # cost_multiplier <- 3/(nvessels_start^2)*(nvessels_start-nrow(v_set))^2-3/(nvessels_start^2)+1

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
              cache_e = cache_e,
              cache_f = cache_f,
              cache_b = cache_b,
              cache_tcor = cache_tcor,
              cache_ttran = cache_ttran,
              cache_e_v = cache_e_v,
              cache_f_v = cache_f_v,
              cache_dfchoice = cache_dfchoice))
}

# view(yc$choice)
# t2 <- bind_rows(cache_vessels) %>%
#   group_by(Vessel_ID) %>% add_tally() %>%
#   filter(Year %in% c(5,8)) %>%
#   arrange(Year) %>%
#   mutate(Dif = Revenue_Fished - lag(Revenue_Fished))
