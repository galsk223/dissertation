rm(list = ls())
library(evd)
list.files("westcoast-networks/scripts/02_Simulation/0_Port_Regions/01_Choice_Function_Functions/", 
           full.names = TRUE) %>%
  walk(source)
source("westcoast-networks/scripts/02_Simulation/0_Port_Regions/06_MetaNetwork/CalibrationBenchmarkReadin_MetaNetwork.R")
source("westcoast-networks/scripts/02_Simulation/0_Port_Regions/01_Choice_Function_Functions/01_dimensionprep.R")
source("westcoast-networks/scripts/02_Simulation/0_Port_Regions/01_Choice_Function_Functions/02a_AdjRevFcns.R")
year <- 2017
scale_t1ev <- 1000
asc_fc_blanks_meta <- fcn_asc_fc_meta(fishing_bench)
prep_df_fc <- asc_fc_blanks_meta$prep_df_fc
asc_fc_0 <- asc_fc_blanks_meta$asc_fc_blanks_list
asc_fc <- asc_fc_0
asc_fc <- map(asc_fc_0, ~ .x * 0.5)

asc_sc_blanks <- fcn_asc_sc_meta(switching_bench)
prep_sc_mat <- asc_sc_blanks$prep_sc_mat
asc_sc <- asc_sc_blanks$asc_sc
asc_sc_0 <- asc_sc_blanks$asc_sc*rep(.5,length(asc_sc))
  
fisherylist_use <- unique(exprev$RegionFishery)
nfisheries <- length(fisherylist_use)

fishery_pairs <- expand.grid(Fishery1 = fisherylist_use, Fishery2 = fisherylist_use, stringsAsFactors = FALSE) %>%
  filter(Fishery1 != Fishery2) %>% 
  arrange(Fishery1, Fishery2)  
vessels_in <- 350
year_ref <- 2017
scale_t1ev <- 1000

cache_all <- read_rds("~/westcoast-networks/data/Simulation/ASC_Calibration_RegionalMeta.rds")
asc_sc <- cache_all$cache_sc[,5]
asc_fc <- cache_all$cache_fc[,5]

# CHOICE FUNCTION ---------------------------------------------------------

choices_asc_cal <- function(start, year_ref, vessels_in, fishery_pairs, prep_df_fc,
                            scale_t1ev, asc_sc, asc_fc){

  # Inputs ------------------------------------------------------------------
  
  vset <- start$df_vsets %>% 
    filter(LANDING_YEAR == 2017) %>% 
    mutate(SimFleet = vessels_in,
           RegionExp = RegionAll/YearAll*SimFleet,
           LengthRegionExp = RegionExp*Prob) %>% 
    filter(LengthRegionExp > 5)
  
  df_all <- start$df_filter %>% 
    filter(LengthBin %in% vset$LengthBin &
             Region %in% vset$Region)
  
  asc_sc_blanks <- fcn_asc_sc_meta(start$df_edges_meta %>% 
                                     filter(LANDING_YEAR == 2017)%>% 
                                     mutate(SimFleet = vessels_in,
                                            EdgeExp = SimFleet*NormUV) )
  prep_sc_mat <- asc_sc_blanks$prep_sc_mat
  
  bin_df <- vset
  nvessels <- sum(ceiling(bin_df$LengthRegionExp))
  
  v_set <- tibble(Vessel_ID = 101:(nvessels+100),
                  HomeRegion = purrr::map2(bin_df$Region, ceiling(bin_df$LengthRegionExp), rep) %>%
                    unlist(),
                  LengthBin = purrr::map2(bin_df$LengthBin, ceiling(bin_df$LengthRegionExp), rep) %>%
                    unlist())
  
  df_exprev <- start$df_exprev %>% 
    mutate(RegionFishery = paste0(FISHERY_ID,", ",Region)) %>% 
    filter(LANDING_YEAR == year_ref)
  df_fc_cf <- prep_df_fc %>% 
    arrange(LengthBin, RegionFishery) %>% 
    mutate(FC = unlist(asc_fc))
  df_dist_cf <- start$regionadjacency
  # distparameter <- .3
  # regionadjacency <- start$regionadjacency %>% 
  #   mutate(DistanceCost = ifelse(is.na(value), 1, (value+1)*distparameter+1-distparameter))
    
  fisherylist_use <- unique(df_exprev$RegionFishery)
  nfisheries <- length(fisherylist_use)
  
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
    filter(LANDING_YEAR == year_ref) %>%
    mutate(lead = lead(VesselWeeksRemaining),
           RegionFishery = paste0(FISHERY_ID,", ",Region)) %>% 
    filter(!FISHERY_ID %in% c("Hawaii HMS Longline",
                              "High Seas HMS Longline"))
  setDT(stockstart)
  setkey(stockstart, RegionFishery, LANDING_WEEK)
  
  # Inputs Compile -----------------------------------------------------------
  # print("choice inputs")
  
  inputs_fishing <- v_set %>%
    left_join(df_exprev, by = c("LengthBin"), relationship = "many-to-many") %>%
    left_join(df_fc, by = c("LengthBin", "RegionFishery")) %>% 
    mutate(ExpRev = ifelse(ExpRev == 0, 1, ExpRev))
  
  input_notfishing <- inputs_fishing %>% 
    distinct(Vessel_ID, LengthBin, LANDING_YEAR, HomeRegion, LANDING_WEEK) %>% 
    mutate(FISHERY_ID = "Not Fishing",
           RegionFishery = "Not Fishing",
           ExpRev = 0,
           FC = 0)
  
  set.seed(420)
  inputs <- bind_rows(inputs_fishing, input_notfishing) %>% 
    left_join(df_dist, by = c("HomeRegion" = "Region",
                              "Region" = "name")) %>% 
    mutate(DistanceCost = ifelse(RegionFishery == "Not Fishing", 0,DistanceCost),
           T1EV = rgev(n = nrow(.), loc = 0, scale = 1)*scale_t1ev,
           SubTotal = ExpRev-DistanceCost*FC+T1EV) %>% 
    filter(!FISHERY_ID %in% c("Hawaii HMS Longline",
                              "High Seas HMS Longline"))
  
  # Choices -----------------------------------------------------------------

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
  
  adj_stockrev <- adj_stock_fcn_meta(choicearray[,,1], stockstart, 1, fisherylist_use)
  
  w1 <- inputs %>% 
    filter(LANDING_WEEK == 1) %>% 
    merge(adj_stockrev, by = c("RegionFishery"), all.x = TRUE) %>%
    mutate(Total = ExpRev*Stock_FX - DistanceCost * FC + T1EV) %>% 
    group_by(Vessel_ID) %>%
    filter(Total == max(Total, na.rm = T)) %>%
    ungroup() %>% 
    mutate(W = 1) %>% 
    arrange(Vessel_ID)
  
  revenue[,1] <- w1$Total
  choice[,1] <- w1$RegionFishery
  lastnf <- w1$RegionFishery
  week_mat <- model.matrix(~ factor(choice[,1], 
                                    levels = sort(c(fisherylist_use, "Not Fishing"))) - 1) 
  colnames(week_mat) <- sort(c(fisherylist_use, "Not Fishing"))
  choicearray[,,2] <- week_mat
  
  adj_stockrev <- adj_stock_fcn_meta(choicearray[,,2], stockstart, 1, fisherylist_use)
  
  i <- 2
  for (i in 2:max(inputs$LANDING_WEEK)){
    
    # print(i)
    path_choice_cf <- data.table(Vessel_ID = sort(v_set$Vessel_ID),
                              Prev = lastnf) %>%
      merge(inputs %>%
              filter(LANDING_WEEK == i), by = "Vessel_ID", all.x = TRUE) %>% 
      merge(sc_vessel, by.x = c("Prev", "RegionFishery"),
            by.y = c("Fishery1", "Fishery2"), all.x = TRUE) %>%
      merge(adj_stockrev, by = c("RegionFishery"), all.x = TRUE) %>%
      .[, `:=`(Total = ExpRev * Stock_FX - DistanceCost * FC + T1EV - SC)] %>% 
      .[!is.na(Total)] %>%
      .[, .SD[Total == max(Total)], by = Vessel_ID] %>%
      .[, W := 1] %>% .[order(Vessel_ID)]
    
    revenue[,i] <- path_choice$Total
    choice[,i] <- path_choice$RegionFishery
    prevfish <- choice[,i]
    prevfish[prevfish == "Not Fishing"] <- lastnf[prevfish == "Not Fishing"]
    lastnf <- prevfish
    
    week_mat <- model.matrix(~ factor(choice[,i], 
                                      levels = sort(c(fisherylist_use, "Not Fishing"))) - 1) 
    colnames(week_mat) <- sort(c(fisherylist_use, "Not Fishing"))
    choicearray[,,i] <- week_mat
    
    adj_stockrev <- adj_stock_fcn_meta(choicearray[,,2:(i+1)], stockstart, week = i, fisherylist_use)
    
  }
  
  df_choice <- left_join(
    bind_cols(v_set, 
              as.data.frame(choice)) %>% 
      pivot_longer(-c(Vessel_ID, HomeRegion, LengthBin),
                   names_to = "Week",
                   values_to = "RegionFishery"),
    bind_cols(v_set, 
              as.data.frame(revenue)) %>% 
      pivot_longer(-c(Vessel_ID, HomeRegion, LengthBin),
                   names_to = "Week",
                   values_to = "Revenue"),
    by = join_by(Vessel_ID, HomeRegion, LengthBin, Week)) 
  
  fishing <- df_choice %>% 
    group_by(LengthBin) %>% 
    mutate(NVessels = n_distinct(Vessel_ID)*53) %>% 
    ungroup() %>% 
    group_by(LengthBin, RegionFishery, NVessels) %>% 
    tally() %>% 
    mutate(Shares = n/NVessels) %>% 
    ungroup() %>% 
    group_by(LengthBin) %>%
    mutate(Check = sum(Shares),
           Check2 = sum(Shares*(RegionFishery!="Not Fishing"))) 
  
  edges_ext <- df_choice %>%
    distinct(Vessel_ID, RegionFishery) %>%
    mutate(present = 1) %>%
    pivot_wider(names_from = RegionFishery, values_from = present, values_fill = 0) %>%
    column_to_rownames("Vessel_ID") %>%
    as.matrix() %>%
    { t(.) %*% . } %>%
    as.data.frame() %>%
    rownames_to_column("Fishery1") %>%
    pivot_longer(-Fishery1, names_to = "Fishery2", values_to = "UniqueVessels") %>%
    filter(Fishery1 != Fishery2,
           Fishery1 != "Not Fishing",
           Fishery2 != "Not Fishing") %>%
    full_join(fishery_pairs, by = c("Fishery1", "Fishery2")) %>%
    mutate(UniqueVessels = replace_na(UniqueVessels, 0)) 
  
  return(list(fishing = fishing, 
              edges_ext = edges_ext,
              df_choice = df_choice))
  
}
