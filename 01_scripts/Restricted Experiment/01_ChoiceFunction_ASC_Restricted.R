rm(list = ls())
library(tidyverse)
source("westcoast-networks/scripts/02_Simulation/Port Regions/01_ChoiceFunction_Functions.R", echo = F, verbose = F)
start <- read_rds("westcoast-networks/data/clean/Simulation/empiricalbenchmarks_region.rds")

regions <- c("Eureka", "Monterey", "Morro Bay",  "San Francisco", "Santa Barbara")
region <- regions[3]
year <- 2017


# regions <- unique(start$df_filter$Region)
# 
vessels_in <- 200
# scale_t1ev <- 1000
# stochastic <- F
# 
# year <- 2017
# region <- "Monterey"
# 

asc_fc_blanks <- fcn_asc_fc(start$df_filter, region, year)
prep_df_fc <- asc_fc_blanks$prep_df_fc
asc_fc <- asc_fc_blanks$asc_fc_blanks_list
# 
asc_sc_blanks <- fcn_asc_sc(start$df_edges, region, year, vessels_in)
prep_sc_mat <- asc_sc_blanks$prep_sc_mat
asc_sc <- asc_sc_blanks$asc_sc_0

# m17 <- choices_asc_cal(start = start, scale_t1ev = 1000, vessels_in = 200, stochastic = F,
#                         region = region, year = year, 
#                         prep_df_fc = asc_fc_blanks$prep_df_fc, 
#                         asc_fc = asc_fc_blanks$asc_fc_blanks_list,
#                         prep_sc_mat = asc_sc_blanks$prep_sc_mat, 
#                         asc_sc = asc_sc_blanks$asc_sc_0)

# Function ----------------------------------------------------------------
# INPUTS: start, region, year, vessels_in, scale_t1ev, stochastic,
#        prep_df_fc, prep_sc_mat, asc_sc, asc_fc, 

asc_fc <- asc_fc_0
asc_sc <- asc_sc_0
choices_asc_cal <- function(start, region, year, vessels_in, scale_t1ev, stochastic,
                            prep_df_fc, prep_sc_mat, asc_sc, asc_fc){

    # er_k <- 1
  # print("prep")

    bin_df <- start$df_filter %>% 
      filter(LANDING_YEAR == year,
             Region == region) %>% 
      mutate(Total = sum(n),
             NUse = round(Prob*vessels_in),
             VUse = sum(NUse))
    nvessels <- sum(bin_df$NUse)
    
    v_set <- tibble(Vessel_ID = 101:(sum(bin_df$NUse)+100),
                    LengthBin = purrr::map2(bin_df$LengthBin, bin_df$NUse, rep) %>%
                      unlist())
    
  # Load In
    # print("load in")
    
    df_exprev <- start$df_exprev %>% 
      filter(LANDING_YEAR == year,
             Region == region) 
    
    df_fc <- prep_df_fc %>% 
      arrange(LengthBin, FISHERY_ID) %>% 
      mutate(FC = unlist(asc_fc))
    
   
    fisherylist_use <- unique(df_fc$FISHERY_ID)
    nfisheries <- length(fisherylist_use)
    
    fishery_pairs <- expand.grid(Fishery1 = fisherylist_use, Fishery2 = fisherylist_use, stringsAsFactors = FALSE) %>%
      filter(Fishery1 != Fishery2) %>% 
      arrange(Fishery1, Fishery2)  
    
    
    # print("sc1")
    
    sc_mat <- matrix(Inf, nfisheries+1, nfisheries+1)
    diag(sc_mat) <- 0
    sc_mat[,nfisheries+1] <- 0
    sc_mat[nfisheries+1,] <- 0
    colnames(sc_mat) <- c(fisherylist_use, "Not Fishing")
    rownames(sc_mat) <- c(fisherylist_use, "Not Fishing")
    
    # print("sc2")
    i <- 1
    for (i in seq_len(nrow(prep_sc_mat))) {
      f1 <- prep_sc_mat$Fishery1[i]
      f2 <- prep_sc_mat$Fishery2[i]
      sc <- asc_sc[i]
      sc_mat[f1, f2] <- sc
      sc_mat[f2, f1] <- sc  # ensure symmetry
    }
    
    # print("sc3")
    sc_vessel <- as.data.table(reshape2::melt(sc_mat, value.name = "SC")) %>% 
      setnames(c(1, 2, 3), c("Fishery1", "Fishery2", "SC")) %>% 
      mutate(Fishery1 = as.character(Fishery1),
             Fishery2 = as.character(Fishery2))
    
    # Choice Inputs -----------------------------------------------------------
    # print("choice inputs")
    
    inputs_fishing <- v_set %>%
      left_join(df_exprev, by = c("LengthBin"), relationship = "many-to-many") %>%
      left_join(df_fc, by = c("LengthBin", "FISHERY_ID")) 
    
    input_notfishing <- inputs_fishing %>% 
      distinct(Vessel_ID, LengthBin, LANDING_YEAR, Region, LANDING_WEEK) %>% 
      mutate(FISHERY_ID = "Not Fishing",
             ExpRev = 0,
             FC = 0)
    
    # if(stochastic == T) {
    #   set.seed(Sys.time())
    #   inputs <- bind_rows(inputs_fishing, input_notfishing) %>% 
    #     mutate(T1EV = rgev(n = nrow(.), loc = 0, scale = 1)*scale_t1ev,
    #            SubTotal = ExpRev-FC+T1EV)
    #   } 
    # else {
      set.seed(420)
      inputs <- bind_rows(inputs_fishing, input_notfishing) %>% 
        mutate(T1EV = rgev(n = nrow(.), loc = 0, scale = 1)*scale_t1ev,
               SubTotal = ExpRev-FC-T1EV)
      # } 
    
    
    # Fishery Choices ---------------------------------------------------------
    # print("fishery choices")
    
    w1 <- inputs %>% 
      filter(LANDING_WEEK == 1) %>% 
      group_by(Vessel_ID) %>%
      filter(SubTotal == max(SubTotal, na.rm = T)) %>%
      ungroup() %>% 
      mutate(W = 1) %>% 
      arrange(Vessel_ID)
    
    choice <- matrix(nrow = nrow(v_set),
                     ncol = max(inputs$LANDING_WEEK))
    revenue <- matrix(nrow = nrow(v_set),
                      ncol = max(inputs$LANDING_WEEK))
    lastnf <- matrix(nrow = nrow(v_set),
                     ncol = 1)
    
    revenue[,1] <- w1$SubTotal
    choice[,1] <- w1$FISHERY_ID
    lastnf <- w1$FISHERY_ID
    
    i <- 8
    for (i in 2:max(inputs$LANDING_WEEK)){
      
      # print(i)
      path_choice <- data.table(Vessel_ID = sort(v_set$Vessel_ID),
                                Prev = lastnf) %>%
        merge(inputs %>%
                filter(LANDING_WEEK == i), by = "Vessel_ID", all.x = TRUE) %>% 
        merge(sc_vessel, by.x = c("Prev", "FISHERY_ID"),
              by.y = c("Fishery1", "Fishery2"), all.x = TRUE) %>%
        .[, `:=`(Total = SubTotal - SC)] %>% 
        .[!is.na(Total)] %>%
        .[, .SD[Total == max(Total)], by = Vessel_ID] %>%
        .[, W := 1] %>% .[order(Vessel_ID)]
      
      # pfix <- path_choice %>%
      #   group_by(Vessel_ID) %>%
      #   add_tally()
      
      revenue[,i] <- path_choice$Total
      choice[,i] <- path_choice$FISHERY_ID
      prevfish <- choice[,i]
      prevfish[prevfish == "Not Fishing"] <- lastnf[prevfish == "Not Fishing"]
      lastnf <- prevfish
      
    }
    
    # print("collate")
    
    df_choice <- left_join(
      bind_cols(v_set, 
                as.data.frame(choice)) %>% 
        pivot_longer(-c(Vessel_ID, LengthBin),
                     names_to = "Week",
                     values_to = "FISHERY_ID"),
      bind_cols(v_set, 
                as.data.frame(revenue)) %>% 
        pivot_longer(-c(Vessel_ID, LengthBin),
                     names_to = "Week",
                     values_to = "Revenue"),
      by = join_by(Vessel_ID, LengthBin, Week)) 
    
    fishing <- df_choice %>% 
      group_by(LengthBin) %>% 
      mutate(NVessels = n_distinct(Vessel_ID)*53) %>% 
      ungroup() %>% 
      group_by(LengthBin, FISHERY_ID, NVessels) %>% 
      tally() %>% 
      mutate(Shares = n/NVessels) %>% 
      ungroup() %>% 
      group_by(LengthBin) %>%
      mutate(Check = sum(Shares),
             Check2 = sum(Shares*(FISHERY_ID!="Not Fishing"))) 
    
    bipmat <- df_choice %>%  
      group_by(Vessel_ID, FISHERY_ID) %>% 
      summarise(Weights = n_distinct(Week), .groups = "drop") %>% 
      arrange(FISHERY_ID) %>% 
      pivot_wider(names_from = FISHERY_ID, values_from = Weights, values_fill = 0) %>%
      column_to_rownames("Vessel_ID") %>%
      as.matrix()
    
    connectivity <- matrix(0, nfisheries+1, nfisheries+1)
    colnames(connectivity) <- rownames(connectivity) <- sort(c(fisherylist_use,"Not Fishing"))
    
    edges_fuller <- fulleredges(connectivity, bipmat) %>% 
      filter(Fishery1 != "Not Fishing",
             Fishery2 != "Not Fishing")
    
    edges_ext <- df_choice %>%
      distinct(Vessel_ID, FISHERY_ID) %>%
      mutate(present = 1) %>%
      pivot_wider(names_from = FISHERY_ID, values_from = present, values_fill = 0) %>%
      column_to_rownames("Vessel_ID") %>%
      as.matrix() %>%
      { t(.) %*% . } %>%
      as.data.frame() %>%
      rownames_to_column("Fishery1") %>%
      pivot_longer(-Fishery1, names_to = "Fishery2", values_to = "UniqueVessels") %>%
      filter(Fishery1 != Fishery2) %>%
      full_join(fishery_pairs, by = c("Fishery1", "Fishery2")) %>%
      mutate(UniqueVessels = replace_na(UniqueVessels, 0))
    
    return(list(fishing = fishing, 
                edges_fuller = edges_fuller, 
                edges_ext = edges_ext,
                df_choice = df_choice))

    }
    






