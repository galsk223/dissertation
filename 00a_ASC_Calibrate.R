library(patchwork)
library(tidyverse)
library(data.table)
library(evd)
library(nloptr)
library(DEoptim)

regions <- c("Eureka", "Monterey", "Morro Bay",  "San Francisco", "Santa Barbara")
region <- regions[3]
year <- 2017
source("westcoast-networks/scripts/02_Simulation/0_Port_Regions/Choice_Function_Functions/01_dimensionprep.R", echo = F, verbose = F)
source("westcoast-networks/scripts/02_Simulation/0_Port_Regions/01a_ChoiceFunction_ASC.R", echo = F, verbose = F)
source("westcoast-networks/scripts/02_Simulation/0_Port_Regions/01a_ASC_Calibrate_Functions.R", echo = F, verbose = F)
source("westcoast-networks/scripts/02_Simulation/0_Port_Regions/Choice_Function_Functions/03a_fulleredges.R", echo = F, verbose = F)
# optimize ----------------------------------------------------------------

stol <- .01
dtol <- 5
max_iter_loop <- 4
max_iter_sc <- 100
max_iter_fc <- 25
vessels_in <- 200
scalet1ev <- 1000

regions <- c("Eureka", "Monterey", "Morro Bay", "Fort Bragg", "Bodega", "San Francisco")

projections <- c("fuller", "ext")
years <- unique(start$df_filter$LANDING_YEAR)

# Region Year Load In -----------------------------------------------------
# R_Use <- regions[6]
# Y_Use <- years[4]

for(R_Use in regions){
  for(Y_Use in years){
    
    region <- regions[3]
    year <- 2017
      
    df <- start$df_filter %>% 
      filter(LANDING_YEAR == year,
             Region == region)
    fisherylist_use <- unique(df$FISHERY_ID)
    nfisheries <- length(fisherylist_use)
    
    targets_shares <- df %>% 
      distinct(VESSEL_ID, LengthBin, FISHERY_ID, LANDING_WEEK) %>% 
      group_by(VESSEL_ID, LengthBin) %>% 
      add_tally() %>% 
      ungroup() %>% 
      group_by(LengthBin) %>% 
      mutate(NVessels = n_distinct(VESSEL_ID)*53) %>% 
      ungroup() %>% 
      group_by(LengthBin, FISHERY_ID, NVessels) %>% 
      tally() %>% 
      mutate(Shares = n/NVessels) %>% 
      ungroup() %>% 
      group_by(LengthBin) %>%
      mutate(Check = sum(Shares)) %>% 
      arrange(FISHERY_ID) %>% 
      group_by(LengthBin) %>% 
      group_split()
    
    targets_ee <- start$df_edges %>% 
      filter(LANDING_YEAR == year,
             Region == region) %>%
      distinct(Fishery1, Fishery2, UniqueVessels) %>% 
      arrange(as.character(Fishery1), as.character(Fishery2)) %>%
      pivot_wider(names_from = Fishery2, values_from = UniqueVessels) %>%
      column_to_rownames("Fishery1") %>%
      select(sort(colnames(.))[[1]], everything()) %>%
      as.matrix()
    targets_lower <- targets_ee[lower.tri(targets_ee)]
    
    bipmat <- df %>%   
      group_by(VESSEL_ID, FISHERY_ID) %>% 
      summarise(Weights = n_distinct(LANDING_WEEK), .groups = "drop") %>% 
      arrange(FISHERY_ID) %>% 
      pivot_wider(names_from = FISHERY_ID, values_from = Weights, values_fill = 0) %>%
      column_to_rownames("VESSEL_ID") %>%
      as.matrix()
    
    connectivity <- matrix(0, nfisheries+1, nfisheries+1)
    colnames(connectivity) <- rownames(connectivity) <- sort(c(fisherylist_use,"Not Fishing"))
    
    edges_fuller <- fulleredges(connectivity, bipmat) %>% 
      filter(Fishery1 != "Not Fishing",
             Fishery2 != "Not Fishing")
    targets_ef <- edges_fuller$Weight
    
    asc_fc_blanks <- fcn_asc_fc(start$df_filter, region, year)
    asc_sc_blanks <- fcn_asc_sc(start$df_edges, region, year, vessels_in)
      
    df_exprev <- start$df_exprev %>% 
      filter(LANDING_YEAR == year,
             Region == region) 
    
    guess <- df_exprev %>% 
      filter(ExpRev > 0) %>% 
      group_by(FISHERY_ID, LengthBin) %>% 
      summarise(Min = min(ExpRev)) %>% 
      ungroup() %>% 
      arrange(LengthBin, FISHERY_ID) %>% 
      group_split(LengthBin)
    
      
      # 
      asc_fc_0 <- map(guess, ~(.x$Min))
      asc_fc_in <- asc_fc_0
      # asc_fc_0 <- map(asc_fc_blanks$asc_fc_blanks_list, ~ runif(length(.x), 100, 1000))
      set.seed(Sys.time())
      asc_sc_0 <- runif(length(asc_sc_blanks$asc_sc_0), 1000, 20000)
      
      ebench <- Inf
      asc_sc_try <- asc_sc_0 
      for (try in 1:10){
       print(try)
        e <- obj_fcn1_f(asc_sc_try)
        if(e < ebench){
          ebench <- e
          asc_sc_0 <- asc_sc_try
        } 
        set.seed(Sys.time())
        asc_sc_try <- runif(length(asc_sc_blanks$asc_sc_0), 1000, 20000)
        print(e)
        print(asc_sc_0)
      }
      obj_fcn1_f(asc_sc_0)
      
      l <- 1
      # bothprojections <- for(l in 1:2){
        
        projection <- projections[1]
        
        obs_0 <- choices_asc_cal(start = start, scale_t1ev = scalet1ev, vessels_in = 200, stochastic = F,
                                 region = region, year = year,
                                 prep_df_fc = asc_fc_blanks$prep_df_fc, asc_fc = asc_fc_0,
                                 prep_sc_mat = asc_sc_blanks$prep_sc_mat, asc_sc = asc_sc_0)
        
        shares_0 <- asc_fc_blanks$prep_df_fc %>% 
          left_join(obs_0$fishing) %>% 
          filter(FISHERY_ID != "Not Fishing") %>%
          mutate(Shares = replace_na(Shares, 0)) 
        ee_start <- obs_0$edges_ext %>%
          filter(Fishery1 %in% fisherylist_use,
                 Fishery2 %in% fisherylist_use) %>%
          arrange(as.character(Fishery1), as.character(Fishery2)) %>%
          pivot_wider(names_from = Fishery2, values_from = UniqueVessels) %>%
          column_to_rownames("Fishery1") %>%
          select(sort(colnames(.))[[1]], everything()) %>%
          as.matrix()
        ee_start_lower <- ee_start[lower.tri(ee_start)]
        
        asc_fc_in <- asc_fc_0
        asc_sc_in <- asc_sc_0
        
        cache_edges <- matrix(nrow = length(targets_ef), ncol = max_iter_loop + 2)
        cache_shares <- matrix(nrow = nrow(shares_0), ncol = max_iter_loop + 2)
        
        cache_sc <- matrix(nrow = length(asc_sc_0), ncol = max_iter_loop + 1)
        cache_fc <- matrix(nrow = length(unlist(asc_fc_0)), ncol = max_iter_loop + 1)
        cache_alg_fc <- matrix(nrow = length(asc_fc_0), ncol = max_iter_loop)
        cache_alg_sc <- matrix(nrow = 2, ncol = max_iter_loop)
        
        if (projection == "fuller"){cache_edges[,1] <- targets_ef} else {cache_edges[,1] <- targets_lower}
        cache_shares[,1] <- bind_rows(targets_shares)$Shares
        
        cache_sc[,1] <- c(asc_sc_0)
        cache_fc[,1] <- unlist(asc_fc_0)
        if (projection == "fuller"){cache_edges[,2] <- obs_0$edges_fuller$Weight} else {cache_edges[,2] <- ee_start_lower}
        cache_shares[,2] <- shares_0$Shares
        
        i <- 1
        # j <- 1
        # max_iter_loop
        for (i in 1:4){
          
          fc_benchmark <- unlist(asc_fc_in)
          sc_benchmark <- asc_sc_in
          
          result <- nloptr(
            x0 = asc_sc_in,                    
            eval_f = if(projection == "fuller"){obj_fcn1_f}else{obj_fcn1_e},              
            opts = list(
              algorithm = "NLOPT_LN_NELDERMEAD",  
              # maxeval = max_iter_sc,
              maxeval = 25,
              ftol_rel = .1,
              xtol_rel = .1,
              print_level = 1
            )
            ,
            lb = rep(0, length(asc_sc_0)),
            ub = rep(20000, length(asc_sc_0))
          )
          
          asc_sc_in <- result$solution
          cache_alg_sc[1,i] <- result$iterations
          cache_alg_sc[2,i] <- result$objective
          
          # length(asc_fc_0)
          j <- 2
          for (j in 1:length(asc_fc_0)) {
            cat("Optimizing ASC for LengthBin", j, "\n")
            S_obs <- targets_shares[[j]]$Shares
            delta_costs <- asc_fc_in[[j]]
            
            # max_iter_fc
            for (iter in 1:40) {
              # delta_costs <- delta_new
              S_sim <- share_sim_j(f, delta_costs, j, asc_fc_in, asc_sc_in)
              
              # if(max(abs(S_sim - S_obs)) > .5){
                # delta_new <- delta_costs - log(pmax(S_obs, 1e-8) / pmax(S_sim, 1e-8)) * 10
              # } else {
                delta_new <- delta_costs - log(pmax(S_obs, 1e-8) / pmax(S_sim, 1e-8)) * 50
              # }

              S_obs
              S_sim
              delta_costs
              delta_new
              
              if (max(abs(S_sim - S_obs)) < stol 
                  || max(abs(log(pmax(S_obs, 1e-8) / pmax(S_sim, 1e-8)))) < 1
              ) {
                cat("  Converged in", iter, "iterations\n Costs: $", 
                    round(delta_new), "\n")
                break
              }
              
              delta_costs <- delta_new
              print(paste0(iter,": ",sort(abs(round(S_obs-S_sim,4)))))
            }
            
            # Store updated delta
            asc_fc_in[[j]] <- delta_costs
            cache_alg_fc[[j,i]] <- iter
          }
          
          cache <- choices_asc_cal(start = start, scale_t1ev = scalet1ev, vessels_in = 200, stochastic = F,
                                   region = region, year = year,
                                   prep_df_fc = asc_fc_blanks$prep_df_fc, asc_fc = asc_fc_in,
                                   prep_sc_mat = asc_sc_blanks$prep_sc_mat, asc_sc = asc_sc_in)
          
          shares_cache <- asc_fc_blanks$prep_df_fc %>% 
            left_join(cache$fishing) %>% 
            filter(FISHERY_ID != "Not Fishing") %>%
            mutate(Shares = replace_na(Shares, 0)) 
          ee_cache <- cache$edges_ext %>%
            filter(Fishery1 %in% fisherylist_use,
                   Fishery2 %in% fisherylist_use) %>%
            arrange(as.character(Fishery1), as.character(Fishery2)) %>%
            pivot_wider(names_from = Fishery2, values_from = UniqueVessels) %>%
            column_to_rownames("Fishery1") %>%
            select(sort(colnames(.))[[1]], everything()) %>%
            as.matrix()
          ee_cache_lower <- ee_cache[lower.tri(ee_cache)]
          
          if (projection == "fuller"){cache_edges[,i+2] <- cache$edges_fuller$Weight} else {cache_edges[,i+2] <- ee_cache_lower}
          cache_shares[,i+2] <- shares_cache$Shares
          cache_sc[,i+1] <- c(asc_sc_in)
          cache_fc[,i+1] <- unlist(asc_fc_in)
          
          if ((max(abs(cache_sc[,i]-cache_sc[,i+1]))<200)*
              (max(abs(cache_fc[,i]-cache_fc[,i+1]))<200)*
              (max(abs(cache_edges[,1]-cache_edges[,i+2]))<5)*
              (max(abs(cache_shares[,1]-cache_shares[,i+2]))<.05)) {
            cat("  Converged in", i, "\n")
            break
          } else {
            cat("Completed iter",i,"\n")
          }
        }
        
        cache_all <- list(cache_alg_fc = cache_alg_fc, cache_alg_sc = cache_alg_sc, 
                          cache_edges = cache_edges, cache_sc = cache_sc,
                          cache_shares = cache_shares, cache_fc = cache_fc)
        write_rds(cache_all, paste0("~/westcoast-networks/data/Simulation/ASC_Calibration_Regional/",
                                    region,"_",year,"_",projection,".rds"))
        
      }
      
    }
    

cor(cache_shares[,1],cache_shares[,6])
cor(cache_edges[,1],cache_edges[,6])

plot(cache_shares[,1],cache_shares[,6])
plot(cache_edges[,1],cache_edges[,6])

cor(Monterey_2017_1_fuller$cache_shares[,1],Monterey_2017_1_fuller$cache_shares[,6])
cor(Monterey_2017_1_fuller$cache_edges[,1],Monterey_2017_1_fuller$cache_edges[,6])

plot(Monterey_2017_1_fuller$cache_shares[,1],Monterey_2017_1_fuller$cache_shares[,6])
plot(Monterey_2017_1_fuller$cache_edges[,1],Monterey_2017_1_fuller$cache_edges[,6])

Monterey_2017_1_fuller$cache_sc

unlist(targets_shares$)

















