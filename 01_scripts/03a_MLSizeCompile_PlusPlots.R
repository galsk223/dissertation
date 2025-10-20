rm(list = ls())
library(zoo)
library(DescTools)
library(tidyverse)
library(future)

base_name <- "~/westcoast-networks/data/Simulation/DynamicSimulationOutcomes/"
fileid <- "MLSizeCacheMeta2_"
allfolders <- list(fl1 = list.files(paste0(base_name, fileid, 1, "/"), full.names = T),
                   fl2 = list.files(paste0(base_name, fileid, 2, "/"), full.names = T),
                   fl3 = list.files(paste0(base_name, fileid, 3, "/"), full.names = T),
                   fl4 = list.files(paste0(base_name, fileid, 4, "/"), full.names = T),
                   fl5 = list.files(paste0(base_name, fileid, 5, "/"), full.names = T),
                   fl6 = list.files(paste0(base_name, fileid, 6, "/"), full.names = T),
                   fl7 = list.files(paste0(base_name, fileid, 7, "/"), full.names = T),
                   fl8 = list.files(paste0(base_name, fileid, 8, "/"), full.names = T),
                   fl9 = list.files(paste0(base_name, fileid, 9, "/"), full.names = T),
                   fl10 = list.files(paste0(base_name, fileid, 10, "/"), full.names = T))

# functions ---------------------------------------------------------------


# df <- d$sim_run$cache_e_v[[1]]
outprep <- function(df, steadystate_pre, droppedfeature, return, vessels1510, end, d, fn){

  drop <- list(d$sim_id$Drop)

  do <- df %>%
    mutate(SS_Vessels = steadystate_pre$SS_Vessels,
           SS_Revenue = steadystate_pre$SS_Revenue,
           SS_Weeks = steadystate_pre$SS_Weeks,
           SS_Fisheries = steadystate_pre$SS_Fisheries,
           Return = ifelse(nrow(return) == 0, 25, return$Year-16),
           Vessels1 = vessels1510$NVessels[[1]]/steadystate_pre$SS_Vessels,
           Vessels5 = vessels1510$NVessels[[2]]/steadystate_pre$SS_Vessels,
           Vessels10 = vessels1510$NVessels[[3]]/steadystate_pre$SS_Vessels,
           Revenue1 = vessels1510$MeanRevenue[[1]]/steadystate_pre$SS_Revenue,
           Revenue5 = vessels1510$MeanRevenue[[2]]/steadystate_pre$SS_Revenue,
           Revenue10 = vessels1510$MeanRevenue[[3]]/steadystate_pre$SS_Revenue,
           EndVessels = end$SS_Vessels,
           EndRevenue = end$SS_Revenue,
           FishCosts = mean(d$rscalef),
           SwitchCosts = mean(d$rscales),
           Drop = drop) %>%
    bind_cols(distinct(d$sim_id %>% select(-Drop)))

  if(fn == T){
    do <- do %>%
      left_join(droppedfeature, by = c("Fishery" = "FISHERY_ID"))
  } else {
    do
  }

}


# df <- d$sim_run$cache_vessels
# years <- (end$Year-4):(end$Year)
ineqfx <- function(df, years){

  pre <- bind_rows(df) %>%
    filter(Year %in% c(11:15)) %>%
    group_by(Year) %>%
    mutate(Decile = ntile(Revenue_Fished, 10)) %>%
    ungroup() %>%
    filter(Revenue_Fished > 0) %>%
    mutate(MedianRevenue = median(Revenue_Fished),
           Gini = Gini(Revenue_Fished)) %>%
    group_by(Decile) %>%
    mutate(MeanRevenue = mean(Revenue_Fished))

  post <- bind_rows(df) %>%
    filter(Year %in% years) %>%
    group_by(Year) %>%
    mutate(Decile = ntile(Revenue_Fished, 10)) %>%
    ungroup() %>%
    filter(Revenue_Fished > 0) %>%
    mutate(MedianRevenue = median(Revenue_Fished),
           Gini = Gini(Revenue_Fished, na.rm = T)) %>%
    group_by(Decile) %>%
    mutate(MeanRevenue = mean(Revenue_Fished))

  out <- tibble(MedianRevenue = unique(post$MedianRevenue)/unique(pre$MedianRevenue),
                Gini = unique(post$Gini) - unique(pre$Gini),
                RevenueGap = (sort(unique(post$MeanRevenue))[10]-sort(unique(post$MeanRevenue))[1])/
                  (sort(unique(pre$MeanRevenue))[10]-sort(unique(pre$MeanRevenue))[1]))

}

start <- read_rds("/home/gkoss/westcoast-networks/data/clean/Simulation/empiricalbenchmarks_meta.rds")
log_dir <- "/home/gkoss/westcoast-networks/data/Simulation/DynamicSimulationOutcomes/MLSizeCacheMeta2_Logs/"
dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)

# loop  -------------------------------------------------------------------

f <- 8
c <- 4

for (f in 7:length(allfolders)){

  folder <- allfolders[[f]]

  log_file <- file.path(log_dir, sprintf("worker_%02d.log", f))
  if (file.exists(log_file)) file.remove(log_file)

  plan(multisession, workers = 32)
  df_sim <- furrr::future_map(1:length(folder), function(c){
    # df_sim <- map(1:length(folder), function(c){
    if(c %% 10 == 0){
    msg <- sprintf("f = %d, c = %d : started at %s", f, c, Sys.time())
    write(msg, file = log_file, append = TRUE)

      print(paste("folder",f,"- file",c))
    }

    d <- read_rds(folder[c])
    # glimpse(d$sim_id)

    if(length(d$sim_run) == 0){return(NULL)}

    vessels_time <- bind_rows(d$sim_run$cache_vessels) %>%
      group_by(Year) %>%
      summarise(NVessels = n_distinct(Vessel_ID),
                MeanRevenue = mean(Revenue_Fished),
                MedianRevenue = median(Revenue_Fished),
                MeanWeeks = mean(Weeks_Fished),
                Iteration = unique(d$sim_id$iter),
                FishCosts = mean(d$rscalef),
                SwitchCosts = mean(d$rscales)) %>%
      ungroup()

    dropped_time <- bind_rows(d$sim_run$cache_dfchoice, .id = "Year") %>%
      filter(FISHERY_ID != "Not Fishing") %>%
      mutate(Year = as.numeric(Year)) %>%
      group_by(Year, FISHERY_ID) %>%
      summarise(NVessels = n_distinct(Vessel_ID),
                TotalRev = sum(Revenue),
                TotalWks = n(),
                MeanRev = TotalRev/NVessels,
                MeanWks = TotalWks/NVessels, .groups = "drop") %>%
      group_by(Year) %>%
      mutate(NFisheries = n_distinct(FISHERY_ID),
             Dropped = ifelse(FISHERY_ID %in% d$sim_id$Drop, 1, 0)) %>%
      ungroup()

    if("vessels_in" %in% colnames(d$sim_id)){

      bin_df <- start$df_vsets %>%
        filter(LANDING_YEAR == 2017) %>%
        mutate(SimFleet = unique(d$sim_id$vessels_in),
               RegionExp = RegionAll/YearAll*SimFleet,
               LengthRegionExp = RegionExp*Prob) %>%
        filter(LengthRegionExp > 5)
      nvessels <- sum(ceiling(bin_df$LengthRegionExp))
      v_set <- tibble(Vessel_ID = 101:(nvessels+100),
                      HomeRegion = purrr::map2(bin_df$Region, ceiling(bin_df$LengthRegionExp), rep) %>%
                        unlist(),
                      LengthBin = purrr::map2(bin_df$LengthBin, ceiling(bin_df$LengthRegionExp), rep) %>%
                        unlist())

      vregion_time <- bind_rows(d$sim_run$cache_vessels) %>%
        left_join(v_set, by = join_by(Vessel_ID, LengthBin)) %>%
        group_by(Year, HomeRegion) %>%
        summarise(NVessels = n_distinct(Vessel_ID),
                  MeanRevenue = mean(Revenue_Fished),
                  MeanWeeks = mean(Weeks_Fished), .groups = "drop")

      dropped_time <- bind_rows(d$sim_run$cache_dfchoice, .id = "Year") %>%
        left_join(v_set, by = join_by(Vessel_ID, LengthBin)) %>%
        filter(FISHERY_ID != "Not Fishing") %>%
        mutate(Year = as.numeric(Year)) %>%
        group_by(Year, FISHERY_ID, HomeRegion) %>%
        summarise(NVessels = n_distinct(Vessel_ID),
                  TotalRev = sum(Revenue),
                  TotalWks = n(),
                  MeanRev = TotalRev/NVessels,
                  MeanWks = TotalWks/NVessels, .groups = "drop") %>%
        group_by(Year) %>%
        mutate(NFisheries = n_distinct(FISHERY_ID),
               Dropped = ifelse(FISHERY_ID %in% d$sim_id$Drop, 1, 0), .groups = "drop") %>%
        left_join(vregion_time, by = c("Year", "HomeRegion"))

    }

    fisheries <- bind_rows(d$sim_run$cache_dfchoice, .id = "Year") %>%
      mutate(Year = as.numeric(Year)) %>%
      group_by(Year) %>%
      filter(FISHERY_ID != "Not Fishing") %>%
      summarise(NFisheries_Total = n_distinct(FISHERY_ID))

    steadystate_pre <- vessels_time %>%
      filter(Year %in% c(11:15)) %>%
      left_join(fisheries, by = "Year") %>%
      summarise(SS_Vessels = mean(NVessels),
                SS_Revenue = mean(MeanRevenue),
                SS_MedRevenue = mean(MedianRevenue),
                SS_Weeks = mean(MeanWeeks),
                SS_Fisheries = mean(NFisheries_Total))

    # return <- vessels_time %>%
    #   filter(Year >= 16) %>%
    #   mutate(SS_Vessels = rollmean(NVessels, k = 5, fill = NA, align = "right"),
    #          SS_Revenue = rollmean(MeanRevenue, k = 5, fill = NA, align = "right"),
    #          SS_Weeks = rollmean(MeanWeeks, k = 5, fill = NA, align = "right"),
    #          SS_Vessels_Pre = steadystate_pre$SS_Vessels,
    #          Return = ifelse(SS_Vessels >= SS_Vessels_Pre, 1, 0)) %>%
    #   filter(Return == 1) %>%
    #   dplyr::slice(1)

    return <- vessels_time %>%
      filter(Year >= 16) %>%
      mutate(SS_Vessels = rollmean(NVessels, k = 3, fill = NA, align = "right"),
             SS_Revenue = rollmean(MeanRevenue, k = 3, fill = NA, align = "right"),
             SS_Weeks = rollmean(MeanWeeks, k = 3, fill = NA, align = "right"),
             SS_Vessels_Pre = steadystate_pre$SS_Vessels,
             Return = ifelse(SS_Vessels >= SS_Vessels_Pre, 1, 0)) %>%
      filter(Return == 1) %>%
      dplyr::slice(1)

    end <- vessels_time %>%
      filter(Year >= 16) %>%
      mutate(SS_Vessels = rollmean(NVessels, k = 3, fill = NA, align = "right"),
             SS_Revenue = rollmean(MeanRevenue, k = 3, fill = NA, align = "right"),
             SS_Weeks = rollmean(MeanWeeks, k = 3, fill = NA, align = "right"),
             SS_Vessels_Pre = steadystate_pre$SS_Vessels,
             Return = ifelse(SS_Vessels >= SS_Vessels_Pre, 1, 0)) %>%
      dplyr::slice_tail(n = 1)

    vessels1510 <- vessels_time %>%
      filter(Year %in% c(16,21,26)) %>%
      arrange(Year) %>%
      mutate(VesselWeeks = NVessels*MeanWeeks) %>%
      left_join(fisheries, by = "Year")

    if (nrow(end) == 0){return(NULL)}

    droppedfeature <- bind_rows(d$sim_run$cache_dfchoice[11:15], .id = "Year") %>%
      filter(FISHERY_ID %in% d$sim_id$Drop) %>%
      group_by(FISHERY_ID, Year, Vessel_ID) %>%
      summarise(SS_Drop_Weeks = n(),
                SS_Drop_Revenue = sum(Revenue), .groups = "drop") %>%
      group_by(FISHERY_ID, Year) %>%
      summarise(SS_Drop_Vessels = n_distinct(Vessel_ID),
                SS_Drop_Weeks = mean(SS_Drop_Weeks),
                SS_Drop_Revenue = mean(SS_Drop_Revenue), .groups = "drop") %>%
      group_by(FISHERY_ID) %>%
      summarise(SS_Drop_Vessels = mean(SS_Drop_Vessels),
                SS_Drop_Weeks = mean(SS_Drop_Weeks),
                SS_Drop_Revenue = mean(SS_Drop_Revenue), .groups = "drop")

    if (nrow(return) == 0){
      ineqall <- ineqfx(bind_rows(d$sim_run$cache_vessels), (end$Year-4):(end$Year))
    } else {
      ineqall <- ineqfx(bind_rows(d$sim_run$cache_vessels), (return$Year-4):(return$Year))
      }

    # if(vesseldropfix == T){
    #   cache_dfchoice <- d$sim_run$cache_e[[1]]
    #   largestcc <- T
    #   burnin <- 13
    #   yi <- 14
    #   nvessels <-
    #
    #   cache_ttran[[yi+1-burnin]] <- graph_time_trans(cache_dfchoice, burnin, yi, nvessels, largestcc)
    #   cache_tcor[[yi+1-burnin]] <- graph_time_corr(cache_dfchoice, yi, nvessels, largestcc)
    #
    #   cache_e[[yi+1-burnin]] <- graph_ext_fcn(cache_dfchoice[[yi]], fishery_pairs, fisherylist_use, yi, largestcc)
    #   cache_f[[yi+1-burnin]] <- graph_fuller_fcn(cache_dfchoice[[yi]], yi, fisherylist_use, largestcc)
    #   cache_b[[yi+1-burnin]] <- graph_bip_fcn(cache_dfchoice[[yi]], yi, nfisheries)
    #
    #   vessel_pairs <- expand.grid(Vessel1 = as.character(unique(cache_dfchoice[[yi]]$Vessel_ID)),
    #                               Vessel2 = as.character(unique(cache_dfchoice[[yi]]$Vessel_ID)), stringsAsFactors = FALSE) %>%
    #     filter(Vessel1 != Vessel2) %>%
    #     arrange(Vessel1, Vessel2)
    #
    #   cache_e_v[[yi+1-burnin]] <- graph_ext_fcn_ves(cache_dfchoice[[yi]], fisherylist_use, vessel_pairs, yi, drop)
    #   cache_f_v[[yi+1-burnin]] <- graph_fuller_fcn_ves(cache_dfchoice[[yi]], yi, fisherylist_use, drop)
    # }

    if(nrow(d$sim_run$cache_e[[1]])<=1){
      network_pre_e <- NULL
    } else {
      network_pre_e <- outprep(d$sim_run$cache_e[[1]], steadystate_pre, droppedfeature, return, vessels1510, end, d, T)
      # %>%
      #   mutate(SI2 = si_e(d))
    }
    if(nrow(d$sim_run$cache_f[[1]])<=1){
      network_pre_f <- NULL
    } else {
      network_pre_f <- outprep(d$sim_run$cache_f[[1]], steadystate_pre, droppedfeature, return, vessels1510, end, d, T)
      # %>%
      #   mutate(SI = si_f(d))
    }
    if(nrow(d$sim_run$cache_b[[1]])==0){
      network_pre_b <- NULL
    } else {
      network_pre_b <- outprep(d$sim_run$cache_b[[1]], steadystate_pre, droppedfeature, return, vessels1510, end, d, T)
      # %>%
      #   left_join(si_b(d), by = "Componenet")
    }
    network_pre_tc <- outprep(d$sim_run$cache_tcor[[1]], steadystate_pre, droppedfeature, return, vessels1510, end, d, T)
    # %>%
    #   mutate(SI = si_tc(d))
    network_pre_tt <- outprep(d$sim_run$cache_ttran[[1]], steadystate_pre, droppedfeature, return, vessels1510, end, d, T)
    # %>%
    #   mutate(SI = si_tt(d))
    if(nrow(d$sim_run$cache_e_v[[1]])==0){
      network_pre_ev <- NULL
    } else {
      network_pre_ev <- outprep(d$sim_run$cache_e_v[[1]], steadystate_pre, droppedfeature, return, vessels1510, end, d, F)
      # %>%
      #   left_join(si_ev(d), by = "Component") %>%
      #   mutate(SI = SI.y)
    }
    if(nrow(d$sim_run$cache_f_v[[1]])==0){
      network_pre_fv <- NULL
    } else {
      network_pre_fv <- outprep(d$sim_run$cache_f_v[[1]], steadystate_pre, droppedfeature, return, vessels1510, end, d, F)
      # %>%
      #   left_join(si_fv(d), by = "Component") %>%
      #   mutate(SI = SI.y)
    }

    msg <- sprintf("f = %d, c = %d : finished at %s", f, c, Sys.time())
    write(msg, file = log_file, append = TRUE)

    return(list(vessels_time = vessels_time,
                dropped_time = dropped_time,
                network_pre_e = network_pre_e,
                network_pre_f = network_pre_f,
                network_pre_b = network_pre_b,
                network_pre_tc = network_pre_tc,
                network_pre_tt = network_pre_tt,
                network_pre_ev = network_pre_ev,
                network_pre_fv = network_pre_fv,
                ineqall = ineqall,
                id = d$sim_id$iter))
    rm(vessels1510)

  })

  iter <- sample(1:150, 150)
  vplot <- map_dfr(df_sim, function(i){i$vessels_time})
  # %>%
  #   filter(Iteration %in% iter)

  vp <- ggplot(vplot, aes(x = Year, y = NVessels, group = Iteration, color = FishCosts)) +
    geom_line(linewidth = 1, alpha = .5) +
    geom_vline(xintercept = 16, color = "#B592A0") +
    geom_label(data = data.frame(x = 20, y = 195, label = "Fishery Closure"),
               aes(x = x, y = y, label = label),
               color = "#1E585C", size = 2,
               inherit.aes = FALSE) +
    theme_minimal() +
    scale_color_gradient(low = "#AAFAC8", high = "#2D728B") +
    labs(y = "# Vessels",
         title = "Regional Fleets Before and After Shock",
         color = "Fishing Costs \n Scaled by:")

  df_net <- map_dfr(df_sim, function(i){i$network_pre_f}) %>%
    distinct(SS_Vessels, SS_Fisheries, N_Fisheries, Vessels1, Vessels5, Vessels10, Return, iter) %>%
    mutate(Recover = ifelse(Return < 25, "Recovered", "New Steady State"),
           RecoverI = ifelse(Return < 25, 1, 0),
           Vessel1N = Vessels1*SS_Vessels,
           VesselDrop = SS_Vessels-Vessel1N)
  # hist(df_net$Return)
  np <- ggplot(df_net, aes(x = SS_Vessels, y = Return, color = Recover)) +
    geom_point(size = 2) +
    theme_minimal()

  allout <- list(df_sim, vp, np)
  outfile <- paste0("~/westcoast-networks/data/clean/Simulation/projectiondataforML_",fileid,f,".rds")
  write_rds(allout, outfile)

}



f <- 6
t1 <- read_rds(paste0("~/westcoast-networks/data/clean/Simulation/projectiondataforML_",fileid,f,".rds"))
t1[[3]]
t1[[2]]
t <- t1[[1]][[1]]
#
# df_net <- map_dfr(dfst, function(i){i$network_pre_b}) %>%
#   distinct(SS_Vessels, Return) %>%
#   mutate(Recover = ifelse(Return < 35, "Recovered", "New Steady State"))
# # hist(df_net$Return)
# np <- ggplot(df_net, aes(x = SS_Vessels, y = Return, color = Recover)) +
#   geom_point(size = 2) +
#   theme_minimal()
# np
#




# randconditions <- expand_grid(skillrand = c(T,F),
#                               scalerand = c(T,F),
#                               costbyfishery = c(T,F),
#                               shockpermanent = c(T,F)) %>%
#   rowid_to_column() %>%
#   filter(rowid %in% c(1,2,15))
#
# # 1 = all true
# # 2 = stochastic true, single year shock
# # 3 = no stochastic with permanent shock
# o1 <- read_rds("~/westcoast-networks/data/clean/Simulation/projectiondataforML_preprocess_1.rds")
# o2 <- read_rds("~/westcoast-networks/data/clean/Simulation/projectiondataforML_preprocess_2.rds")
# o3 <- read_rds("~/westcoast-networks/data/clean/Simulation/projectiondataforML_preprocess_3.rds")
#
# p1 <- o1[[2]]
# +
#   geom_line(alpha = .5) +
#   labs(title = "",
#        x = "",
#        subtitle = "Stochastic Costs + Starting Skill, Allee Effect by Fishery, Permanent Closure")
# p2 <- o2[[2]] +
#   labs(title = "",
#        subtitle = "Stochastic Costs + Starting Skill, Allee Effect by Fishery, Single Year Closure")
# p3 <- o3[[2]] +
#   labs(subtitle = "Permanent Closure",
#        x = "")
#
# library(patchwork)
# p3 / p1 / p2
