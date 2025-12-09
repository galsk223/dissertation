# rm(list = ls())
library(zoo)
library(DescTools)
library(tidyverse)
library(future)

base_name <- "~/westcoast-networks/data/Simulation/StaticSimulationOutcomes/"
fileid <- "Static_adjustment"
allfiles <- list.files(paste0(base_name), full.names = T)
allfiles <- allfiles[file.info(allfiles)$isdir == FALSE]

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

df_dist <- start$regionadjacency %>%
  select(-DistanceCostP) %>%
  mutate(DistanceCost = ifelse(is.na(value), 1,
                               (value+1)*distparameter+1-distparameter))

# loop  -------------------------------------------------------------------

jall <- c("scalar", "sc",
          "fc","fc1","fc2",
          "kk", "knk", "nknk",
          "kk1", "kk2",
          "knk1", "knk2",
          "nknk1", "nknk2", "smaller")

j <- 5
f <- 1
c <- 1
# for (j in 1:5){

jfiles <- allfiles[str_detect(allfiles,str_c(jall[j],"[:punct:]|[5]"))]

# allf <- map(1:length(jfiles), function(f){

  alliter <- read_rds(jfiles[[f]])

  allnet <- map(1:length(alliter), function(c){

    print(c)
    d <- alliter[[c]]

    if(length(d$sim_run) == 0){return(NULL)}

    diverse <- d$sim_run$cache_dfchoice[[1]] %>%
      group_by(Vessel_ID) %>%
      filter(FISHERY_ID != "Not Fishing") %>%
      summarise(Diversification = n_distinct(FISHERY_ID),
                .groups = "drop")

    weeksportion <- d$sim_run$cache_dfchoice[[1]] %>%
      filter(FISHERY_ID != "Not Fishing") %>%
      group_by(Vessel_ID, FISHERY_ID) %>%
      summarise(Weeks = n(),
                .groups = "drop") %>%
      full_join(diverse, by = "Vessel_ID") %>%
      mutate(KeyVessel = str_detect(FISHERY_ID, "Dungeness|Salmon"),
             WeeksKey = Weeks*KeyVessel) %>%
      group_by(Vessel_ID, Diversification) %>%
      summarise(WeeksKey = sum(WeeksKey),
                KeyVessel = WeeksKey > 0,
                .groups = "drop")

    if(max(diverse$Diversification) == 1){return(NULL)}
    dtrav <- d$sim_run$cache_dfchoice[[1]] %>%
      left_join(v_set, by = c("Vessel_ID","LengthBin")) %>%
      filter(FISHERY_ID != "Not Fishing") %>%
      mutate(DestRegion = str_extract(FISHERY_ID, "(?<=, ).+")) %>%
      left_join(df_dist, by = c("HomeRegion" = "Region",
                                "DestRegion" = "name")) %>%
      mutate(RegionTrav = replace_na(value, 0)) %>%
      group_by(Vessel_ID) %>%
      summarise(MeanDist = mean(RegionTrav, na.rm = T),
                Revenue = sum(Revenue)) %>%
      mutate(iter = (f-1)*100+unique(d$sim_id$iter))
    revquant <- d$sim_run$cache_dfchoice[[1]] %>%
      group_by(Vessel_ID) %>%
      filter(FISHERY_ID != "Not Fishing") %>%
      summarise(AnnualRevenue = sum(Revenue))

    strategies <- d$sim_run$cache_dfchoice[[1]] %>%
      group_by(Vessel_ID) %>%
      filter(FISHERY_ID != "Not Fishing") %>%
      summarize(
        n_fisheries = n_distinct(FISHERY_ID),
        strategy = paste(sort(unique(FISHERY_ID)), collapse = "-"),
        .groups = "drop"
      ) %>%
      filter(n_fisheries >= 2) %>%
      mutate(Key = str_detect(strategy, "Dungeness|Salmon"))

    out <- tibble(iter = (f-1)*100+unique(d$sim_id$iter),
                  FishingCost = d$sim_id$rscalef,
                  SwitchingCost = d$sim_id$rscales,
                  N_diversified2 = sum(diverse$Diversification >= 2),
                  N_diversified3 = sum(diverse$Diversification >= 3),
                  N_diversified4 = sum(diverse$Diversification >= 4),
                  N_diversified5 = sum(diverse$Diversification >= 5),
                  MeanDiversification_All = mean(diverse$Diversification),
                  MeanDiversification_Diverse = mean(diverse$Diversification[diverse$Diversification >= 2]),
                  MeanDiversification_Key = mean(weeksportion$Diversification[weeksportion$KeyVessel == T]),
                  MeanDiversification_NKey = mean(weeksportion$Diversification[weeksportion$KeyVessel == F]),
                  N_strategies = n_distinct(strategies$strategy),
                  N_strategiesKey = n_distinct(strategies$strategy[strategies$Key == T]),
                  KeyVessels = sum(weeksportion$KeyVessel),
                  KeyWeeks = mean(weeksportion$WeeksKey),
                  N_diversified2 = sum(diverse$Diversification >= 2),
                  DistMean = mean(dtrav$MeanDist),
                  DistMed = median(dtrav$MeanDist),
                  RevMean = mean(dtrav$Revenue),
                  RevMed = median(dtrav$Revenue),
                  RevGini = Gini(dtrav$Revenue),
                  Rev10 = quantile(revquant$AnnualRevenue, .1),
                  Rev90 = quantile(revquant$AnnualRevenue, .9),
                  Rev9010 = Rev90-Rev10)

    net_e <- bind_cols(out, d$sim_run$cache_e[[1]] %>%
                         mutate(Cluster = as.numeric(Cluster),
                                NClusters = max(Cluster)))
    net_f <- bind_cols(out, d$sim_run$cache_f[[1]] %>%
                         mutate(Cluster = as.numeric(Cluster),
                                NClusters = max(Cluster)))
    net_b <- bind_cols(out, d$sim_run$cache_b)

    return(list(net_e = net_e,
                net_f = net_f,
                net_b = net_b,
                dtrav = dtrav,
                d = d))

  })

  k <- map_dfr(allnet, "net_e") %>%
    distinct(iter, .keep_all = T)
  nk <- map_dfr(allnet, "net_e") %>%
    distinct(iter, .keep_all = T)

plot(k$FishingCost, k$N_strategies)
plot(nk$FishingCost, nk$N_strategies)

# plot(k$FishingCost, k$N_strategiesKey)
# plot(nk$FishingCost, nk$N_strategiesKey)

plot(k$FishingCost, k$N_strategiesKey/k$N_strategies)
plot(nk$FishingCost, nk$N_strategiesKey/nk$N_strategies)

plot(k$FishingCost, k$KeyVessels)
plot(k$FishingCost, k$KeyWeeks)
plot(k$FishingCost, k$MeanDiversification_Diverse)
plot(k$FishingCost, k$MeanDiversification_Key)
plot(k$FishingCost, k$MeanDiversification_NKey)

plot(nk$FishingCost, nk$KeyVessels)
plot(nk$FishingCost, nk$KeyWeeks)
plot(nk$FishingCost, nk$MeanDiversification_Diverse)
plot(nk$FishingCost, nk$MeanDiversification_Key)
plot(nk$FishingCost, nk$MeanDiversification_NKey)

hist(nk$MeanDiversification_Key/nk$MeanDiversification_All)
hist(nk$MeanDiversification_Key/nk$MeanDiversification_NKey)
mean(nk$MeanDiversification_Key/nk$MeanDiversification)
t.test(nk$MeanDiversification_Key/nk$MeanDiversification_NKey)$conf.int
feols(KeyVessels ~ scale(FishingCost), nk)

