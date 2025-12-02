allcombos <- tibble(r1 = combn(1:8, 2)[1,],
                    r2 = combn(1:8, 2)[2,])
j <- 1
allcomb <- map(1:nrow(allcombos), function(j){

  r1 <- allregimes[[allcombos$r1[[j]]]] %>%
    ungroup()
  r2 <- allregimes[[allcombos$r2[[j]]]] %>%
    ungroup()

  rt <- inner_join(r1, r2, by = c("HomeRegion", "LANDING_YEAR"))

  r <- 3
  allemp <- map_dfr(1:nrow(rt), function(r){

    cat(j,r,"\n")
    if(r == 1){
      ridx <<- 1
      ridxn <<- ridx
    } else if(rt$RegimeKey.x[[r]] == rt$RegimeKey.x[[r-1]] &
              rt$LANDING_YEAR[[r]] == (rt$LANDING_YEAR[[r-1]]+1) &
              rt$HomeRegion[[r]] == (rt$HomeRegion[[r-1]])){
      ridxn <<- ridx
    } else {
      ridxn <<- ridx + 1
    }
    if(r == 1){
      ridy <<- 1
      ridyn <<- ridy
    } else if(rt$RegimeKey.y[[r]] == rt$RegimeKey.y[[r-1]] &
              rt$LANDING_YEAR[[r]] == (rt$LANDING_YEAR[[r-1]]+1) &
              rt$HomeRegion[[r]] == (rt$HomeRegion[[r-1]])){
      ridyn <<- ridy
    } else {
      ridyn <<- ridy + 1
    }
    if(r == 1){
      rid <<- 1
    } else if(ridxn!=ridx |
              ridyn!=ridyn){
      rid <<- rid+1
    } else {
      rid <<- rid
    }

    ridx <- ridxn
    ridy <- ridyn

    df_choice <- trsplit %>%
      filter(HomeRegion == reg$HomeRegion[[r]],
             LANDING_YEAR == reg$LANDING_YEAR[[r]]) %>%
      select(Vessel_ID = VESSEL_ID,
             HomeRegion,
             Revenue = EXVESSEL_REVENUE,
             FISHERY_ID = FisheryRegion)
    fisherylist_use <- unique(df_choice$FISHERY_ID)
    fishery_pairs <- expand.grid(Fishery1 = fisherylist_use,
                                 Fishery2 = fisherylist_use,
                                 stringsAsFactors = FALSE) %>%
      filter(Fishery1 != Fishery2) %>%
      arrange(Fishery1, Fishery2)

    diverse <- df_choice %>%
      group_by(Vessel_ID) %>%
      summarise(Diversification = n_distinct(FISHERY_ID))
    strategies <- df_choice %>%
      group_by(Vessel_ID) %>%
      summarize(
        n_fisheries = n_distinct(FISHERY_ID),
        strategy = paste(sort(unique(FISHERY_ID)), collapse = "-"),
        .groups = "drop"
      ) %>%
      filter(n_fisheries >= 2)
    dtrav <- df_choice %>%
      mutate(DestRegion = str_extract(FISHERY_ID, "(?<=, ).+")) %>%
      left_join(df_dist, by = c("HomeRegion" = "Region",
                                "DestRegion" = "name")) %>%
      mutate(RegionTrav = replace_na(value, 0)) %>%
      group_by(Vessel_ID) %>%
      summarise(MeanDist = mean(RegionTrav, na.rm = T),
                Revenue = sum(Revenue))

    g_out <- graph_ext_fcn(df_choice, fishery_pairs, fisherylist_use, yi = 1, largestcc = T) %>%
      mutate(
        NVessels = n_distinct(df_choice$Vessel_ID),
        Region = rt$HomeRegion[[r]],
        Year = rt$LANDING_YEAR[[r]],
        Regime = rid,
        RegimeKey.x = rt$RegimeKey.x[[r]],
        RegimeKey.y = rt$RegimeKey.y[[r]],
        Classif.x = allcombos$r1[[j]],
        Classif.y = allcombos$r2[[j]],
        Cluster = as.numeric(Cluster),
        N_diversified2 = sum(diverse$Diversification >= 2),
        N_diversified3 = sum(diverse$Diversification >= 3),
        N_diversified4 = sum(diverse$Diversification >= 4),
        N_diversified5 = sum(diverse$Diversification >= 5),
        MeanDiversification_All = mean(diverse$Diversification),
        MeanDiversification_Diverse = mean(diverse$Diversification[diverse$Diversification >= 2]),
        N_strategies = n_distinct(strategies$strategy),
        DistMean = mean(dtrav$MeanDist),
        RevMean = mean(dtrav$Revenue),
        RevMed = median(dtrav$Revenue),
        RevGini = Gini(dtrav$Revenue))

  })

})

all <- bind_rows(allcomb, .id = "Scheme") %>%
  ungroup() %>%
  distinct(
    Region, Year, Scheme, Regime, Classif.x, Classif.y, RegimeKey.x, RegimeKey.y, N_diversified2, MeanDiversification_All, N_strategies, DistMean, RevMean,
    Year, N_Fisheries, NVessels, N_Edges, Mean_Weight,
    ClusteringCoefficient_Global, Modularity, FragMeasure) %>%
  mutate(Connectance = N_Edges/(N_Fisheries*(N_Fisheries-1)/2)) %>%
  group_by(Scheme, Regime) %>%
  mutate(YearList = list(unique(Year)),
         Label = paste0(Region,"\n",min(Year),"-",max(Year)),
         DivSign = sign(MeanDiversification_All-lag(MeanDiversification_All)),
         WeightSign = sign(Mean_Weight-lag(Mean_Weight)),
         PerfectDivWeight = sum(DivSign == WeightSign, na.rm = T),
         DivWeight = cor(MeanDiversification_All,Mean_Weight),
         StratWeight = cor(N_strategies,Mean_Weight),
         DivMod = cor(MeanDiversification_All,Modularity),
         StratMod = cor(N_strategies,Modularity),
         DivDen = cor(MeanDiversification_All,ClusteringCoefficient_Global),
         StratDen = cor(N_strategies,ClusteringCoefficient_Global),
         MeanFisheries = mean(N_Fisheries),
         FisheryDirection = mean(N_Fisheries-lag(N_Fisheries), na.rm = T),
         MeanDiverse = mean(MeanDiversification_All),
         DiverseDirection = mean(MeanDiversification_All-lag(MeanDiversification_All), na.rm = T),
         MeanStrategies = mean(N_strategies),
         StrategiesDirection = mean(N_strategies-lag(N_strategies), na.rm = T)) %>%
  distinct(Region, Label, YearList, Scheme, Classif.x, Classif.y, RegimeKey.x, RegimeKey.y, MeanFisheries, FisheryDirection,
           MeanDiverse, DiverseDirection, MeanStrategies, StrategiesDirection,
           DivWeight, StratWeight, DivMod, StratMod, DivDen, StratDen) %>%
  unnest(YearList) %>%
  mutate(RegimeT = paste0(RegimeKey.x,"-",RegimeKey.y),
         ClassT = paste0(Classif.x,",",Classif.y))

ggplot(all %>%
         filter(as.numeric(Scheme) <= 14),
       aes(x = YearList, y = DivMod, color = as.character(RegimeT))) +
  annotate("rect", xmin = 2008, xmax = 2010, ymin = -Inf, ymax = Inf,
           fill = "#90EE90", alpha = 0.5) +
  annotate("rect", xmin = 2015, xmax = 2017, ymin = -Inf, ymax = Inf,
           fill = "#90EE90", alpha = 0.5) +
  geom_point(size = 3, alpha = .5) +
  facet_wrap(vars(ClassT))








p <- 1
plots <- list()
for(p in 1:length(allcomb)){

  compregime <- allcomb[[p]] %>%
    ungroup() %>%
    distinct(
      Region, Year, Regime, Classif.x, Classif.y, RegimeKey.x, RegimeKey.y, N_diversified2, MeanDiversification_All, N_strategies, DistMean, RevMean,
      Year, N_Fisheries, NVessels, N_Edges, Mean_Weight,
      ClusteringCoefficient_Global, Modularity, FragMeasure) %>%
    mutate(Connectance = N_Edges/(N_Fisheries*(N_Fisheries-1)/2)) %>%
    group_by(Regime) %>%
    mutate(Label = paste0(Region,"\n",min(Year),"-",max(Year)),
           DivSign = sign(MeanDiversification_All-lag(MeanDiversification_All)),
           WeightSign = sign(Mean_Weight-lag(Mean_Weight)),
           PerfectDivWeight = sum(DivSign == WeightSign, na.rm = T),
           DivWeight = cor(MeanDiversification_All,Mean_Weight),
           StratWeight = cor(N_strategies,Mean_Weight),
           DivWeight_like = case_when(n() == 1 ~ NA_real_,
                                      n() == 2 ~ scaled_slope_ratio(MeanDiversification_All, Mean_Weight),
                                      TRUE     ~ cor(MeanDiversification_All, Mean_Weight)),
           StratWeight_like = case_when(n() == 1 ~ NA_real_,
                                        n() == 2 ~ scaled_slope_ratio(N_strategies, Mean_Weight),
                                        TRUE     ~ cor(N_strategies, Mean_Weight)),
           DivMod = cor(MeanDiversification_All,Modularity),
           StratMod = cor(N_strategies,Modularity),
           DivMod_like = case_when(n() == 1 ~ NA_real_,
                                   n() == 2 ~ scaled_slope_ratio(MeanDiversification_All, Modularity),
                                   TRUE     ~ cor(MeanDiversification_All, Modularity)),
           StratMod_like = case_when(n() == 1 ~ NA_real_,
                                     n() == 2 ~ scaled_slope_ratio(N_strategies, Modularity),
                                     TRUE     ~ cor(N_strategies, Modularity)),
           DivDen = cor(MeanDiversification_All,ClusteringCoefficient_Global),
           StratDen = cor(N_strategies,ClusteringCoefficient_Global),
           MeanFisheries = mean(N_Fisheries),
           FisheryDirection = mean(N_Fisheries-lag(N_Fisheries), na.rm = T),
           MeanDiverse = mean(MeanDiversification_All),
           DiverseDirection = mean(MeanDiversification_All-lag(MeanDiversification_All), na.rm = T),
           MeanStrategies = mean(N_strategies),
           StrategiesDirection = mean(N_strategies-lag(N_strategies), na.rm = T)) %>%
    distinct(Region, Label, Classif.x, Classif.y, RegimeKey.x, RegimeKey.y, MeanFisheries, FisheryDirection,
             MeanDiverse, DiverseDirection, MeanStrategies, StrategiesDirection,
             DivWeight, StratWeight, DivMod, StratMod, DivDen, StratDen,
             DivWeight_like, StratWeight_like, DivMod_like, StratMod_like) %>%
    mutate(RegimeT = paste0(RegimeKey.x,"-",RegimeKey.y))

  longdat <- compregime %>%
    pivot_longer(
      cols = c(DivWeight, StratWeight, DivMod, StratMod),
      names_to = "Measure",
      values_to = "Value"
    )

  plotdat <- longdat %>%
    mutate(RegimeKey = as.factor(RegimeT)) %>%
    bind_rows(
      longdat %>%
        mutate(RegimeKey = "All")
    )

  plots[[p]] <- ggplot(plotdat, aes(x = Value, y = RegimeKey, fill = RegimeKey)) +
    geom_density_ridges(alpha = 0.6) +
    facet_wrap(~ Measure, scales = "free_x") +
    theme_ridges() +
    labs(title = paste(nrow(compregime %>%
                              filter(!is.na(FisheryDirection))),"total Regimes"),
         subtitle = paste(unique(compregime$Classif.x),
                          unique(compregime$Classif.y)))

}

plots











