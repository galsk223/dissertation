allregimes <- read_rds("../westcoast-networks/data/clean/Simulation/empregimes.rds")
trsplit <- read_rds("../westcoast-networks/data/clean/Simulation/empiricalbenchmarks_df_empnet.rds")
df_dist <- read_rds("../westcoast-networks/data/clean/Simulation/empiricalbenchmarks_meta.rds")$regionadjacency %>%
  select(-DistanceCostP)
list.files("/home/gkoss/dissertation/01_scripts/01_Choice_Function_Functions/",
           full.names = TRUE) %>%
  walk(source)

scaled_slope_ratio <- function(x, y) {
  if (length(x) != 2 || length(y) != 2) return(NA_real_)

  dx <- x[2] - x[1]
  dy <- y[2] - y[1]
  denom <- abs(dx) + abs(dy)

  if (denom == 0) return(0)
  (dy / denom) * sign(dx)
}


c <- 8
r <- 6
ridl <- list()
allempreg <- map(1:length(allregimes), function(c){

  reg <- allregimes[[c]] %>%
    ungroup()

  allemp <- map_dfr(1:nrow(reg), function(r){

    print(r)
    if(r == 1){
      rid <<- 1
    } else if(reg$RegimeKey[[r]] == reg$RegimeKey[[r-1]] &
              reg$LANDING_YEAR[[r]] == (reg$LANDING_YEAR[[r-1]]+1) &
              reg$HomeRegion[[r]] == (reg$HomeRegion[[r-1]])){
      rid <<- rid
    } else {
      rid <<- rid + 1
    }
    # print(rid)
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
        Region = reg$HomeRegion[[r]],
        Year = reg$LANDING_YEAR[[r]],
        Regime = rid,
        RegimeKey = reg$RegimeKey[[r]],
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

# 5 stratmod
all <- bind_rows(allempreg, .id = "Scheme") %>%
  ungroup() %>%
  distinct(
    Region, Year, Scheme, Regime, RegimeKey, N_diversified2, MeanDiversification_All, N_strategies, DistMean, RevMean,
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
  distinct(Region, Label, YearList, Scheme, RegimeKey, MeanFisheries, FisheryDirection,
           MeanDiverse, DiverseDirection, MeanStrategies, StrategiesDirection,
           DivWeight, StratWeight, DivMod, StratMod, DivDen, StratDen) %>%
  unnest(YearList)

ggplot(all, aes(x = YearList, y = DivMod, color = as.character(RegimeKey))) +
  annotate("rect", xmin = 2008, xmax = 2010, ymin = -Inf, ymax = Inf,
           fill = "#1FD643", alpha = 0.5) +
  annotate("rect", xmin = 2015, xmax = 2017, ymin = -Inf, ymax = Inf,
           fill = "#1FD643", alpha = 0.5) +
  geom_point(size = 3, alpha = .5) +
  facet_wrap(vars(Scheme))


compregime <- allempreg[[8]] %>%
  ungroup() %>%
  distinct(
    Region, Year, Regime, RegimeKey, N_diversified2, MeanDiversification_All, N_strategies, DistMean, RevMean,
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
  distinct(Region, Label, RegimeKey, MeanFisheries, FisheryDirection,
           MeanDiverse, DiverseDirection, MeanStrategies, StrategiesDirection,
           DivWeight, StratWeight, DivMod, StratMod, DivDen, StratDen)

longdat <- compregime %>%
  pivot_longer(
    cols = c(DivWeight, StratWeight, DivMod, StratMod),
    names_to = "Measure",
    values_to = "Value"
  )

plotdat <- longdat %>%
  mutate(RegimeKey = as.factor(RegimeKey)) %>%
  bind_rows(
    longdat %>%
      mutate(RegimeKey = "All")
  )

color_list <- c("#4c6085","#39a0ed","#52050A","#EC0868","#f7b32b","#C09BD8",
                "#fe5f55","#A44A3F","#13c4a3","#161613")

ggplot() + geom_density_ridges(data = plotdat %>%
                                 filter(RegimeKey == "All"), aes(x = Value, y = Measure),
                               fill = NA, color = "black", size = 0.5,alpha = 0.8) +
  geom_density_ridges(data = plotdat %>% filter(RegimeKey != "All"),
                      aes(x = Value, y = Measure, fill = RegimeKey),
                      alpha = 0.4,color = NA) +
  scale_fill_manual(values = c("#f7b32b", "#C09BD8")) +
  theme_ridges() +
  theme(
    legend.position = "bottom",
    panel.spacing = unit(1, "lines")
  )

ggplot(plotdat, aes(x = Value, y = RegimeKey, fill = RegimeKey)) +

  geom_density_ridges(alpha = 0.6) +
  facet_wrap(~ Measure, scales = "free_x") +
  theme_ridges()


hist(compregime$DivMod)


ggplot(compregime, aes(x = FisheryDirection, y = DiverseDirection)) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0,
           fill = "#1FD643", alpha = 0.5) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = Inf,
           fill = "#1FD643", alpha = 0.5) +
  geom_hline(yintercept = 0, color = "grey80") +
  geom_vline(xintercept = 0, color = "grey80") +
  geom_point(size = 2) +
  ggthemes::theme_tufte() +
  geom_text(aes(label = RegimeKey), size = 8) +
  labs(x = "Mean network size change \n(annual growth [contraction] in fishery nodes)",
       y = "Mean vessel diversification change \n(annual inc [dec] in fisheries / vessel)",
       # color = "Region",
       title = "Network Measure Interpretations by Regime",
       subtitle = "Using observed California landings data",
       caption = "A regime is a sequence of 3 years in a region (2006-2021), in which I compare the correlation between
       vessel diversification and network outcomes specific to the regime state of growth (contraction)")

ggplot(compregime, aes(x = FisheryDirection, y = StrategiesDirection)) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0,
           fill = "#1FD643", alpha = 0.5) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = Inf,
           fill = "#1FD643", alpha = 0.5) +
  geom_hline(yintercept = 0, color = "grey80") +
  geom_vline(xintercept = 0, color = "grey80") +
  geom_point(size = 2) +
  ggthemes::theme_tufte() +
  geom_text(aes(label = RegimeKey), size = 8) +
  labs(x = "Mean network size change \n(annual growth [contraction] in fishery nodes)",
       y = "Mean fleet strategy count change \n(annual inc [dec] in # unique strategies)",
       # color = "Region",
       title = "Network Measure Interpretations by Regime",
       subtitle = "Using observed California landings data",
       caption = "A regime is a sequence of 3 years in a region (2006-2021), in which I compare the correlation between
       vessel diversification and network outcomes specific to the regime state of growth (contraction)")

p <- ggplot(compregime, aes(x = FisheryDirection, y = DivMod, color = as.character(RegimeKey))) +
  geom_hline(yintercept = 0, color = "grey80") +
  geom_vline(xintercept = 0, color = "grey80") +
  geom_point(size = 2) +
  geom_text(aes(label = Label), size = 3) +
  ggthemes::theme_tufte() +
  scale_color_manual(values = color_list[1:n_distinct(compregime$RegimeKey)]) +
  labs(x = "Mean network size change \n(annual growth [contraction] in fishery nodes)",
       y = "Correlation between modularity and \n mean vessel diversification",
       color = "Regime Key",
       title = "Network Measure Interpretations by Regime",
       subtitle = "Using observed California landings data",
       caption = "A regime is a sequence of 4 years in a region (2005-2021), in which I compare the correlation between
       vessel diversification and network outcomes specific to the regime state of growth (contraction)")

ggMarginal(p, type = "boxplot", size = 75,
           fill="grey50", color = "grey90", margins = "y")

p <- ggplot(compregime, aes(x = StratWeight, y = StratMod, color = as.character(RegimeKey))) +
  geom_hline(yintercept = 0, color = "grey80") +
  geom_vline(xintercept = 0, color = "grey80") +
  geom_point(size = 2) +
  ggthemes::theme_tufte() +
  geom_text(aes(label = Label), size = 3) +
  scale_color_manual(values = color_list[1:n_distinct(compregime$RegimeKey)]) +
  labs(
    # x = "Mean network size change \n(annual growth [contraction] in fishery nodes)",
       y = "Correlation between modularity \n and # unique strategies",
       color = "Regime Key",
       title = "Network Measure Interpretations by Regime",
       subtitle = "Using observed California landings data",
       caption = "A regime is a sequence of 4 years in a region (2005-2021), in which I compare the correlation between
       vessel diversification and network outcomes specific to the regime state of growth (contraction)")

ggMarginal(p, type = "boxplot", size = 75,
           fill="grey50", color = "grey90", margins = "y")



