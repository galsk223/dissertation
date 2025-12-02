
fc <- read_rds("~/westcoast-networks/data/clean/Simulation/static_fc_a2.rds")[[1]] %>%
  mutate(SwitchCostInc = "None",
         FishCostInc = "All") %>%
  distinct(iter, .keep_all = T) %>%
  mutate(Connectance = N_Edges/(N_Fisheries*N_Fisheries-1)/2)
fc1 <- read_rds("~/westcoast-networks/data/clean/Simulation/static_fc1_a2.rds")[[1]] %>%
  mutate(SwitchCostInc = "None",
         FishCostInc = "Key") %>%
  distinct(iter, .keep_all = T) %>%
  mutate(Connectance = N_Edges/(N_Fisheries*N_Fisheries-1)/2)
fc2 <- read_rds("~/westcoast-networks/data/clean/Simulation/static_fc2_a2.rds")[[1]] %>%
  mutate(SwitchCostInc = "None",
         FishCostInc = "Non-Key") %>%
  distinct(iter, .keep_all = T) %>%
  mutate(Connectance = N_Edges/(N_Fisheries*N_Fisheries-1)/2)

scstart <- bind_rows(fc,fc1,fc2) %>%
  distinct(iter, FishCostInc, SwitchingCost, FishingCost, N_Fisheries, N_Edges, Mean_Weight,
           MeanDiversification_All, N_strategies, N_diversified2,
           Modularity, ClusteringCoefficient_Global, H, MedianToCentroid, HullAreaTrimmed, MeanToCentroid, HullArea, FragMeasure) %>%
  mutate(
    FishingCostS = as.numeric(scale(FishingCost)),
    FCostClassS = case_when(FishingCostS > .6 ~ "High FC",
                            FishingCostS < -.6 ~ "Low FC", T ~ "Med FC"),
    KCostClassS = paste(FishCostInc,FCostClassS),
         FCostClass = case_when(FishingCost > 2 ~ "High FC",
                                FishingCost < 1 ~ "Low FC", T ~ "Med FC"),
         KCostClass = paste(FishCostInc,FCostClass))
# %>%
#   # filter(FCostClass != "Med FC") %>%
  # group_by(KCostClassS) %>%
  # tally(name = "CostCount")
# %>%
#   group_by(KCostClassS) %>%
#   add_tally(name = "CostCountS")

kloop <- unique(scstart$KCostClass)
loop <- kloop

allcomp <- map_dfr(1:length(loop), function(c){

  dfclass <- scstart %>%
    filter(KCostClass == loop[[c]])
  out <- permutations(n = nrow(dfclass), r = k, v = dfclass$iter) %>%
    as_tibble() %>%
    sample_n(100)

  classcomp <- map_dfr(1:nrow(out), function(o){

    print(o)
    dcom <- dfclass %>%
      filter(iter %in% out[o,]) %>%
      mutate(order = match(iter, out[o,])) %>%
      arrange(order) %>%
      select(-order) %>%
      mutate(Set = paste0(KCostClass,"\n",o),
             Iter = list(iter),
             # SumPos = sum(N_Fisheries-lag(N_Fisheries)>=0, na.rm = T),
             # SumNeg = sum(N_Fisheries-lag(N_Fisheries)<=0, na.rm = T),
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
             StrategiesDirection = mean(N_strategies-lag(N_strategies), na.rm = T))

  })

})

compregime <- allcomp %>%
  distinct(Set, Iter, KCostClass, FishCostInc,FCostClass,MeanFisheries, FisheryDirection,
           MeanDiverse, DiverseDirection, MeanStrategies, StrategiesDirection,
           DivWeight, StratWeight, DivMod, StratMod, DivDen, StratDen) %>%
  mutate(KCostClass = factor(KCostClass, levels = c(
    "All Low FC", "All Med FC", "All High FC",
    "Key Low FC", "Key Med FC", "Key High FC",
    "Non-Key Low FC", "Non-Key Med FC", "Non-Key High FC")),
    FCostClass = factor(FCostClass, levels = c(
      "Low FC", "Med FC", "High FC"
    )))

color_list <- c("#4c6085","#39a0ed","#52050A","#EC0868","#f7b32b","#C09BD8",
                "#fe5f55","#A44A3F","#13c4a3","#161613")

ggplot(compregime, aes(x = FisheryDirection, y = DiverseDirection)) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0,
           fill = "#90EE90", alpha = 0.35) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = Inf,
           fill = "#90EE90", alpha = 0.35) +
  geom_hline(yintercept = 0, color = "grey80") +
  geom_vline(xintercept = 0, color = "grey80") +
  geom_point(size = 2) +
  ggthemes::theme_tufte() +
  labs(x = "Mean network size change \n(annual growth [contraction] in fishery nodes)",
       y = "Mean vessel diversification change \n(annual inc [dec] in fisheries / vessel)",
       # color = "Region",
       title = "Network Measure Interpretations by Regime",
       subtitle = "Using RUM simulated fishing data",
       caption = "Each point is a regime: a combination of 4 simulation iterations of a similar fishing cost and switching cost.
                  For each regime I compare the correlation between vessel diversification and network outcomes specific to
                  the regime state of cost setting (9 settings total)")

ggplot(compregime, aes(x = FisheryDirection, y = StrategiesDirection)) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0,
           fill = "#90EE90", alpha = 0.35) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = Inf,
           fill = "#90EE90", alpha = 0.35) +
  geom_hline(yintercept = 0, color = "grey80") +
  geom_vline(xintercept = 0, color = "grey80") +
  geom_point(size = 2) +
  ggthemes::theme_tufte() +
  labs(x = "Mean network size change \n(annual growth [contraction] in fishery nodes)",
       y = "Mean fleet strategy count change \n(annual inc [dec] in # unique strategies)",
       # color = "Region",
       title = "Network Measure Interpretations by Regime",
       subtitle = "Using RUM simulated fishing data",
       caption = "Each point is a regime: a combination of 4 simulation iterations of a similar fishing cost and switching cost.
                  For each regime I compare the correlation between vessel diversification and network outcomes specific to
                  the regime state of cost setting (9 settings total)")

# FisheryDirection, y =
p <- ggplot(compregime %>%
              filter(FishCostInc != "All"), aes(x = DivMod)) +
  # annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf,
  #          fill = "#1FD643", alpha = 0.5) +
  # geom_hline(yintercept = 0, color = "grey80") +
  # geom_vline(xintercept = 0, color = "grey80") +
  # geom_point(aes(y= MeanFisheries), size = 3, alpha = .4) +
  # facet_wrap(vars(KCostClass)) +
  facet_grid(cols = vars(FishCostInc),
             rows = vars(FCostClass)) +
  # geom_text(aes(label = Set), size = 3) +
  geom_histogram(bins = 20) +
  theme_minimal() +
  scale_color_manual(values = color_list[1:n_distinct(compregime$Set)],
                     guide = "none") +
  labs(
    # x = "Mean network size change \n(annual growth [contraction] in fishery nodes)",
       y = "Correlation between outcome and \n mean vessel diversification",
       # color = "Region",
       title = "Network Measure Interpretations by Regime",
       subtitle = "Using RUM simulated fishing data",
       caption = "Each point is a regime: a combination of 4 simulation iterations of a similar fishing cost and switching cost.
                  For each regime I compare the correlation between vessel diversification and network outcomes specific to
                  the regime state of cost setting (9 settings total)")
p
# ggMarginal(p, type = "boxplot", size = 75,
#            fill="grey50", color = "grey90", margins = "y")


# , color = CostClass FisheryDirection, y =
p <- ggplot(compregime, aes(x = StratMod)) +
  # annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf,
  #          fill = "#1FD643", alpha = 0.5) +
  # geom_hline(yintercept = 0, color = "grey80") +
  # geom_vline(xintercept = 0, color = "grey80") +
  # geom_point(size = 3, alpha = .4, aes(y = MeanFisheries)) +
  geom_histogram(bins = 20) +
  # facet_wrap(vars(KCostClass)) +
  facet_grid(cols = vars(FishCostInc),
             rows = vars(FCostClass)) +
  ggthemes::theme_tufte() +
  # geom_text(aes(label = Set), size = 3) +
  # scale_color_manual(values = color_list[1:n_distinct(compregime$Set)]) +
  labs(
    # x = "Mean network size change \n(annual growth [contraction] in fishery nodes)",
       y = "Correlation between outcome \n and # unique strategies",
       # color = "Region",
       title = "Network Measure Interpretations by Regime (Switching Cost x Fishing Cost)",
       subtitle = "Using RUM simulated fishing data",
       caption = "Each point is a regime: a combination of 4 simulation iterations of a similar fishing cost and switching cost.
                  For each regime I compare the correlation between vessel diversification and network outcomes specific to
                  the regime state of cost setting (9 settings total)")

p
# ggMarginal(p, type = "boxplot", size = 75,
#            fill="grey50", color = "grey90", margins = "y")


