library(gtools)
rm(list = ls())
color_list <- c("#4c6085","#39a0ed","#52050A","#EC0868","#161613","#f7b32b","#C09BD8",
                "#fe5f55","#A44A3F","#13c4a3")

scplot <- read_rds("~/westcoast-networks/data/clean/Simulation/static_scalar500.rds")
# scplot <- read_rds("~/westcoast-networks/data/clean/Simulation/static_sc.rds")
scstart <- scplot[[1]] %>%
  distinct(iter, SwitchingCost, FishingCost, N_Fisheries, N_Edges, Mean_Weight,
           MeanDiversification_All, N_strategies, N_diversified2,
           Modularity, ClusteringCoefficient_Global, H, MedianToCentroid, HullAreaTrimmed, MeanToCentroid, HullArea, FragMeasure) %>%
  filter(iter <= 100) %>%
  mutate(SCostClass = case_when(SwitchingCost > 2.5 ~ "H",
                               SwitchingCost < 1.5 ~ "L", T ~ "M"),
         FCostClass = case_when(FishingCost > 2.5 ~ "H",
                                FishingCost < 1.5 ~ "L", T ~ "M"),
         CostClass = paste0(SCostClass, FCostClass))

cloop <- unique(scstart$CostClass)
c <- 9
k <- 4
o <- 1
allcomp <- map_dfr(1:length(cloop), function(c){

  dfclass <- scstart %>%
    filter(CostClass == cloop[[c]])
  out <- permutations(n = nrow(dfclass), r = k, v = dfclass$iter) %>%
    as_tibble() %>%
    sample_n(50)

  classcomp <- map_dfr(1:nrow(out), function(o){

    print(o)
    dcom <- dfclass %>%
      filter(iter %in% out[o,]) %>%
      mutate(order = match(iter, out[o,])) %>%
      arrange(order) %>%
      select(-order) %>%
      mutate(Set = paste0(CostClass,"\n",o),
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
  distinct(Set, Iter, CostClass, MeanFisheries, FisheryDirection,
           MeanDiverse, DiverseDirection, MeanStrategies, StrategiesDirection,
           DivWeight, StratWeight, DivMod, StratMod, DivDen, StratDen)

color_list <- c("#4c6085","#39a0ed","#52050A","#EC0868","#f7b32b","#C09BD8",
                "#fe5f55","#A44A3F","#13c4a3","#161613")

ggplot(compregime, aes(x = FisheryDirection, y = DiverseDirection)) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0,
           fill = "#1FD643", alpha = 0.5) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = Inf,
           fill = "#1FD643", alpha = 0.5) +
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
           fill = "#1FD643", alpha = 0.5) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = Inf,
           fill = "#1FD643", alpha = 0.5) +
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

p <- ggplot(compregime, aes(x = FisheryDirection, y = DivWeight)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf,
           fill = "#1FD643", alpha = 0.5) +
  geom_hline(yintercept = 0, color = "grey80") +
  geom_vline(xintercept = 0, color = "grey80") +
  geom_point(size = 3, alpha = .4) +
  facet_wrap(vars(CostClass)) +
  # geom_text(aes(label = Set), size = 3) +
  ggthemes::theme_tufte() +
  # scale_color_manual(values = color_list[1:n_distinct(compregime$Set)],
  #                    guide = "none") +
  labs(x = "Mean network size change \n(annual growth [contraction] in fishery nodes)",
       y = "Correlation between\n mean vessel diversification and edge weight",
       # color = "Region",
       title = "Network Measure Interpretations by Regime",
       subtitle = "Using RUM simulated fishing data",
       caption = "Each point is a regime: a combination of 4 simulation iterations of a similar fishing cost and switching cost.
                  For each regime I compare the correlation between vessel diversification and network outcomes specific to
                  the regime state of cost setting (9 settings total)")

ggMarginal(p, type = "boxplot", size = 75,
           fill="grey50", color = "grey90", margins = "y")

# , color = CostClass
p <- ggplot(compregime, aes(x = FisheryDirection, y = StratWeight)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf,
           fill = "#1FD643", alpha = 0.5) +
  geom_hline(yintercept = 0, color = "grey80") +
  geom_vline(xintercept = 0, color = "grey80") +
  geom_point(size = 3, alpha = .4) +
  # facet_wrap(vars(CostClass)) +
  ggthemes::theme_tufte() +
  # geom_text(aes(label = Set), size = 3) +
  # scale_color_manual(values = color_list[1:n_distinct(compregime$Set)]) +
  labs(x = "Mean network size change \n(annual growth [contraction] in fishery nodes)",
       y = "Correlation between\n # unique strategies and edge weight",
       # color = "Region",
       title = "Network Measure Interpretations by Regime (Switching Cost x Fishing Cost)",
       subtitle = "Using RUM simulated fishing data",
       caption = "Each point is a regime: a combination of 4 simulation iterations of a similar fishing cost and switching cost.
                  For each regime I compare the correlation between vessel diversification and network outcomes specific to
                  the regime state of cost setting (9 settings total)")

ggMarginal(p, type = "boxplot", size = 75,
           fill="grey50", color = "grey90", margins = "y")



# Examples ----------------------------------------------------------------

loopfind <- compregime %>%
  filter(FisheryDirection <= -1)

loopd <- c(26,2,19,57)
excloopd <- c(98,10,93,56)

plotloop <- excloopd
plots <- list()
p <- 1
for(p in 1:length(plotloop)){

  t <- scplot[[4]] %>%
    filter(iter == plotloop[[p]])
  info <- scstart %>%
    filter(iter == plotloop[[p]])
  nfisheries <- n_distinct(t$FISHERY_ID) - 1
  fisherylist_use <- unique(t$FISHERY_ID)[unique(t$FISHERY_ID) != "Not Fishing"]
  fishery_pairs <- expand.grid(Fishery1 = fisherylist_use, Fishery2 = fisherylist_use, stringsAsFactors = FALSE) %>%
    filter(Fishery1 != Fishery2) %>%
    arrange(Fishery1, Fishery2)
  nvessels <- n_distinct(t$Vessel_ID)

  edges_ext <- t %>%
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
    mutate(UniqueVessels = replace_na(UniqueVessels, 0),
           V1 = pmin(as.character(Fishery1), as.character(Fishery2)),
           V2 = pmax(as.character(Fishery1), as.character(Fishery2))) %>%
    as.data.table() %>%
    distinct(V1, V2, UniqueVessels) %>%
    rename(Fishery1 = V1, Fishery2 = V2) %>%
    arrange(Fishery1, Fishery2)

  vy <- t %>%
    group_by(FISHERY_ID) %>%
    summarise(UniqueVessels = n_distinct(Vessel_ID),
              .groups = "drop") %>%
    filter(FISHERY_ID != "Not Fishing") %>%
    select(FISHERY_ID, UniqueVessels)

  ey <- edges_ext %>%
    filter(UniqueVessels > 0) %>%
    filter(Fishery1 != "Not Fishing",
           Fishery2 != "Not Fishing") %>%
    select(Fishery1, Fishery2, weight = UniqueVessels)

  gy <- graph_from_data_frame(ey,
                              directed = F,
                              vertices = vy)

  gy <- induced_subgraph(gy,
                         vids = which(igraph::components(gy)$membership ==
                                        which.max(igraph::components(gy)$csize)))

  cw <- cluster_walktrap(gy, modularity = T)
  mem <- as.factor(membership(cw))
  out <- set_vertex_attr(gy, "Cluster",
                         value = mem) %>%
    as_tbl_graph() %>%
    activate(nodes) %>%
    arrange(as.numeric(Cluster))

  plots[[p]] <- ggraph(out,  layout = "linear", circular = TRUE) +
    geom_edge_link(aes(width = weight), alpha = 0.5,
                   color = color_list[10]) +
    geom_node_point(aes(size = UniqueVessels, color = Cluster)) +
    # geom_node_text(aes(label = name), repel = TRUE, size = 3) +
    scale_size_continuous(limits = c(0,100),
                          range = c(.5,10)) +
    scale_edge_width(limits = c(0,50),
                     range = c(.5,8)) +
    scale_color_manual(values = color_list[1:max(membership(cw))], guide = "none") +
    theme_void() +
    guides(size = "none",
           edge_width = "none"
    ) +
    labs(
      title = paste0("Mean Diversification: ",round(unique(info$MeanDiversification_All),2),
                     "\n Mean Weight: ",round(unique(info$Mean_Weight),2)),
      # title = paste0("Fishing Costs ",round(unique(info$FishingCost),2)," | Switching Costs: ",round(unique(info$SwitchingCost),2)),
      subtitle = paste0(unique(info$N_Fisheries)," fisheries & ", unique(info$N_Edges), " edges"),
      # title = paste0("Modularity ",round(unique(info$Modularity),2)," | Density: ",round(unique(info$ClusteringCoefficient_Global),2)),
      # subtitle = paste0("Fishing Costs ",round(unique(info$FishingCost),2)," | Switching Costs: ",round(unique(info$SwitchingCost),2),
      #                   " \n(", unique(info$N_Fisheries)," fisheries & ", unique(info$N_Edges), " edges)"),
      caption = paste0("\nN Strategies: ",round(unique(info$N_strategies),2)," | Modularity: ",round(unique(info$Modularity),2),
                       "\nFishing Costs: ",round(unique(info$FishingCost),2)," | Switching Costs: ",round(unique(info$SwitchingCost),2))) +
    theme(plot.margin = margin(5,5,5,5),
          plot.title = element_text(size=11),
          plot.subtitle = element_text(size=9, color="grey40"),
          plot.caption = element_text(size = 9, hjust = 0, color="grey40"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA))


}

(plots[[2]]+plots[[3]]+ plots[[4]])





