scalar <- read_rds("~/westcoast-networks/data/clean/Simulation/static_scalar500.rds")[[1]]
scall <- scalar %>%
  distinct(iter, SwitchingCost, FishingCost, N_Fisheries, N_Edges, Mean_Weight,
           MeanDiversification_All, MeanDiversification_Diverse, N_strategies, N_diversified2, FragMeasure,
           Modularity, ClusteringCoefficient_Global, H, MedianToCentroid, HullAreaTrimmed) %>%
  filter(iter <= 100) %>%
  mutate(SwitchingCost = as.numeric(scale(SwitchingCost)),
         FishingCost = as.numeric(scale(FishingCost)),
         Connectance = N_Edges/(N_Fisheries*(N_Fisheries-1)/2))

ggplot(scall, aes(x = MeanDiversification_All, y = N_strategies)) +
  geom_point() +
  geom_text(vjust = -0.5, aes(label = iter))



# Regression context ------------------------------------------------------


c <- cor(scall)

corrplot::corrplot(c)

pca <- prcomp(scall %>%
                select(N_Fisheries, N_Edges,
                       MeanDiversification_All, N_strategies), scale. = TRUE)
fviz_pca_var(pca, col.var = "black", repel = T)
scall$Z_PC1 <- pca$x[, 1]

Z_resid <- lapply(scall[, c("N_Fisheries", "N_Edges","MeanDiversification_All",
                            "N_strategies", "N_diversified2")], function(z) {
  resid(lm(z ~ FishingCost, data = scall))
}) %>% as.data.frame() %>%
  rename(N_FisheriesR = N_Fisheries,
         N_EdgesR = N_Edges,
         MeanDiversification_AllR = MeanDiversification_All,
         N_strategiesR = N_strategies,
         N_diversified2R = N_diversified2)

scall_resid <- cbind(scall, Z_resid)

feols(Modularity ~ SwitchingCost + N_Fisheries, scall)
feols(Modularity ~ MeanDiversification_All + N_strategies, scall)
feols(Mean_Weight ~ SwitchingCost, scall)

formulas_scall <- list(
  N_strategies ~ SwitchingCost + FishingCost,
  MeanDiversification_All ~ SwitchingCost + FishingCost,
  MeanDiversification_Diverse ~ SwitchingCost + FishingCost,
  N_diversified2 ~ SwitchingCost + FishingCost,

  # log(N_strategies) ~ SwitchingCost + FishingCost,
  # log(MeanDiversification_All) ~ SwitchingCost + FishingCost,
  # log(MeanDiversification_Diverse) ~ SwitchingCost + FishingCost,
  # log(N_diversified2) ~ SwitchingCost + FishingCost,

  N_Fisheries ~ SwitchingCost + FishingCost,
  N_Edges ~ SwitchingCost + FishingCost,
  Connectance ~ SwitchingCost + FishingCost,
  Mean_Weight ~ SwitchingCost + FishingCost,
  # N_Fisheries ~ SwitchingCost + FishingCost + MeanDiversification_All + N_strategies,
  N_Fisheries ~ MeanDiversification_All + N_strategies,
  # N_Edges ~ SwitchingCost + FishingCost + MeanDiversification_All + N_strategies,
  N_Edges ~ MeanDiversification_All + N_strategies,
  # Connectance ~ SwitchingCost + FishingCost + MeanDiversification_All + N_strategies,
  Connectance ~ MeanDiversification_All + N_strategies,
  Mean_Weight ~ MeanDiversification_All + N_strategies,
  Modularity ~ MeanDiversification_All + N_strategies,

  Mean_Weight ~ N_Fisheries,
  Mean_Weight ~ SwitchingCost + FishingCost,
  N_Fisheries ~ SwitchingCost + FishingCost,
  ClusteringCoefficient_Global ~ SwitchingCost + FishingCost,
  ClusteringCoefficient_Global ~ N_Fisheries,
  Modularity ~ SwitchingCost + FishingCost,
  Modularity ~ N_Fisheries,
  FragMeasure ~ SwitchingCost + FishingCost,
  FragMeasure ~ N_Fisheries,
  H ~ SwitchingCost + FishingCost,
  H ~ N_Fisheries
)

# Function to run linear models and return a tidy table
run_models <- function(data, formulas) {
  lapply(formulas, function(f) {
    feols(f, data = data)
  })
}

# Run for scall
results_scall <- run_models(scall, formulas_scall[1:13])

names(results_scall) <- c("N_strategies","MeanDiversification_All",
  "MeanDiversification_Diverse", "N_diversified2",
  "N_Fisheries", "N_Edges", "Connectance", "Mean_Weight",
  "N_Fisheries", "N_Edges", "Connectance", "Mean_Weight", "Modularity")
# ,
#   "N_Fisheries", "N_Edges", "N_Edges", "Mean_Weight", "Mean_Weight",
#   "N_Fisheries", "ClusteringCoefficient_Global", "ClusteringCoefficient_Global",
#   "Modularity", "Modularity","FragMeasure","FragMeasure","H","H"
# )

options(modelsummary_factory_word = 'huxtable')
modelsummary(
  results_scall[9:13],
  stars = TRUE,           # adds significance stars
  coef_rename = NULL,    # or "html", "latex" depending on context
  gof_map = c("nobs", "r.squared")
)

plotq <- tibble("Outcome" = rep(c("# Strategies", "Mean Vessel Diversification\n(all vessels)",
                              "Mean Vessel Diversification\n(diversified vessels)",
                              "# Diversified Vessels"),each = 3),
                "Parameter" = rep(c("(intercept)","Switching Cost","Fishing Cost"),4),
                "Estimate" = c(results_scall[[1]]$coefficients[[1]],
                               results_scall[[1]]$coefficients[[2]],
                               results_scall[[1]]$coefficients[[3]],
                               results_scall[[2]]$coefficients[[1]],
                               results_scall[[2]]$coefficients[[2]],
                               results_scall[[2]]$coefficients[[3]],
                               results_scall[[3]]$coefficients[[1]],
                               results_scall[[3]]$coefficients[[2]],
                               results_scall[[3]]$coefficients[[3]],
                               results_scall[[4]]$coefficients[[1]],
                               results_scall[[4]]$coefficients[[2]],
                               results_scall[[4]]$coefficients[[3]]),
                "SE" = c(results_scall[[1]]$se[[1]],
                               results_scall[[1]]$se[[2]],
                               results_scall[[1]]$se[[3]],
                               results_scall[[2]]$se[[1]],
                               results_scall[[2]]$se[[2]],
                               results_scall[[2]]$se[[3]],
                               results_scall[[3]]$se[[1]],
                               results_scall[[3]]$se[[2]],
                               results_scall[[3]]$se[[3]],
                               results_scall[[4]]$se[[1]],
                               results_scall[[4]]$se[[2]],
                               results_scall[[4]]$se[[3]]))

# focal fisheries ---------------------------------------------------------

scf <- scalar %>%
  filter(iter < 100,
         str_detect(Fishery,"Crab|Salmon"))

formulas_sc <- list(
  Strength ~ SwitchingCost + FishingCost,
  Strength ~ N_Fisheries,
  Closeness ~ SwitchingCost + FishingCost,
  Closeness ~ N_Fisheries,
  ClusteringCoefficient ~ SwitchingCost + FishingCost,
  ClusteringCoefficient ~ N_Fisheries,
  FragmentationCentrality ~ SwitchingCost + FishingCost,
  FragmentationCentrality ~ N_Fisheries
)
results_sc <- run_models(scf, formulas_sc)

names(results_sc) <- c("Strength", "Strength",
                       "Closeness", "Closeness",
                       "ClusteringCoefficient", "ClusteringCoefficient",
                       "FragmentationCentrality", "FragmentationCentrality")

feols(Strength ~ SwitchingCost + FishingCost | N_Fisheries, scf)
feols(Closeness ~ SwitchingCost + FishingCost | N_Fisheries, scf)
feols(ClusteringCoefficient ~ SwitchingCost | N_Fisheries, scf)
feols(FragmentationCentrality ~ SwitchingCost + FishingCost | N_Fisheries, scf)


modelsummary(
  results_sc,
  stars = TRUE,           # adds significance stars
  coef_rename = NULL    # or "html", "latex" depending on context
)

ggplot(scf, aes(x = SwitchingCost, y = Mean_Weight, color = N_Fisheries)) +
  geom_point()


# Plot examples -----------------------------------------------------------
rm(list = ls())
color_list <- c("#4c6085","#39a0ed","#52050A","#EC0868","#161613","#f7b32b","#C09BD8",
                "#fe5f55","#A44A3F","#13c4a3")

scplot <- read_rds("~/westcoast-networks/data/clean/Simulation/static_scalar500.rds")
scstart <- scplot[[1]] %>%
  distinct(iter, SwitchingCost, FishingCost, N_Fisheries, N_Edges, Mean_Weight,
           MeanDiversification_All, N_strategies, N_diversified2,
           Modularity, ClusteringCoefficient_Global, H, MedianToCentroid, HullAreaTrimmed, MeanToCentroid, HullArea, FragMeasure) %>%
  filter(iter <= 100)

ggplot(scstart %>%
         filter(N_Fisheries > 10), aes(x = MeanDiversification_All, y = N_strategies, color = N_Fisheries
                                      # , size = N_Fisheries, color = Mean_Weight
                                      )) +
  geom_point() +
  theme_minimal() +
  geom_text(vjust = -0.5, aes(label = iter))

divloop <- c(26,24,53,50)
# modmat <- c(8,88,35,60)
modmat <- c(8,88,4,87)
costloop <- c(28,80,20)
# modloop <- c(37,85,88)
# modloop2 <- c(11,87,10)
# modloop3 <- c(75,26,79)

plotloop <- divloop

plots <- list()
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
                     "\n # Strategies: ",round(unique(info$N_strategies),2)),
      # title = paste0("Fishing Costs ",round(unique(info$FishingCost),2)," | Switching Costs: ",round(unique(info$SwitchingCost),2)),
      subtitle = paste0(unique(info$N_Fisheries)," fisheries & ", unique(info$N_Edges), " edges"),
      # title = paste0("Modularity ",round(unique(info$Modularity),2)," | Density: ",round(unique(info$ClusteringCoefficient_Global),2)),
      # subtitle = paste0("Fishing Costs ",round(unique(info$FishingCost),2)," | Switching Costs: ",round(unique(info$SwitchingCost),2),
      #                   " \n(", unique(info$N_Fisheries)," fisheries & ", unique(info$N_Edges), " edges)"),
      caption = paste0("\nMean Weight: ",round(unique(info$Mean_Weight),2)," | Modularity: ",round(unique(info$Modularity),2),
                       "\nFishing Costs: ",round(unique(info$FishingCost),2)," | Switching Costs: ",round(unique(info$SwitchingCost),2))) +
    theme(plot.margin = margin(5,5,5,5),
          plot.title = element_text(size=11),
          plot.subtitle = element_text(size=9, color="grey40"),
          plot.caption = element_text(size = 9, hjust = 0, color="grey40"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA))


}

(plots[[4]]+plots[[2]])/
  (plots[[1]]+plots[[3]])



