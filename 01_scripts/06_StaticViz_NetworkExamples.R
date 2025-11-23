rm(list = ls())

sc <- read_rds("~/westcoast-networks/data/clean/Simulation/static_sc.rds")
sc <- read_rds("~/westcoast-networks/data/clean/Simulation/static_sc_a1.rds")

sm <- read_rds("~/westcoast-networks/data/clean/Simulation/static_smaller.rds")
scalar <- read_rds("~/westcoast-networks/data/clean/Simulation/static_scalar500.rds")

kk <- read_rds("~/westcoast-networks/data/clean/Simulation/static_kk.rds")
kk1 <- read_rds("~/westcoast-networks/data/clean/Simulation/static_kk1.rds")
kk2 <- read_rds("~/westcoast-networks/data/clean/Simulation/static_kk2.rds")

knk <- read_rds("~/westcoast-networks/data/clean/Simulation/static_knk.rds")
knk1 <- read_rds("~/westcoast-networks/data/clean/Simulation/static_knk1.rds")
knk2 <- read_rds("~/westcoast-networks/data/clean/Simulation/static_knk2.rds")

nknk <- read_rds("~/westcoast-networks/data/clean/Simulation/static_nknk.rds")
nknk1 <- read_rds("~/westcoast-networks/data/clean/Simulation/static_nknk1.rds")
nknk2 <- read_rds("~/westcoast-networks/data/clean/Simulation/static_nknk2.rds")

color_list <- c("#4c6085","#39a0ed","#f7b32b","#EC0868","#13c4a3","#C09BD8","#52050A",
                "#fe5f55","#161613","#A44A3F")

# plotloop <- c(97,76,40,60,7,68,77,54)
df <- sc
dffind <- df[[1]] %>%
  distinct(iter,SwitchingCost,ClusteringCoefficient_Global,Modularity, Mean_Weight, N_Fisheries,
           MeanDiversification_All, N_strategies, DistMean, RevMean, RevMed, RevGini) %>%
  # filter(N_Fisheries > 7) %>%
  arrange(ClusteringCoefficient_Global)

sc01 <- which(dffind$SwitchingCost < 1)
drop_rows <- sample(sc01, 75)
dffind <- dffind[-drop_rows, ]
hist(dffind$RevGini)

ggplot(dffind, aes(x = ClusteringCoefficient_Global, y = Modularity, color = N_Fisheries)) +
  geom_point() +
  theme_minimal() +
  geom_text(vjust = -0.5, aes(label = iter))

ggplot(dffind, aes(x = SwitchingCost, y = Modularity, color = N_Fisheries)) +
  geom_point() +
  theme_minimal() +
  geom_text(vjust = -0.5, aes(label = iter))
divloop <- c(26,55,29,58,67,24)
# kkplotloop <- c(26,75,71,54,7,44)
scplotloop <- c(97,71,64,66,7,63,77,72)
# knkplotloop <- c(22,46,26,40,79,72)
smplotloop <- c(7,13,63,95,76,80,39,100, 19, 70, 65, 18, 52, 57, 61, 11, 81, 96, 40)

feols(ClusteringCoefficient_Global ~ SwitchingCost + N_Fisheries,
      data = dffind)

plotloop <- divloop
p <- 1
plots <- list()
for(p in 1:length(plotloop)){

  t <- df[[4]] %>%
    filter(iter == plotloop[[p]])
  info <- df[[1]] %>%
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

  out <- gy %>%
    as_tbl_graph() %>%
    activate(nodes)

  interior <- str_subset(V(gy)$name, "Dungeness|Salmon")
  outer <- setdiff(V(gy)$name, interior)

  # Set ring radii
  r_inner <- 1
  r_outer <- 3

  # Place interior nodes evenly on the inner circle
  theta_inner <- seq(0, 2 * pi, length.out = length(interior) + 1)[-1]
  layout_inner <- cbind(r_inner * cos(theta_inner), r_inner * sin(theta_inner))

  # Place outer nodes evenly on the outer circle
  theta_outer <- seq(0, 2 * pi, length.out = length(outer) + 1)[-1]
  layout_outer <- cbind(r_outer * cos(theta_outer), r_outer * sin(theta_outer))

  # Combine
  layout <- rbind(layout_inner, layout_outer)
  rownames(layout) <- c(interior, outer)

  # Ensure correct order
  layout <- layout[V(gy)$name, ]

  # layout = layout

  plots[[p]] <- ggraph(out, layout = "star") +
    geom_edge_link(aes(width = weight), alpha = 0.5,
                   color = color_list[5]) +
    geom_node_point(aes(size = UniqueVessels), color = color_list[9]) +
    geom_node_text(aes(label = name), repel = TRUE, size = 3) +
    scale_size_continuous(limits = c(0,250), #75
                          range = c(.5,10)) +
    scale_edge_width(limits = c(0,50),
                     range = c(.5,8)) +
    theme_void() +
    guides(size = "none",
           edge_width = "none"
    ) +
    labs(
      title = paste0("Switching Costs: ",round(unique(info$SwitchingCost),2)),
      subtitle = paste0(unique(info$N_Fisheries)," fisheries & ", unique(info$N_Edges), " edges"),
      caption = paste0("\nMean Diversification: ",round(unique(info$MeanDiversification_All),2),
                       "\n# Strategies: ",round(unique(info$N_strategies),2),
                       "\nMean Weight: ",round(unique(info$Mean_Weight),2),
                       "\nDensity: ",round(unique(info$ClusteringCoefficient_Global),2),
                       "\nModularity: ",round(unique(info$Modularity),2),
                       "\nFragmentation: ",round(unique(info$FragMeasure),2),
                       "\nIter:",unique(info$iter))) +
    theme(plot.margin = margin(5,5,5,5),
          plot.subtitle = element_text(size=12, color="grey40"),
          plot.caption = element_text(size = 8, hjust = 0),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA))


}

plots
plots[[1]]
plots[[9]]
plots[[12]]
