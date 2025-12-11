library(fixest)
library(modelsummary)
library(fixest)
library(broom)
library(dplyr)
library(stringr)
library(ggplot2)
library(patchwork)

color_list <- c("#4c6085","#39a0ed","#f7b32b","#EC0868","#13c4a3","#C09BD8","#52050A",
                "#fe5f55","#161613","#A44A3F")

regsmall <- function(df){
  sc01 <- which(df$SwitchingCost < 1)
  drop_rows <- sample(sc01, 75)
  df <- df[-drop_rows, ]
}

rds <- "~/westcoast-networks/data/clean/Simulation/static_sc_a1.rds"
nodelevel <- function(rds){
  df <- read_rds(rds)[[1]] %>%
    filter(str_detect(Fishery,"Dungeness|Salmon"))
}

sm <- read_rds("~/westcoast-networks/data/clean/Simulation/static_smaller.rds")
fc <- read_rds("~/westcoast-networks/data/clean/Simulation/static_fc_a1.rds")
fc1 <- read_rds("~/westcoast-networks/data/clean/Simulation/static_fc1_a1.rds")
fc2 <- read_rds("~/westcoast-networks/data/clean/Simulation/static_fc2_a1.rds")
sc <- read_rds("~/westcoast-networks/data/clean/Simulation/static_sc_a1.rds")
kk <- read_rds("~/westcoast-networks/data/clean/Simulation/static_kk_a1.rds")
knk <- read_rds("~/westcoast-networks/data/clean/Simulation/static_knk_a1.rds")

# [[1]] %>%
#   mutate(SwitchCostInc = "None",
#          FishCostInc = "Non-Key") %>%
#   distinct(iter, .keep_all = T) %>%
#   mutate(Connectance = N_Edges/(N_Fisheries*N_Fisheries-1)/2)

df <- fc2
dffind <- df[[1]] %>%
  distinct(iter, .keep_all = T) %>%
  mutate(Connectance = N_Edges/(N_Fisheries*N_Fisheries-1)/2) %>%
  arrange(ClusteringCoefficient_Global) %>%
    mutate(SwitchCostInc = "None",
           FishCostInc = "Non-Key") %>%
    distinct(iter, .keep_all = T) %>%
    mutate(Connectance = N_Edges/(N_Fisheries*N_Fisheries-1)/2)

ggplot(dffind
       %>% filter(Modularity > 0),
       aes(x = SwitchingCost, y = Mean_Weight, size = N_Fisheries,color = Modularity)) +
  geom_point(alpha = .6) +
  theme_minimal() +
  geom_text(vjust = -0.5, aes(label = iter), size = 4)

smplotloop <- c(65,18)
fcplotloop <- c(24,57,53)
fc1plotloop <- c(65,72,53)
fc2plotloop <- c(65,64,73)

scplotloop <- c(31,103,135)
kkplotloop <- c(6,102,194)
knkplotloop <- c(204, 198, 378)

plotloop <- fc2plotloop
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

  cw <- cluster_walktrap(gy, modularity = T)
  mem <- as.factor(membership(cw))
  out <- set_vertex_attr(gy, "Cluster",
                         value = mem) %>%
    as_tbl_graph() %>%
    activate(nodes) %>%
    arrange(as.numeric(Cluster))

  node_names <- out %>% activate(nodes) %>% pull(name)

  # out <- gy %>%
  #   as_tbl_graph() %>%
  #   activate(nodes)

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
  layout <- layout[node_names, ]

  if(n_distinct(dffind$FishingCost) == 1 |
     n_distinct(dffind$SwitchingCost) == 1){
    layout = layout
  } else {
    layout = "star"
  }

  plots[[p]] <- ggraph(out, layout = layout) +
    geom_edge_link(aes(width = weight), alpha = 0.5,
                   color = color_list[5]) +
    geom_node_point(aes(size = UniqueVessels, color = Cluster)) +
    geom_node_text(
      data = function(d) d[d$name %in% interior, ],
      aes(label = name),
      repel = TRUE,
      size = 3
    ) +
    scale_size_continuous(limits = c(0,250), #75
                          range = c(.5,15)) +
    scale_edge_width(limits = c(0,75),
                     range = c(.5,10)) +
    scale_color_manual(values = color_list[1:max(membership(cw))], guide = "none") +
    theme_void() +
    guides(size = "none",
           edge_width = "none"
    ) +
    labs(
      caption = paste0("Fishing Costs: ",round(unique(info$FishingCost),2)),
      # title = paste0("Switching Costs: ",round(unique(info$SwitchingCost),2)),
      # subtitle = paste0(unique(info$N_Fisheries)," fisheries & ", unique(info$N_Edges), " edges"),
      subtitle = paste0("Mean Diversification: ",round(unique(info$MeanDiversification_All),2),
                       "\n# Strategies: ",round(unique(info$N_strategies),2)),
      # caption = paste0("\nMean Diversification: ",round(unique(info$MeanDiversification_All),2),
      #                  "\n# Strategies: ",round(unique(info$N_strategies),2),
      #                  "\nMean Weight: ",round(unique(info$Mean_Weight),2),
      #                  "\nModularity: ",round(unique(info$Modularity),2))
      title = paste0("Mean Weight: ",round(unique(info$Mean_Weight),2),
                       " | Modularity: ",round(unique(info$Modularity),2))) +
    theme(plot.margin = margin(5,5,5,5),
          plot.title = element_text(size=18),
          plot.subtitle = element_text(size=16, color="grey40"),
          plot.caption = element_text(size = 10, hjust = 0, color="grey40"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA))

}

pnk <- plots[[1]] + plots[[3]] &
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )

pk <- plots[[1]] + plots[[3]] &
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )

pb <- pk/pnk &
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )

ggsave(plot = pb, paste0("~/westcoast-networks/output/Simulation/Static/3_knkgrid.png"),
       width = 14, height = 15.5,
       bg = "transparent")
