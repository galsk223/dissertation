# cache_all <- read_rds("~/westcoast-networks/data/Simulation/ASC_Calibration_RegionalMeta.rds")
# sscale <- runif(1,.25,2)
# fscale <- runif(1,.25,2)
# asc_sc <- cache_all$cache_sc[,5]*sscale
# asc_fc <- cache_all$cache_fc[,5]*fscale
# 
# choices <- choices_asc_cal(start, year_ref, vessels_in, fishery_pairs, prep_df_fc,
#                            scale_t1ev, asc_sc, asc_fc)
# 
# vy <- choices$df_choice %>% 
#   group_by(RegionFishery) %>% 
#   summarise(UniqueVessels = n_distinct(Vessel_ID),
#             .groups = "drop") %>% 
#   filter(RegionFishery != "Not Fishing") %>%
#   select(RegionFishery, UniqueVessels) 
# 
# ey <- choices$edges_ext %>% 
#   filter(Fishery1 %in% vy$RegionFishery,
#          Fishery2 %in% vy$RegionFishery,
#          UniqueVessels > 0) %>% 
#   filter(Fishery1 != "Not Fishing",
#          Fishery2 != "Not Fishing") %>% 
#   select(Fishery1, Fishery2, weight = UniqueVessels)
# 
# gy <- graph_from_data_frame(ey,
#                             directed = F,
#                             vertices = vy)
# 
# a <- ggraph(gy) +
#   geom_edge_link(aes(width = weight), alpha = 0.5,
#                  color = "lightblue") + # Edge color by facet
#   geom_node_point(aes(size = UniqueVessels, color = "purple")) +
#   theme_void() +
#   guides(size = "none",
#          # guide_legend(title = "UniqueVessels"),
#          color = "none",
#          edge_width = "none"
#          # guide_legend(title = "Weight")
#   ) +
#   scale_size_continuous(limits = c(0,150),
#                         range = c(1,10)) +
#   scale_edge_width(limits = c(0,100),
#                    range = c(1,8)) +
#   theme(plot.margin = margin(5,5,5,5),
#         plot.subtitle = element_text(size=8, color="grey40"),
#         plot.caption = element_text(size = 12),
#         plot.background = element_rect(fill = "transparent", color = NA),
#         panel.background = element_rect(fill = "transparent", color = NA))
# a
# 
# nrow(ey)
# n_distinct(ey$Fishery1)
# 
# 
# A <- as_adjacency_matrix(gy, sparse=FALSE, attr="weight")
# lA <- A[lower.tri(A)]
# Ainv <- A
# Ainv[A!=0] <- 1/A[A!=0]
# 
# # graph level
# OnnelaClust <- ClustF(A, "undirected") 
# cw <- cluster_walktrap(gy, modularity = T)
# m <- modularity(cw)
# nc <- networkcentralization(gy)
# urf <- mean(E(gy)$weight) + var(E(gy)$weight)/mean(E(gy)$weight)
# SI <- sparsityindex(ey, nvessels, nfisheries, F)
# p <- vy$UniqueVessels/sum(vy$UniqueVessels)
# H <- -sum(p * log(p))
# DF <- fragmentation(Ainv)
# 
# # node level
# if (nrow(Ainv) > 2){
#   fc <- fragment(Ainv, binary = F)[,1]
# } else {
#   fc <- 0
# }
# 
# closeness <- igraph::closeness(gy)
# strength <- igraph::strength(gy)
# 
# dfgraph_out <- tibble(Year = yi,
#                       Fishery = names(membership(cw)),
#                       UniqueVessels = vy$UniqueVessels,
#                       Closeness = closeness,
#                       Strength = strength,
#                       ClusteringCoefficient = OnnelaClust$LocalCC,
#                       FragmentationCentrality = fc,
#                       FragMeasure = DF,
#                       SI = SI,
#                       H = H,
#                       N_Fisheries = n_distinct(vy$RegionFishery),
#                       N_Edges = nrow(ey)/2,
#                       Mean_Weight = mean(lA[lA > 0]),
#                       ClusteringCoefficient_Global = OnnelaClust$GlobalCC,
#                       Cluster = membership(cw),
#                       Modularity = m,
#                       NetworkCentralization = nc,
#                       URF = urf)