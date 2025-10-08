# library(latex2exp)
# vessel_pairs <- expand.grid(Vessel1 = as.character(unique(df_choice$Vessel_ID)),
#                             Vessel2 = as.character(unique(df_choice$Vessel_ID)), stringsAsFactors = FALSE) %>%
#   filter(Vessel1 != Vessel2) %>%
#   arrange(Vessel1, Vessel2)
# cache_all <- read_rds("~/westcoast-networks/data/Simulation/ASC_Calibration_RegionalMeta.rds")
# 
# vesselgraphtest <- map(1:25, function(i){
# 
#   print(i)
#   set.seed(Sys.time())
#   sscale <- runif(1,.25,4)
#   fscale <- runif(1,.25,4)
#   asc_sc <- cache_all$cache_sc[,5]*sscale
#   asc_fc <- cache_all$cache_fc[,5]*fscale
#   choices <- choices_asc_cal(start, year_ref, vessels_in, fishery_pairs, prep_df_fc,
#                       scale_t1ev, asc_sc, asc_fc)
# 
# 
# # Extensive ---------------------------------------------------------------
# 
#   print("extensive graph")
#   edges_ext <- choices$df_choice %>%
#     filter(RegionFishery != "Not Fishing") %>%
#     distinct(Vessel_ID, RegionFishery) %>%
#     mutate(present = 1) %>%
#     pivot_wider(names_from = Vessel_ID, values_from = present, values_fill = 0) %>%
#     column_to_rownames("RegionFishery") %>%
#     as.matrix() %>%
#     { t(.) %*% . } %>%
#     as.data.frame() %>%
#     rownames_to_column("Vessel1") %>%
#     pivot_longer(-Vessel1, names_to = "Vessel2", values_to = "SharedFisheries") %>%
#     filter(Vessel1 != Vessel2) %>%
#     full_join(vessel_pairs, by = c("Vessel1", "Vessel2")) %>%
#     mutate(SharedFisheries = replace_na(SharedFisheries, 0))
# 
#   ey <- edges_ext %>%
#     filter(SharedFisheries > 0) %>%
#     select(Vessel1, Vessel2, weight = SharedFisheries)
# 
#   vy <- choices$df_choice %>%
#     filter(RegionFishery != "Not Fishing",
#            Vessel_ID %in% ey$Vessel1) %>%
#     group_by(Vessel_ID) %>%
#     summarise(NFisheries = n_distinct(RegionFishery),
#               .groups = "drop")  %>%
#     select(Vessel_ID, NFisheries)
# 
#   gy <- graph_from_data_frame(ey,
#                               directed = F,
#                               vertices = vy)
# 
#   A <- as_adjacency_matrix(gy, sparse=FALSE, attr="weight")
#   lA <- A[lower.tri(A)]
#   Ainv <- A
#   Ainv[A!=0] <- 1/A[A!=0]
# 
#   # graph level
#   OnnelaClust <- ClustF(A, "undirected")
#   cw <- cluster_walktrap(gy, modularity = T)
#   m <- modularity(cw)
#   nc <- networkcentralization(gy)
#   urf <- mean(E(gy)$weight) + var(E(gy)$weight)/mean(E(gy)$weight)
#   # SI <- sparsityindex(ey, nvessels, nfisheries, F)
#   p <- vy$NFisheries/sum(vy$NFisheries)
#   H <- -sum(p * log(p))
#   # DF <- fragmentation(Ainv)
#   # if (nrow(Ainv) > 2){
#   #   fc <- fragment(Ainv, binary = F)[,1]
#   # } else {
#   #   fc <- 0
#   # }
#   closeness <- igraph::closeness(gy)
#   strength <- igraph::strength(gy)
# 
#   dfgraph_out_ext <- tibble(Iteration = i,
#                             sscale = sscale,
#                             fscale = fscale,
#                         Vessel = names(membership(cw)),
#                         nfisheries = vy$NFisheries,
#                         Closeness = closeness,
#                         Strength = strength,
#                         ClusteringCoefficient = OnnelaClust$LocalCC,
#                         # FragmentationCentrality = fc,
#                         # FragMeasure = DF,
#                         # SI = SI,
#                         H = H,
#                         N_VesselNodes = n_distinct(vy$Vessel_ID),
#                         N_Edges = nrow(ey)/2,
#                         Mean_Weight = mean(lA[lA > 0]),
#                         ClusteringCoefficient_Global = OnnelaClust$GlobalCC,
#                         Cluster = membership(cw),
#                         Modularity = m,
#                         NetworkCentralization = nc,
#                         URF = urf)
# 
# 
# # Quasi-Fuller ------------------------------------------------------------
# 
#   print("fuller graph")
#   nfisheries <- length(fisherylist_use)
#   nvessels <- n_distinct(df_choice$Vessel_ID)
#   bipmat <- choices$df_choice %>%
#     filter(RegionFishery != "Not Fishing") %>%
#     group_by(Vessel_ID, RegionFishery) %>%
#     summarise(Weights = n_distinct(Week), .groups = "drop") %>%
#     arrange(as.numeric(Vessel_ID)) %>%
#     pivot_wider(names_from = Vessel_ID, values_from = Weights, values_fill = 0) %>%
#     column_to_rownames("RegionFishery") %>%
#     as.matrix()
# 
#   connectivity <- matrix(0, nvessels, nvessels)
#   colnames(connectivity) <- rownames(connectivity) <- as.numeric(unique(df_choice$Vessel_ID))
#   bipmat_0 <- bipmat
#   edges_fuller <- fulleredges(connectivity, bipmat)
# 
# 
# 
#   ey <- edges_fuller %>%
#     filter(Weight > 1) %>%
#     select(Fishery1, Fishery2, weight = Weight)
# 
#   vy <- choices$df_choice %>%
#     filter(RegionFishery != "Not Fishing",
#            Vessel_ID %in% ey$Vessel1) %>%
#     group_by(Vessel_ID) %>%
#     summarise(NFisheries = n_distinct(RegionFishery),
#               .groups = "drop")
# 
#   gy <- graph_from_data_frame(ey,
#                               directed = F,
#                               vertices = vy)
# 
#   A <- as_adjacency_matrix(gy, sparse=FALSE, attr="weight")
#   Ainv <- A
#   Ainv[A!=0] <- 1/A[A!=0]
# 
#   # graph level
#   OnnelaClust <- ClustF(A, "undirected")
#   cw <- cluster_walktrap(gy, modularity = T)
#   m <- modularity(cw)
#   nc <- networkcentralization(gy)
#   urf <- mean(E(gy)$weight) + var(E(gy)$weight)/mean(E(gy)$weight)
#   # SI <- sparsityindex(ey, nvessels, nfisheries, F)
#   p <- vy$NFisheries/sum(vy$NFisheries)
#   H <- -sum(p * log(p))
#   # DF <- fragmentation(Ainv)
# 
#   # node level
#   # if (nrow(Ainv) > 2){
#   #   fc <- fragment(Ainv, binary = F)[,1]
#   # } else {
#   #   fc <- 0
#   # }
# 
#   closeness <- igraph::closeness(gy)
#   strength <- igraph::strength(gy)
# 
#   dfgraph_out_ful <- tibble(Iteration = i,
#                             sscale = sscale,
#                             fscale = fscale,
#                             Vessel = names(membership(cw)),
#                           UniqueVessels = vy$NFisheries,
#                           Closeness = closeness,
#                           Strength = strength,
#                           ClusteringCoefficient = OnnelaClust$LocalCC,
#                           # FragmentationCentrality = fc,
#                           # FragMeasure = DF,
#                           # SI = SI,
#                           H = H,
#                           N_VesselNodes = n_distinct(vy$Vessel_ID),
#                           N_Edges = nrow(ey),
#                           Mean_Weight = mean(ey$weight),
#                           ClusteringCoefficient_Global = OnnelaClust$GlobalCC,
#                           Cluster = membership(cw),
#                           Modularity = m,
#                           NetworkCentralization = nc,
#                           URF = urf)
# 
#   return(list(dfgraph_out_ext = dfgraph_out_ext,
#               dfgraph_out_ful = dfgraph_out_ful))
# 
# })
# 
# i <- 1
# v <- vesselgraphtest[[3]]$dfgraph_out_ext
# vesselplotnode <- do.call(rbind, lapply(vesselgraphtest, `[[`, "dfgraph_out_ext"))
# feols(ClusteringCoefficient ~ sscale + fscale, data = vesselplotnode)
# feols(Strength ~ sscale + fscale, data = vesselplotnode)
# 
# e <- ggplot(vesselplotnode, aes(x = factor(round(sscale,3)),
#                                 y =  Strength, color = fscale,
#                                 group = interaction(fscale, sscale))) +
#   geom_boxplot(alpha = .8) +
#   ggthemes::theme_tufte() +
#   scale_color_gradient(low = "#C4EBC8", high = "#2C3A33") +
#   labs(x = "Switching Costs",
#        color = "Fishing Costs",
#        title = "Vessel Network",
#        subtitle = "Co-Participation Projection")
# 
# e
# 
# vesselplotnodef <- do.call(rbind, lapply(vesselgraphtest, `[[`, "dfgraph_out_ful"))
# # feols(ClusteringCoefficient ~ sscale + fscale, data = vesselplotnode)
# 
# f <- ggplot(vesselplotnodef, aes(x = factor(round(fscale,2)),
#                                  y =  Strength, color = sscale)) +
#   geom_boxplot(alpha = .8) +
#   ggthemes::theme_tufte() +
#   scale_color_gradient(low = "#C4EBC8", high = "#2C3A33") +
#   labs(x = "Fishing Costs",
#        color = "Switching Costs",
#        title = "Vessel Network",
#        subtitle = "Week-based Projection (Inverse Fuller)*",
#        caption = TeX("*$A_{vb} = \\sum_f^N \\frac{Weeks_{vfw}Weeks_{bfw}}{Weeks_{fw}}(Weeks_{vfw}+Weeks_{bfw})$"))
# 
# f
# 
# e+f+plot_layout(guides = "collect")
# 
# vesselplotnet <- do.call(rbind, lapply(vesselgraphtest, `[[`, "dfgraph_out_ful")) %>%
#   distinct(fscale, sscale, Modularity, Mean_Weight, N_Edges,
#            ClusteringCoefficient_Global, NetworkCentralization)
# feols(N_Edges ~ sscale + fscale, data = vesselplotnet)
# feols(Modularity ~ sscale + fscale, data = vesselplotnet)
# feols(Mean_Weight ~ sscale + fscale, data = vesselplotnet)
# feols(ClusteringCoefficient_Global ~ sscale + fscale, data = vesselplotnet)
# feols(NetworkCentralization ~ sscale + fscale, data = vesselplotnet)
# 
# ne <- ggplot(vesselplotnet, aes(x = sscale, y =  ClusteringCoefficient_Global,
#                                 color = fscale)) +
#   geom_point(alpha = .8) +
#   ggthemes::theme_tufte() +
#   labs(x = "Fishing Costs",
#        title = "Vessel Network",
#        subtitle = "Co-Participation Projection")
# 
# ne
# 
# vesselplotnet <- do.call(rbind, lapply(vesselgraphtest, `[[`, "dfgraph_out_ext")) %>%
#   distinct(fscale, sscale, Modularity, Mean_Weight, N_Edges,
#            ClusteringCoefficient_Global, NetworkCentralization)
# feols(N_Edges ~ sscale + fscale, data = vesselplotnet)
# feols(Modularity ~ sscale + fscale, data = vesselplotnet)
# feols(Mean_Weight ~ sscale + fscale, data = vesselplotnet)
# feols(ClusteringCoefficient_Global ~ sscale + fscale, data = vesselplotnet)
# feols(NetworkCentralization ~ sscale + fscale, data = vesselplotnet)
# 
# ne <- ggplot(vesselplotnet, aes(x = fscale, y =  NetworkCentralization, color = sscale)) +
#   geom_point(alpha = .8) +
#   ggthemes::theme_tufte() +
#   labs(x = "Fishing Costs",
#        y = "Measure",
#        title = "Vessel Network",
#        subtitle = "Co-Participation Projection")
# 
# ne
# 
# 
# 
# 
# comp <- igraph::components(gy)
# giant_id <- which.max(comp$csize)
# gy_giant <- igraph::induced_subgraph(gy, which(comp$membership == giant_id))
# emb <- embed_laplacian_matrix(gy_giant, no = 5)
# coords <- as.data.frame(emb$X)
# coords$node <- igraph::V(gy_giant)$name
# ggplot(coords, aes(x = V1, y = V2)) +
#   geom_point(na.rm = TRUE) +
#   theme_minimal()
# 
# 
# emb <- embed_adjacency_matrix(gy, no = 8)
# 
# embplot <- df_choice %>%
#   filter(RegionFishery != "Not Fishing",
#          Vessel_ID %in% ey$Fishery1) %>%
#   group_by(Vessel_ID, LengthBin) %>%
#   summarise(Revenue = sum(Revenue), .groups = "drop") %>%
#   mutate(x = emb$X[,1],
#          y = emb$X[,2])
# 
# plot(emb$X[,1], emb$X[,2])
# ggplot(embplot, aes(x = x, y = y, color = LengthBin, size = Revenue)) +
#   geom_point()
# 
# nrow(ey)/2/nrow(vy)^2
# n_distinct(ey$Vessel1)
