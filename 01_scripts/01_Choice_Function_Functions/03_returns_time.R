graph_time_trans <- function(cache_dfchoice, burnin, yi, nvessels, largestcc){

  ey <- transition5(cache_dfchoice, yi) %>%
    filter(weight > 0)

  vy <- bind_rows(cache_dfchoice[[yi-4]],
                  cache_dfchoice[[yi-3]],
                  cache_dfchoice[[yi-2]],
                  cache_dfchoice[[yi-1]],
                  cache_dfchoice[[yi]], .id = "source") %>%
    group_by(FISHERY_ID) %>%
    summarise(UniqueVessels = n_distinct(Vessel_ID),
              .groups = "drop") %>%
    filter(FISHERY_ID != "Not Fishing") %>%
    select(FISHERY_ID, UniqueVessels)

  # print(setdiff(unique(c(ey$F1, ey$F2)), vy$FISHERY_ID))
  gy <- graph_from_data_frame(ey,
                              directed = F,
                              vertices = vy)

  if(largestcc == T){
    gy <- induced_subgraph(gy,
                           vids = which(igraph::components(gy)$membership ==
                                          which.max(igraph::components(gy)$csize)))
    vy <- vy %>%
      filter(FISHERY_ID %in% names(V(gy)))
    }

  A <- as_adjacency_matrix(gy, sparse=FALSE, attr="weight")
  lA <- A[lower.tri(A)]
  Ainv <- A
  Ainv[A!=0] <- 1/A[A!=0]

  # graph level
  OnnelaClust <- ClustF(A, "undirected")
  cw <- cluster_walktrap(gy, modularity = T)
  m <- modularity(cw)
  nc <- networkcentralization(gy)
  urf <- mean(E(gy)$weight) + var(E(gy)$weight)/mean(E(gy)$weight)

  # SI <- sparsityindex(ey, nlinks = nvessels, nnodes = nrow(vy), F)
  df_sub <- bind_rows(cache_dfchoice[10:14]) %>%
    filter(FISHERY_ID %in% vy$FISHERY_ID)
  nv <- n_distinct(df_sub$Vessel_ID)
  esi <- ey %>%
    filter(F1 %in% vy$FISHERY_ID,
           F2 %in% vy$FISHERY_ID)

  SI <- sparsityindex(esi, nlinks = nv, nnodes = n_distinct(df_sub$FISHERY_ID), "tt")

  p <- vy$UniqueVessels/sum(vy$UniqueVessels)
  H <- -sum(p * log(p))
  DF <- fragmentation(Ainv)

  # node level
  if (nrow(Ainv) > 2){
    fc <- fragment(Ainv, binary = F)[,1]
  } else {
    fc <- 0
  }

  # closeness <- igraph::closeness(gy)
  strength <- igraph::strength(gy)

  embstats <- fleet_summary_emb_safe(gy, .01)

  eyw <- esi %>%
    mutate(weight = 1/weight)
  gy <- graph_from_data_frame(eyw,
                              directed = F,
                              vertices = vy)
  closeness <- igraph::closeness(gy)

  dfgraph_out <- tibble(Year = yi,
                        Fishery = names(membership(cw)),
                        Size = filter(vy, FISHERY_ID %in% names(membership(cw)))$UniqueVessels,
                        Closeness = closeness,
                        Strength = strength,
                        ClusteringCoefficient = OnnelaClust$LocalCC,
                        FragmentationCentrality = fc,
                        FragMeasure = DF,
                        SI = SI,
                        H = H,
                        N_Fisheries = n_distinct(vy$FISHERY_ID),
                        N_Edges = nrow(esi),
                        Mean_Weight = mean(lA[lA > 0]),
                        ClusteringCoefficient_Global = OnnelaClust$GlobalCC,
                        Cluster = membership(cw),
                        Modularity = m,
                        NetworkCentralization = nc,
                        URF = urf,
                        HullArea = embstats$hull_area,
                        HullAreaTrimmed = embstats$hull_area_trimmed,
                        MedianToCentroid = embstats$median_dist_to_centroid,
                        MeanToCentroid = embstats$mean_dist_to_centroid)

}

graph_time_corr <- function(cache_dfchoice, yi, nvessels, largestcc){

  ey <- coredges(cache_dfchoice, yi) %>%
    filter(weight > 0)

  vy <- bind_rows(cache_dfchoice[[yi-4]],
                  cache_dfchoice[[yi-3]],
                  cache_dfchoice[[yi-2]],
                  cache_dfchoice[[yi-1]],
                  cache_dfchoice[[yi]], .id = "source") %>%
    group_by(FISHERY_ID)  %>%
    summarise(NWeeks = n(),
              .groups = "drop") %>%
    filter(FISHERY_ID != "Not Fishing") %>%
    select(FISHERY_ID, NWeeks)

  gy <- graph_from_data_frame(ey,
                              directed = F,
                              vertices = vy)

  if(largestcc == T){
    gy <- induced_subgraph(gy,
                           vids = which(igraph::components(gy)$membership ==
                                          which.max(igraph::components(gy)$csize)))
    vy <- vy %>%
      filter(FISHERY_ID %in% names(V(gy)))
  }

  A <- as_adjacency_matrix(gy, sparse=FALSE, attr="weight")
  lA <- A[lower.tri(A)]
  Ainv <- A
  Ainv[A!=0] <- 1/A[A!=0]

  # graph level
  OnnelaClust <- ClustF(A, "undirected")
  cw <- cluster_walktrap(gy, modularity = T)
  m <- modularity(cw)
  nc <- networkcentralization(gy)
  urf <- mean(E(gy)$weight) + var(E(gy)$weight)/mean(E(gy)$weight)

  # SI <- sparsityindex(ey, nlinks = nvessels, nnodes = nrow(vy), F)
  df_sub <- bind_rows(cache_dfchoice[10:14]) %>%
    filter(FISHERY_ID %in% vy$FISHERY_ID)
  nv <- n_distinct(df_sub$Vessel_ID)
  esi <- ey %>%
    filter(Fishery1 %in% vy$FISHERY_ID,
           Fishery2 %in% vy$FISHERY_ID)

  SI <- sparsityindex(esi, nlinks = nv, nnodes = n_distinct(df_sub$FISHERY_ID), "tc")

  p <- vy$NWeeks/sum(vy$NWeeks)
  H <- -sum(p * log(p))
  DF <- fragmentation(Ainv)

  # node level
  if (nrow(Ainv) > 2){
    fc <- fragment(Ainv, binary = F)[,1]
  } else {
    fc <- 0
  }

  # closeness <- igraph::closeness(gy)
  strength <- igraph::strength(gy)

  embstats <- fleet_summary_emb_safe(gy, .01)

  eyw <- esi %>%
    mutate(weight = 1/weight)
  gy <- graph_from_data_frame(eyw,
                              directed = F,
                              vertices = vy)
  closeness <- igraph::closeness(gy)

  dfgraph_out <- tibble(Year = yi,
                        Fishery = names(membership(cw)),
                        Size = filter(vy, FISHERY_ID %in% names(membership(cw)))$NWeeks,
                        Closeness = closeness,
                        Strength = strength,
                        ClusteringCoefficient = OnnelaClust$LocalCC,
                        FragmentationCentrality = fc,
                        FragMeasure = DF,
                        SI = SI,
                        H = H,
                        N_Fisheries = n_distinct(vy$FISHERY_ID),
                        N_Edges = nrow(esi),
                        Mean_Weight = mean(lA[lA > 0]),
                        ClusteringCoefficient_Global = OnnelaClust$GlobalCC,
                        Cluster = membership(cw),
                        Modularity = m,
                        NetworkCentralization = nc,
                        URF = urf,
                        HullArea = embstats$hull_area,
                        HullAreaTrimmed = embstats$hull_area_trimmed,
                        MedianToCentroid = embstats$median_dist_to_centroid,
                        MeanToCentroid = embstats$mean_dist_to_centroid)

}
