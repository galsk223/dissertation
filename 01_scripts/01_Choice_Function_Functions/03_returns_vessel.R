# library(latex2exp)

# ext ---------------------------------------------------------------------

graph_ext_fcn_ves <- function(df_choice, fisherylist_use, vessel_pairs, yi, drop){

  nfisheries <- length(fisherylist_use)
  dropfishers <- df_choice %>%
    filter(FISHERY_ID %in% drop)

  edges_ext <- df_choice %>%
    filter(FISHERY_ID != "Not Fishing") %>%
    distinct(Vessel_ID, FISHERY_ID) %>%
    mutate(present = 1) %>%
    pivot_wider(names_from = Vessel_ID, values_from = present, values_fill = 0) %>%
    column_to_rownames("FISHERY_ID") %>%
    as.matrix() %>%
    { t(.) %*% . } %>%
    as.data.frame() %>%
    rownames_to_column("Vessel1") %>%
    pivot_longer(-Vessel1, names_to = "Vessel2", values_to = "SharedFisheries") %>%
    filter(Vessel1 != Vessel2,
           Vessel1 %in% dropfishers$Vessel_ID) %>%
    full_join(vessel_pairs, by = c("Vessel1", "Vessel2")) %>%
    mutate(SharedFisheries = replace_na(SharedFisheries, 0),
           V1 = pmin(as.character(Vessel1), as.character(Vessel2)),
           V2 = pmax(as.character(Vessel1), as.character(Vessel2))) %>%
    as.data.table() %>%
    distinct(V1, V2, SharedFisheries) %>%
    rename(Vessel1 = V1, Vessel2 = V2) %>%
    arrange(Vessel1, Vessel2)

  ey <- edges_ext %>%
    filter(SharedFisheries > 0) %>%
    select(Vessel1, Vessel2, weight = SharedFisheries)

  vy <- df_choice %>%
    filter(FISHERY_ID != "Not Fishing",
           Vessel_ID %in% c(ey$Vessel1, ey$Vessel2)) %>%
    group_by(Vessel_ID) %>%
    summarise(NFisheries = n_distinct(FISHERY_ID),
              .groups = "drop")  %>%
    select(Vessel_ID, NFisheries)

  gy <- graph_from_data_frame(ey,
                              directed = F,
                              vertices = vy)

  componentuse <- tibble(comps = igraph::components(gy)$membership) %>%
    group_by(comps) %>% tally() %>%
    filter(n > 20)

  i <- unique(componentuse$comps)[1]
  dfgraph_out_ext <- map_dfr(unique(componentuse$comps), function(i){

    vy <- df_choice %>%
      filter(FISHERY_ID != "Not Fishing",
             Vessel_ID %in% c(ey$Vessel1, ey$Vessel2)) %>%
      group_by(Vessel_ID) %>%
      summarise(NFisheries = n_distinct(FISHERY_ID),
                .groups = "drop")  %>%
      select(Vessel_ID, NFisheries)

    gy <- graph_from_data_frame(ey,
                                directed = F,
                                vertices = vy)

    gy <- induced_subgraph(gy,
                           vids = which(igraph::components(gy)$membership == i))
    vy <- vy %>%
      filter(Vessel_ID %in% names(V(gy)))

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

    # SI <- sparsityindex(ey, nlinks = nfisheries, nnodes = nrow(vy), F)
    df_sub <- bind_rows(cache_dfchoice[10:14]) %>%
      filter(Vessel_ID %in% vy$Vessel_ID)
    nf <- n_distinct(df_sub$FISHERY_ID)
    esi <- ey %>%
      filter(Vessel1 %in% vy$Vessel_ID,
             Vessel2 %in% vy$Vessel_ID)

    SI <- sparsityindex(esi, nlinks = nf, nnodes = nrow(vy), "ev")

    p <- vy$NFisheries/sum(vy$NFisheries)
    H <- -sum(p * log(p))
    DF <- fragmentation(Ainv)

    closeness <- igraph::closeness(gy)
    strength <- igraph::strength(gy)

    embstats <- fleet_summary_emb_safe(gy, .01)

    allbars <- tibble(Year = yi,
                      Component = i,
                              Vessel_ID = as.integer(names(membership(cw))),
                              NFisheries = vy$NFisheries,
                              Closeness = closeness,
                              Strength = strength,
                              ClusteringCoefficient = OnnelaClust$LocalCC,
                              FragMeasure = DF,
                              SI = SI,
                              H = H,
                              N_VesselNodes = n_distinct(vy$Vessel_ID),
                              N_Edges = nrow(ey),
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

  })

}


# fuller ------------------------------------------------------------------

graph_fuller_fcn_ves <- function(df_choice, yi, fisherylist_use, drop){

  dropfishers <- df_choice %>%
    filter(FISHERY_ID %in% drop)

  nfisheries <- length(fisherylist_use)
  nvessels <- n_distinct(df_choice$Vessel_ID)
  bipmat <- df_choice %>%
    filter(FISHERY_ID != "Not Fishing") %>%
    group_by(Vessel_ID, FISHERY_ID) %>%
    summarise(Weights = n_distinct(Week), .groups = "drop") %>%
    arrange(as.numeric(Vessel_ID)) %>%
    pivot_wider(names_from = Vessel_ID, values_from = Weights, values_fill = 0) %>%
    column_to_rownames("FISHERY_ID") %>%
    as.matrix()

  connectivity <- matrix(0, nvessels, nvessels)
  colnames(connectivity) <- rownames(connectivity) <- as.numeric(unique(df_choice$Vessel_ID))
  # bipmat_0 <- bipmat
  edges_fuller <- fulleredges_vessel(connectivity, bipmat, dropfishers)
  bipmat_0 <- bipmat

  ey <- edges_fuller %>%
    filter(Weight > 0) %>%
    select(Vessel1, Vessel2, weight = Weight)

  vy <- df_choice %>%
    filter(FISHERY_ID != "Not Fishing",
           Vessel_ID %in% c(ey$Vessel1, ey$Vessel2)) %>%
    group_by(Vessel_ID) %>%
    summarise(NWeeks = n(),
              .groups = "drop")

  gy <- graph_from_data_frame(ey,
                              directed = F,
                              vertices = vy)

  componentuse <- tibble(comps = igraph::components(gy)$membership) %>%
    group_by(comps) %>% tally() %>%
    filter(n > 20)

  i <- unique(componentuse$comps)[1]
  dfgraph_out_ext <- map_dfr(unique(componentuse$comps), function(i){

    vy <- df_choice %>%
      filter(FISHERY_ID != "Not Fishing",
             Vessel_ID %in% c(ey$Vessel1, ey$Vessel2)) %>%
      group_by(Vessel_ID) %>%
      summarise(NFisheries = n_distinct(FISHERY_ID),
                .groups = "drop")  %>%
      select(Vessel_ID, NFisheries)

    gy <- graph_from_data_frame(ey,
                                directed = F,
                                vertices = vy)

    gy <- induced_subgraph(gy,
                           vids = which(igraph::components(gy)$membership == i))
    vy <- vy %>%
      filter(Vessel_ID %in% names(V(gy)))

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

    # SI <- sparsityindex(ey, nlinks = nfisheries,
    #                     nnodes = nrow(vy), F)
    df_sub <- bind_rows(cache_dfchoice[10:14]) %>%
      filter(Vessel_ID %in% vy$Vessel_ID)
    nf <- n_distinct(df_sub$FISHERY_ID)
    esi <- ey %>%
      filter(Vessel1 %in% vy$Vessel_ID,
             Vessel2 %in% vy$Vessel_ID)

    SI <- sparsityindex(esi, nlinks = nf, nnodes = nrow(vy), "fv")

    p <- vy$NFisheries/sum(vy$NFisheries)
    H <- -sum(p * log(p))
    DF <- fragmentation(Ainv)

    closeness <- igraph::closeness(gy)
    strength <- igraph::strength(gy)

    embstats <- fleet_summary_emb_safe(gy, .01)

    allbars <- tibble(Year = yi,
                      Component = i,
                      Vessel_ID = as.integer(names(membership(cw))),
                      NFisheries = vy$NFisheries,
                      Closeness = closeness,
                      Strength = strength,
                      ClusteringCoefficient = OnnelaClust$LocalCC,
                      FragMeasure = DF,
                      SI = SI,
                      H = H,
                      N_VesselNodes = n_distinct(vy$Vessel_ID),
                      N_Edges = nrow(ey),
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

  })

}



