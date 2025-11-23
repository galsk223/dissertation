
df_choice_fcn <- function(choice, revenue, cost_multiplier, v_set){

  v_setc <- v_set %>%
    select(Vessel_ID, LengthBin)
  df_choice <- left_join(
    bind_cols(v_setc,
              as.data.frame(choice)) %>%
      pivot_longer(-c(Vessel_ID, LengthBin),
                   names_to = "Week",
                   values_to = "FISHERY_ID"),
    bind_cols(v_setc,
              as.data.frame(revenue)) %>%
      pivot_longer(-c(Vessel_ID, LengthBin),
                   names_to = "Week",
                   values_to = "Revenue"),
    by = join_by(Vessel_ID, LengthBin, Week)) %>%
    mutate(Cost_Multiplier = cost_multiplier)

}

fishing_fcn <- function(df_choice, yi){

  fishing <- df_choice %>%
    group_by(LengthBin) %>%
    mutate(NVessels = n_distinct(Vessel_ID)*53) %>%
    ungroup() %>%
    group_by(LengthBin, FISHERY_ID, NVessels) %>%
    tally() %>%
    mutate(Shares = n/NVessels) %>%
    ungroup() %>%
    group_by(LengthBin) %>%
    mutate(Check = sum(Shares),
           Check2 = sum(Shares*(FISHERY_ID!="Not Fishing")),
           Year = yi)

}

graph_ext_fcn <- function(df_choice, fishery_pairs, fisherylist_use, yi, largestcc){

  nfisheries <- length(fisherylist_use)
  nvessels <- n_distinct(df_choice$Vessel_ID)
  edges_ext <- df_choice %>%
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

  vy <- df_choice %>%
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

  if(nrow(ey) == 0){
    return(dfgraph_out <- tibble(Year = yi,
                                 NEdges = 0))}

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

  df_sub <- df_choice %>%
    filter(FISHERY_ID %in% vy$FISHERY_ID)
  nv <- n_distinct(df_sub$Vessel_ID)
  esi <- ey %>%
    filter(Fishery1 %in% vy$FISHERY_ID)
  SI <- sparsityindex(esi, nlinks = nvessels, nnodes = nrow(vy), "extensive")

  # SI <- sparsityindex(ey, nlinks = nvessels, nnodes = nrow(vy), F)
  p <- vy$UniqueVessels/sum(vy$UniqueVessels)
  H <- -sum(p * log(p))
  pe <- esi$weight/sum(esi$weight)
  He <- -sum(pe * log(pe))
  DF <- fragmentation(Ainv)

  # node level
  if (nrow(Ainv) > 2){
    fc <- fragment(Ainv, binary = F)[,1]
  } else {
    fc <- 0
  }

  # E(g)$inv_weight <- 1 / E(g)$weight
  # closeness(g, weights = E(g)$inv_weight)
  closeness <- igraph::closeness(gy)

  strength <- igraph::strength(gy)

  embstats <- fleet_summary_emb_safe(gy, .01)

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

graph_fuller_fcn <- function(df_choice, yi, fisherylist_use, largestcc){

  nfisheries <- length(fisherylist_use)
  nvessels <- n_distinct(df_choice$Vessel_ID)
  bipmat <- df_choice %>%
    group_by(Vessel_ID, FISHERY_ID) %>%
    summarise(Weights = n_distinct(Week), .groups = "drop") %>%
    arrange(FISHERY_ID) %>%
    pivot_wider(names_from = FISHERY_ID, values_from = Weights, values_fill = 0) %>%
    column_to_rownames("Vessel_ID") %>%
    as.matrix()

  connectivity <- matrix(0, nfisheries+1, nfisheries+1)
  colnames(connectivity) <- rownames(connectivity) <- sort(c(fisherylist_use,"Not Fishing"))

  edges_fuller <- fulleredges(connectivity, bipmat) %>%
    filter(Fishery1 != "Not Fishing",
           Fishery2 != "Not Fishing")

  vy <- df_choice %>%
    group_by(FISHERY_ID) %>%
    summarise(NWeeks = n(),
              .groups = "drop") %>%
    filter(FISHERY_ID != "Not Fishing") %>%
    select(FISHERY_ID, NWeeks)

  ey <- edges_fuller %>%
    filter(Fishery1 %in% vy$FISHERY_ID,
           Fishery2 %in% vy$FISHERY_ID,
           Weight > 0) %>%
    filter(Fishery1 != "Not Fishing",
           Fishery2 != "Not Fishing") %>%
    select(Fishery1, Fishery2, weight = Weight)

  if(nrow(ey) == 0){
    return(dfgraph_out <- tibble(Year = yi,
                                 NEdges = 0))}

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
  Ainv <- A
  Ainv[A!=0] <- 1/A[A!=0]

  # graph level
  OnnelaClust <- ClustF(A, "undirected")
  cw <- cluster_walktrap(gy, modularity = T)
  m <- modularity(cw)
  nc <- networkcentralization(gy)
  urf <- mean(E(gy)$weight) + var(E(gy)$weight)/mean(E(gy)$weight)

  # SI <- sparsityindex(ey, nlinks = nvessels, nnodes = nrow(vy), F)
  df_sub <- df_choice %>%
    filter(FISHERY_ID %in% vy$FISHERY_ID)
  nv <- n_distinct(df_sub$Vessel_ID)
  esi <- ey %>%
    filter(Fishery1 %in% vy$FISHERY_ID)
  SI <- sparsityindex(ey, nlinks = nv, nnodes = nrow(vy), "fuller")

  p <- vy$NWeeks/sum(vy$NWeeks)
  H <- -sum(p * log(p))
  DF <- fragmentation(Ainv)

  # node level
  if (nrow(Ainv) > 2){
    fc <- fragment(Ainv, binary = F)[,1]
  } else {
    fc <- 0
  }

  closeness <- igraph::closeness(gy)
  strength <- igraph::strength(gy)

  embstats <- fleet_summary_emb_safe(gy, .01)

  dfgraph_out_f <- tibble(Year = yi,
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
                        Mean_Weight = mean(ey$weight),
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

graph_bip_fcn <- function(df_choice, yi, nfisheries){

  nvessels <- n_distinct(df_choice$Vessel_ID)

  # igraph::is_bipartite(gy)
  # igraph::is_weighted(gy)
  # A <- as_adjacency_matrix(gy, sparse=FALSE, attr="weight")
  # Ainv <- A
  # Ainv[A!=0] <- 1/A[A!=0]

  # if(largestcc == T){
  #
  #   bip <- df_choice %>%
  #     filter(FISHERY_ID != "Not Fishing") %>%
  #     group_by(FISHERY_ID, Vessel_ID) %>%
  #     summarise(NTrips = n(), .groups = "drop") %>%
  #     mutate(weight = NTrips)
  #
  #   if(n_distinct(bip$FISHERY_ID) <= 2){
  #     return(dfgraph_out <- tibble(Year = yi,
  #                                  Fishery = unique(bip$FISHERY_ID)))
  #
  #     gy <- graph_from_data_frame(bip,
  #                                 directed = F)
  #     V(gy)$type <- bipartite_mapping(gy)$type
  #     gy <- induced_subgraph(gy,
  # vids = which(igraph::components(gy)$membership ==
  #                which.max(igraph::components(gy)$csize)))
  #   }
  #
  # affiliation.mat <- bip %>%
  #   pivot_wider(names_from = FISHERY_ID,
  #               values_from = weight,
  #               values_fill = 0) %>%
  #   select(-c(Vessel_ID, NTrips))

  bip <- df_choice %>%
    filter(FISHERY_ID != "Not Fishing") %>%
    group_by(FISHERY_ID, Vessel_ID) %>%
    summarise(NTrips = n(), .groups = "drop") %>%
    mutate(weight = NTrips)

  if(n_distinct(bip$FISHERY_ID) <= 2){
    return(dfgraph_out <- tibble(Year = yi,
                                 Fishery = unique(bip$FISHERY_ID)))}

  gs <- graph_from_data_frame(bip,
                              directed = F)

  componentuse <- tibble(comps = igraph::components(gs)$membership) %>%
    group_by(comps) %>% tally() %>%
    filter(n > 20)

  c <- componentuse$comps[1]
    dfgraph_out <- map_dfr(componentuse$comps, function(c){

      gy <- induced_subgraph(gs,
                             vids = which(igraph::components(gs)$membership == c))
      V(gy)$type <- bipartite_mapping(gy)$type
      igraph::is_bipartite(gy)
      igraph::is_weighted(gy)

      edge_list <- data.frame(from = as.vector(as_edgelist(gy)[,1]),
                              to = as.vector(as_edgelist(gy)[,2]),
                              weight = E(gy)$weight)
      nfisheries <- n_distinct(edge_list$from)

      if(nfisheries == 1){
        return(NULL)
      }

      eb <- edge_list %>%
        select(lower = from, higher = to,
               freq = weight) %>%
        mutate(webID = 1)
      web <- frame2webs(eb)[[1]]

      tw <- try(as.tnet(web, type="weighted two-mode tnet"), silent = TRUE)

      clusterlocal <- list()
      if (inherits(tw, "try-error")) {
        clusterglobal <- NA
        clusterlocal$lc <- NA
        H2 <- NA
      } else {
        tw <- as.tnet(web, type="weighted two-mode tnet")
        clusterglobal <- bipartite::clustering_tm(web)
        clusterlocal <- tnet::clustering_local_tm(tw)
        H2 <- bipartite::networklevel(web, index = "H2")
      }

      affiliation.mat <- edge_list %>%
        pivot_wider(names_from = from,
                    values_from = weight,
                    values_fill = 0) %>%
        select(-to)

      # promising package functions
      # modularity <- DIRT_LPA_wb_plus(web, mini = 5)$modularity
      library(BipartiteModularityMaximization)
      modularity <- bipmod(affiliation.mat)$MODULARITY

      strength <- bipartite::strength(t(web))
      closeness <- igraph::closeness(gy)[1:length(strength)]
      size <- df_choice %>%
        filter(FISHERY_ID %in% names(V(gy))) %>%
        group_by(FISHERY_ID) %>%
        summarise(NWeeks = n(),
                  .groups = "drop") %>%
        filter(FISHERY_ID != "Not Fishing") %>%
        select(FISHERY_ID, NWeeks)

      # closeness <- bipartite::closeness_w(t(web), gconly = F)
      fcb <- fragmentation.centrality.bipartite(affiliation.mat)

      # check these
      redundancy <- robustness(second.extinct(web, participant = "lower"))
      complementarity <- fc(web, dist="euclidean", method="average", weighted=TRUE)
      # embstats <- fleet_summary_emb_safe(gy, .01)

      urf <- mean(eb$freq) + var(eb$freq)/mean(eb$freq)
        esi <- eb %>%
          mutate(weight = freq)

      # SI <- sparsityindex(esi, nlinks = nvessels, nnodes = nfisheries, F)
      esi <- edge_list %>%
        select(lower = from, higher = to,
               freq = weight) %>%
        mutate(weight = freq)

      SI <- sparsityindex(esi, nlinks = n_distinct(esi$lower),
                          nnodes = n_distinct(esi$higher), "bipartite")

      nc <- networkcentralization_bipartite(strength, eb, nfisheries, nvessels)
      DF <- fragmentation_bipartite(affiliation.mat)

      bars <- tibble(Year = yi,
                     Componenet = c,
                    Fishery = names(strength),
                    Size = size$NWeeks,
                    Closeness = closeness,
                    Strength = strength,
                    ClusteringCoefficient = clusterlocal$lc,
                    FragmentationCentrality_Intra = fcb$Intramodal,
                    FragmentationCentrality_Cross = fcb$Crossmodal,
                    FragmentationCentrality_Total = fcb$Overall,
                    FragMeasure = DF,
                    SI = SI,
                    H2 = H2,
                    N_Fisheries = n_distinct(bip$FISHERY_ID),
                    N_Edges = nrow(bip),
                    Mean_Weight = mean(eb$freq),
                    ClusteringCoefficient_Global = clusterglobal,
                    Modularity = modularity,
                    NetworkCentralization = nc,
                    URF = urf,
                    Redundancy = redundancy,
                    Complementarity = complementarity)

    })


}





