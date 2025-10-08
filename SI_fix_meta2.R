
si_e <- function(d){
  df_choice <- d$sim_run$cache_dfchoice[[14]]
  
  nfisheries <- n_distinct(df_choice$FISHERY_ID)-1
  fisherylist_use <- unique(df_choice$FISHERY_ID)[unique(df_choice$FISHERY_ID) != "Not Fishing"]
  fishery_pairs <- expand.grid(Fishery1 = fisherylist_use, 
                               Fishery2 = fisherylist_use, 
                               stringsAsFactors = FALSE) %>%
    filter(Fishery1 != Fishery2) %>% 
    arrange(Fishery1, Fishery2)  
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
  
    gy <- induced_subgraph(gy, 
                           vids = which(igraph::components(gy)$membership == 
                                          which.max(igraph::components(gy)$csize)))
    vy <- vy %>% 
      filter(FISHERY_ID %in% names(V(gy)))
    ey <- ey %>% 
      filter(Fishery1 %in% names(V(gy)),
             Fishery2 %in% names(V(gy)))
    
    df_sub <- df_choice %>% 
      filter(FISHERY_ID %in% vy$FISHERY_ID)
    nv <- n_distinct(df_sub$Vessel_ID)
    
    SI <- sparsityindex(ey, nlinks = nv, nnodes = nrow(vy), "extensive")
  
}

si_f <- function(d){
  df_choice <- d$sim_run$cache_dfchoice[[14]]
  
  nfisheries <- n_distinct(df_choice$FISHERY_ID)-1
  fisherylist_use <- unique(df_choice$FISHERY_ID)[unique(df_choice$FISHERY_ID) != "Not Fishing"]
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
  
  gy <- induced_subgraph(gy, 
                         vids = which(igraph::components(gy)$membership == 
                                        which.max(igraph::components(gy)$csize)))
  vy <- vy %>% 
    filter(FISHERY_ID %in% names(V(gy)))
  ey <- ey %>% 
    filter(Fishery1 %in% names(V(gy)),
           Fishery2 %in% names(V(gy)))
  
  df_sub <- df_choice %>% 
    filter(FISHERY_ID %in% vy$FISHERY_ID)
  nv <- n_distinct(df_sub$Vessel_ID)
  
  SI <- sparsityindex(ey, nlinks = nv, nnodes = nrow(vy), "fuller")
  
}

si_b <- function(d){
  df_choice <- d$sim_run$cache_dfchoice[[14]]
  nvessels <- n_distinct(df_choice$Vessel_ID)
  
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
  
  c <- componentuse$comps[2]
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
    
    esi <- edge_list %>% 
      select(lower = from, higher = to, 
             freq = weight) %>% 
      mutate(weight = freq)
    
    SI <- sparsityindex(esi, nlinks = n_distinct(esi$lower), 
                        nnodes = n_distinct(esi$higher), "bipartite")
    
    out <- tibble(Componenet = c,
                  SI = SI)
    
  })
    
} 
  
si_tc <- function(d){
  
  cache_dfchoice <- d$sim_run$cache_dfchoice
  ey <- coredges(cache_dfchoice, 14) %>%
    filter(weight > 0)
  
  vy <- bind_rows(cache_dfchoice[[14-4]],
                  cache_dfchoice[[14-3]],
                  cache_dfchoice[[14-2]],
                  cache_dfchoice[[14-1]],
                  cache_dfchoice[[14]], .id = "source") %>% 
    group_by(FISHERY_ID)  %>% 
    summarise(NWeeks = n(),
              .groups = "drop") %>% 
    filter(FISHERY_ID != "Not Fishing") %>%
    select(FISHERY_ID, NWeeks) 
  
  gy <- graph_from_data_frame(ey,
                              directed = F,
                              vertices = vy)
  
    gy <- induced_subgraph(gy, 
                           vids = which(igraph::components(gy)$membership == 
                                          which.max(igraph::components(gy)$csize)))
    vy <- vy %>% 
      filter(FISHERY_ID %in% names(V(gy)))
    ey <- ey %>% 
      filter(Fishery1 %in% names(V(gy)),
             Fishery2 %in% names(V(gy)))
  
    df_sub <- bind_rows(cache_dfchoice[10:14]) %>% 
      filter(FISHERY_ID %in% vy$FISHERY_ID)
    nv <- n_distinct(df_sub$Vessel_ID)
    
    SI <- sparsityindex(ey, nlinks = nv, nnodes = n_distinct(df_sub$FISHERY_ID), "tc")
    
}
  
si_tt <- function(d){
  
  cache_dfchoice <- d$sim_run$cache_dfchoice
  ey <- transition5(cache_dfchoice, 14) %>% 
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
  
    gy <- induced_subgraph(gy, 
                           vids = which(igraph::components(gy)$membership == 
                                          which.max(igraph::components(gy)$csize)))
    vy <- vy %>% 
      filter(FISHERY_ID %in% names(V(gy)))
    ey <- ey %>% 
      filter(F1 %in% names(V(gy)),
             F2 %in% names(V(gy)))
    
    df_sub <- bind_rows(cache_dfchoice[10:14]) %>% 
      filter(FISHERY_ID %in% vy$FISHERY_ID)
    nv <- n_distinct(df_sub$Vessel_ID)
    
    SI <- sparsityindex(ey, nlinks = nv, nnodes = n_distinct(df_sub$FISHERY_ID), "tt")
  
}

si_ev <- function(d){
  
  df_choice <- d$sim_run$cache_dfchoice[[14]]
  nfisheries <- n_distinct(df_choice$FISHERY_ID)
  dropfishers <- df_choice %>% 
    filter(FISHERY_ID %in% drop)
  vessel_pairs <- expand.grid(Vessel1 = as.character(unique(dropfishers$Vessel_ID)), 
                              Vessel2 = as.character(unique(dropfishers$Vessel_ID)), 
                               stringsAsFactors = FALSE) %>%
    filter(Vessel1 != Vessel2) %>% 
    arrange(Vessel1, Vessel2)  
  
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
    
    df_sub <- bind_rows(cache_dfchoice[10:14]) %>% 
      filter(Vessel_ID %in% vy$Vessel_ID)
    nf <- n_distinct(df_sub$FISHERY_ID)
    
    SI <- sparsityindex(ey, nlinks = nf, nnodes = nrow(vy), "ev")
    
    out <- tibble(Component = i,
                  SI = SI)
    
  })
}

si_fv <- function(d){
  
  df_choice <- d$sim_run$cache_dfchoice[[14]]
  dropfishers <- df_choice %>% 
    filter(FISHERY_ID %in% drop)
  
  nfisheries <- n_distinct(df_choice$FISHERY_ID)
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
    
    df_sub <- bind_rows(cache_dfchoice[10:14]) %>% 
      filter(Vessel_ID %in% vy$Vessel_ID)
    nf <- n_distinct(df_sub$FISHERY_ID)
    
    SI <- sparsityindex(ey, nlinks = nf, nnodes = nrow(vy), "fv")
    
    out <- tibble(Component = i,
                  SI = SI)
    
  })
  
}
