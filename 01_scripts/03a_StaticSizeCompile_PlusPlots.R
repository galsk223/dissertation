rm(list = ls())
library(zoo)
library(DescTools)
library(tidyverse)
library(future)

base_name <- "~/westcoast-networks/data/Simulation/StaticSimulationOutcomes/"
fileid <- "Static_adjustment"
allfiles <- list.files(paste0(base_name), full.names = T)
allfiles <- allfiles[file.info(allfiles)$isdir == FALSE]

# loop  -------------------------------------------------------------------

color_list <- c("#4c6085","#39a0ed","#13c4a3","#f7b32b","#C09BD8","#EC0868","#52050A",
                "#fe5f55","#161613","#A44A3F")

f <- 10
c <- 6
for (f in 1:length(allfiles)){

  alliter <- read_rds(allfiles[[f]])

  allnet <- map(1:length(alliter), function(c){

    print(c)
    d <- alliter[[c]]

    if(length(d$sim_run) == 0){return(NULL)}
    diverse <-d$sim_run$cache_dfchoice[[1]] %>%
      group_by(Vessel_ID) %>%
      filter(FISHERY_ID != "Not Fishing") %>%
      summarise(Diversification = n_distinct(FISHERY_ID))
    if(max(diverse$Diversification) == 1){return(NULL)}
    strategies <- d$sim_run$cache_dfchoice[[1]] %>%
      group_by(Vessel_ID) %>%
      filter(FISHERY_ID != "Not Fishing") %>%
      summarize(
        n_fisheries = n_distinct(FISHERY_ID),
        strategy = paste(sort(unique(FISHERY_ID)), collapse = "-"),
        .groups = "drop"
      ) %>%
      filter(n_fisheries >= 2)

    out <- tibble(iter = unique(d$sim_id$iter),
                  FishingCost = d$sim_id$rscalef,
                  SwitchingCost = d$sim_id$rscales,
                  N_diversified2 = sum(diverse$Diversification >= 2),
                  N_diversified3 = sum(diverse$Diversification >= 3),
                  N_diversified4 = sum(diverse$Diversification >= 4),
                  N_diversified5 = sum(diverse$Diversification >= 5),
                  MeanDiversification_All = mean(diverse$Diversification),
                  MeanDiversification_Diverse = mean(diverse$Diversification[diverse$Diversification >= 2]),
                  N_strategies = n_distinct(strategies$strategy))

    net_e <- bind_cols(out, d$sim_run$cache_e[[1]] %>%
                         mutate(Cluster = as.numeric(Cluster),
                                NClusters = max(Cluster)))
    net_f <- bind_cols(out, d$sim_run$cache_f[[1]] %>%
                         mutate(Cluster = as.numeric(Cluster),
                                NClusters = max(Cluster)))
    net_b <- bind_cols(out, d$sim_run$cache_b)

    return(list(net_e = net_e,
                net_f = net_f,
                net_b = net_b))

  })

  all_e <- map_dfr(allnet, "net_e")
  %>%
    filter(N_Fisheries < 10)
  all_f <- map_dfr(allnet, "net_f")
  all_b <- map_dfr(allnet, "net_b")

  plot(all_e$SwitchingCost, all_e$MeanToCentroid)

  plotloop <- c(54,20,63,33,10)
  plotloop <- c(97,76,40,60,7,68,77,54)

  p <- 1
  plots <- list()
  for(p in 1:length(plotloop)){


    t <- alliter[[plotloop[[p]]]]$sim_run$cache_dfchoice[[1]]
    info <- all_e %>%
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

    # plot(gy)

      # compute node strength (sum of weighted edges)
      strengths <- igraph::strength(gy)

      # get name of the strongest node
      center_node <- names(which.max(strengths))

    out <- gy %>%
      as_tbl_graph() %>%
      activate(nodes)

    layout_star <- igraph::layout_as_star(gy, center = which(V(gy)$name == center_node))

    plots[[p]] <-
      ggraph(out, layout = "manual", x = layout_star[,1], y = layout_star[,2]) +
      geom_edge_link(aes(width = weight), alpha = 0.5,
                     color = color_list[3]) +
      geom_node_point(aes(size = UniqueVessels)) +
      geom_node_text(aes(label = name), repel = TRUE, size = 3) +
      scale_size_continuous(limits = c(0,75),
                            range = c(.5,10)) +
      scale_edge_width(limits = c(0,50),
                       range = c(.5,8)) +
      theme_void() +
      guides(size = "none",
             edge_width = "none"
      ) +
      labs(
        subtitle = "Changing Switching Costs",
        caption = paste0("Switching Costs: ",round(unique(info$SwitchingCost),2),
                         "\nMean Diversification: ",round(unique(info$MeanDiversification_All),2),
                         "\n# Strategies: ",round(unique(info$N_strategies),2),
                         "\nMean Weight: ",round(unique(info$Mean_Weight),2),
                         "\nDensity: ",round(unique(info$ClusteringCoefficient_Global),2),
                         "\nModularity: ",round(unique(info$Modularity),2),
                         "\nFragmentation: ",round(unique(info$FragMeasure),2))) +
      theme(plot.margin = margin(5,5,5,5),
            plot.subtitle = element_text(size=12, color="grey40"),
            plot.caption = element_text(size = 8, hjust = 0),
            plot.background = element_rect(fill = "transparent", color = NA),
            panel.background = element_rect(fill = "transparent", color = NA))


  }

  plots

  # allout <- list(df_sim, vp, np)
  # outfile <- paste0("~/westcoast-networks/data/clean/Simulation/projectiondataforML_",fileid,f,".rds")
  # write_rds(allout, outfile)

}
