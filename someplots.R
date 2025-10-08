library(tidyverse)
library(data.table)
library(igraph)
library(ggraph)
library(tidygraph)
list.files("westcoast-networks/scripts/02_Simulation/0_Port_Regions/01_Choice_Function_Functions/",
           full.names = TRUE) %>%
  walk(source)

color_list <- c("#4c6085","#39a0ed","#13c4a3","#f7b32b","#C09BD8","#EC0868","#52050A",
               "#fe5f55","#161613","#A44A3F")

folder6 <- list.files(paste0(base_name, fileid, 6, "/"), full.names = T)
filesiter <- str_subset(folder6, "(?<=Meta_)(261.|120.|101.|24\\.|28\\.|957.|303.|992.)+")

c <- vi_long %>% 
  filter(series == 6,
         Dep == "V5")
f <- 1
znets <- list()
embs <- list()
for (f in 1:length(filesiter)){
  
  print(f)
  t <- read_rds(filesiter[[f]])$sim_run$cache_dfchoice[[14]]
  
  nfisheries <- n_distinct(t$FISHERY_ID) - 1
  fisherylist_use <- unique(t$FISHERY_ID)[unique(t$FISHERY_ID) != "Not Fishing"]
  nvessels <- n_distinct(t$Vessel_ID)
  bipmat <- t %>%  
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
  
  vyall <- t %>% 
    group_by(FISHERY_ID) %>% 
    summarise(NWeeks = n(),
              .groups = "drop") %>% 
    filter(FISHERY_ID != "Not Fishing") %>%
    select(FISHERY_ID, NWeeks)
  
  ey <- edges_fuller %>% 
    filter(Fishery1 %in% vyall$FISHERY_ID,
           Fishery2 %in% vyall$FISHERY_ID,
           Weight > 0) %>% 
    filter(Fishery1 != "Not Fishing",
           Fishery2 != "Not Fishing") %>% 
    select(Fishery1, Fishery2, weight = Weight)
  
  gyall <- graph_from_data_frame(ey,
                              directed = F,
                              vertices = vyall)
  largestcc <- T
  if(largestcc == T){
    gy <- induced_subgraph(gyall, 
                           vids = which(igraph::components(gyall)$membership == 
                                          which.max(igraph::components(gyall)$csize)))
    vy <- vy %>% 
      filter(FISHERY_ID %in% names(V(gyall)))
  }
  
  Mean_Weight <- mean(ey$weight)
  SI <- sparsityindex(ey, nlinks = nvessels, nnodes = nrow(vy), F)
  closeness <- igraph::closeness(gy)
  vals_str <- closeness[names(closeness) %in% read_rds(filesiter[[f]])$sim_id$Drop] %>%
    round(2) %>%
    sort(decreasing = TRUE) %>%
    paste(collapse = ", ")
  
  embstats <- fleet_summary_emb_safe(gy, .01)
  emb <- embed_laplacian_matrix(gy, no = 2, type = "dad")
  coords <- as.data.frame(emb$X)
  coordf <- coords %>% 
    mutate(Node = igraph::V(gy)$name,
           Group = ifelse(Node %in% read_rds(filesiter[[f]])$sim_id$Drop, 
                          "Dropped", "Other"))
  centroid <- as.data.frame(t(colMeans(coords))) 
  hull_idx <- chull(coords$V1, coords$V2)
  hull_coords <- coords[c(hull_idx, hull_idx[1]), ]
  
  embs[[f]] <- ggplot(coordf, aes(x = V1, y = V2)) +
    geom_polygon(data = hull_coords, aes(x = V1, y = V2),
                 fill = color_list[[2]], color = color_list[[5]], alpha = .1, linewidth = 1) +
    geom_point(size = 2.5, 
               # color = color_list[[1]],
               aes(color = Group)) + 
    scale_color_manual(values = c("Dropped" = color_list[7], "Other" = color_list[2])) +
    geom_point(data = centroid, aes(x = V1, y = V2),
               color = color_list[[3]], size = 3.5, shape = 17) +
    ggthemes::theme_tufte() +
    # theme_void() +
    coord_cartesian(xlim = c(-0.7, 0.7), ylim = c(-0.7, 0.7)) + 
    labs(x = "",
         y = "")
    
  HullArea <- embstats$hull_area
  MedianToCentroid <- embstats$median_dist_to_centroid

  out <- gyall %>% 
    as_tbl_graph() %>%
    activate(nodes) %>%
    mutate(color_group = ifelse(name %in% read_rds(filesiter[[f]])$sim_id$Drop, 
                                "Dropped", "Other"))
  
  znets[[f]] <- ggraph(out, layout = "linear", circular = TRUE) +
    geom_edge_link(aes(width = weight), alpha = 0.5,
                   color = color_list[3]) + # Edge color by facet
    geom_node_point(aes(size = NWeeks, color = color_group)) +
    theme_void() +
    guides(size = "none",
           # guide_legend(title = "UniqueVessels"),
           color = "none",
           edge_width = "none"
           # guide_legend(title = "Weight")
    ) +
    scale_color_manual(values = c("Dropped" = color_list[7], "Other" = color_list[2])) +
    scale_size_continuous(limits = c(0,2500),
                          range = c(.5,10)) +
    scale_edge_width(limits = c(0,200),
                     range = c(.5,8)) +
    labs(
      # title = paste0("some title"),
      subtitle = str_sub(filesiter[[f]],99,-5),
      caption = paste0("Mean Weight: ",round(Mean_Weight,2),
                       # "\nHullArea: ",round(HullArea,2),
                       "\nMedianToCentroid: ", round(MedianToCentroid,2),
                       "\nSI: ", round(SI,2),
                       "\nCloseness: ", vals_str,
                       "\nSS Vessels: ", nvessels)) +
    theme(plot.margin = margin(5,5,5,5),
          plot.subtitle = element_text(size=8, color="grey40"),
          plot.caption = element_text(size = 8, hjust = 0),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA))
  
}

(znets[[3]] + znets[[5]] + znets[[7]]) /
  (znets[[6]] + znets[[8]] + plot_spacer()) /
  (znets[[4]] + znets[[2]] + znets[[1]]) +
  plot_layout(guides = "collect")

(embs[[3]] + embs[[5]] + embs[[7]]) /
  (embs[[6]] + embs[[8]] + plot_spacer()) /
  (embs[[4]] + embs[[2]] + embs[[1]]) +
  plot_layout(guides = "collect")




