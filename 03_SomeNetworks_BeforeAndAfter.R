color_list <- c("#ad2c1d", "#c3c3ff", "#edbf82", '#4b5468', "#397662", 
                "#fe3f8e", "#140f15", "pink", "#2c0c1d", "orange")

df_before <- sim_run$cache_dfchoice[[2]]
df_after <- sim_run$cache_dfchoice[[5]]

edgefunction <- function(df){
  fisheries <- unique(df$FISHERY_ID)
  fishery_pairs <- expand.grid(Fishery1 = fisheries, Fishery2 = fisheries, stringsAsFactors = FALSE) %>%
    filter(Fishery1 != Fishery2) %>% 
    arrange(Fishery1, Fishery2) 
  
  nv <- n_distinct(df$Vessel_ID)
  e <- df %>% 
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
    mutate(UniqueVessels = replace_na(UniqueVessels, 0))
}

edges_before <- edgefunction(df_before) %>% 
  filter(Fishery1 != "Not Fishing",
         Fishery2 != "Not Fishing")
edges_after <- edgefunction(df_after) %>% 
  filter(Fishery1 != "Not Fishing",
         Fishery2 != "Not Fishing")


# Network Statistics ------------------------------------------------------
df <- df_before
networkplotfunction(df_before)
networkplotfunction(df_after)

networkplotfunction <- function(df){
  vy <- df %>% 
    group_by(FISHERY_ID) %>% 
    summarise(UniqueVessels = n_distinct(Vessel_ID),
              .groups = "drop") %>% 
    filter(FISHERY_ID != "Not Fishing") %>%
    select(FISHERY_ID, UniqueVessels)
  
  ey <- edgefunction(df) %>% 
    filter(Fishery1 != "Not Fishing",
           Fishery2 != "Not Fishing",
           Fishery1 %in% vy$FISHERY_ID,
           Fishery2 %in% vy$FISHERY_ID,
           UniqueVessels > 0) %>% 
    select(Fishery1, Fishery2, weight = UniqueVessels)
  
  gy <- graph_from_data_frame(ey,
                              directed = F,
                              vertices = vy)
  
  cw <- cluster_walktrap(gy, modularity = T)
  m <- modularity(cw)
  p <- vy$UniqueVessels/sum(vy$UniqueVessels)
  H <- -sum(p * log(p))
  
  mem <- as.factor(membership(cw))
  out <- set_vertex_attr(gy, "Cluster", 
                         value = mem) %>% 
    as_tbl_graph() %>%
    activate(nodes) %>%
    arrange(as.numeric(Cluster))
  
  a <- ggraph(out, layout = "linear", circular = TRUE) +
    geom_edge_link(aes(width = weight), alpha = 0.5,
                   color = color_list[4]) + # Edge color by facet
    geom_node_point(aes(size = UniqueVessels, color = Cluster)) +
    theme_void() +
    guides(size = "none",
           # guide_legend(title = "UniqueVessels"),
           color = "none",
           edge_width = "none"
           # guide_legend(title = "Weight")
    ) +
    scale_color_manual(values = color_list[5:(as.numeric(max(levels(mem)))+5)]) +
    scale_size_continuous(limits = c(0,150),
                          range = c(1,10)) +
    scale_edge_width(limits = c(0,100),
                     range = c(1,8)) +
    labs(
      # title = paste0("some title"),
         # subtitle = paste("Mean Fisheries / Vessel: ", do$Div[[i-23]],
         #                  "\nMean Weeks Fishing: ", mean(check$Weeks_Fishing),
         #                  "\n# Fisheries: ", nrow(vy)),
         caption = paste0("Modularity: ",round(m,2),
                          "\n H: ",round(H,2))) +
    theme(plot.margin = margin(5,5,5,5),
          plot.subtitle = element_text(size=8, color="grey40"),
          plot.caption = element_text(size = 12),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA))
  a
}

y1 <- networkplotfunction(sim_run$cache_dfchoice[[2]]) + 
  labs(title = "Network Pre-Shock")
y2 <- networkplotfunction(sim_run$cache_dfchoice[[3]]) + 
  labs(title = "Network During Crab Closure")
y3 <- networkplotfunction(sim_run$cache_dfchoice[[4]]) + 
  labs(title = "Network 1 Year Post-Shock")
y4 <- networkplotfunction(sim_run$cache_dfchoice[[5]]) + 
  labs(title = "Network 2 Years Post-Shock")

y1 
y2
y3
y4

ggsave(plot = y1, paste0("westcoast-networks/output/Simulation/somenetworks_preshock.png"),
       width = 6.25, height = 6.65,
       bg = "transparent")
ggsave(plot = y2, paste0("westcoast-networks/output/Simulation/somenetworks_inshock.png"),
       width = 6.25, height = 6.65,
       bg = "transparent")
ggsave(plot = y3, paste0("westcoast-networks/output/Simulation/somenetworks_postshock1.png"),
       width = 6.25, height = 6.65,
       bg = "transparent")
ggsave(plot = y4, paste0("westcoast-networks/output/Simulation/somenetworks_postshock2.png"),
       width = 6.25, height = 6.65,
       bg = "transparent")

