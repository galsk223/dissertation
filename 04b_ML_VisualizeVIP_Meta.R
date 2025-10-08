resultfolder <- "MLResults_Meta3"
fl <- list.files(path = paste0("~/westcoast-networks/data/clean/Simulation/",resultfolder),
                 full.names = T)
flr <- fl[!str_detect(fl, "NoNet")]


f <- 1
d <- 1
results_vip <- map_dfr(1:length(flr),function(f){
  
  r <- flr[[f]]
  t <- read_rds(r)
  de <- expand_grid(Data = 1:length(t),
                    Engine = 1:3)
  
  sum1 <- map_dfr(1:nrow(de), function(d){
    
    # if(f == 24 & d == 4)
    # print(d)
    id <- t[[de$Data[d]]][[de$Engine[[d]]]]$sum
    if(is.null(id)){return(NULL)}
    vi <- t[[de$Data[d]]][[de$Engine[[d]]]]$vi
    if (is.null(vi)) return(NULL)
    
    if(unique(id$Type) == "Regression"){
      temp <- enframe(vi,name = "Variable",value = "Importance") %>%
        bind_cols(id)
    } else {
      temp <- vi %>%
        bind_cols(id) %>%
        mutate(Variable = as.character(Variable))
    } 
    
    if(nrow(temp) == 1) return(NULL) else temp
    
  }) %>%
    mutate(series = str_sub(r,76,76),
           Dep = str_sub(r,90,-5))
  
})

vi_long <- results_vip %>%   
  filter(Type == "Regression") %>% 
  inner_join(bestresults 
             # %>% 
             #   filter(Dep == "Reg")
             , by = c("Projection", "Type", "series", "Dep", "DataSubset" = "DataSubset.x")) %>% 
  filter(ModelPerformance < Baseline, #CHECK THIS
         !Variable %in% c("SS_Vessels", "SS_Revenue", "SS_Weeks", "N_Fisheries",
                          "SS_Drop_Vessels", "SS_Drop_Revenue", "SS_Drop_Weeks")) %>% 
  mutate(DataSubset = str_remove(DataSubset,"_Drop"),
         Engine = case_when(Engine.x == "NNET" ~ "Neural Net",
                            Engine.x == "GLMNET" ~ "Lasso",
                            Engine.x == "RANGER" ~ "Random Forest"),
         Projection = case_when(Projection == "network_pre_b" ~ "Bipartite",
                                Projection == "network_pre_e" ~ "Fishery Network (Co-Participation)",
                                Projection == "network_pre_ev" ~ "Vessel Network (Co-Fished)",
                                Projection == "network_pre_f" ~ "Fishery Network (Sum Product)",
                                Projection == "network_pre_fv" ~ "Vessel Network (Sum Product)",
                                Projection == "network_pre_tc" ~ "Fishery Network (Catch Correlation)",
                                Projection == "network_pre_tt" ~ "Fishery Network (Sequential Switching)")) %>% 
  # filter(series == s,
  #        Dep == d,
  #        Projection == bettername,
  #        DataSubset == "All") %>% 
  group_by(DataSubset, series, Projection, Dep) %>%
  slice_max(order_by = Importance, n = 5, with_ties = FALSE) %>%
  ungroup() 
write_rds(vi_long, paste0("~/westcoast-networks/output/Simulation/",resultfolder,"/vi_long.rds"))

colorlist <- c("#4c6085","#39a0ed","#13c4a3","#f7b32b","#C09BD8","#EC0868","#52050A",
               "#fe5f55","#161613","#A44A3F")

s <- 6
j <- 1
pimp <- list()
whole <- list()
position <- list()
for(s in unique(vi_long$series)){
  
  sact <- vi_long %>% 
    filter(series == s) %>% 
    distinct(Projection, DataSubset) %>% 
    filter(!is.na(DataSubset))
  
  pimp <- list()
  position <- list()
  
  for(j in 1:nrow(sact)){
    
    print(j)
    splot <- vi_long %>% 
      filter(series == s,
             Projection == sact$Projection[[j]],
             DataSubset == sact$DataSubset[[j]]) %>% 
      mutate(Variable = fct_reorder(Variable, Importance),
             VarIndex = as.numeric(factor(Variable, levels = unique(Variable)))) 
    
    splot$DataSubset <- factor(splot$DataSubset,
                                 levels = c("All", "Network", "FisheryNode"))
    splot$Projection <- factor(splot$Projection, 
                                 levels = c("Fishery Network (Co-Participation)", 
                                            "Fishery Network (Sum Product)", 
                                            "Fishery Network (Catch Correlation)", 
                                            "Fishery Network (Sequential Switching)",
                                            "Vessel Network (Co-Fished)",
                                            "Vessel Network (Sum Product)",
                                            "Bipartite"))
    
    pimp[[j]] <- ggplot(splot, aes(x = Importance, 
                      y = Variable, 
                      fill = Projection)) +
      geom_col(fill = colorlist[as.numeric(splot$Projection)]) +
      labs(x = "", 
           y = "") +
      theme_minimal() 
    
    rank <- vi_long %>% 
      filter(series == s,
             DataSubset == sact$DataSubset[[j]]) %>% 
      distinct(Projection, ModelPerformance) %>% 
      group_by(Projection) %>% 
      filter(ModelPerformance == min(ModelPerformance)) %>% 
      ungroup() %>% 
      arrange((ModelPerformance)) %>%
      mutate(RowNumber = row_number()) 
    
    position[[j]] <- list(
      col = dplyr::case_match(
        unique(splot$DataSubset),
        "All" ~ 1,
        "Network" ~ 2,
        "FisheryNode" ~ 3
      ),
      row = rank$RowNumber[rank$Projection == sact$Projection[[j]]]
    )
    
  } 
  
  library(patchwork)
  
  design <- c()  # start with empty patch_area_list
  
  for (j in seq_along(position)) {
    design <- c(
      design,
      area(
        t = position[[j]]$row,
        l = position[[j]]$col,
        b = position[[j]]$row,
        r = position[[j]]$col
      )
    )
  }
  
  # Turn each entry in position into a string like area(t = 1, l = 7, b = 1, r = 7)
  out <- vapply(
    position,
    function(pos) {
      sprintf("area(t = %d, l = %d, b = %d, r = %d)",
              pos$row, pos$col, pos$row, pos$col)
    },
    character(1)
  )
  
  # Wrap them in a c( ... ) call
  layout_code <- paste0("c(\n  ", paste(out, collapse = ",\n  "), "\n)")
  layout <- eval(parse(text = layout_code))
  layout
  
  bottom <- wrap_plots(pimp)+
    plot_layout(design = layout)
  
  whole[[as.numeric(s)]] <- preg[[as.numeric(s)]] / bottom +
    plot_layout(heights = c(1,3))
  
}








ggplot(splot, aes(y = VarIndex, fill = Projection)) +
  geom_tile(aes(x = Importance / 2,    # midpoint of bar
                width = Importance,    # bar length
                height = PerfScale * 0.8), 
            alpha = 0.8) +
  scale_y_continuous(breaks = seq_along(levels(splot$Variable)),
                     labels = levels(splot$Variable)) +
  labs(x = "Importance", 
       y = "Variable", 
       title = "Importance (bar length) vs Model Performance (bar height)") +
  theme_minimal()


splot <- vi_long %>% 
  filter(series == s) %>% 
  mutate(Variable = fct_reorder(Variable, Importance)) %>% 
  mutate(VarIndex = as.numeric(factor(Variable, levels = unique(Variable))))

ggplot(splot, aes(y = VarIndex, fill = Projection)) +
  geom_rect(aes(xmin = 0, 
                xmax = Importance, 
                ymin = VarIndex - 0.4, 
                ymax = VarIndex - 0.4 + ((1/ModelPerformance) / Baseline) * 0.8)) +
  scale_y_continuous(breaks = seq_along(levels(splot$Variable)),
                     labels = levels(splot$Variable)) +
  labs(x = "Importance", 
       y = "Variable", 
       title = "Importance (bar length) vs Model Performance (bar height)") +
  theme_minimal()


ggplot(vi_long, aes(x = Variable, y = Importance)) +
  geom_col() +
  coord_flip() + 
  facet_grid(cols = vars(Projection),
             rows = vars(DataSubset))

ggplot(vi_long, aes(y = Variable)) +
  geom_rect(aes(xmin = 0, 
                xmax = Importance, 
                ymin = as.numeric(Variable) - 0.4, 
                ymax = as.numeric(Variable) - 0.4 + ((1/ModelPerformance) / Baseline) * 0.8)) +
  scale_y_continuous(breaks = seq_along(unique(vi_long$Variable)), 
                     labels = unique(vi_long$Variable)) +
  labs(x = "Importance", 
       y = "Variable", 
       title = "Importance (bar length) vs Model Performance (bar height)")





vip_r <- vi_long %>% 
  filter(Type == "Regression") %>% 
  group_by(DataSubset, Type, Engine, series, Projection) %>%
  slice_max(order_by = Importance, n = 5, with_ties = FALSE) %>%
  ungroup() 

r1 <- vip_r %>% 
  filter(series == 6, 
         DataSubset == "RegionNetwork")

vip_re <- vip_r %>%
  filter(Projection == "Extensive")

r1 <- ggplot(vip_re, aes(x = reorder_within(Variable, Importance, DataSubset), y = Importance)) +
  geom_col() +
  coord_flip() +
  facet_grid(rows = vars(DataSubset), scales = "free_y") +
  scale_x_reordered() +
  labs(title = "Extensive",
       x = "Variable", y = "") +
  theme_minimal()

vip_rf <- vip_r %>%
  filter(Projection == "Fuller")

r2 <- ggplot(vip_rf, aes(x = reorder_within(Variable, Importance, DataSubset), y = Importance)) +
  geom_col() +
  coord_flip() +
  facet_grid(rows = vars(DataSubset), scales = "free_y") +
  scale_x_reordered() +
  labs(title = "Fuller",
       x = "", y = "Importance") +
  theme_minimal()

vip_rb <- vip_r %>%
  filter(Projection == "Bipartite")

r3 <- ggplot(vip_rb, aes(x = reorder_within(Variable, Importance, DataSubset), y = Importance)) +
  geom_col() +
  coord_flip() +
  facet_grid(rows = vars(DataSubset), scales = "free_y") +
  scale_x_reordered() +
  labs(title = "Bipartite",
       x = "", y = "") +
  theme_minimal()

library(patchwork)
r1+r2+r3+plot_annotation(title = "Regression Random Forest Comparisons",
                         subtitle = "Variable Importance by Projection and DataSubset")

vip_c <- vi_long %>%
  mutate(ModelPerformance = ifelse(Type == "Regression",
                                   1/ModelPerformance, ModelPerformance),
         DataSubset = paste(DataSubset,round(ModelPerformance,2))) %>% 
  group_by(DataSubset, Type, Engine, Projection) %>%
  slice_max(order_by = Importance, n = 5, with_ties = FALSE) %>%
  ungroup() %>% 
  filter(Type == "Classification",
         Engine == "RANGER") 

vip_ce <- vip_c %>%
  filter(Projection == "Extensive")

c1 <- ggplot(vip_ce, aes(x = reorder_within(Variable, Importance, DataSubset), y = Importance)) +
  geom_col() +
  coord_flip() +
  facet_grid(rows = vars(DataSubset), scales = "free_y") +
  scale_x_reordered() +
  labs(title = "Extensive",
       x = "Variable", y = "") +
  theme_tufte()

vip_cf <- vip_c %>%
  filter(Projection == "Fuller")

c2 <- ggplot(vip_cf, aes(x = reorder_within(Variable, Importance, DataSubset), y = Importance)) +
  geom_col() +
  coord_flip() +
  facet_grid(rows = vars(DataSubset), scales = "free_y") +
  scale_x_reordered() +
  labs(title = "Fuller",
       x = "", y = "Variable Importance in Predicting if the System Recovered") +
  theme_tufte()

vip_cb <- vip_c %>%
  filter(Projection == "Bipartite")

c3 <- ggplot(vip_cb, aes(x = reorder_within(Variable, Importance, DataSubset), y = Importance)) +
  geom_col() +
  coord_flip() +
  facet_grid(rows = vars(DataSubset), scales = "free_y") +
  scale_x_reordered() +
  labs(title = "Bipartite",
       x = "", y = "") +
  theme_tufte()

library(patchwork)
call <- c1+c2+c3+plot_annotation(title = "Classification Random Forest Comparisons",
                                 subtitle = "Variable Importance by Projection and DataSubset") & 
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )
ggsave(plot = call, paste0("westcoast-networks/output/Simulation/classificationVIcomps.png"),
       width = 12, height = 8, bg = "transparent")