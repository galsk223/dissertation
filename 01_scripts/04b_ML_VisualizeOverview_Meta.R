resultfolder <- "MLResults_Meta3"
fl <- list.files(path = paste0("~/westcoast-networks/data/clean/Simulation/",resultfolder),
                 full.names = T)

f <- 1
d <- 1


# nonetwork  ----------------------------------------------------------


flnn <- fl[str_detect(fl, "NoNet")] 
f <- 1
d <- 1
results_sum_nonet1 <- map_dfr(1:length(flnn),function(f){
  
  r <- flnn[[f]]
  t <- read_rds(r)
  de <- expand_grid(Data = length(t),
                    Engine = 1:3)
  
  sum <- map_dfr(1:nrow(de), function(d){
    
    temp <- t[[de$Data[[d]]]][[de$Engine[[d]]]]$sum
    
  }) %>% 
    mutate(series = str_sub(r,76,76),
           Dep = str_sub(r,84,-5)
           ,
           BestPerformance = ifelse(Type == "Regression", 1/ModelPerformance, ModelPerformance)
           ) %>%
    group_by(DataSubset) %>%
    filter(BestPerformance == max(BestPerformance))
  
}) %>% 
  mutate(Grouping = ifelse(str_detect(DataSubset, "Drop"), 
                           "w/ Dropped Fishery Level Measures", ""))
  



# plot r & c --------------------------------------------------------------


flr <- fl[!str_detect(fl, "NoNet")]
results_sum <- map_dfr(1:length(flr),function(f){
  
  r <- flr[[f]]
  t <- read_rds(r)
  de <- expand_grid(Data = 1:length(t),
                    Engine = 1:3)
  
  sum <- map_dfr(1:nrow(de), function(d){
    
    temp <- t[[de$Data[d]]][[de$Engine[[d]]]]$sum
    
  }) %>% 
    mutate(series = str_sub(r,76,76),
           Dep = str_sub(r,90,-5))
  
  }) 
# %>%
#   filter(DataSubset != "NoNetwork")

colorlist <- c("#4c6085","#39a0ed","#13c4a3","#f7b32b","#C09BD8","#EC0868","#52050A",
               "#fe5f55","#161613","#A44A3F")

setup <- results_sum %>% 
  mutate(Grouping = ifelse(str_detect(DataSubset, "Drop") |
                             DataSubset == "FisheryNode", "w/ Dropped Fishery Level Measures", ""),
         DataSubset = str_remove(DataSubset,"_Drop"),
         Engine = case_when(Engine == "NNET" ~ "Neural Net",
                            Engine == "GLMNET" ~ "Lasso",
                            Engine == "RANGER" ~ "Random Forest"),
         Projection = case_when(Projection == "network_pre_b" ~ "Bipartite",
                                Projection == "network_pre_e" ~ "Fishery Network (Co-Participation)",
                                Projection == "network_pre_ev" ~ "Vessel Network (Co-Fished)",
                                Projection == "network_pre_f" ~ "Fishery Network (Sum Product)",
                                Projection == "network_pre_fv" ~ "Vessel Network (Sum Product)",
                                Projection == "network_pre_tc" ~ "Fishery Network (Catch Correlation)",
                                Projection == "network_pre_tt" ~ "Fishery Network (Sequential Switching)"))

setup$DataSubset <- factor(setup$DataSubset, 
                                     levels = c("All", "Network", "FisheryNode"))
setup$Projection <- factor(setup$Projection, 
                                     levels = c("Fishery Network (Co-Participation)", 
                                                "Fishery Network (Sum Product)", 
                                                "Fishery Network (Catch Correlation)", 
                                                "Fishery Network (Sequential Switching)",
                                                "Vessel Network (Co-Fished)",
                                                "Vessel Network (Sum Product)",
                                                "Bipartite"))

outcomes <- c("Return", "Vessels5", "Vessels10",
              "Revenue5", "Revenue10")
# outcomes <- c("Reg", "V5", "V10")

o <- 2
i <- 1
g <- 1
groups <- c("","w/ Dropped Fishery Level Measures")
preg <- list()
for(i in 1:6){
    
    pi <- setup %>% 
      filter(Type == "Regression",
             Dep == outcomes[[o]],
             series == i,
             Grouping == "w/ Dropped Fishery Level Measures") %>% 
      mutate(UpPerformance = 1/ModelPerformance) %>%
      group_by(DataSubset, Projection, series) %>%
      summarise(LowRMSE = min(ModelPerformance),
                BestPerformance = max(UpPerformance),
                BestEngine = Engine[which.max(BestPerformance)], .groups = "drop") 
    
    nonet <- results_sum_nonet1 %>% 
      filter(Type == "Regression",
             Dep == outcomes[[o]],
             series == i,
             Grouping == "w/ Dropped Fishery Level Measures")
    
    preg[[i]] <- ggplot(pi, aes(x = DataSubset, y = BestPerformance, 
                                color = Projection, shape = BestEngine)) +
      geom_hline(yintercept = nonet$BestPerformance, color = "grey") +    
      geom_point(size = 4.5) +
      scale_color_manual(values = colorlist[1:7]) +
      # scale_y_continuous(limits = c(.15, .3)) +
      # facet_wrap(vars(Projection)) +
      # facet_grid(rows = vars(Projection),
      #            cols = vars(DataSubset)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Regression Model Comparisons",
           subtitle = paste(unique(pi$series), outcomes[[o]]),
           caption = paste("Performance in 1/RMSE 
           \nThe best network measure is",
                           -round(min(pi$LowRMSE)-nonet$ModelPerformance,4)*100,"years more accurate"))
  
}

i <- 6
pclass <- list()
for(i in 1:6){
  
  pi <- setup %>% 
    filter(Type == "Classification",
           series == i,
           Grouping == "w/ Dropped Fishery Level Measures") %>% 
    group_by(DataSubset, Projection, series) %>% 
    summarise(BestPerformance = max(ModelPerformance),
              BestEngine = Engine[which.max(ModelPerformance)], .groups = "drop")
  
  nonet <- results_sum_nonet1 %>% 
    filter(Type == "Classification",
           series == i,
           Grouping == "w/ Dropped Fishery Level Measures")
  
  pclass[[i]] <- ggplot(pi, aes(x = DataSubset, y = BestPerformance, 
                              color = Projection, shape = BestEngine)) +
  geom_hline(yintercept = nonet$ModelPerformance, color = "grey") +
    geom_point(size = 4) +
    scale_color_manual(values = colorlist[1:7]) +
    # scale_y_continuous(limits = c(.5, 1)) +
    # facet_wrap(vars(Projection)) +
    # facet_grid(rows = vars(Projection),
    #            cols = vars(DataSubset)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Classification Model Comparisons",
         subtitle = unique(pi$series),
         caption = "Performance in Accuracy")
  
}


# Best Results ------------------------------------------------------------

bestresults <- results_sum %>% 
  filter(Type == "Regression") %>% 
  mutate(RMSE_Inv = ifelse(Type == "Regression", 1/ModelPerformance, ModelPerformance)) %>% 
  group_by(Projection, DataSubset, Type, series, Dep) %>% 
  reframe(
    # LowestRMSE = pmin(ModelPerformance),
    #         BestLowest = Engine[which.min(LowestRMSE)],
            BestPerformance = max(RMSE_Inv),
            BestEngine = Engine[which.max(RMSE_Inv)]) %>%
  distinct() %>% 
  left_join(results_sum_nonet1 %>% 
              filter(DataSubset == "NoNetwork_Drop") %>% 
              select(-c(Projection,Grouping)) %>% 
              rename(Baseline = ModelPerformance), by = c("Type", "Dep", "series")) %>% 
  filter(BestPerformance.x > BestPerformance.y,
         DataSubset.x %in% c("All_Drop", "Network_Drop", "FisheryNode")) 

dir.create(paste0("~/westcoast-networks/output/Simulation/",resultfolder))
write_rds(bestresults, paste0("~/westcoast-networks/output/Simulation/",resultfolder,"/bestresults.rds"))

plotbest <- bestresults %>% 
  mutate(Projection = case_when(Projection == "network_pre_b" ~ "Bipartite",
                       Projection == "network_pre_e" ~ "Fishery Network (Co-Participation)",
                       Projection == "network_pre_ev" ~ "Vessel Network (Co-Fished)",
                       Projection == "network_pre_f" ~ "Fishery Network (Sum Product)",
                       Projection == "network_pre_fv" ~ "Vessel Network (Sum Product)",
                       Projection == "network_pre_tc" ~ "Fishery Network (Catch Correlation)",
                       Projection == "network_pre_tt" ~ "Fishery Network (Sequential Switching)"))

plotbest$Projection <- factor(plotbest$Projection, 
                           levels = c("Fishery Network (Co-Participation)", 
                                      "Fishery Network (Sum Product)", 
                                      "Fishery Network (Catch Correlation)", 
                                      "Fishery Network (Sequential Switching)",
                                      "Vessel Network (Co-Fished)",
                                      "Vessel Network (Sum Product)",
                                      "Bipartite"))

lims_df <- plotbest %>%
  group_by(Dep) %>%
  summarise(
    lim_min = min(c(BestPerformance.x, BestPerformance.y), na.rm = TRUE),
    lim_max = max(c(BestPerformance.x, BestPerformance.y), na.rm = TRUE)
  )

plotbest2 <- plotbest %>%
  left_join(lims_df, by = "Dep")

ggplot(plotbest2, aes(x = BestPerformance.y, y = BestPerformance.x,
                        shape = series, color = Projection)) +
  facet_wrap(vars(Dep), scales = "free") +
  geom_point(size = 4) +
  geom_abline(intercept = 0, slope = 1, color = "grey", data = NULL) +
  geom_blank(aes(x = lim_min, y = lim_min)) +
  geom_blank(aes(x = lim_max, y = lim_max)) +
  scale_color_manual(values = colorlist[1:7]) +
  theme_minimal() 
