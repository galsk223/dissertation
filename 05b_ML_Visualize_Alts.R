o1 <- read_rds("~/westcoast-networks/data/clean/Simulation/projectiondataforML_preprocess_round2_1.rds")

tpde <- expand_grid(Projection = 1:3,
                    Data = 1:4,
                    Engine = 1:3)

j <- 1
sum_long <- map_dfr(1:nrow(tpde), function(j){
  
  temp <- pout[[tpde$Projection[[j]]]][[tpde$Data[[j]]]][[tpde$Engine[[j]]]]$sum
  
})

regression_comp <- sum_long %>% 
  filter(Type == "Regression") %>% 
  mutate(ModelPerformance = 1/ModelPerformance,
         Engine = case_when(Engine == "NNET" ~ "Neural Net",
                            Engine == "GLMNET" ~ "Lasso",
                            Engine == "RANGER" ~ "Random Forest"))

regression_comp$DataSubset <- factor(regression_comp$DataSubset, 
                                     levels = c("NoNetwork", "All", "RegionNetwork", "FisheryNode"))

ggplot(regression_comp, aes(x = Engine, y = ModelPerformance)) +
  geom_point() +
  facet_grid(rows = vars(Projection),
             cols = vars(DataSubset)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Regression Model Comparisons",
       subtitle = "Performance in 1/RMSE")

j <- 38
vi_long <- map_dfr(1:nrow(tpde), function(j){
  
  temp_info <- pout[[tpde$Projection[[j]]]][[tpde$Data[[j]]]][[tpde$Engine[[j]]]]$sum
  # if(j <= 36){
  #   temp <- pout[[tpde$Projection[[j]]]][[tpde$Data[[j]]]][[tpde$Engine[[j]]]]$vi %>% 
  #     mutate(Projection = temp_info$Projection,
  #            DataSubset = temp_info$DataSubset,
  #            Type = temp_info$Type,
  #            Engine = temp_info$Engine,
  #            ModelPerformance = temp_info$ModelPerformance)
  # } else {
    temp_pre <- pout[[tpde$Projection[[j]]]][[tpde$Data[[j]]]][[tpde$Engine[[j]]]]$vi
    temp <- tibble(Variable = names(temp_pre),
                   Importance = temp_pre) %>% 
      mutate(Projection = temp_info$Projection,
             DataSubset = temp_info$DataSubset,
             Type = temp_info$Type,
             Engine = temp_info$Engine,
             ModelPerformance = temp_info$ModelPerformance)
  # }
  
})

vi_long$DataSubset <- factor(vi_long$DataSubset, 
                             levels = c("NoNetwork", "All", "RegionNetwork", "FisheryNode"))

vi_long$Projection <- factor(vi_long$Projection, 
                             levels = c("Extensive", "Fuller", "Bipartite"))

vip_r <- vi_long %>%
  mutate(ModelPerformance = ifelse(Type == "Regression",
                                   1/ModelPerformance, ModelPerformance),
         DataSubset = paste(DataSubset,round(ModelPerformance,2))) %>% 
  group_by(DataSubset, Type, Engine, Projection) %>%
  slice_max(order_by = Importance, n = 5, with_ties = FALSE) %>%
  ungroup() %>% 
  filter(Type == "Regression",
         Engine == "RANGER") 

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
  theme_minimal()

vip_cf <- vip_c %>%
  filter(Projection == "Fuller")

c2 <- ggplot(vip_cf, aes(x = reorder_within(Variable, Importance, DataSubset), y = Importance)) +
  geom_col() +
  coord_flip() +
  facet_grid(rows = vars(DataSubset), scales = "free_y") +
  scale_x_reordered() +
  labs(title = "Fuller",
       x = "", y = "Importance") +
  theme_minimal()

vip_cb <- vip_c %>%
  filter(Projection == "Bipartite")

c3 <- ggplot(vip_cb, aes(x = reorder_within(Variable, Importance, DataSubset), y = Importance)) +
  geom_col() +
  coord_flip() +
  facet_grid(rows = vars(DataSubset), scales = "free_y") +
  scale_x_reordered() +
  labs(title = "Bipartite",
       x = "", y = "") +
  theme_minimal()

library(patchwork)
c1+c2+c3+plot_annotation(title = "Classification Random Forest Comparisons",
                         subtitle = "Variable Importance by Projection and DataSubset")

