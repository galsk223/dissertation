rm(list = ls())
library(fixest)
library(tidyverse)

projections <- read_rds("~/westcoast-networks/data/clean/Simulation/projectiondataforML.rds")
p1 <- projections[[1]]
p2 <- projections[[2]]
p3 <- projections[[3]]
projections$Bipartite <- projections$Bipartite %>% 
  mutate(FragmentationCentrality = FragmentationCentrality_Total,
         H = H2)

targets <- c("Modularity", "NetworkCentralization", 
             "ClusteringCoefficient_Global", "Mean_Weight", "N_Edges", "FragMeasure", 
             "Closeness", "Strength", "ClusteringCoefficient","FragmentationCentrality")

df_name <- names(projections)[[1]]
target <- targets[[2]]
results <- map_dfr(
  names(projections), 
  function(df_name) {
    map_dfr(
      targets,
      function(target) {
        print(target)
        
        model <- feols(as.formula(paste("log(", target, ") ~ FishCosts + SwitchCosts")), 
                       data = projections[[df_name]])
        
        return <- tidy(model) %>%
          select(term, estimate, std.error, p.value) %>%
          mutate(dataset = df_name, target = target)
      }
    )
  }
)

out <- results %>% 
  mutate(LB = estimate - 1.95*std.error,
         UB = estimate + 1.95*std.error,
         Color = ifelse(estimate > 0, "Pos", "Neg"),
         Sig = case_when(p.value < .1 ~ ".01",
                         p.value < .05 ~ ".05",
                         p.value < .01 ~ ".1"
                         )) %>% 
  filter(term == "FishCosts",
         !(dataset == "Bipartite" & target == "N_Edges"))

network <- out %>% 
  filter(!target %in% c("Closeness", "Strength", "ClusteringCoefficient","FragmentationCentrality"))

ggplot(network, aes(x = fct_reorder(target, estimate), 
                    y = estimate, color = dataset)) +
  geom_hline(yintercept = 0) +
  # geom_col(position = "dodge") +
  # geom_point(aes(color = Color), size = 2) +
  geom_errorbar(aes(ymin = LB, ymax = UB), #50808E
                linewidth = 1, position = "dodge") +
  # facet_wrap(vars(dataset))  +
  ggthemes::theme_tufte() +
  scale_color_manual(values = c("#50808E", "#90F3FF", "#EB9486")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Impact of Increased Fishing Costs on (logged) Network Measures",
       subtitle = "Network Level Measures, by Projection",
       color = "",
       x = "",
       y = "")

node <- out %>% 
  filter(target %in% c("Closeness", "Strength", "ClusteringCoefficient","FragmentationCentrality"))

ggplot(node, aes(x = fct_reorder(target, estimate), 
                    y = estimate, color = dataset)) +
  geom_hline(yintercept = 0) +
  # geom_col(position = "dodge") +
  # geom_point(aes(color = Color), size = 2) +
  geom_errorbar(aes(ymin = LB, ymax = UB), 
                linewidth = 1, position = "dodge") +
  # facet_wrap(vars(dataset))  +
  ggthemes::theme_tufte() +
  scale_color_manual(values = c("#50808E", "#90F3FF", "#EB9486")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Impact of Increased Fishing Costs on (logged) Network Measures",
       subtitle = "Node Level Measures of the Crab Fishery, by Projection",
       color = "",
       x = "",
       y = "")

