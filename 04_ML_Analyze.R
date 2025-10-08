library(tidyverse)
library(zoo)
library(tidymodels)
library(themis)       
library(vip) 
library(shapviz)
library(kernelshap)
library(ranger)
library(xgboost)
library(earth)
library(factoextra)
library(fastshap)
library(NeuralNetTools)
library(tidytext)
library(brulee)

rm(list = ls())
source("~/westcoast-networks/scripts/02_Simulation/0_Port_Regions/03a_MLSizeCompile_PlusPlots.R")
list.files("westcoast-networks/scripts/02_Simulation/0_Port_Regions/04_ML_Functions/", 
           full.names = TRUE) %>%
  walk(source)

df_net_e <- map_dfr(df_crab, function(i){i$network_pre_e}) %>% 
  filter(Fishery == "CA Dungeness Crab") %>% 
  mutate(Recover = ifelse(Return < 35, "Recovered", "New Steady State")) 

df_net_f <- map_dfr(df_crab, function(i){i$network_pre_f}) %>% 
  filter(Fishery == "CA Dungeness Crab") %>% 
  mutate(Recover = ifelse(Return < 35, "Recovered", "New Steady State"))

df_net_b <- map_dfr(df_crab, function(i){i$network_pre_b}) %>% 
  filter(Fishery == "CA Dungeness Crab") %>% 
  mutate(Recover = ifelse(Return < 35, "Recovered", "New Steady State"),
         UniqueVessels = df_net_f$UniqueVessels)

df_net_b_slim <- df_net_b %>% 
  filter(!is.na(Closeness),
         !is.na(ClusteringCoefficient))
df_net_e_slim <- df_net_e %>% 
  filter(Iteration %in% df_net_b_slim$Iteration)
df_net_f_slim <- df_net_f %>% 
  filter(!is.na(Closeness),
         !is.na(ClusteringCoefficient),
         Iteration %in% df_net_b_slim$Iteration)

projections <- list(Extensive = df_net_e_slim, 
                    Fuller = df_net_f_slim, 
                    Bipartite = df_net_b_slim)

write_rds(projections, "~/westcoast-networks/data/clean/Simulation/projectiondataforML.rds")
projections <- read_rds("~/westcoast-networks/data/clean/Simulation/projectiondataforML.rds")

type <- c("Class", "Reg")
type <- c("Reg")
t <- type[1]
p <- 3
# check_b <- dfassemble(projections[[3]], t)$NoNetwork
# check_f <- dfassemble(projections[[2]], t)$NoNetwork
# identical(check_b, check_f)

longout <- map(type, function(t){
  print(t)
  
  pout <- map(1:length(projections), function(p){
    
    pname <- names(projections[p])
    print(pname)
    
    df_subdata <- dfassemble(projections[[p]], t)
    
    d <- 1
    if(t == "Class"){
      df_typeprojsub <- out_class(df_subdata, models_c, pname)
    } else if(t == "Reg") {
      df_typeprojsub <- out_reg(df_subdata, models_r, pname)
    }
    
    return(df_typeprojsub)
    
  })
  
  return(pout)
  
})

# test_bipartite <- df_typeprojsub
# test_fuller <- df_typeprojsub

write_rds(longout, "~/westcoast-networks/data/clean/Simulation/projectiondataML_longout.rds")

pout_class <- pout


# vi_long <- results %>%
#   mutate(Model = case_when(model == "lasso" ~ "Lasso",
#                            model == "rf" ~ "Random Forest",
#                            model == "nn" ~ "Neural Network"))
# # ,
# #          # Features = case_when(feature_set == "all" ~ "All Attributes",
# #          #                      feature_set == "onlynetwork" ~ "Only Network Measures",
# #          #                      feature_set == "nonetwork" ~ "No Network Measures"),
# #          model_fset = paste0(Model, " (", Features, "): ",round(ss,2))) 
# 
# ggplot(vi_long, aes(x = reorder_within(Variable, Importance, Model), y = Importance)) +
#   geom_col() +
#   coord_flip() +
#   facet_wrap(~ Model, scales = "free_y", dir = "v") +
#   labs(title = "Model Accuracy & Variable Importance",
#        x = "Variable", y = "Importance") +
#   theme_minimal()
# 


