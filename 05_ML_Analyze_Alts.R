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
# source("~/westcoast-networks/scripts/02_Simulation/0_Port_Regions/02_Simulations.R")
# source("~/westcoast-networks/scripts/02_Simulation/0_Port_Regions/03a_MLSizeCompile_PlusPlots.R")
list.files("westcoast-networks/scripts/02_Simulation/0_Port_Regions/04_ML_Functions/", 
           full.names = TRUE) %>%
  walk(source)

randconditions <- expand_grid(skillrand = c(T,F),
                              scalerand = c(T,F),
                              costbyfishery = c(T,F),
                              shockpermanent = c(T,F)) %>% 
  rowid_to_column() %>% 
  filter(rowid %in% c(1,2,4,13,14,16))

o1 <- read_rds("~/westcoast-networks/data/clean/Simulation/projectiondataforML_preprocess_round2_1.rds")
o2 <- read_rds("~/westcoast-networks/data/clean/Simulation/projectiondataforML_preprocess_round2_2.rds")
o3 <- read_rds("~/westcoast-networks/data/clean/Simulation/projectiondataforML_preprocess_round2_3.rds")
o4 <- read_rds("~/westcoast-networks/data/clean/Simulation/projectiondataforML_preprocess_round2_4.rds")
o5 <- read_rds("~/westcoast-networks/data/clean/Simulation/projectiondataforML_preprocess_round2_5.rds")
o6 <- read_rds("~/westcoast-networks/data/clean/Simulation/projectiondataforML_preprocess_round2_6.rds")

altsims <- list(o1[[1]],o2[[1]],o3[[1]],
                o4[[1]],o5[[1]],o6[[1]])

as <- altsims[[1]]
df_net_all <- map(altsims, function(as){
  
  print("Extensive")
  df_net_e <- map_dfr(as, function(i){i$network_pre_e}) %>% 
    filter(Fishery == "CA Dungeness Crab") %>% 
    mutate(Recover = ifelse(Return < 35, "Recovered", "New Steady State"),
           PortionVessels = EndVessels / SS_Vessels,
           PortionRevenue = EndRevenue / SS_Revenue) 
  
  print("Fuller")
  df_net_f <- map_dfr(as, function(i){i$network_pre_f}) %>% 
    filter(Fishery == "CA Dungeness Crab") %>% 
    mutate(Recover = ifelse(Return < 35, "Recovered", "New Steady State"),
           PortionVessels = EndVessels / SS_Vessels,
           PortionRevenue = EndRevenue / SS_Revenue)
  
  print("Bipartite")
  df_net_b <- map_dfr(as, function(i){i$network_pre_b}) %>% 
    filter(Fishery == "CA Dungeness Crab") %>% 
    mutate(Recover = ifelse(Return < 35, "Recovered", "New Steady State"),
           PortionVessels = EndVessels / SS_Vessels,
           PortionRevenue = EndRevenue / SS_Revenue,
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
  
  out <- list(Simulation = names(as),
             Extensive = df_net_e_slim, 
             Fuller = df_net_f_slim, 
             Bipartite = df_net_b_slim)
  
})
df_net_all
write_rds(df_net_all, "~/westcoast-networks/data/clean/Simulation/projectiondataforML_alts2.rds")
df_net_all <- read_rds("~/westcoast-networks/data/clean/Simulation/projectiondataforML_alts2.rds")

# check_b <- dfassembleportion(df_net_all$`no stochastic, single year shock`$Bipartite)
# check_f <- dfassembleportion(projections[[2]], t)$NoNetwork
# identical(check_b, check_f)
i <- 3
p <- 2
longout <- map(1:length(altsims), function(i){ # each simulation
  
  projections <- df_net_all[[i]]
  names(altsims)[i]
  
  pout <- map(2:4, function(p){ # each projection
    
    pname <- names(projections[p])
    print(pname)
    
    df_subdata <- dfassembleportion(projections[[p]])
    df_typeprojsub <- out_portion(df_subdata, models_r, pname)
    
    return(df_typeprojsub)
    
  })
  write_rds(pout, "~/westcoast-networks/data/clean/Simulation/projectiondataML_longout_allnewstoch.rds")
  return(pout)
  
})


write_rds(longout, "~/westcoast-networks/data/clean/Simulation/projectiondataML_longout_alts2.rds")








