rm(list = ls())
library(tidyverse)
library(data.table)
library(evd)
library(igraph)
library(DirectedClustering)
library(keyplayer)
library(bipartite)
library(tnet)
library(statnet)
library(furrr)
source("westcoast-networks/scripts/02_Simulation/0_Port_Regions/01_Choices_Function_NYears.R")

# skillrand <- T
# scalerand <- T
# costbyfishery <- T
# shockpermanent <- F
# closureadjust <- T
randconditions <- expand_grid(skillrand = c(T,F),
                              scalerand = c(T,F),
                              costbyfishery = c(T,F),
                              shockpermanent = c(T,F)) %>% 
  rowid_to_column() %>% 
  filter(rowid %in% c(1,2,4,13,14,16))

base_name <- "~/westcoast-networks/data/Simulation/DynamicSimulationOutcomes/MLSizeCacheRound3_"
numbers <- 1:nrow(randconditions)  
for (i in numbers) {
  dir.create(file.path(paste0(base_name, i)))
}

# %>% 
#   filter(rowid %in% c(1,2,15))
# already have 16
# 0 = all false
# 1 = all true
# 2 = stochastic true, single year shock
# 3 = no stochastic with permanent shock
# 4 = stochastic cost scaling and skill, costs not by fishery, single year shock

regions <- c("Eureka", "Monterey")
year_ref <- 2017
vessels_in <- 200
scale_t1ev <- 500

nyears <- 40
burnin <- 15

i <- 1
region <- regions[i]
print(region)
if(region == "Monterey"){
    d17 <- read_rds("westcoast-networks/data/Simulation/ASC_Calibration_Regional/Monterey_2017_1_fuller.rds")
} else {
    d17 <- read_rds("westcoast-networks/data/Simulation/ASC_Calibration_Regional/Eureka_2017_fuller.rds")
}
asc_fc_start <- d17$cache_fc[,ncol(d17$cache_fc)]
asc_sc_start <- d17$cache_sc[,ncol(d17$cache_sc)]
  
asc_fc_blanks <- fcn_asc_fc(start$df_filter, region, year_ref)
drops <- unique(asc_fc_blanks$prep_df_fc$FISHERY_ID)
  
d <- 1
drop <- drops[d]
print(drop)
j <- 1
    
for(j in 1:nrow(randconditions)){
  
  skillrand <- randconditions$skillrand[j]
  scalerand <- randconditions$scalerand[j]
  costbyfishery <- randconditions$costbyfishery[j]
  shockpermanent <- randconditions$shockpermanent[j]
  
  plan(multisession, workers = 64)
  options(future.rng.onMisuse="ignore")
  cache_simulation_par <- furrr::future_map(1:1000, function(s){
    
    rscalef <- .75
    rscales <- 1
    rscalef <- runif(1,.75,1.5)
    asc_fc <- asc_fc_start*rscalef
    if(region == "Eureka"){
      asc_fc[32] <- asc_fc_start[32]-rscalef*abs(asc_fc_start[32])
    }
    rscales <- runif(1,.75,1.5)
    asc_sc <- asc_sc_start*rscales
    
    if(scalerand == T){
      rscalef <- runif(length(asc_fc_start),.0,1.5)
      asc_fc <- asc_fc_start*rscalef
      rscales <- runif(length(asc_fc_start),.0,1.5)
      asc_sc <- asc_sc_start*rscales
    }
    nyears <- 30
    burnin <- 10
    sim_run <- choices_asc_cal(start, region, year_ref, nyears,
                               vessels_in, burnin,
                               scale_t1ev, asc_sc, asc_fc, 
                               drop, skillrand, scalerand,
                               costbyfishery, shockpermanent, closureresponse)
    
    cache_iter <- list(sim_run = sim_run,
                       iter = s,
                       rscalef = rscalef,
                       rscales = rscales,
                       skillrand = skillrand,
                       scalerand = scalerand,
                       costbyfishery = costbyfishery,
                       shockpermanent = shockpermanent)
    
    write_rds(cache_iter, 
              paste0("~/westcoast-networks/data/Simulation/DynamicSimulationOutcomes/MLSizeCacheRound3_",j,"/",
                     region,"_",str_replace_all(drop, " ", "_"),"_",s,".rds"))
    
    return(list(sim_run = sim_run,
                iter = s,
                rscalef = rscalef,
                rscales = rscales))
    
  })

}


# Test Set ----------------------------------------------------------------
# 
# nyears <- 5
# burnin <- 10
# 
# if(scalerand == T){
#   rscalef <- runif(length(asc_fc_start),.0,1.5)
#   asc_fc <- asc_fc_start*rscalef
#   rscales <- runif(length(asc_fc_start),.0,1.5)
#   asc_sc <- asc_sc_start*rscales
# }
# 
# sim_run <- choices_asc_cal(start, region, year_ref, nyears,
#                            vessels_in, burnin,
#                            scale_t1ev, asc_sc, asc_fc, 
#                            drop, skillrand, scalerand,
#                            costbyfishery, shockpermanent)
# 
# # Loop Drops and Region ---------------------------------------------------
# 
# i <- 1
# for(i in 1:2){
#   
#   region <- regions[i]
#   print(region)
#   if(region == "Monterey"){
#     d17 <- read_rds("westcoast-networks/data/Simulation/ASC_Calibration_Regional/Monterey_2017_1_fuller.rds")
#   } else {
#     d17 <- read_rds("westcoast-networks/data/Simulation/ASC_Calibration_Regional/Eureka_2017_fuller.rds")
#   }
#   
#   asc_fc_start <- d17$cache_fc[,ncol(d17$cache_fc)]
#   asc_sc_start <- d17$cache_sc[,ncol(d17$cache_sc)]
#   
#   asc_fc_blanks <- fcn_asc_fc(start$df_filter, region, year_ref)
#   drops <- unique(asc_fc_blanks$prep_df_fc$FISHERY_ID)
#   
#   d <- 1
#   for(d in 1:length(drops)){
#     
#     drop <- drops[d]
#     print(drop)
#     
#     cache_simulation <- list()
#     # s <- 1
#     s <- Sys.time()
#     for(s in 1:4){
# 
#       rscalef <- runif(1,.75,1.5)
#       asc_fc <- asc_fc_start*rscalef
#       if(region == "Eureka"){
#         asc_fc[32] <- asc_fc_start[32]-rscalef*abs(asc_fc_start[32])
#       }
#       rscales <- runif(1,.75,1.5)
#       asc_sc <- asc_sc_start*rscales
#       
#       if(scalerand == T){
#         rscalef <- runif(length(asc_fc_start),.75,1.5)
#         asc_fc <- asc_fc_start*rscalef
#         rscales <- runif(length(asc_fc_start),.75,1.5)
#         asc_sc <- asc_sc_start*rscales
#       }
# 
#       sim_run <- choices_asc_cal(start, region, year_ref, nyears,
#                                  vessels_in, burnin,
#                                  scale_t1ev, asc_sc, asc_fc, 
#                                  drop, skillrand, costbyfishery, shockpermanent)
# 
#       cache_simulation[[s]] <- sim_run
# 
#     }
#     fl <- Sys.time() - s
#     
#     s <- Sys.time()
#     plan(multisession, workers = 64)
#     options(future.rng.onMisuse="ignore")
#     cache_simulation_par <- furrr::future_map(501:1000, function(s){
# 
#       rscalef <- runif(1,.1,3.5)
#       asc_fc <- asc_fc_start*rscalef
#       if(region == "Eureka"){
#         asc_fc[32] <- asc_fc_start[32]-rscalef*abs(asc_fc_start[32])
#       }
#       rscales <- runif(1,.1,3.5)
#       asc_sc <- asc_sc_start*rscales
#       
#       sim_run <- choices_asc_cal(start, region, year_ref, nyears,
#                                  vessels_in, burnin,
#                                  scale_t1ev, asc_sc, asc_fc, drop)
#       
#       cache_iter <- list(sim_run = sim_run,
#                          iter = s,
#                          rscalef = rscalef,
#                          rscales = rscales)
#       
#       write_rds(cache_iter, paste0("~/westcoast-networks/data/Simulation/DynamicSimulationOutcomes/MLSizeCache/",
#                                              region,"_",str_replace_all(drop, " ", "_"),"_",s,".rds"))
#       
#       return(list(sim_run = sim_run,
#                   iter = s,
#                   rscalef = rscalef,
#                   rscales = rscales))
# 
#     })
#     pl <- Sys.time() - s
#     gc()
#     print(pl)
#     
#     # plan(sequential)
#     
#     write_rds(cache_simulation_par, paste0("~/westcoast-networks/data/Simulation/DynamicSimulationOutcomes/",
#                                 region,"_",str_replace_all(drop, " ", "_"),".rds"))
#     
#   }
#   
# }
# 
# 
# 



