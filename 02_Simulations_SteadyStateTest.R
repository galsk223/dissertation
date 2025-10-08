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
list.files("westcoast-networks/scripts/02_Simulation/0_Port_Regions/Choice_Function_Functions/", 
           full.names = TRUE) %>%
  walk(source)
source("westcoast-networks/scripts/02_Simulation/0_Port_Regions/01_Choices_Function_NYears.R")
start <- read_rds("westcoast-networks/data/clean/Simulation/empiricalbenchmarks_region.rds")

regions <- c("Eureka", "Monterey", "Morro Bay")
region <- regions[1]
year_ref <- 2017
nyears <- 30
burnin <- 30
drop <- "CA Dungeness Crab"

e17 <- read_rds("westcoast-networks/data/Simulation/ASC_Calibration_Regional/Eureka_2017_fuller.rds")

vessels_in <- 200
scale_t1ev <- 500

asc_fc <- e17$cache_fc[,ncol(e17$cache_fc)]
asc_sc <- e17$cache_sc[,ncol(e17$cache_sc)]

cache_steadystatetest <- list()
i <- 1
for(i in 1:10){
  
  rscale <- runif(1,.5,1.5)
  asc_fc <- asc_fc*rscale
  asc_fc[32] <- asc_fc[32]-rscale*abs(asc_fc[32])
  asc_sc <- asc_sc*rscale
  
  test_steadystate <- choices_asc_cal(start, region, year_ref, nyears, 
                                      vessels_in, burnin,
                                      scale_t1ev, asc_sc, asc_fc, drop)
  
  cache_steadystatetest[[i]] <- test_steadystate
  
}

sst <- map_dfr(1:10, function(c){
  
  out <- bind_rows(cache_steadystatetest[[c]]$cache_vessels) %>% 
    mutate(Sim = c)
  
})

plot <- sst %>% 
  group_by(Sim, Year) %>% 
  tally()

ggplot(plot, aes(x = Year, y = n, group = Sim)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Simulation Year",
       y = "# Vessels",
       title = "Steady States in an Unshocked Dynamic System")

