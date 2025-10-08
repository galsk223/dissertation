rm(list = ls())
library(tidyverse)
library(ggthemes)
library(zoo)

fl <- list.files("~/westcoast-networks/data/Simulation/DynamicSimulationOutcomes/",
                 full.names = T)

fcrab <- read_rds(fl[[2]])

fc <- 1
df_crab <- map(1:length(fcrab), function(fc){
  
  d <- fcrab[[fc]]
  
  vessels_time <- bind_rows(d$sim_run$cache_vessels) %>% 
    group_by(Year) %>% 
    summarise(NVessels = n_distinct(Vessel_ID),
              MeanRevenue = mean(Revenue_Fished),
              MeanWeeks = mean(Weeks_Fished),
              Iteration = d$iter,
              FishCosts = d$rscalef,
              SwitchCosts = d$rscales)
  
  steadystate_pre <- vessels_time %>% 
    filter(Year %in% c(12:16)) %>% 
    summarise(SS_Vessels = mean(NVessels),
              SS_Revenue = mean(MeanRevenue),
              SS_Weeks = mean(MeanWeeks))
  
  return <- vessels_time %>% 
    filter(Year >= 17) %>% 
    mutate(SS_Vessels = rollmean(NVessels, k = 5, fill = NA, align = "right"),
           SS_Revenue = rollmean(MeanRevenue, k = 5, fill = NA, align = "right"),
           SS_Weeks = rollmean(MeanWeeks, k = 5, fill = NA, align = "right"),
           SS_Vessels_Pre = steadystate_pre$SS_Vessels,
           Return = ifelse(SS_Vessels >= SS_Vessels_Pre, 1, 0)) %>%
    filter(Return == 1) %>%
    dplyr::slice(1)
    
  network_pre_e <- d$sim_run$cache_e[[1]] %>% 
    mutate(SS_Vessels = steadystate_pre$SS_Vessels,
           SS_Revenue = steadystate_pre$SS_Revenue,
           SS_Weeks = steadystate_pre$SS_Weeks,
           Return = ifelse(nrow(return) == 0, 35, return$Year-17),
           Iteration = d$iter,
           FishCosts = d$rscalef,
           SwitchCosts = d$rscales)
  
  network_pre_f <- d$sim_run$cache_f[[1]] %>% 
    mutate(SS_Vessels = steadystate_pre$SS_Vessels,
           SS_Revenue = steadystate_pre$SS_Revenue,
           SS_Weeks = steadystate_pre$SS_Weeks,
           Return = ifelse(nrow(return) == 0, 35, return$Year-17),
           Iteration = d$iter,
           FishCosts = d$rscalef,
           SwitchCosts = d$rscales)
  
  network_pre_b <- d$sim_run$cache_f[[1]] %>% 
    mutate(SS_Vessels = steadystate_pre$SS_Vessels,
           SS_Revenue = steadystate_pre$SS_Revenue,
           SS_Weeks = steadystate_pre$SS_Weeks,
           Return = ifelse(nrow(return) == 0, 35, return$Year-17),
           Iteration = d$iter,
           FishCosts = d$rscalef,
           SwitchCosts = d$rscales)
  
  return(list(vessels_time = vessels_time,
              network_pre_e = network_pre_e,
              network_pre_f = network_pre_f,
              network_pre_b = network_pre_b))
  
})

vplot <- map_dfr(df_crab, function(i){i$vessels_time})

timesims <- ggplot(vplot, aes(x = Year-2, y = NVessels, group = Iteration, color = FishCosts)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 15, color = "#B592A0") +
  geom_label(aes(x = 18, y = 195), label = "Dungeness Crab Closure",
             color = "#1E585C") +
  ggthemes::theme_tufte() +
  scale_color_gradient(low = "#AAFAC8", high = "#2D728B") +
  labs(y = "# Vessels",
       x = "Simulation Year",
       title = "Regional Fleets Before and After Shock",
       color = "Fishing Costs \n Scaled by:")

ggsave(plot = timesims, paste0("westcoast-networks/output/Simulation/simsovertime.png"),
      width = 10, height = 6, bg = "transparent")

df_net <- map_dfr(df_crab, function(i){i$network_pre_b}) %>% 
  filter(Fishery == "CA Dungeness Crab") %>% 
  mutate(Recover = ifelse(Return < 35, "Recovered", "New Steady State"))
hist(df_net$Return)
ggplot(df_net, aes(x = Return, y = H, color = Recover)) +
  geom_point(size = 2) +
  theme_minimal()









# Others ------------------------------------------------------------------
flindex <- 1
f_all <- map(1:length(fl), function(flindex){
  
  ff <- read_rds(fl[[flindex]])
  
  df_all <- map(1:length(ff), function(fc){
    
    d <- ff[[fc]]
    
    vessels_time <- bind_rows(d$sim_run$cache_vessels) %>% 
      group_by(Year) %>% 
      summarise(NVessels = n_distinct(Vessel_ID),
                MeanRevenue = mean(Revenue_Fished),
                MeanWeeks = mean(Weeks_Fished),
                Iteration = d$iter,
                FishCosts = d$rscalef,
                SwitchCosts = d$rscales,
                Fishery = str_extract(fl[[flindex]],"(?<=Outcomes//).+(?=.rds)"))
    
    steadystate_pre <- vessels_time %>% 
      filter(Year %in% c(12:16)) %>% 
      summarise(SS_Vessels = mean(NVessels),
                SS_Revenue = mean(MeanRevenue),
                SS_Weeks = mean(MeanWeeks))
    
    return <- vessels_time %>% 
      filter(Year >= 17) %>% 
      mutate(SS_Vessels = rollmean(NVessels, k = 5, fill = NA, align = "right"),
             SS_Revenue = rollmean(MeanRevenue, k = 5, fill = NA, align = "right"),
             SS_Weeks = rollmean(MeanWeeks, k = 5, fill = NA, align = "right"),
             SS_Vessels_Pre = steadystate_pre$SS_Vessels,
             Return = ifelse(SS_Vessels >= SS_Vessels_Pre, 1, 0)) %>%
      filter(Return == 1) %>%
      slice(1)
    
    network_pre <- d$sim_run$cache_e[[1]] %>% 
      mutate(SS_Vessels = steadystate_pre$SS_Vessels,
             SS_Revenue = steadystate_pre$SS_Revenue,
             SS_Weeks = steadystate_pre$SS_Weeks,
             Return = ifelse(nrow(return) == 0, 35, return$Year-17),
             Iteration = d$iter,
             FishCosts = d$rscalef,
             SwitchCosts = d$rscales,
             Fishery = str_extract(fl[[flindex]],"(?<=Outcomes//).+(?=.rds)"))
    
    return(list(vessels_time = vessels_time,
                network_pre = network_pre))
    
  })
  
  vplot <- map_dfr(df_all, function(i){i$vessels_time})
  
})

vall <- bind_rows(f_all)

ggplot(vall, aes(x = Year, y = NVessels, group = Iteration, color = FishCosts)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 17, color = "#B592A0") +
  geom_label(data = data.frame(x = 20, y = 195, label = "Fishery Closure"),
             aes(x = x, y = y, label = label),
             color = "#1E585C", size = 2,
             inherit.aes = FALSE) +
  facet_wrap(vars(Fishery)) +
  theme_minimal() +
  scale_color_gradient(low = "#AAFAC8", high = "#2D728B") +
  labs(y = "# Vessels",
       title = "Regional Fleets Before and After Shock",
       color = "Fishing Costs \n Scaled by:")















