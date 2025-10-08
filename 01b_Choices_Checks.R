time2 <- cache_dfchoice[[2]] %>% 
  group_by(Week, FISHERY_ID) %>% 
  summarise(NVessels = n_distinct(Vessel_ID), .groups = "drop") %>% 
  mutate(Week = as.double(str_remove(Week, "V"))) %>% 
  arrange(Week) %>% 
  pivot_wider(names_from = Week,
              values_from = NVessels)

time3 <- cache_dfchoice[[3]] %>% 
  group_by(Week, FISHERY_ID) %>% 
  summarise(NVessels = n_distinct(Vessel_ID), .groups = "drop") %>% 
  mutate(Week = as.double(str_remove(Week, "V")))  %>% 
  arrange(Week) %>% 
  pivot_wider(names_from = Week,
              values_from = NVessels)


rs <- map_dfr(1:length(cache_dfchoice), function(cdc){
  
  r <- cache_dfchoice[[cdc]] %>% 
    # group_by(LengthBin) %>% 
    # mutate(NVessels_Total = n_distinct(Vessel_ID), .groups = "drop") %>%    
    group_by(FISHERY_ID, LengthBin) %>% 
    summarise(Weeks = n(), .groups = "drop") %>% 
    pivot_wider(names_from = LengthBin,
                values_from = Weeks) %>%
    mutate(Year = cdc)
  
})

ycheck <- 3

r <- bind_rows(cache_vessels) %>% 
  # mutate(Catch = paste(Weeks_Fished, Revenue_Fished)) %>% 
  select(Vessel_ID, Revenue_Fished, Year) %>% 
  pivot_wider(names_from = Year,
              values_from = Revenue_Fished) %>% 
  mutate(ChangeShock = `3`-`2`,
         ChangePost = `4`-`3`,
         ChangePost2 = `4`-`2`)

rf <- map_dfr(1:length(cache_dfchoice), function(cdc){
  
  r <- cache_dfchoice[[cdc]] %>% 
    # filter(FISHERY_ID != "Not Fishing") %>% 
    group_by(Vessel_ID, Week) %>% 
    summarise(FISHERY_ID = get_mode(FISHERY_ID)) %>% 
    group_by(Vessel_ID, FISHERY_ID) %>% 
    tally() %>% 
    pivot_wider(names_from = FISHERY_ID,
                values_from = n) %>% 
    mutate(Year = cdc)

})



tt <- cache_dfchoice[[3]] %>% 
  filter(Vessel_ID == 267)
dfrevcheck <- df_exprev %>% 
  filter(FISHERY_ID == "LE/IFQ Trawl Groundfish Non-Whiting") 
  

# inputscheck <- inputs %>% 
#   group_by(Vessel_ID, LengthBin, FISHERY_ID) %>% 
#   summarise(Rev = sum(ExpRev))

v_sumr <- cache_dfchoice[[ycheck]] %>% 
  filter(FISHERY_ID != "Not Fishing") %>% 
  group_by(LengthBin, Week) %>% 
  summarise(Revenue = sum(Revenue)) %>% 
  mutate(Week = as.double(str_remove(Week, "V"))) %>% 
  pivot_wider(names_from = Week,
              values_from = Revenue)

v_sumf <- cache_dfchoice[[ycheck]] %>% 
  filter(FISHERY_ID != "Not Fishing") %>% 
  group_by(LengthBin, Week) %>% 
  summarise(FISHERY_ID = get_mode(FISHERY_ID)) %>% 
  mutate(Week = as.double(str_remove(Week, "V"))) %>% 
  pivot_wider(names_from = Week,
              values_from = FISHERY_ID)

v_sumnf <- cache_dfchoice[[burnin]] %>% 
  filter(FISHERY_ID == "Not Fishing") %>% 
  group_by(LengthBin, Week) %>% 
  summarise(FISHERY_ID = sum(Revenue)) %>% 
  mutate(Week = as.double(str_remove(Week, "V"))) %>% 
  pivot_wider(names_from = Week,
              values_from = FISHERY_ID)






get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
