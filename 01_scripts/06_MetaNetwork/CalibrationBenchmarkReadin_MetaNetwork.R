library(data.table)
library(evd)
# rm(list = ls())

start <- read_rds("westcoast-networks/data/clean/Simulation/empiricalbenchmarks_meta.rds")

NVessel <- 350

homeregion <- start$df_filter %>% 
  filter(LANDING_YEAR %in% 2017,
         LengthBin %in% start$df_vsets$LengthBin &
           Region%in% start$df_vsets$Region) %>% 
  group_by(VESSEL_ID, Region) %>%
  summarise(total_trips = n(), .groups = "drop") %>%
  group_by(VESSEL_ID) %>% 
  slice_max(order_by = total_trips, n = 1, with_ties = FALSE) %>%
  select(VESSEL_ID, HomeRegion = Region) 

vset <- start$df_vsets %>% 
  filter(LANDING_YEAR == 2017) %>% 
  mutate(SimFleet = NVessel,
         RegionExp = RegionAll/YearAll*SimFleet,
         LengthRegionExp = RegionExp*Prob) %>% 
  filter(LengthRegionExp > 5)

fishing_bench <- start$df_filter %>% 
  filter(LANDING_YEAR %in% 2017,
         LengthBin %in% vset$LengthBin &
           Region%in% vset$Region) %>% 
  left_join(homeregion, by = "VESSEL_ID") %>% 
  mutate(RegionFishery = paste0(FISHERY_ID,", ",Region)) %>% 
  distinct(VESSEL_ID, LengthBin, RegionFishery, LANDING_WEEK, LANDING_YEAR) %>%
  group_by(LengthBin) %>% 
  mutate(NVesselsWeeks = n_distinct(VESSEL_ID)*53) %>% #10-20 ft vessel weeks available
  ungroup() %>% 
  group_by(LengthBin, RegionFishery, NVesselsWeeks) %>% 
  tally() %>% #No. of 10-20 ft vessels observed participating in "crab, SF"
  mutate(Shares = round(n/NVesselsWeeks,4)) %>% 
  ungroup() %>% 
  group_by(LengthBin) %>%
  mutate(Check = sum(Shares)) %>% 
  ungroup()

targetshares <- fishing_bench %>% 
  arrange(RegionFishery) %>% 
  group_split(LengthBin)

# nvesselcheck <- map_dfr(seq(100,1000,50), function(i){
#   vset <- start$df_vsets %>% 
#     filter(LANDING_YEAR == 2017) %>% 
#     mutate(SimFleet = i,
#            RegionExp = RegionAll/YearAll*SimFleet,
#            LengthRegionExp = RegionExp*Prob) %>% 
#     filter(LengthRegionExp > 5)
#   out <- tibble(NVessel = i,
#                 NGroups = nrow(vset))
# })
# 
# edgecutoffcheck <- map_dfr(seq(1,10,1), function(i){
#   switching_bench <- start$df_edges_meta %>% 
#     filter(LANDING_YEAR == 2017) %>% 
#     mutate(EdgeExp = NVessel*NormUV) %>% 
#     filter(EdgeExp > i)
#   out <- tibble(Min = i,
#                 NEdges = nrow(switching_bench))
# })


switching_bench <- start$df_edges_meta %>% 
  filter(LANDING_YEAR == 2017) %>% 
  mutate(SimFleet = NVessel,
         EdgeExp = SimFleet*NormUV) 
targets_ee <- switching_bench %>% 
  select(Fishery1, Fishery2, EdgeExp) %>% 
  filter(EdgeExp > 2) %>% 
  arrange(as.character(Fishery1), as.character(Fishery2)) 
# %>%
#   pivot_wider(names_from = Fishery2, values_from = EdgeExp, values_fill = 0) %>%
#   column_to_rownames("Fishery1") %>%
#   select(sort(colnames(.))) %>% 
#   as.matrix()
# targets_lower <- targets_ee[lower.tri(targets_ee)]


# %>% 
#   filter(EdgeExp > 2)

df_all <- start$df_filter %>% 
  filter(LengthBin %in% vset$LengthBin &
           Region %in% vset$Region)

exprev <- start$df_exprev %>% 
  mutate(RegionFishery = paste(FISHERY_ID, Region, sep = ", "))

crossbridge <- start$regionadjacency
