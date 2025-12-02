rm(list = ls())
library(factoextra)
library(tidyverse)
library(ggExtra)
library(patchwork)
library(ggridges)
library(fixest)
library(data.table)

cal <- read_rds("../westcoast-networks/data/confidential/pacfin/clean/fishtickets-comp-1990-2021-cal.csv")
vessel <- readxl::read_xlsx("../westcoast-networks/data/confidential/pacfin/raw/permits-registration/VES_REG_ALL 1990 2022.xlsx")

vuse <- vessel %>%
  select(VESSEL_ID, VESSEL_LENGTH, VESSEL_WEIGHT, VESSEL_HORSEPOWER) %>%
  mutate(VESSEL_ID = as.numeric(VESSEL_ID),
         VESSEL_LENGTH = as.numeric(VESSEL_LENGTH),
         VESSEL_WEIGHT = as.numeric(VESSEL_WEIGHT),
         VESSEL_HORSEPOWER = as.numeric(VESSEL_HORSEPOWER)) %>%
  group_by(VESSEL_ID) %>%
  summarise(VESSEL_LENGTH = mean(VESSEL_LENGTH, na.rm = T),
            VESSEL_WEIGHT = mean(VESSEL_WEIGHT, na.rm = T),
            VESSEL_HORSEPOWER = mean(VESSEL_HORSEPOWER, na.rm = T)) %>%
  ungroup()

df_year <- cal %>%
  group_by(VESSEL_ID) %>%
  inner_join(., vuse, by = "VESSEL_ID") %>%
  mutate(LANDING_WEEK = lubridate::week(LANDING_DATE)) %>%
  ungroup() %>%
  mutate(LengthBin = paste0(floor(VESSEL_LENGTH/10)*10,"-",(floor(VESSEL_LENGTH/10)+1)*10),
         LANDING_YEAR_A = ifelse(LANDING_WEEK <= 45, LANDING_YEAR, LANDING_YEAR+1),
         LANDING_WEEK_A = ifelse(LANDING_WEEK >= 46, LANDING_WEEK-45, LANDING_WEEK+8)) %>%
  filter(!is.na(VESSEL_LENGTH),
         LANDING_YEAR_A >= 2000,
         LANDING_YEAR_A < 2022,
         FISHERY_ID != "Everything Else") %>%
  mutate(LANDING_WEEK = LANDING_WEEK_A,
         LANDING_YEAR = LANDING_YEAR_A,
         # Region = Region_LW11
  )

revfilter <- function(df){df %>%
    group_by(VESSEL_ID, LANDING_YEAR) %>%
    mutate(AnnualRevenue = sum(EXVESSEL_REVENUE)) %>%
    ungroup() %>%
    filter(AnnualRevenue > 5000)}

fisheryfilter <- function(df){df %>%
    group_by(VESSEL_ID, Region, FISHERY_ID, LANDING_YEAR) %>%
    mutate(FisheryRevenue = sum(EXVESSEL_REVENUE)) %>%
    ungroup() %>%
    filter(FisheryRevenue > 500)}

contfilter <- function(df){df %>%
    group_by(FISHERY_ID, VESSEL_ID, Region, LANDING_YEAR) %>%
    mutate(FisheryContribution = sum(EXVESSEL_REVENUE)/AnnualRevenue) %>%
    ungroup() %>%
    group_by(FISHERY_ID, LANDING_YEAR, Region) %>%
    mutate(MedianContribution = median(FisheryContribution)) %>%
    ungroup() %>%
    filter(MedianContribution > .1)}

df_filter <- df_year %>%
  revfilter() %>%
  contfilter() %>%
  fisheryfilter() %>%
  group_by(FISHERY_ID, LANDING_YEAR, Region) %>%
  mutate(VesselN = n_distinct(VESSEL_ID)) %>%
  ungroup() %>%
  filter(VesselN > 3)

homeregion <- df_filter %>%
  group_by(VESSEL_ID, Region, LANDING_YEAR) %>%
  tally() %>%
  ungroup() %>%
  group_by(VESSEL_ID, LANDING_YEAR) %>%
  filter(n == max(n)) %>%
  select(VESSEL_ID, LANDING_YEAR, HomeRegion = Region)

tr <- df_filter %>%
  mutate(FisheryRegion = paste0(FISHERY_ID,", " ,Region),
         priority = case_when(str_detect(tolower(FISHERY_ID), "salmon|dungeness") ~ 1,
                              T ~ 2)) %>%
  left_join(homeregion, relationship = "many-to-many",
            by = c("VESSEL_ID" = "VESSEL_ID",
                   "LANDING_YEAR" = "LANDING_YEAR")) %>%
  group_by(VESSEL_ID, LANDING_YEAR, LANDING_WEEK, FisheryRegion) %>%
  mutate(Revenue = sum(EXVESSEL_REVENUE)) %>%
  ungroup() %>%
  group_by(VESSEL_ID, LANDING_YEAR, LANDING_WEEK) %>%
  arrange(desc(Revenue), priority, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()


# Regime Classifications; Monotonicity --------------------------------------------------

# monotonicity in portion of home region catch from key fisheries
homekeyfishingportion <- tr %>%
  filter(HomeRegion == Region) %>%
  group_by(LANDING_YEAR, HomeRegion) %>%
  mutate(AnnualRevenue = sum(EXVESSEL_REVENUE)) %>%
  ungroup() %>%
  group_by(LANDING_YEAR, HomeRegion, FisheryRegion, AnnualRevenue) %>%
  summarise(Revenue = sum(EXVESSEL_REVENUE), .groups = "drop") %>%
  ungroup() %>%
  mutate(RevPortion = Revenue/AnnualRevenue,
         KeyFishery = str_detect(FisheryRegion, "Dungeness|Salmon")) %>%
  filter(KeyFishery == T) %>%
  group_by(LANDING_YEAR, HomeRegion, KeyFishery, AnnualRevenue) %>%
  summarise(Revenue = sum(Revenue),
            RevPortion = sum(RevPortion), .groups = "drop") %>%
  group_by(HomeRegion) %>%
  arrange(HomeRegion, LANDING_YEAR) %>%
  filter(LANDING_YEAR == lag(LANDING_YEAR)+1 |
           LANDING_YEAR == lead(LANDING_YEAR)-1) %>%
  mutate(KeyPortionDif = (RevPortion - lag(RevPortion))/lag(RevPortion),
         RegimeKey = case_when(KeyPortionDif > 0 ~ 1,
                               KeyPortionDif < 0 ~ 2),
         RegimeLabel = case_when(RegimeKey == lag(RegimeKey) |
                                   RegimeKey == lead(RegimeKey) ~ 1, T ~ 0)) %>%
  filter(RegimeLabel == 1) %>%
  select(HomeRegion, LANDING_YEAR, RegimeKey)

# monotonicity in portion of catch local
homerevportion <- tr %>%
  filter(HomeRegion == Region) %>%
  group_by(LANDING_YEAR, HomeRegion) %>%
  mutate(AnnualRevenue = sum(EXVESSEL_REVENUE)) %>%
  ungroup() %>%
  group_by(LANDING_YEAR, HomeRegion, FisheryRegion, AnnualRevenue) %>%
  summarise(Revenue = sum(EXVESSEL_REVENUE), .groups = "drop") %>%
  ungroup() %>%
  mutate(RevPortion = Revenue/AnnualRevenue,
         KeyFishery = str_detect(FisheryRegion, "Dungeness|Salmon")) %>%
  filter(KeyFishery == T) %>%
  group_by(LANDING_YEAR, HomeRegion, KeyFishery, AnnualRevenue) %>%
  summarise(Revenue = sum(Revenue),
            RevPortion = sum(RevPortion), .groups = "drop") %>%
  group_by(HomeRegion) %>%
  arrange(HomeRegion, LANDING_YEAR) %>%
  filter(LANDING_YEAR == lag(LANDING_YEAR)+1 |
           LANDING_YEAR == lead(LANDING_YEAR)-1) %>%
  mutate(AnnualRevDif = (AnnualRevenue - lag(AnnualRevenue))/lag(AnnualRevenue),
         RegimeKey = case_when(AnnualRevDif > 0 ~ 1,
                               AnnualRevDif < 0 ~ 2),
         RegimeLabel = case_when(RegimeKey == lag(RegimeKey) |
                                   RegimeKey == lead(RegimeKey) ~ 1, T ~ 0)) %>%
  filter(RegimeLabel == 1) %>%
  select(HomeRegion, LANDING_YEAR, RegimeKey)

# monotonicity in portion of all catch key fisheries
keyfisheryportion <- tr %>%
  group_by(LANDING_YEAR, HomeRegion) %>%
  mutate(AnnualRevenue = sum(EXVESSEL_REVENUE)) %>%
  ungroup() %>%
  group_by(LANDING_YEAR, HomeRegion, FisheryRegion, AnnualRevenue) %>%
  summarise(Revenue = sum(EXVESSEL_REVENUE), .groups = "drop") %>%
  ungroup() %>%
  mutate(RevPortion = Revenue/AnnualRevenue,
         KeyFishery = str_detect(FisheryRegion, "Dungeness|Salmon")) %>%
  filter(KeyFishery == T) %>%
  group_by(LANDING_YEAR, HomeRegion, KeyFishery, AnnualRevenue) %>%
  summarise(Revenue = sum(Revenue),
            RevPortion = sum(RevPortion), .groups = "drop") %>%
  group_by(HomeRegion) %>%
  arrange(HomeRegion, LANDING_YEAR) %>%
  filter(LANDING_YEAR == lag(LANDING_YEAR)+1 |
           LANDING_YEAR == lead(LANDING_YEAR)-1) %>%
  mutate(KeyPortionDif = (RevPortion - lag(RevPortion))/lag(RevPortion),
         RegimeKey = case_when(KeyPortionDif > 0 ~ 1,
                               KeyPortionDif < 0 ~ 2),
         RegimeLabel = case_when(RegimeKey == lag(RegimeKey) |
                                   RegimeKey == lead(RegimeKey) ~ 1, T ~ 0)) %>%
  filter(RegimeLabel == 1) %>%
  select(HomeRegion, LANDING_YEAR, RegimeKey)

# monotonicity in all catch
revenueportion <- tr %>%
  group_by(LANDING_YEAR, HomeRegion) %>%
  mutate(AnnualRevenue = sum(EXVESSEL_REVENUE)) %>%
  ungroup() %>%
  group_by(LANDING_YEAR, HomeRegion, FisheryRegion, AnnualRevenue) %>%
  summarise(Revenue = sum(EXVESSEL_REVENUE), .groups = "drop") %>%
  ungroup() %>%
  mutate(RevPortion = Revenue/AnnualRevenue,
         KeyFishery = str_detect(FisheryRegion, "Dungeness|Salmon")) %>%
  filter(KeyFishery == T) %>%
  group_by(LANDING_YEAR, HomeRegion, KeyFishery, AnnualRevenue) %>%
  summarise(Revenue = sum(Revenue),
            RevPortion = sum(RevPortion), .groups = "drop") %>%
  group_by(HomeRegion) %>%
  arrange(HomeRegion, LANDING_YEAR) %>%
  filter(LANDING_YEAR == lag(LANDING_YEAR)+1 |
           LANDING_YEAR == lead(LANDING_YEAR)-1) %>%
  mutate(AnnualRevDif = (AnnualRevenue - lag(AnnualRevenue))/lag(AnnualRevenue),
         KeyPortionDif = (RevPortion - lag(RevPortion))/lag(RevPortion),
         RegimeKey = case_when(AnnualRevDif > 0 ~ 1,
                               AnnualRevDif < 0 ~ 2),
         RegimeLabel = case_when(RegimeKey == lag(RegimeKey) |
                                   RegimeKey == lead(RegimeKey) ~ 1, T ~ 0)) %>%
  filter(RegimeLabel == 1) %>%
  select(HomeRegion, LANDING_YEAR, RegimeKey)

# monotonicity in vessels
vesselportion <- tr %>%
  group_by(LANDING_YEAR, HomeRegion) %>%
  summarise(NVessels = n_distinct(VESSEL_ID)) %>%
  ungroup() %>%
  group_by(HomeRegion) %>%
  arrange(HomeRegion, LANDING_YEAR) %>%
  filter(LANDING_YEAR == lag(LANDING_YEAR)+1 |
           LANDING_YEAR == lead(LANDING_YEAR)-1) %>%
  mutate(VesselDif = (NVessels - lag(NVessels))/lag(NVessels),
         RegimeKey = case_when(VesselDif > 0 ~ 1,
                               VesselDif < 0 ~ 2),
         RegimeLabel = case_when(RegimeKey == lag(RegimeKey) |
                                   RegimeKey == lead(RegimeKey) ~ 1, T ~ 0)) %>%
  filter(RegimeLabel == 1) %>%
  select(HomeRegion, LANDING_YEAR, RegimeKey)

# monotonicity in portion key fishery vessels
keyvesselportion <- tr %>%
  mutate(KeyFishery = str_detect(FisheryRegion, "Dungeness|Salmon")) %>%
  group_by(LANDING_YEAR, HomeRegion, KeyFishery) %>%
  summarise(NVessels = n_distinct(VESSEL_ID)) %>%
  ungroup() %>%
  group_by(HomeRegion, LANDING_YEAR) %>%
  mutate(NVesselsAll = sum(NVessels),
         PortionVessels = NVessels/NVesselsAll) %>%
  filter(KeyFishery == 1) %>%
  group_by(HomeRegion) %>%
  arrange(HomeRegion, LANDING_YEAR) %>%
  filter(LANDING_YEAR == lag(LANDING_YEAR)+1 |
           LANDING_YEAR == lead(LANDING_YEAR)-1) %>%
  mutate(VesselDif = (PortionVessels - lag(PortionVessels))/lag(PortionVessels),
         RegimeKey = case_when(VesselDif > 0 ~ 1,
                               VesselDif < 0 ~ 2),
         RegimeLabel = case_when(RegimeKey == lag(RegimeKey) |
                                   RegimeKey == lead(RegimeKey) ~ 1, T ~ 0)) %>%
  filter(RegimeLabel == 1) %>%
  select(HomeRegion, LANDING_YEAR, RegimeKey)



# Regime Classifications; SD ----------------------------------------------

# quantile of revenue from key fisheries
keyfisheryrev <- tr %>%
  mutate(KeyFishery = str_detect(FisheryRegion, "Dungeness|Salmon")) %>%
  filter(HomeRegion == Region) %>%
  group_by(HomeRegion, LANDING_YEAR) %>%
  mutate(AnnualRevenue = sum(EXVESSEL_REVENUE)) %>%
  ungroup() %>%
  group_by(HomeRegion, LANDING_YEAR, KeyFishery, AnnualRevenue) %>%
  summarise(Revenue = sum(EXVESSEL_REVENUE), .groups = "drop") %>%
  filter(KeyFishery == T) %>%
  group_by(HomeRegion) %>%
  mutate(RevPortion = Revenue/AnnualRevenue,
         RegimeKey = case_when(RevPortion > quantile(RevPortion, .67) ~ 1,
                               RevPortion > quantile(RevPortion, .33) ~ 2,
                               # RevPortion > quantile(RevPortion, .25) ~ 3,
                               T ~ 3),
         RegimeLabel = case_when(RegimeKey == lag(RegimeKey) |
                                   RegimeKey == lead(RegimeKey) ~ 1, T ~ 0)) %>%
  filter(RegimeLabel == 1) %>%
  select(HomeRegion, LANDING_YEAR, RegimeKey)

# quantile of vessels in key fisheries
keyfisheryves <- tr %>%
  mutate(KeyFishery = str_detect(FisheryRegion, "Dungeness|Salmon")) %>%
  filter(HomeRegion == Region) %>%
  group_by(HomeRegion, LANDING_YEAR) %>%
  mutate(AnnualVes = n_distinct(VESSEL_ID)) %>%
  ungroup() %>%
  group_by(HomeRegion, LANDING_YEAR, KeyFishery, AnnualVes) %>%
  summarise(Ves = n_distinct(VESSEL_ID), .groups = "drop") %>%
  filter(KeyFishery == T) %>%
  group_by(HomeRegion) %>%
  mutate(VesPortion = Ves/AnnualVes,
         RegimeKey = case_when(VesPortion > quantile(VesPortion, .67) ~ 1,
                               VesPortion > quantile(VesPortion, .33) ~ 2,
                               # VesPortion > quantile(VesPortion, .25) ~ 3,
                               T ~ 4),
         RegimeLabel = case_when(RegimeKey == lag(RegimeKey) |
                                   RegimeKey == lead(RegimeKey) ~ 1, T ~ 0)) %>%
  filter(RegimeLabel == 1) %>%
  select(HomeRegion, LANDING_YEAR, RegimeKey)

allregimes <- list(homekeyfishingportion, homerevportion, keyfisheryportion,
                   revenueportion, vesselportion, keyvesselportion,
                   keyfisheryrev, keyfisheryves)

write_rds(allregimes,"../westcoast-networks/data/clean/Simulation/empregimes.rds")
