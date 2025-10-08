rm(list = ls())
library(factoextra)
library(tidyverse)
library(ggExtra)
library(patchwork)
library(ggridges)
library(fixest)
library(data.table)


# Load In -----------------------------------------------------------------

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
         LANDING_YEAR_A >= 2017,
         LANDING_YEAR_A < 2022,
         FISHERY_ID != "Everything Else")

write_rds(df_year,"../westcoast-networks/data/clean/Simulation/empiricalbenchmarks_df_LY.rds")
rm(list = ls())
df_year <- read_rds("../westcoast-networks/data/clean/Simulation/empiricalbenchmarks_df_LY.rds")

# df_check <- df %>%
#   group_by(LANDING_YEAR_A) %>%
#   summarise(Weeks = n_distinct(LANDING_WEEK))

# Filter ------------------------------------------------------------------

df_meta <- read_rds("../westcoast-networks/data/clean/Simulation/empiricalbenchmarks_df_LY.rds") %>%
  mutate(LANDING_WEEK = LANDING_WEEK_A,
         LANDING_YEAR = LANDING_YEAR_A,
         # Region = Region_LW11
         )
df_small <- read_rds("../westcoast-networks/data/clean/Simulation/empiricalbenchmarks_df_LY.rds") %>%
  mutate(LANDING_WEEK = LANDING_WEEK_A,
         LANDING_YEAR = LANDING_YEAR_A,
         Region = Region_LW11
  )
df <- df_small

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

df_filter <- df %>%
  revfilter() %>%
  contfilter() %>%
  fisheryfilter() %>%
  group_by(FISHERY_ID, LANDING_YEAR, Region) %>%
  mutate(VesselN = n_distinct(VESSEL_ID)) %>%
  ungroup() %>%
  filter(VesselN > 3)

check <- df_filter %>%
  distinct(Region, LANDING_YEAR, FISHERY_ID) %>%
  group_by(Region, LANDING_YEAR) %>%
  add_tally() %>%
  distinct(Region, LANDING_YEAR, n)

ggplot(check, aes(x = LANDING_YEAR, y = n)) +
  geom_point() +
  geom_line() +
  facet_wrap(vars(Region)) +
  theme_minimal()

hist(check$n)

t <- df_filter %>%
  group_by(Region, LANDING_YEAR, FISHERY_ID) %>%
  tally()

# Vessel Lengths ----------------------------------------------------------

df_vsets <- df_filter %>%
  group_by(LengthBin, Region, LANDING_YEAR) %>%
  tally() %>% ungroup() %>%
  group_by(LANDING_YEAR, Region) %>%
  mutate(Total = sum(n),
         Prob = n/Total,
         Check = sum(Prob))

df_vsets_all <- df_filter %>%
  group_by(LANDING_YEAR) %>% add_tally(name = "YearAll") %>%
  group_by(Region, LANDING_YEAR) %>%
  add_tally(name = "RegionAll") %>%
  group_by(LengthBin, Region, LANDING_YEAR, YearAll, RegionAll) %>%
  tally() %>% ungroup() %>%
  group_by(LANDING_YEAR, Region) %>%
  mutate(Total = sum(n),
         Prob = n/Total,
         Check = sum(Prob),
         SimFleet = 400,
         RegionExp = RegionAll/YearAll*SimFleet,
         LengthRegionExp = RegionExp*Prob)

check <- df_vsets_all %>%
  group_by(LANDING_YEAR) %>% summarise(total = sum(LengthRegionExp))


# StockWeeks --------------------------------------------------------------

stock <- df_filter %>%
  group_by(FISHERY_ID, LANDING_YEAR, Region, LANDING_WEEK) %>%
  summarise(VesselWeeks = n_distinct(VESSEL_ID)) %>%
  ungroup()

df_stockwks <- stock %>%
  distinct(FISHERY_ID, LANDING_YEAR, Region) %>%
  expand_grid(LANDING_WEEK = 1:53) %>%
  left_join(stock, by = c("FISHERY_ID", "LANDING_YEAR", "Region","LANDING_WEEK")) %>%
  mutate(VesselWeeks = replace_na(VesselWeeks, 0)) %>%
  group_by(FISHERY_ID, LANDING_YEAR, Region) %>%
  arrange(FISHERY_ID, LANDING_YEAR, Region, LANDING_WEEK) %>%
  mutate(VesselWeeksRunning = cumsum(VesselWeeks),
         VesselWeeksTotal = sum(VesselWeeks),
         VesselWeeksRemaining = ifelse(is.na(VesselWeeksTotal-lag(VesselWeeksRunning)),VesselWeeksTotal,
                                       VesselWeeksTotal-lag(VesselWeeksRunning)))

# check_sw <- df_exprev %>%
#   group_by(LANDING_YEAR, LengthBin, Region, FISHERY_ID) %>%
#   tally()

# ExpRev ------------------------------------------------------------------

df_weekR <- df_filter %>%
  group_by(VESSEL_ID, LengthBin, LANDING_WEEK, LANDING_YEAR, FISHERY_ID, Region) %>%
  summarise(Rev_ITF = sum(EXVESSEL_REVENUE)) %>%
  ungroup() %>%
  mutate(alpha_yf = paste0(LANDING_YEAR,": ",Region,": ",FISHERY_ID),
         beta_cf = paste0(LengthBin,": ",Region,": ",FISHERY_ID)) %>%
  group_by(LANDING_WEEK, LANDING_YEAR, FISHERY_ID) %>%
  mutate(AR_TF = sum(Rev_ITF)) %>%
  ungroup() %>%
  mutate(LogRev_ITF = log(Rev_ITF+1),
         LogAR_TF = log(AR_TF+1))

library(fixest)
l1 <- feols(LogRev_ITF ~ alpha_yf + beta_cf:LogAR_TF, data = df_weekR)

coef <- l1$coeftable %>%
  select(Estimate, `Std. Error`) %>%
  rownames_to_column()

df_exprevR1 <- df_weekR %>%
  mutate(ExpRev = exp(l1$fitted.values)) %>%
  distinct(LANDING_WEEK, LANDING_YEAR, FISHERY_ID, LengthBin, ExpRev, Region)

week_13_data <- df_exprevR1 %>%
  filter(LANDING_YEAR == 2020, LANDING_WEEK == 13+8)
week_14_data <- week_13_data %>% mutate(LANDING_WEEK = 14+8)
week_15_data <- week_13_data %>% mutate(LANDING_WEEK = 15+8)
week_16_data <- week_13_data %>% mutate(LANDING_WEEK = 16+8)
week_17_data <- week_13_data %>% mutate(LANDING_WEEK = 17+8)

df_exprev_impute <- bind_rows(df_exprevR1,
                       week_14_data, week_15_data, week_16_data, week_17_data)

df_exprev <- df_exprev_impute %>%
  distinct(LANDING_YEAR, LengthBin, Region, FISHERY_ID) %>%
  expand_grid(LANDING_WEEK = 1:53) %>%
  left_join(df_exprev_impute, by = c("LANDING_YEAR", "Region", "LengthBin", "FISHERY_ID","LANDING_WEEK")) %>%
  mutate(ExpRev = replace_na(ExpRev, 0))

check_sw <- df_exprev %>%
  group_by(LANDING_YEAR, LengthBin, Region, FISHERY_ID) %>%
  tally()
unique(check_sw$n)


# Cross-overRevenue -------------------------------------------------------

distparameter <- .3
regionadjacency <- read_csv("../westcoast-networks/data/regionadjacency.csv",
                            show_col_types = FALSE) %>%
  pivot_longer(-Region) %>%
  mutate(DistanceCost = ifelse(is.na(value), 1, (value+1)*.5+.5),
         DistanceCostP = ifelse(is.na(value), 1, (value+1)*distparameter+1-distparameter))

# pre_cross <- df_exprev %>%
#   filter(LengthBin %in% df_vsets_all$LengthBin &
#            LANDING_YEAR %in% df_vsets_all$LANDING_YEAR &
#            Region %in% df_vsets_all$Region) %>%
#   distinct(LengthBin, FISHERY_ID, LANDING_WEEK, LANDING_YEAR)
#
# cross <- pre_cross %>%
#   left_join(pre_cross,
#             by = c("LengthBin", "FISHERY_ID", "LANDING_WEEK", "LANDING_YEAR"),
#             relationship = "many-to-many") %>%
#   left_join(regionadjacency, by = c("Region" = "name"))

# Edges -------------------------------------------------------------------

# togetheruse <- read_rds("westcoast-networks/data/clean/Simulation/empiricalbenchmarks_usereduced.rds")

df_yearregion <- df_filter %>%
  group_split(LANDING_YEAR, Region)

df_edges_restricted <- map_dfr(df_yearregion, function(dy){

  fisheries <- unique(dy$FISHERY_ID)
  fishery_pairs <- expand.grid(Fishery1 = fisheries, Fishery2 = fisheries, stringsAsFactors = FALSE) %>%
    filter(Fishery1 != Fishery2) %>%
    arrange(Fishery1, Fishery2)
  # RYUse <- togetheruse %>%
  #   filter(LANDING_YEAR == unique(dy$LANDING_YEAR),
  #          Region == unique(dy$Region))

  nv <- n_distinct(dy$VESSEL_ID)
  e <- dy %>%
    # filter(LengthBin %in% RYUse$LengthBin) %>%
    distinct(VESSEL_ID, FISHERY_ID) %>%
    mutate(present = 1) %>%
    pivot_wider(names_from = FISHERY_ID, values_from = present, values_fill = 0) %>%
    column_to_rownames("VESSEL_ID") %>%
    as.matrix() %>%
    { t(.) %*% . } %>%
    as.data.frame() %>%
    rownames_to_column("Fishery1") %>%
    pivot_longer(-Fishery1, names_to = "Fishery2", values_to = "UniqueVessels") %>%
    filter(Fishery1 != Fishery2) %>%
    full_join(fishery_pairs, by = c("Fishery1", "Fishery2")) %>%
    mutate(LANDING_YEAR = unique(dy$LANDING_YEAR),
           Region = unique(dy$Region),
           UniqueVessels = replace_na(UniqueVessels, 0),
           NormUV = UniqueVessels/nv)

})

df_edges <- map_dfr(df_yearregion, function(dy){

  fisheries <- unique(dy$FISHERY_ID)
  fishery_pairs <- expand.grid(Fishery1 = fisheries, Fishery2 = fisheries, stringsAsFactors = FALSE) %>%
    filter(Fishery1 != Fishery2) %>%
    arrange(Fishery1, Fishery2)

  nv <- n_distinct(dy$VESSEL_ID)
  e <- dy %>%
    distinct(VESSEL_ID, FISHERY_ID) %>%
    mutate(present = 1) %>%
    pivot_wider(names_from = FISHERY_ID, values_from = present, values_fill = 0) %>%
    column_to_rownames("VESSEL_ID") %>%
    as.matrix() %>%
    { t(.) %*% . } %>%
    as.data.frame() %>%
    rownames_to_column("Fishery1") %>%
    pivot_longer(-Fishery1, names_to = "Fishery2", values_to = "UniqueVessels") %>%
    filter(Fishery1 != Fishery2) %>%
    full_join(fishery_pairs, by = c("Fishery1", "Fishery2")) %>%
    mutate(LANDING_YEAR = unique(dy$LANDING_YEAR),
           Region = unique(dy$Region),
           UniqueVessels = replace_na(UniqueVessels, 0),
           NormUV = UniqueVessels/nv)

})



vset <- df_vsets_all %>%
  filter(LANDING_YEAR == 2017) %>%
  mutate(SimFleet = NVessel,
         RegionExp = RegionAll/YearAll*SimFleet,
         LengthRegionExp = RegionExp*Prob) %>%
  filter(LengthRegionExp > 5)

df_year <- df_filter %>%
  group_split(LANDING_YEAR)
dy <- df_year[[1]]
df_edges_meta <- map_dfr(df_year, function(dy){

  fisheries <- dy %>%
    mutate(RegionFishery = paste0(FISHERY_ID,", ",Region)) %>%
    semi_join(vset, by = c("LengthBin", "Region")) #edges only from active vessels
  fishery_pairs <- expand.grid(Fishery1 = unique(fisheries$RegionFishery),
                               Fishery2 = unique(fisheries$RegionFishery),
                               stringsAsFactors = FALSE) %>%
    filter(Fishery1 != Fishery2) %>%
    arrange(Fishery1, Fishery2)

  nv <- n_distinct(fisheries$VESSEL_ID)
  e <- fisheries %>%
    distinct(VESSEL_ID, RegionFishery) %>%
    mutate(present = 1) %>%
    pivot_wider(names_from = RegionFishery, values_from = present, values_fill = 0) %>%
    column_to_rownames("VESSEL_ID") %>%
    as.matrix() %>%
    { t(.) %*% . } %>%
    as.data.frame() %>%
    rownames_to_column("Fishery1") %>%
    pivot_longer(-Fishery1, names_to = "Fishery2", values_to = "UniqueVessels") %>%
    filter(Fishery1 != Fishery2) %>%
    full_join(fishery_pairs, by = c("Fishery1", "Fishery2")) %>%
    mutate(LANDING_YEAR = unique(dy$LANDING_YEAR),
           UniqueVessels = replace_na(UniqueVessels, 0),
           NormUV = UniqueVessels/nv)

})




# Export ------------------------------------------------------------------

out <- list(df_filter = df_filter, df_vsets = df_vsets, df_stockwks = df_stockwks,
            df_exprev = df_exprev, df_edges = df_edges)
write_rds(out,"../westcoast-networks/data/clean/Simulation/empiricalbenchmarks_region.rds")

out_meta <- list(regionadjacency = regionadjacency, df_filter = df_filter, df_vsets = df_vsets_all, df_stockwks = df_stockwks,
                 df_exprev = df_exprev, df_edges_meta = df_edges_meta)
write_rds(out_meta,"../westcoast-networks/data/clean/Simulation/empiricalbenchmarks_meta.rds")

outcheck <- read_rds("../westcoast-networks/data/clean/Simulation/empiricalbenchmarks_region.rds")
