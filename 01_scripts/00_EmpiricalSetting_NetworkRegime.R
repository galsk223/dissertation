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

topfishery <- tr %>%
  group_by(LANDING_YEAR, HomeRegion) %>%
  mutate(AnnualRevenue = sum(EXVESSEL_REVENUE)) %>%
  ungroup() %>%
  group_by(LANDING_YEAR, HomeRegion, FisheryRegion, AnnualRevenue) %>%
  summarise(Revenue = sum(EXVESSEL_REVENUE)) %>%
  ungroup() %>%
  mutate(RevPortion = Revenue/AnnualRevenue) %>%
  group_by(LANDING_YEAR, HomeRegion) %>%
  arrange(desc(RevPortion), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(TopFishery = paste0(FisheryRegion," (",round(RevPortion,2),")")) %>%
  select(LANDING_YEAR, HomeRegion, TopFishery)

tr <- df_filter %>%
  mutate(FisheryRegion = paste0(FISHERY_ID,", " ,Region),
         priority = case_when(str_detect(tolower(FISHERY_ID), "salmon|dungeness") ~ 1,
                              T ~ 2)) %>%
  left_join(homeregion, relationship = "many-to-many",
            by = c("VESSEL_ID" = "VESSEL_ID",
                   "LANDING_YEAR" = "LANDING_YEAR")) %>%
  left_join(topfishery, relationship = "many-to-many",
            by = c("LANDING_YEAR" = "LANDING_YEAR",
                   "HomeRegion" = "HomeRegion")) %>%
  group_by(VESSEL_ID, LANDING_YEAR, LANDING_WEEK, FisheryRegion) %>%
  mutate(Revenue = sum(EXVESSEL_REVENUE)) %>%
  ungroup() %>%
  group_by(VESSEL_ID, LANDING_YEAR, LANDING_WEEK) %>%
  arrange(desc(Revenue), priority, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()

write_rds(tr,"../westcoast-networks/data/clean/Simulation/empiricalbenchmarks_df_empnet.rds")

# compile-------------------------------------------------------------------------

list.files("/home/gkoss/dissertation/01_scripts/01_Choice_Function_Functions/",
           full.names = TRUE) %>%
  walk(source)
df_dist <- read_rds("../westcoast-networks/data/clean/Simulation/empiricalbenchmarks_meta.rds")$regionadjacency %>%
  select(-DistanceCostP)

trsplit <- read_rds("../westcoast-networks/data/clean/Simulation/empiricalbenchmarks_df_empnet.rds") %>%
  filter(HomeRegion == "San Francisco",
         LANDING_YEAR == 2019) %>%
  group_split(HomeRegion, LANDING_YEAR)
# group_split(LANDING_YEAR)

i <- 1
allemp <- map_dfr(1:length(trsplit), function(i){

  df_choice <- trsplit[[i]] %>%
    select(Vessel_ID = VESSEL_ID,
           HomeRegion,
           Revenue = EXVESSEL_REVENUE,
           FISHERY_ID = FisheryRegion)
  fisherylist_use <- unique(df_choice$FISHERY_ID)
  fishery_pairs <- expand.grid(Fishery1 = fisherylist_use,
                               Fishery2 = fisherylist_use,
                               stringsAsFactors = FALSE) %>%
    filter(Fishery1 != Fishery2) %>%
    arrange(Fishery1, Fishery2)

  diverse <- df_choice %>%
    group_by(Vessel_ID) %>%
    summarise(Diversification = n_distinct(FISHERY_ID))
  strategies <- df_choice %>%
    group_by(Vessel_ID) %>%
    summarize(
      n_fisheries = n_distinct(FISHERY_ID),
      strategy = paste(sort(unique(FISHERY_ID)), collapse = "-"),
      .groups = "drop"
    ) %>%
    filter(n_fisheries >= 2)
  dtrav <- df_choice %>%
    mutate(DestRegion = str_extract(FISHERY_ID, "(?<=, ).+")) %>%
    left_join(df_dist, by = c("HomeRegion" = "Region",
                              "DestRegion" = "name")) %>%
    mutate(RegionTrav = replace_na(value, 0)) %>%
    group_by(Vessel_ID) %>%
    summarise(MeanDist = mean(RegionTrav, na.rm = T),
              Revenue = sum(Revenue))

  g_out <- graph_ext_fcn(df_choice, fishery_pairs, fisherylist_use, yi = 1, largestcc = T) %>%
    mutate(
      NVessels = n_distinct(df_choice$Vessel_ID),
      Region = unique(trsplit[[i]]$HomeRegion),
           Year = unique(trsplit[[i]]$LANDING_YEAR),
      TopFishery = unique(trsplit[[i]]$TopFishery),
           Cluster = as.numeric(Cluster),
      N_diversified2 = sum(diverse$Diversification >= 2),
      N_diversified3 = sum(diverse$Diversification >= 3),
      N_diversified4 = sum(diverse$Diversification >= 4),
      N_diversified5 = sum(diverse$Diversification >= 5),
      MeanDiversification_All = mean(diverse$Diversification),
      MeanDiversification_Diverse = mean(diverse$Diversification[diverse$Diversification >= 2]),
      N_strategies = n_distinct(strategies$strategy),
      DistMean = mean(dtrav$MeanDist),
      RevMean = mean(dtrav$Revenue),
      RevMed = median(dtrav$Revenue),
      RevGini = Gini(dtrav$Revenue))

})

comp <- allemp %>%
  distinct(
    Region, TopFishery, N_diversified2, MeanDiversification_All, N_strategies, DistMean,
    RevMean,
           Year, N_Fisheries, NVessels, N_Edges, Mean_Weight,
           ClusteringCoefficient_Global, Modularity, FragMeasure) %>%
  mutate(Connectance = N_Edges/(N_Fisheries*(N_Fisheries-1)/2),
         YearLab = paste(Year,"\n",TopFishery))

ggplot(comp, aes(x = Year, y = N_Fisheries, color = N_diversified2, size = Modularity)) +
  geom_point() +
  facet_wrap(vars(Region), scales = "free") +
  theme_minimal() +
  labs(title = "Observed Networks",
       y = "# Fishery Nodes",
       color = "# Vessel Links",
       size = "Mean Edge Weight")

ggplot(comp, aes(x = Year, y = N_Fisheries, color = Modularity, size = N_strategies)) +
  geom_point() +
  facet_wrap(vars(Region), scales = "free") +
  theme_minimal() +
  labs(title = "Observed Networks",
       y = "# Fishery Nodes")



# regime compare ----------------------------------------------------------

regionyear <- expand_grid(Region = unique(comp$Region),
                          Year = 2004:2021)
nyrs <- 3
j <- 1
trailing5 <- map_dfr(1:nrow(regionyear), function(j){

  df <- comp %>%
    filter(Region == regionyear$Region[[j]],
           Year %in% (regionyear$Year[[j]]-(nyrs-1)):regionyear$Year[[j]]) %>%
    mutate(Set = j,
           Years = paste0(regionyear$Year[[j]]-(nyrs-1),"-",regionyear$Year[[j]]),
           SumPos = sum(N_Fisheries-lag(N_Fisheries)>=0, na.rm = T),
           SumNeg = sum(N_Fisheries-lag(N_Fisheries)<=0, na.rm = T),
           DivSign = sign(MeanDiversification_All-lag(MeanDiversification_All)),
           WeightSign = sign(Mean_Weight-lag(Mean_Weight)),
           PerfectDivWeight = sum(DivSign == WeightSign, na.rm = T),
           DivWeight = cor(MeanDiversification_All,Mean_Weight),
           StratWeight = cor(N_strategies,Mean_Weight),
           DivMod = cor(MeanDiversification_All,Modularity),
           StratMod = cor(N_strategies,Modularity),
           DivDen = cor(MeanDiversification_All,ClusteringCoefficient_Global),
           StratDen = cor(N_strategies,ClusteringCoefficient_Global),
           MeanFisheries = mean(N_Fisheries),
           FisheryDirection = mean(N_Fisheries-lag(N_Fisheries), na.rm = T),
           MeanDiverse = mean(MeanDiversification_All),
           DiverseDirection = mean(MeanDiversification_All-lag(MeanDiversification_All), na.rm = T),
           MeanStrategies = mean(N_strategies),
           StrategiesDirection = mean(N_strategies-lag(N_strategies), na.rm = T))

})

compregime <- trailing5 %>%
  filter(
    # Year > 2007
    # ,SumPos >= 3 | SumNeg >= 3
    ) %>%
  distinct(Region, Years, SumPos, SumNeg, PerfectDivWeight, MeanFisheries, FisheryDirection,
           MeanDiverse, DiverseDirection, MeanStrategies, StrategiesDirection,
           DivWeight, StratWeight, DivMod, StratMod, DivDen, StratDen) %>%
  mutate(RegimeName = paste0(Region,"\n",Years))

color_list <- c("#4c6085","#39a0ed","#52050A","#EC0868","#f7b32b","#C09BD8",
                "#fe5f55","#A44A3F","#13c4a3","#161613")

ggplot(compregime, aes(x = FisheryDirection, y = DiverseDirection)) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0,
           fill = "#1FD643", alpha = 0.5) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = Inf,
           fill = "#1FD643", alpha = 0.5) +
  geom_hline(yintercept = 0, color = "grey80") +
  geom_vline(xintercept = 0, color = "grey80") +
  geom_point(size = 2) +
  ggthemes::theme_tufte() +
  labs(x = "Mean network size change \n(annual growth [contraction] in fishery nodes)",
       y = "Mean vessel diversification change \n(annual inc [dec] in fisheries / vessel)",
       # color = "Region",
       title = "Network Measure Interpretations by Regime",
       subtitle = "Using observed California landings data",
       caption = "A regime is a sequence of 3 years in a region (2006-2021), in which I compare the correlation between
       vessel diversification and network outcomes specific to the regime state of growth (contraction)")

ggplot(compregime, aes(x = FisheryDirection, y = StrategiesDirection)) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0,
           fill = "#1FD643", alpha = 0.5) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = Inf,
           fill = "#1FD643", alpha = 0.5) +
  geom_hline(yintercept = 0, color = "grey80") +
  geom_vline(xintercept = 0, color = "grey80") +
  geom_point(size = 2) +
  ggthemes::theme_tufte() +
  labs(x = "Mean network size change \n(annual growth [contraction] in fishery nodes)",
       y = "Mean fleet strategy count change \n(annual inc [dec] in # unique strategies)",
       # color = "Region",
       title = "Network Measure Interpretations by Regime",
       subtitle = "Using observed California landings data",
       caption = "A regime is a sequence of 3 years in a region (2006-2021), in which I compare the correlation between
       vessel diversification and network outcomes specific to the regime state of growth (contraction)")

p <- ggplot(compregime, aes(x = FisheryDirection, y = DivWeight, color = Region)) +
  geom_hline(yintercept = 0, color = "grey80") +
  geom_vline(xintercept = 0, color = "grey80") +
  geom_point(size = 2) +
  geom_text(aes(label = RegimeName), size = 3) +
  ggthemes::theme_tufte() +
  scale_color_manual(values = color_list[1:n_distinct(compregime$Region)],
                     guide = "none") +
  labs(x = "Mean network size change \n(annual growth [contraction] in fishery nodes)",
       y = "Correlation between\n mean vessel diversification and edge weight",
       color = "Region",
       title = "Network Measure Interpretations by Regime",
       subtitle = "Using observed California landings data",
       caption = "A regime is a sequence of 4 years in a region (2005-2021), in which I compare the correlation between
       vessel diversification and network outcomes specific to the regime state of growth (contraction)")

ggMarginal(p, type = "boxplot", size = 75,
           fill="grey50", color = "grey90", margins = "y")

p <- ggplot(compregime, aes(x = FisheryDirection, y = StratWeight, color = Region)) +
  geom_hline(yintercept = 0, color = "grey80") +
  geom_vline(xintercept = 0, color = "grey80") +
  geom_point(size = 2) +
  ggthemes::theme_tufte() +
  geom_text(aes(label = RegimeName), size = 3) +
  scale_color_manual(values = color_list[1:n_distinct(compregime$Region)],
                     guide = "none") +
  labs(x = "Mean network size change \n(annual growth [contraction] in fishery nodes)",
       y = "Correlation between\n # unique strategies and edge weight",
       color = "Region",
       title = "Network Measure Interpretations by Regime",
       subtitle = "Using observed California landings data",
       caption = "A regime is a sequence of 4 years in a region (2005-2021), in which I compare the correlation between
       vessel diversification and network outcomes specific to the regime state of growth (contraction)")

ggMarginal(p, type = "boxplot", size = 75,
           fill="grey50", color = "grey90", margins = "y")



# regime examples ---------------------------------------------------------

trplot <- read_rds("../westcoast-networks/data/clean/Simulation/empiricalbenchmarks_df_empnet.rds")

loopfind <- compregime %>%
  filter(DiverseDirection < 0)

r1 <- "Central Coast"
r2 <- "Central Coast"
yrs1 <- 2008:2010
yrs2 <- 2014:2016

yrs <- yrs2
r <- r2
plots <- list()
y <- 1
for(y in 1:length(yrs1)){

  t <- trplot %>%
    filter(HomeRegion == r,
           LANDING_YEAR == yrs[[y]]) %>%
    mutate(FISHERY_ID = FisheryRegion)

  info <- allemp %>%
    filter(Region == r,
           Year == yrs[[y]])
  nfisheries <- n_distinct(t$FisheryRegion)
  fisherylist_use <- unique(t$FisheryRegion)[unique(t$FisheryRegion) != "Not Fishing"]
  fishery_pairs <- expand.grid(Fishery1 = fisherylist_use, Fishery2 = fisherylist_use, stringsAsFactors = FALSE) %>%
    filter(Fishery1 != Fishery2) %>%
    arrange(Fishery1, Fishery2)
  nvessels <- n_distinct(t$VESSEL_ID)

  edges_ext <- t %>%
    distinct(VESSEL_ID, FISHERY_ID)%>%
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
    mutate(UniqueVessels = replace_na(UniqueVessels, 0),
           V1 = pmin(as.character(Fishery1), as.character(Fishery2)),
           V2 = pmax(as.character(Fishery1), as.character(Fishery2))) %>%
    as.data.table() %>%
    distinct(V1, V2, UniqueVessels) %>%
    rename(Fishery1 = V1, Fishery2 = V2) %>%
    arrange(Fishery1, Fishery2)

  vy <- t %>%
    group_by(FISHERY_ID) %>%
    summarise(UniqueVessels = n_distinct(VESSEL_ID),
              .groups = "drop") %>%
    filter(FISHERY_ID != "Not Fishing") %>%
    select(FISHERY_ID, UniqueVessels)

  ey <- edges_ext %>%
    filter(UniqueVessels > 0) %>%
    filter(Fishery1 != "Not Fishing",
           Fishery2 != "Not Fishing") %>%
    select(Fishery1, Fishery2, weight = UniqueVessels)

  gy <- graph_from_data_frame(ey,
                              directed = F,
                              vertices = vy)

  gy <- induced_subgraph(gy,
                         vids = which(igraph::components(gy)$membership ==
                                        which.max(igraph::components(gy)$csize)))

  cw <- cluster_walktrap(gy, modularity = T)
  mem <- as.factor(membership(cw))
  out <- set_vertex_attr(gy, "Cluster",
                         value = mem) %>%
    as_tbl_graph() %>%
    activate(nodes) %>%
    arrange(as.numeric(Cluster))

  plots[[y]] <- ggraph(out,  layout = "linear", circular = TRUE) +
    geom_edge_link(aes(width = weight), alpha = 0.5,
                   color = color_list[10]) +
    geom_node_point(aes(size = UniqueVessels, color = Cluster)) +
    # geom_node_text(aes(label = name), repel = TRUE, size = 3) +
    scale_size_continuous(limits = c(0,150),
                          range = c(.5,10)) +
    scale_edge_width(limits = c(0,50),
                     range = c(.5,8)) +
    scale_color_manual(values = color_list[1:max(membership(cw))], guide = "none") +
    theme_void() +
    guides(size = "none",
           edge_width = "none"
    ) +
    labs(
      title = paste(r, yrs[[y]]),
      # title = paste0("Fishing Costs ",round(unique(info$FishingCost),2)," | Switching Costs: ",round(unique(info$SwitchingCost),2)),
      subtitle = paste0("Mean Diversification: ",round(unique(info$MeanDiversification_All),2),
                        "\nMean Weight: ",round(unique(info$Mean_Weight),2)),
      # title = paste0("Modularity ",round(unique(info$Modularity),2)," | Density: ",round(unique(info$ClusteringCoefficient_Global),2)),
      # subtitle = paste0("Fishing Costs ",round(unique(info$FishingCost),2)," | Switching Costs: ",round(unique(info$SwitchingCost),2),
      #                   " \n(", unique(info$N_Fisheries)," fisheries & ", unique(info$N_Edges), " edges)"),
      caption = paste0(paste0(unique(info$N_Fisheries)," fisheries & ", unique(info$N_Edges), " edges"),
                       "\nN Strategies: ",round(unique(info$N_strategies),2)," | Modularity: ",round(unique(info$Modularity),2))) +
    theme(plot.margin = margin(5,5,5,5),
          plot.title = element_text(size=14),
          plot.subtitle = element_text(size=9, color="grey40"),
          plot.caption = element_text(size = 9, hjust = 0, color="grey40"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA))


}

(plots[[1]]+plots[[2]]+ plots[[3]])

dv <- c(1.6818,1.6788,1.8242)
wt <- c(4.3226,3.9118,3.9259)



# expected revenue --------------------------------------------------------

ER <- read_rds("../westcoast-networks/data/clean/Simulation/empiricalbenchmarks_meta.rds")$df_exprev

vessel_week_grid <- expand_grid(VESSEL_ID = unique(trsplit[[1]]$VESSEL_ID),
                                LANDING_WEEK = unique(trsplit[[1]]$LANDING_WEEK))

vesselclass <- trsplit[[1]] %>%
  distinct(VESSEL_ID, LengthBin)

wide_choices <- trsplit[[1]] %>%
  mutate(Value = 1) %>%
  select(VESSEL_ID, FisheryRegion, LANDING_WEEK, Value) %>%
  pivot_wider(id_cols = c(VESSEL_ID, LANDING_WEEK),
              names_from = FisheryRegion,
              values_from = Value,
              values_fill = 0)

df_long <- vessel_week_grid %>%
  left_join(vesselclass, by = c("VESSEL_ID")) %>%
  left_join(wide_choices, by = c("VESSEL_ID", "LANDING_WEEK")) %>%
  mutate(across(-c(VESSEL_ID, LengthBin, LANDING_WEEK), ~replace_na(.x, 0))) %>%
  mutate(`Not Fishing` = if_else(
    rowSums(across(-c(VESSEL_ID, LengthBin, LANDING_WEEK))) == 0,
    1, 0)) %>%
  pivot_longer(cols = -c(VESSEL_ID, LANDING_WEEK, LengthBin),
               names_to = "RegionFishery",
               values_to = "chosen") %>%
  left_join(ER %>%
              select(LANDING_WEEK, LengthBin, RegionFishery, ExpRev),
            by = c("LANDING_WEEK", "LengthBin", "RegionFishery")) %>%
  mutate(ExpRev = replace_na(ExpRev, 0))

df_ml <- df_long %>%
  unite(choice_id, VESSEL_ID, LANDING_WEEK, remove = FALSE)

library(mlogit)

dat <- mlogit.data(
  df_ml,
  choice = "chosen",
  shape = "long",
  alt.var = "RegionFishery",
  chid.var = "choice_id"
)

mxl <- mlogit(
  chosen ~ ExpRev,              # ASCs created automatically
  data = dat,
  # rpar = list(asc = "n"),       # random ASCs
  # heterosc = TRUE,              # allow ASC variation
  R = 200,
  halton = NA,
  reflevel = "Not Fishing"
)

pred_mat <- predict(mxl, newdata = df_ml, type = "probabilities")
pred_long <- pred_mat %>%
  as_tibble() %>%
  mutate(choice_id = unique(df_ml$choice_id)) %>%
  pivot_longer(
    cols = -choice_id,
    names_to = "RegionFishery",
    values_to = "pred_prob"
  )

df_pred <- df_ml %>%
  left_join(pred_long, by = c("choice_id", "RegionFishery")) %>%
  group_by(choice_id) %>%
  mutate(PredChoice = ifelse(pred_prob == max(pred_prob),1,0)) %>%
  filter(PredChoice == 1)


