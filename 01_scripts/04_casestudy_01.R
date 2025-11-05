rm(list = ls())

# collate -----------------------------------------------------------------

fileid <- "MLSizeCacheMeta2_"
base_name <- "~/westcoast-networks/data/Simulation/DynamicSimulationOutcomes/"

allfolders <- list(fl1 = list.files(paste0(base_name, fileid, 1, "/"), full.names = T),
                   fl2 = list.files(paste0(base_name, fileid, 2, "/"), full.names = T),
                   fl3 = list.files(paste0(base_name, fileid, 3, "/"), full.names = T),
                   fl4 = list.files(paste0(base_name, fileid, 4, "/"), full.names = T),
                   fl5 = list.files(paste0(base_name, fileid, 5, "/"), full.names = T),
                   fl6 = list.files(paste0(base_name, fileid, 6, "/"), full.names = T),
                   fl7 = list.files(paste0(base_name, fileid, 7, "/"), full.names = T),
                   fl8 = list.files(paste0(base_name, fileid, 8, "/"), full.names = T),
                   fl9 = list.files(paste0(base_name, fileid, 9, "/"), full.names = T),
                   fl10 = list.files(paste0(base_name, fileid, 10, "/"), full.names = T))
folder <- allfolders[[8]]
fs <- str_subset(folder,"114")

c <- 1

diverse <- map(1:length(folder), function(c){
  d <- read_rds(folder[c])
  print(c)
  # d <- read_rds(fs)
  if(length(d$sim_run) == 0){return(NULL)}
  diverse <- bind_rows(d$sim_run$cache_dfchoice[14]) %>%
    group_by(Vessel_ID) %>%
    filter(FISHERY_ID != "Not Fishing") %>%
    summarise(Diversification = n_distinct(FISHERY_ID))

  strategies <- bind_rows(d$sim_run$cache_dfchoice[14]) %>%
    group_by(Vessel_ID) %>%
    filter(FISHERY_ID != "Not Fishing") %>%
    summarize(
      n_fisheries = n_distinct(FISHERY_ID),
      strategy = paste(sort(unique(FISHERY_ID)), collapse = "-"),
      .groups = "drop"
    ) %>%
    filter(n_fisheries >= 2)

  #n edges check
  df_choice <- d$sim_run$cache_dfchoice[[14]]
  yi <- 14
  largestcc <- T
  fisherylist_use <- unique(df_choice$FISHERY_ID)[unique(df_choice$FISHERY_ID) != "Not Fishing"]
  n_edgeupdate <- graph_fuller_fcn(df_choice, yi, fisherylist_use, largestcc)
  if(nrow(n_edgeupdate) > 1){
    n_edgeupdate <- n_edgeupdate %>%
      mutate(Cluster = as.numeric(Cluster),
             iter = unique(d$sim_id$iter))
  }

  out <- tibble(iter = unique(d$sim_id$iter),
                N_diversified2 = sum(diverse$Diversification >= 2),
                N_diversified3 = sum(diverse$Diversification >= 3),
                N_diversified4 = sum(diverse$Diversification >= 4),
                N_diversified5 = sum(diverse$Diversification >= 5),
                MeanDiversification_All = mean(diverse$Diversification),
                MeanDiversification_Diverse = mean(diverse$Diversification[diverse$Diversification >= 2]),
                N_strategies = n_distinct(strategies$strategy))
  return(list(update = n_edgeupdate,
              diverse = out))

})
outfile <- paste0("~/westcoast-networks/data/clean/Simulation/diversification_",fileid,8,".rds")
write_rds(diverse, outfile)
diverse <- map_dfr(read_rds(paste0("~/westcoast-networks/data/clean/Simulation/diversification_",fileid,8,".rds")),
                   function(i){i$diverse})
newedges <- map_dfr(read_rds(paste0("~/westcoast-networks/data/clean/Simulation/diversification_",fileid,8,".rds")),
                    function(i){i$update}) %>%
  distinct(iter, N_Edges)



sum <- map_dfr(df_sim[[1]], function(i){i$network_pre_f}) %>%
  distinct(iter, vessels_in, SS_Vessels, Vessels1)

df_sim <- read_rds(paste0("/home/gkoss/westcoast-networks/data/clean/Simulation/projectiondataforML_",fileid,8,".rds"))
t <- map_dfr(df_sim[[1]], function(i){i$network_pre_f}) %>%
  distinct(SS_Vessels, SS_Fisheries, N_Fisheries, Vessels1, Vessels5, Vessels10, Return, iter) %>%
  mutate(Recover = ifelse(Return < 25, "Recovered", "New Steady State"),
         RecoverI = ifelse(Return < 25, 1, 0),
         Vessel1N = Vessels1*SS_Vessels,
         VesselDrop = SS_Vessels-Vessel1N) %>%
  distinct(SS_Vessels, SS_Fisheries, N_Fisheries, Vessels1, Vessel1N, Vessels5, Vessels10, VesselDrop, Return, iter) %>%
  mutate(YearPVesLost = Return/VesselDrop) %>%
  filter(YearPVesLost > 0,
         YearPVesLost < quantile(YearPVesLost,.975))

nstart_b <- map_dfr(df_sim[[1]], function(i){i$network_pre_f}) %>%
  group_by(iter) %>%
  mutate(Cluster = pmax(Cluster))

nstart <- nstart_b %>%
  mutate(DropT = map2_lgl(Fishery, Drop, ~ .x %in% .y)) %>%
  mutate(DropT = as.integer(DropT)) %>%
  # group_by(iter, Drop) %>%
  # mutate(Size = sum(Size),
  #        Closeness = mean(Closeness),
  #        Strength = mean(Strength),
  #        ClusteringCoefficient = mean(ClusteringCoefficient),
  #        FragmentationCentrality = mean(FragmentationCentrality)) %>%
  # ungroup() %>%
  filter(DropT == 1) %>%
  # distinct(iter, .keep_all = T) %>%
  left_join(newedges, by = "iter")

td <- nstart %>%
  mutate(N_Edges = N_Edges.y,
         Vessel1N = Vessels1*SS_Vessels,
         VesselDrop = SS_Vessels-Vessel1N,
         YearPVesLost = Return/VesselDrop,
         MedianToCentroid = ifelse(HullArea == 0, 0, MedianToCentroid),
         MeanToCentroid = ifelse(HullArea == 0, 0, MeanToCentroid),
         Big = ifelse(SS_Fisheries > 26,1,0)) %>%
  filter(YearPVesLost > 0,
         VesselDrop > 5) %>%
  left_join(diverse, by = "iter")

ggplot(td, aes(x = N_diversified2, y = FragmentationCentrality)) +
  geom_point() + theme_minimal() +
  ggplot(td, aes(x = MeanDiversification_All, y = FragmentationCentrality)) +
  geom_point() + theme_minimal() +
  ggplot(td, aes(x = N_strategies/SS_Vessels, y = FragmentationCentrality)) +
  geom_point() + theme_minimal()

f <- feols(YearPVesLost ~ SS_Vessels*(N_strategies + Size + ClusteringCoefficient + Mean_Weight + Modularity), td)
f

tdcase <- td %>%
  select(iter, SS_Vessels, SS_Fisheries, SS_Weeks, SS_Revenue, FishCosts, SwitchCosts, Distparameter, N_strategies, N_diversified2, N_diversified3,
         MeanDiversification_All, MeanDiversification_Diverse, Vessels1, Return, VesselDrop, YearPVesLost,
         Size, Closeness, Strength, ClusteringCoefficient, FragmentationCentrality,
         # SizeM, ClosenessM, StrengthM, ClusteringCoefficientM, FragmentationCentralityM,
         Mean_Weight, N_Edges, N_Fisheries, FragMeasure, SI, H, ClusteringCoefficient_Global, Modularity, NetworkCentralization,
         URF, HullArea, HullAreaTrimmed, MedianToCentroid, MeanToCentroid) %>%
  group_by(iter) %>%
  mutate(Size = mean(Size),
         Closeness = mean(Closeness),
         Strength = mean(Strength),
         ClusteringCoefficient = mean(ClusteringCoefficient),
         FragmentationCentrality = mean(FragmentationCentrality)) %>%
  distinct(iter, .keep_all = T)

small1 <- tdcase %>%
  filter(SS_Vessels > 160,
         SS_Vessels < 180) %>%
  filter(iter %in% c(224,417,114)) # all diversification hurts

small2 <- tdcase %>%
  filter(SS_Vessels > 140,
         SS_Vessels < 160) %>%
  filter(iter %in% c(402, 386, 332))

small3 <- tdcase %>%
  filter(SS_Vessels > 120,
         SS_Vessels < 140) %>%
  filter(iter %in% c(493,220,201)) # all diversification hurts

small4 <- tdcase %>%
  filter(SS_Vessels > 120,
         SS_Vessels < 140)  %>%
  filter(iter %in% c(249,107,33))

large1 <- tdcase %>%
  filter(SS_Vessels > 280,
         SS_Vessels < 295) %>%
  filter(iter %in% c(50,191,170)) #230 #diversification helps

large2 <- tdcase %>%
  filter(SS_Vessels > 250,
         SS_Vessels < 275) %>%
  filter(iter %in% c(137,329,272)) # more strategies

large3 <- tdcase %>%
  filter(SS_Vessels > 230,
         SS_Vessels < 250) %>%
  filter(iter %in% c(100, 383, 284)) # more strategies & diversification helps
# %>%
#   #275-290
large4 <- tdcase %>%
  filter(SS_Vessels > 240,
         SS_Vessels < 260) %>%
  filter(iter %in% c(41, 459, 59, 169, 285))

large5 <- tdcase %>%
  filter(SS_Vessels > 290,
         SS_Vessels < 310) %>%
  filter(iter %in% c(40,296))

large6 <- tdcase %>%
  filter(SS_Vessels > 180,
         SS_Vessels < 200) %>%
  filter(iter %in% c(223,343,140,495,282))

plot(td$N_strategies, td$FragmentationCentrality)
plot(td$N_diversified2, td$FragmentationCentrality)
ggplot(td %>%
         filter(FragmentationCentrality > 0), aes(x = N_strategies, y = FragmentationCentrality, color = N_diversified2)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3))

f <- feols(YearPVesLost ~ N_strategies*(N_diversified2 + SS_Vessels), td)
f

gridw <- expand.grid(
  N_strategies = seq(min(td$N_strategies, na.rm = TRUE),
                   max(td$N_strategies, na.rm = TRUE), length.out = 50),
  N_diversified2 = seq(min(td$N_diversified2, na.rm = TRUE),
                    max(td$N_diversified2, na.rm = TRUE), length.out = 50)
)

# gridw$SS_Vessels <- 100
# gridw$SS_Vessels <- 200
# gridw$SS_Vessels <- 300
#
# # Predict from your model
# gridw$Pred <- predict(f, newdata = gridw)

# Plot
g1 <- ggplot(gridw %>%
               mutate(Pred = predict(f, newdata = gridw %>%
                                       mutate(SS_Vessels = 130))) %>%
               filter(Pred > 0),
             aes(y = N_strategies, x = N_diversified2)) +
  geom_tile(aes(fill = Pred)) +
  geom_contour(aes(z = Pred), color = "grey60", alpha = 0.5) +
  scale_fill_viridis_c(name = "Predicted\nYears Per Vessel Lost", option = "magma", direction = -1, limits = c(0,2)) +
  labs(
    y = "Fleet Diversification (# Unique Strategies)",
    x = "Vessel Diversification (# Diversified Vessels)",
    # title = "Years Per Vessel Lost",
    subtitle = "Small Fleet (130 Vessels)"
  ) +
  ggthemes::theme_tufte() +
  theme(legend.position = "bottom")
# g1
g2 <- ggplot(gridw %>%
               mutate(Pred = predict(f, newdata = gridw %>%
                                       mutate(SS_Vessels = 200))) %>%
               filter(Pred > 0), aes(y = N_strategies, x = N_diversified2)) +
  geom_tile(aes(fill = Pred)) +
  geom_contour(aes(z = Pred), color = "grey60", alpha = 0.5) +
  scale_fill_viridis_c(name = "Predicted\nYears Per Vessel Lost", option = "magma", direction = -1, limits = c(0,2)) +
  labs(
    y = "Fleet Diversification (# Unique Strategies)",
    x = "Vessel Diversification (# Diversified Vessels)",
    # title = "Years Per Vessel Lost",
    subtitle = "Medium Fleet (200 Vessels)"
  ) +
  ggthemes::theme_tufte() +
  theme(legend.position = "bottom")

g3 <- ggplot(gridw %>%
               mutate(Pred = predict(f, newdata = gridw %>%
                                       mutate(SS_Vessels = 280))) %>%
               filter(Pred > 0), aes(y = N_strategies, x = N_diversified2)) +
  geom_tile(aes(fill = Pred)) +
  geom_contour(aes(z = Pred), color = "grey60", alpha = 0.5) +
  scale_fill_viridis_c(name = "Predicted\nYears Per Vessel Lost", option = "magma", direction = -1, limits = c(0,2)) +
  labs(
    y = "Fleet Diversification (# Unique Strategies)",
    x = "Vessel Diversification (# Diversified Vessels)",
    # title = "Years Per Vessel Lost",
    subtitle = "Large Fleet (280 Vessels)"
  ) +
  ggthemes::theme_tufte() +
  theme(legend.position = "bottom")

g1+g2+g3+plot_layout(axes = "collect", guides = "collect") &
  theme(legend.position='bottom')

tdd <- tdcase %>%
  mutate(Dpv = N_strategies/N_diversified2)
f2 <- feols(YearPVesLost ~ Dpv*SS_Vessels, tdd)

gridn <- expand.grid(
  Dpv = seq(min(tdd$Dpv, na.rm = TRUE),
                     max(tdd$Dpv, na.rm = TRUE), length.out = 50),
  SS_Vessels = seq(min(tdd$SS_Vessels, na.rm = TRUE),
                       max(tdd$SS_Vessels, na.rm = TRUE), length.out = 50)
)

# Predict from your model
gridn$Pred <- predict(f2, newdata = gridn)

# Plot
gn <- ggplot(gridn %>%
               filter(Pred > 0),
             aes(x = SS_Vessels, y = Dpv)) +
  geom_tile(aes(fill = Pred)) +
  geom_contour(aes(z = Pred), color = "grey60", alpha = 0.5) +
  scale_fill_viridis_c(name = "Predicted\nYears Per Vessel Lost", option = "magma", direction = -1) +
  labs(
    x = "Fleet Size",
    y = "Strategies / Vessel"
    # title = "Years Per Vessel Lost",
    # subtitle = "Small Fleet (100 Vessels)"
  ) +
  ggthemes::theme_tufte() +
  theme(legend.position = "bottom")
gn

ggplot(tdd, aes(x = SS_Vessels, y = Dpv, color = YearPVesLost)) +
  geom_point(size = 3, alpha = .8) +
  scale_color_viridis_c(name = "Predicted\nYears Per Vessel Lost", option = "magma", direction = -1)

# case fleshed -------------------------------------------------------------


