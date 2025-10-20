df_sim <- read_rds(paste0("/home/gkoss/westcoast-networks/data/clean/Simulation/projectiondataforML_",fileid,8,".rds"))
nstart <- map_dfr(df_sim[[1]], function(i){i$network_pre_f})
# %>%
#   group_by(iter) %>%
#   mutate(Cluster = pmax(Cluster)) %>%
  # distinct(iter, .keep_all = T)

nsmall <- nstart %>%
  filter(SS_Vessels > 100,
         SS_Vessels < 170) %>%
  mutate(Vessel1N = Vessels1*SS_Vessels,
         VesselDrop = SS_Vessels-Vessel1N,
         YearPVesLost = Return/VesselDrop,
         MedianToCentroid = ifelse(HullArea == 0, 0, MedianToCentroid),
         MeanToCentroid = ifelse(HullArea == 0, 0, MeanToCentroid),
         Big = ifelse(SS_Fisheries > 26,1,0)) %>%
  filter(YearPVesLost > 0,
         VesselDrop > 5)

nmed <- nstart %>%
  filter(SS_Vessels > 170,
         SS_Vessels < 220) %>%
  mutate(Vessel1N = Vessels1*SS_Vessels,
         VesselDrop = SS_Vessels-Vessel1N,
         YearPVesLost = Return/VesselDrop,
         MedianToCentroid = ifelse(HullArea == 0, 0, MedianToCentroid),
         MeanToCentroid = ifelse(HullArea == 0, 0, MeanToCentroid),
         Big = ifelse(SS_Fisheries > 26,1,0)) %>%
  filter(YearPVesLost > 0,
         VesselDrop > 5)

nlarge <- nstart %>%
  filter(SS_Vessels > 220) %>%
  mutate(Vessel1N = Vessels1*SS_Vessels,
         VesselDrop = SS_Vessels-Vessel1N,
         YearPVesLost = Return/VesselDrop,
         MedianToCentroid = ifelse(HullArea == 0, 0, MedianToCentroid),
         MeanToCentroid = ifelse(HullArea == 0, 0, MeanToCentroid),
         Big = ifelse(SS_Fisheries > 26,1,0)) %>%
  filter(YearPVesLost > 0,
         VesselDrop > 5)

nall <- list(nsmall, nmed, nlarge)
gmed <- list()
gk <- list()
l <- 1
for (l in 1:3){

  n <- nall[[l]]
  for (o in 1:length(cg)){
    i <- cg[o]
    gmed[[o]] <- ggplot(n, aes(x = !!sym(i), y = YearPVesLost, color = SS_Vessels)) + geom_point(size = 3, alpha = .9) + theme_minimal() +
      scale_color_gradient(low = "#E9AFA3", high = "#432323")
  }
  gk[[l]] <- (gmed[[1]] + gmed[[2]] + gmed[[3]]) /
    (gmed[[4]] + gmed[[5]] + gmed[[6]]) + plot_layout(guides = "collect")

}


# next --------------------------------------------------------------------

library(fixest)
ntogether <- nstart %>%
  mutate(Vessel1N = Vessels1*SS_Vessels,
         VesselDrop = SS_Vessels-Vessel1N,
         YearPVesLost = Return/VesselDrop,
         # MedianToCentroid = ifelse(HullArea == 0, 0, MedianToCentroid),
         # MeanToCentroid = ifelse(HullArea == 0, 0, MeanToCentroid),
         Big = ifelse(SS_Fisheries > 26,1,0),
         SizeClass = case_when(SS_Vessels > 100 &
                                 SS_Vessels < 170 ~ 1,
                               SS_Vessels > 170 &
                                 SS_Vessels < 220 ~ 2,
                               SS_Vessels > 220 ~ 3)) %>%
  filter(YearPVesLost > 0,
         VesselDrop > 5) %>%
  ungroup() %>%
  filter(!is.na(Mean_Weight),
         !is.na(SizeClass),
         !is.na(SI),
         # !is.na(HullArea),
         !is.na(Modularity),
         !is.na(SS_Vessels),
         !is.na(SS_Fisheries))

f <- feols(Vessels1 ~ SS_Vessels*(SizeClass + Mean_Weight + SI + Modularity + SS_Fisheries), ntogether)
f
summary(f)

nsecond <- ntogether  %>%
  mutate(Fitted = fitted(f))

f2 <- feols(Return ~ Fitted, nsecond)
f2

fc <- feols(YearPVesLost ~  SS_Vessels*(SizeClass + Mean_Weight + Modularity + HullArea + SS_Fisheries), ntogether)
# fc <- feols(YearPVesLost ~  SS_Fisheries*(SizeClass + Mean_Weight + HullArea + Modularity + SS_Vessels), ntogether)
summary(fc)

# summary(lm(fitted(f2) ~ fitted(fc)))

gridw <- expand.grid(
  SS_Vessels = seq(min(ntogether$SS_Vessels, na.rm = TRUE),
                  max(ntogether$SS_Vessels, na.rm = TRUE), length.out = 50),
  # SS_Fisheries = seq(min(ntogether$SS_Fisheries, na.rm = TRUE),
  #                  max(ntogether$SS_Fisheries, na.rm = TRUE), length.out = 50),
  Mean_Weight = seq(min(ntogether$Mean_Weight, na.rm = TRUE),
                    max(ntogether$Mean_Weight, na.rm = TRUE), length.out = 50)
)

gridw$SI <- mean(ntogether$SI, na.rm = TRUE)
gridw$HullArea <- mean(ntogether$HullArea, na.rm = TRUE)
gridw$N_Edges <- mean(ntogether$N_Edges, na.rm = TRUE)
gridw$Modularity <- mean(ntogether$Modularity, na.rm = TRUE)
gridw$SizeClass <- mean(ntogether$SizeClass, na.rm = TRUE)
gridw$SS_Fisheries <- mean(ntogether$SS_Fisheries, na.rm = TRUE)
# gridw$SS_Vessels <- mean(ntogether$SS_Vessels, na.rm = TRUE)

# Predict from your model
gridw$Pred <- predict(fc, newdata = gridw)

# Plot
ggplot(gridw %>%
         filter(Pred > 0), aes(x = Mean_Weight, y = SS_Vessels)) +
  geom_tile(aes(fill = Pred)) +
  geom_contour(aes(z = Pred), color = "grey60", alpha = 0.5) +
  scale_fill_viridis_c(name = "Predicted\nYears Per Vessel Lost", option = "magma", direction = -1) +
  labs(
    x = "Mean Weight",
    y = "Fleet Size",
    title = "Years Per Vessel Lost",
    subtitle = "Mean Weight (Fuller Projection)"
  ) +
  ggthemes::theme_tufte()

grid <- expand.grid(
  SS_Vessels = seq(min(ntogether$SS_Vessels, na.rm = TRUE),
                  max(ntogether$SS_Vessels, na.rm = TRUE), length.out = 50),
  # SS_Fisheries = seq(min(ntogether$SS_Fisheries, na.rm = TRUE),
  #                    max(ntogether$SS_Fisheries, na.rm = TRUE), length.out = 50),
  Modularity = seq(min(ntogether$Modularity, na.rm = TRUE),
                    max(ntogether$Modularity, na.rm = TRUE), length.out = 50)
)

grid$N_Edges <- mean(ntogether$N_Edges, na.rm = TRUE)
grid$SI <- mean(ntogether$SI, na.rm = TRUE)
grid$HullArea <- mean(ntogether$HullArea, na.rm = TRUE)
grid$Mean_Weight <- mean(ntogether$Mean_Weight, na.rm = TRUE)
grid$SizeClass <- mean(ntogether$SizeClass, na.rm = TRUE)
grid$SS_Fisheries <- mean(ntogether$SS_Fisheries, na.rm = TRUE)

# Predict from your model
grid$Pred <- predict(fc, newdata = grid)

# Plot
ggplot(grid %>%
         filter(Pred > 0), aes(x = Modularity, y = SS_Vessels)) +
  geom_tile(aes(fill = Pred)) +
  geom_contour(aes(z = Pred), color = "grey60", alpha = 0.5) +
  scale_fill_viridis_c(name = "Predicted\nYears Per Vessel Lost", option = "magma", direction = -1) +
  labs(
    x = "Modularity",
    y = "Fleet Size",
    title = "Years Per Vessel Lost",
    subtitle = "Modularity (Transition Projection)"
  ) +
  ggthemes::theme_tufte()








