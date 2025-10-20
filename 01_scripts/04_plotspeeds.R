library(patchwork)#
# write_rds(list(cache_dfchoice = cache_dfchoice,
#                yi = yi,
#                fisherylist_use = fisherylist_use,
#                drop = drop), "/home/gkoss/westcoast-networks/data/choicetempcache.rds")
# cachetemp <- read_rds("/home/gkoss/westcoast-networks/data/choicetempcache.rds")
# cache_dfchoice <- cachetemp$cache_dfchoice
# yi <- 14
# fisherylist_use <- cachetemp$fisherylist_use
# drop <- cachetemp$drop
# vessel_pairs <- expand.grid(Vessel1 = as.character(unique(cache_dfchoice[[yi]]$Vessel_ID)),
#                             Vessel2 = as.character(unique(cache_dfchoice[[yi]]$Vessel_ID)), stringsAsFactors = FALSE) %>%
#   filter(Vessel1 != Vessel2) %>%
#   arrange(Vessel1, Vessel2)
#
# g <- graph_ext_fcn_ves(cache_dfchoice[[yi]], fisherylist_use, vessel_pairs, yi, drop)
# g <- graph_fuller_fcn_ves(cache_dfchoice[[yi]], yi, fisherylist_use, drop)
# g <- graph_bip_fcn(cache_dfchoice[[yi]], yi, nfisheries)


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

p1 <- ggplot(t, aes(x = SS_Vessels, y = VesselDrop, color = YearPVesLost)) + geom_point() + theme_minimal() +
  labs(x = "Pre-Shock Fleet Size",
       y = "# Vessels Exit") +
  scale_color_viridis_c(option = "magma", direction = -1) +
  geom_abline(intercept = 0, slope = 1/4, color = "grey", data = NULL)
p2 <- ggplot(t, aes(x = SS_Vessels, y = Vessel1N, color = YearPVesLost)) + geom_point() + theme_minimal() +
  labs(x = "Pre-Shock Fleet Size",
       y = "# Vessels Remaining")+
  scale_color_viridis_c(option = "magma", direction = -1) +
  geom_abline(intercept = 0, slope = 3/4, color = "grey", data = NULL)
p3 <- ggplot(t, aes(x = SS_Vessels, y = Return, color = YearPVesLost)) + geom_point() + theme_minimal() +
  labs(x = "Pre-Shock Fleet Size",
       y = "Years to Return")+
  scale_color_viridis_c(option = "magma", direction = -1)

p4 <- ggplot(t, aes(x = Return, y = YearPVesLost, color = SS_Vessels)) + geom_point() + theme_minimal() +
  labs(x = "Years to Return",
       y = "Years / Vessel Exit")+
  scale_color_viridis_c(option = "magma", direction = 1)
p5 <- ggplot(t, aes(x = VesselDrop, y = YearPVesLost, color = SS_Vessels)) + geom_point() + theme_minimal() +
  labs(x = "# Vessel Exit",
       y = "Years / Vessel Exit")+
  scale_color_viridis_c(option = "magma", direction = 1)

(p1+p2+p3)/(p4+p5)+plot_layout(guides = "collect")+plot_annotation(caption = "grey line with slope 1/4, showing the simulations in which >1/4 of vessels exited")


f <- fixest::feols(t, Vessels1 ~ SS_Vessels+SS_Fisheries)
f

f <- fixest::feols(t, Return ~ SS_Vessels+SS_Fisheries)
f

f <- fixest::feols(t, YearPVesLost ~ SS_Vessels*SS_Fisheries)
f
b0 <- coef(f)[[1]]
b1 <- coef(f)[[2]]
b2 <- coef(f)[[3]]
b3 <- coef(f)[[4]]

t_gridplot <- expand_grid(SS_Vessels = seq(20,350,1),
                          SS_Fisheries = seq(7,55,1)) %>%
  mutate(YearPVesLost = b0 + b1 * SS_Vessels + b2 * SS_Fisheries + b3 * SS_Vessels * SS_Fisheries) %>%
  filter(YearPVesLost >= 0)

# ggplot(t, aes(x = SS_Vessels, y = SS_Fisheries,
#               size = Return)) +
#   geom_point(alpha = .7) +
#   labs(title = "Speed of Recovery per Exited Vessel",
#        subtitle = "Years for the simulation to retrn to its pre-shock fleet size
#        per vessel that exited during the shock",
#        x = "Fleet Size (pre-shock)",
#        y = "# Fisheries (pre-shock)") +
#   ggthemes::theme_tufte()

ggplot(t, aes(x = SS_Vessels, y = SS_Fisheries,
              size = YearPVesLost)) +
  geom_contour_filled(
    data = t_gridplot,
    aes(x = SS_Vessels, y = SS_Fisheries, z = YearPVesLost),
    alpha = .5,
    # color = "grey50",
    size = 0.6,
    bins = 8
  ) +
  geom_point(alpha = .7) +
  scale_fill_viridis_d(option = "magma", direction = -1) +
  labs(title = "Speed of Recovery per Exited Vessel",
       subtitle = "Years for the simulation to retrn to its pre-shock fleet size
       per vessel that exited during the shock",
       x = "Fleet Size (pre-shock)",
       y = "# Fisheries (pre-shock)",
       fill = "Fitted \nYears / ExitedVessel",
       size = "Observed \nYears / ExitedVessel") +
  ggthemes::theme_tufte()

sv <- seq(30,330,20)
nf <- seq(0,55,2)
t_grid <- expand_grid(SS_Vessels = sv,
                      N_Fisheries = nf)

ranges <- map_dfr(1:(nrow(t_grid)-length(nf)-1), function(i){

  s1 <- t_grid$SS_Vessels[[i]]
  s2 <- t_grid$SS_Vessels[[i+length(nf)+1]]
  f1 <- t_grid$N_Fisheries[[i]]
  f2 <- t_grid$N_Fisheries[[i+length(nf)+1]]

  tsquare <- t %>%
    filter(SS_Vessels > s1,
           SS_Vessels < s2,
           SS_Fisheries > f1,
           SS_Fisheries < f2)

  if(nrow(tsquare) == 0){return(NULL)}

  out <- tibble(SS_Min = s1,
                SS_Max = s2,
                NF_Min = f1,
                NF_Max = f2,
                Obs = nrow(tsquare),
                ShockRange = max(tsquare$VesselDrop)-min(tsquare$VesselDrop),
                ShockMean = mean(tsquare$VesselDrop),
                ReturnRange = max(tsquare$Return)-min(tsquare$Return),
                ReturnMean = mean(tsquare$Return),
                SpeedRange = max(tsquare$YearPVesLost)-min(tsquare$YearPVesLost),
                SpeedMean = mean(tsquare$YearPVesLost))

})

ggplot(ranges) +
  geom_rect(aes(xmin = SS_Min,xmax = SS_Max, ymin = NF_Min,ymax = NF_Max,
                fill = Obs),
            color = "white", linewidth = 0.4) +
  scale_fill_gradient(low = "#AAFAC8", high = "#2D728B") +
  labs(
    x = "Number of vessels",
    y = "Number of fisheries",
    subtitle = "Observations",
    fill = "# Simulations"
  ) +
  ggthemes::theme_tufte() +
  theme(panel.border = element_blank(),
        axis.ticks = element_blank())

g1 <- ggplot(ranges) +
  geom_rect(aes(xmin = SS_Min,xmax = SS_Max, ymin = NF_Min,ymax = NF_Max,
                fill = ShockMean),
            color = "white", linewidth = 0.4) +
  scale_fill_gradient(low = "#AAFAC8", high = "#2D728B") +
  labs(
    x = "Number of vessels",
    y = "Number of fisheries",
    subtitle = "Mean Shock Magnitude",
    fill = "# Vessels Exited"
  ) +
  ggthemes::theme_tufte() +
  theme(panel.border = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom")

g2 <- ggplot(ranges) +
  geom_rect(aes(xmin = SS_Min,xmax = SS_Max, ymin = NF_Min,ymax = NF_Max,
                fill = ReturnMean),
            color = "white", linewidth = 0.4) +
  scale_fill_gradient(low = "#AAFAC8", high = "#2D728B") +
  labs(
    x = "Number of vessels",
    y = "Number of fisheries",
    subtitle = "Mean Shock Duration",
    fill = "Years to Return"
  ) +
  ggthemes::theme_tufte() +
  theme(panel.border = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom")

g3 <- ggplot(ranges) +
  geom_rect(aes(xmin = SS_Min,xmax = SS_Max, ymin = NF_Min,ymax = NF_Max,
                fill = SpeedMean),
            color = "white", linewidth = 0.4) +
  scale_fill_gradient(low = "#AAFAC8", high = "#2D728B") +
  labs(
    x = "Number of vessels",
    y = "Number of fisheries",
    subtitle = "Mean Recovery Speed",
    fill = "Years per Vessel Exited"
  ) +
  ggthemes::theme_tufte() +
  theme(panel.border = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom")

g1+g2+g3


# Now with networks -------------------------------------------------------

nstart <- map_dfr(df_sim[[1]], function(i){i$network_pre_f}) %>%
  group_by(iter) %>%
  mutate(Cluster = pmax(Cluster)) %>%
  distinct(iter, .keep_all = T)

# nall <- nstart %>%
#   filter(SS_Vessels > 100,
#          SS_Vessels < 150) %>%
#   mutate(Vessel1N = Vessels1*SS_Vessels,
#          VesselDrop = SS_Vessels-Vessel1N,
#          YearPVesLost = Return/VesselDrop)

nvconstant <- nstart %>%
  filter(SS_Vessels > 175,
         SS_Vessels < 225) %>%
  mutate(Vessel1N = Vessels1*SS_Vessels,
         VesselDrop = SS_Vessels-Vessel1N,
         YearPVesLost = Return/VesselDrop,
         MedianToCentroid = ifelse(HullArea == 0, 0, MedianToCentroid),
         MeanToCentroid = ifelse(HullArea == 0, 0, MeanToCentroid),
         Big = ifelse(SS_Fisheries > 26,1,0)) %>%
  filter(YearPVesLost > 0,
         VesselDrop > 5)
# ,
#          YearPVesLost < quantile(YearPVesLost,.975))

nfconstant <- nstart %>%
  filter(SS_Fisheries > 30,
         SS_Fisheries < 35) %>%
  mutate(Vessel1N = Vessels1*SS_Vessels,
         VesselDrop = SS_Vessels-Vessel1N,
         YearPVesLost = Return/VesselDrop,
         MedianToCentroid = ifelse(HullArea == 0, 0, MedianToCentroid),
         MeanToCentroid = ifelse(HullArea == 0, 0, MeanToCentroid),
         Big = ifelse(SS_Vessels > 180,1,0)) %>%
  filter(YearPVesLost > 0,
         VesselDrop > 5)
# ,
#          YearPVesLost < quantile(YearPVesLost,.975))

# vessels constant; node level --------------------------------------------

nn1 <- ggplot(nvconstant, aes(x = SS_Fisheries, y = Strength, color = YearPVesLost)) + geom_point() + theme_minimal() +
  labs(x = "Pre-Shock Fishery Breadth",
       y = "Node Strength") +
  geom_smooth(method = "lm", color = "grey40") +
  scale_color_viridis_c(option = "magma", direction = -1)
nn2 <- ggplot(nvconstant, aes(x = SS_Fisheries, y = Closeness, color = YearPVesLost)) + geom_point() + theme_minimal() +
  labs(x = "Pre-Shock Fleet Size",
       y = "Node Closeness")+
  geom_smooth(method = "lm", color = "grey40") +
  scale_color_viridis_c(option = "magma", direction = -1)
nn3 <- ggplot(nvconstant, aes(x = SS_Fisheries, y = ClusteringCoefficient, color = YearPVesLost)) + geom_point() + theme_minimal() +
  labs(x = "Pre-Shock Fleet Size",
       y = "Density at Node")+
  geom_smooth(method = "lm", color = "grey40") +
  scale_color_viridis_c(option = "magma", direction = -1)
nn4 <- ggplot(nvconstant, aes(x = SS_Fisheries, y = FragmentationCentrality, color = YearPVesLost)) + geom_point() + theme_minimal() +
  labs(x = "Pre-Shock Fleet Size",
       y = "Fragmentation with Node Removal")+
  geom_smooth(method = "lm", color = "grey40") +
  scale_color_viridis_c(option = "magma", direction = -1)

nn1+nn2+nn3+nn4+plot_layout(guides = "collect")

nno1 <- ggplot(nvconstant, aes(x = Strength, y = YearPVesLost)) + geom_point() + theme_minimal() +
  # labs(x = "Pre-Shock Fishery Breadth",
  #      y = "Node Strength") +
  geom_smooth(method = "lm", color = "grey40")
nno2 <- ggplot(nvconstant, aes(x = Closeness, y = YearPVesLost)) + geom_point() + theme_minimal() +
  # labs(x = "Pre-Shock Fleet Size",
  #      y = "Node Closeness")+
  geom_smooth(method = "lm", color = "grey40")
nno3 <- ggplot(nvconstant, aes(x = ClusteringCoefficient, y = YearPVesLost)) + geom_point() + theme_minimal() +
  # labs(x = "Pre-Shock Fleet Size",
  #      y = "Density at Node")+
  geom_smooth(method = "lm", color = "grey40")
nno4 <- ggplot(nvconstant, aes(x = FragmentationCentrality, y = YearPVesLost)) + geom_point() + theme_minimal() +
  # labs(x = "Pre-Shock Fleet Size",
  #      y = "Fragmentation with Node Removal")+
  geom_smooth(method = "lm", color = "grey40")

nno1+nno2+nno3+nno4


# vessels constant; graph level --------------------------------------------

# mw, n_fisheries, ClusteringCoefficient_Global

ggplot(nvconstant, aes(x = SS_Vessels, y = SS_Fisheries, color = Mean_Weight, size = N_Edges)) +
  scale_size_continuous(range = c(1, 20)) +
  geom_point(alpha = .8) +
  theme_minimal()
# +
#   coord_cartesian(xlim = c(75, 200), ylim = c(10, 55))

cg <- colnames(nfconstant)[8:22]
cg <- c("Mean_Weight", "N_Edges", "ClusteringCoefficient_Global",
        "HullArea", "Modularity", "H")

gssize <- list()
gv1 <- list()
gr <- list()
gypv <- list()
for (o in 1:length(cg)){
  i <- cg[o]
  gssize[[o]] <- ggplot(nfconstant, aes(x = SS_Vessels, y = !!sym(i), color = YearPVesLost)) + geom_point(size = 3) + theme_minimal() +
    labs(x = "Pre-Shock Fishery Breadth") +
    scale_color_viridis_c(option = "magma", direction = -1)

  gv1[[o]] <- ggplot(nfconstant
                      # %>% filter(HullAreaTrimmed>0)
                      , aes(x = !!sym(i), y = Vessels1, color = SS_Vessels)) + geom_point(size = 4, alpha = .8) + theme_minimal() +
    scale_color_gradient(low = "#F3D4CE", high = "#281515") +
    ggthemes::theme_tufte()
  gr[[o]] <- ggplot(nfconstant
                      # %>% filter(HullAreaTrimmed>0)
                      , aes(x = !!sym(i), y = Return, color = SS_Vessels)) + geom_point(size = 4, alpha = .8) + theme_minimal() +
    scale_color_gradient(low = "#F3D4CE", high = "#281515") +
    ggthemes::theme_tufte()
  gypv[[o]] <- ggplot(nfconstant
         # %>% filter(HullAreaTrimmed>0)
         , aes(x = !!sym(i), y = YearPVesLost, color = SS_Vessels)) + geom_point(size = 4, alpha = .8) + theme_minimal() +
    scale_color_gradient(low = "#F3D4CE", high = "#281515") +
    ggthemes::theme_tufte()
}

(gv1[[1]] + gv1[[2]] + gv1[[3]]) /
  (gv1[[4]] + gv1[[5]] + gv1[[6]]) + plot_layout(guides = "collect") +
  plot_annotation(title = "Shock Magnitude",
                  subtitle = "Holding Fisheries Constant (30-35)")
(gr[[1]] + gr[[2]] + gr[[3]]) /
  (gr[[4]] + gr[[5]] + gr[[6]]) + plot_layout(guides = "collect") +
  plot_annotation(title = "Years to Return",
                  subtitle = "Holding Fisheries Constant (30-35)")
(gypv[[1]] + gypv[[2]] + gypv[[3]]) /
  (gypv[[4]] + gypv[[5]] + gypv[[6]]) + plot_layout(guides = "collect") +
  plot_annotation(title = "Years / Shock Magnitude",
                  subtitle = "Holding Fisheries Constant (30-35)")

gssize2 <- list()
gv12 <- list()
gr2 <- list()
gypv2 <- list()
for (o in 1:length(cg)){
  i <- cg[o]
  gssize2[[o]] <- ggplot(nvconstant, aes(x = SS_Fisheries, y = !!sym(i), color = YearPVesLost)) + geom_point(size = 3) + theme_minimal() +
    labs(x = "Pre-Shock Fishery Breadth") +
    scale_color_viridis_c(option = "magma", direction = -1)

  gv12[[o]] <- ggplot(nvconstant
                     # %>% filter(HullAreaTrimmed>0)
                     , aes(x = !!sym(i), y = Vessels1, color = SS_Fisheries)) + geom_point(size = 4, alpha = .8) + theme_minimal() +
    scale_color_gradient(low = "#F3D4CE", high = "#281515") +
    ggthemes::theme_tufte()
  gr2[[o]] <- ggplot(nvconstant
                    # %>% filter(HullAreaTrimmed>0)
                    , aes(x = !!sym(i), y = Return, color = SS_Fisheries)) + geom_point(size = 4, alpha = .8) + theme_minimal() +
    scale_color_gradient(low = "#F3D4CE", high = "#281515") +
    ggthemes::theme_tufte()
  gypv2[[o]] <- ggplot(nvconstant
                      # %>% filter(HullAreaTrimmed>0)
                      , aes(x = !!sym(i), y = YearPVesLost, color = SS_Fisheries)) + geom_point(size = 4, alpha = .8) + theme_minimal() +
    scale_color_gradient(low = "#F3D4CE", high = "#281515") +
    ggthemes::theme_tufte()
  # scale_color_gradient(low = "#AAFAC8", high = "#2D728B")
  # +
  #   geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), color = "grey40")
}

(gv12[[1]] + gv12[[2]] + gv12[[3]]) /
  (gv12[[4]] + gv12[[5]] + gv12[[6]]) + plot_layout(guides = "collect") +
  plot_annotation(title = "Shock Magnitude",
                  subtitle = "Holding Vessels Constant (175-225)")
(gr2[[1]] + gr2[[2]] + gr2[[3]]) /
  (gr2[[4]] + gr2[[5]] + gr2[[6]]) + plot_layout(guides = "collect") +
  plot_annotation(title = "Years to Return",
                  subtitle = "Holding Vessels Constant (175-225)")
(gypv2[[1]] + gypv2[[2]] + gypv2[[3]])/
(gypv2[[4]] + gypv2[[5]] + gypv2[[6]]) + plot_layout(guides = "collect") +
  plot_annotation(title = "Years / Shock Magnitude",
                  subtitle = "Holding Vessels Constant (175-225)")



# comp sizes --------------------------------------------------------------

nmed <- nstart %>%
  filter(SS_Vessels > 180,
         SS_Vessels < 210) %>%
  mutate(Vessel1N = Vessels1*SS_Vessels,
         VesselDrop = SS_Vessels-Vessel1N,
         YearPVesLost = Return/VesselDrop,
         MedianToCentroid = ifelse(HullArea == 0, 0, MedianToCentroid),
         MeanToCentroid = ifelse(HullArea == 0, 0, MeanToCentroid),
         Big = ifelse(SS_Fisheries > 26,1,0)) %>%
  filter(YearPVesLost > 0,
         VesselDrop > 5)

gmed <- list()
for (o in 1:length(cg)){
  i <- cg[o]
  gssize[[o]] <- ggplot(nfconstant, aes(x = SS_Vessels, y = !!sym(i), color = YearPVesLost)) + geom_point(size = 3) + theme_minimal() +
    labs(x = "Pre-Shock Fishery Breadth") +
    scale_color_viridis_c(option = "magma", direction = -1)

  # gv1[[o]] <- ggplot(nfconstant
  #                     # %>% filter(HullAreaTrimmed>0)
  #                     , aes(x = !!sym(i), y = Vessels1, color = SS_Vessels)) + geom_point(size = 3) + theme_minimal()
  # gr[[o]] <- ggplot(nfconstant
  #                     # %>% filter(HullAreaTrimmed>0)
  #                     , aes(x = !!sym(i), y = Return, color = SS_Vessels)) + geom_point(size = 3) + theme_minimal()
  gmed[[o]] <- ggplot(nmed
                      # %>% filter(HullAreaTrimmed>0)
                      , aes(x = !!sym(i), y = YearPVesLost, color = SS_Vessels)) + geom_point(size = 3, alpha = .9) + theme_minimal() +
    # scale_color_viridis_c(option = "mako", direction = -1)
    scale_color_gradient(low = "#E9AFA3", high = "#432323")
  # +
  #   geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), color = "grey40")
}

(gmed[[1]] + gmed[[2]] + gmed[[3]]) /
  (gmed[[4]] + gmed[[5]] + gmed[[6]]) + plot_layout(guides = "collect")



nlarge <- nstart %>%
  filter(SS_Vessels > 210) %>%
  mutate(Vessel1N = Vessels1*SS_Vessels,
         VesselDrop = SS_Vessels-Vessel1N,
         YearPVesLost = Return/VesselDrop,
         MedianToCentroid = ifelse(HullArea == 0, 0, MedianToCentroid),
         MeanToCentroid = ifelse(HullArea == 0, 0, MeanToCentroid),
         Big = ifelse(SS_Fisheries > 26,1,0)) %>%
  filter(YearPVesLost > 0,
         VesselDrop > 5)

glar <- list()
for (o in 1:length(cg)){
  i <- cg[o]
  gssize[[o]] <- ggplot(nfconstant, aes(x = SS_Vessels, y = !!sym(i), color = YearPVesLost)) + geom_point(size = 3) + theme_minimal() +
    labs(x = "Pre-Shock Fishery Breadth") +
    scale_color_viridis_c(option = "magma", direction = -1)

  # gv1[[o]] <- ggplot(nfconstant
  #                     # %>% filter(HullAreaTrimmed>0)
  #                     , aes(x = !!sym(i), y = Vessels1, color = SS_Vessels)) + geom_point(size = 3) + theme_minimal()
  # gr[[o]] <- ggplot(nfconstant
  #                     # %>% filter(HullAreaTrimmed>0)
  #                     , aes(x = !!sym(i), y = Return, color = SS_Vessels)) + geom_point(size = 3) + theme_minimal()
  glar[[o]] <- ggplot(nlarge
                      # %>% filter(HullAreaTrimmed>0)
                      , aes(x = !!sym(i), y = YearPVesLost, color = SS_Vessels)) + geom_point(size = 3, alpha = .9) + theme_minimal() +
    # scale_color_viridis_c(option = "mako", direction = -1)
    scale_color_gradient(low = "#E9AFA3", high = "#432323")
  # +
  #   geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), color = "grey40")
}

(glar[[1]] + glar[[2]] + glar[[3]]) /
  (glar[[4]] + glar[[5]] + glar[[6]]) + plot_layout(guides = "collect")






