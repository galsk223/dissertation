list.files("/home/gkoss/dissertation/01_scripts/01_Choice_Function_Functions/",
           full.names = TRUE) %>%
  walk(source)
df_dist <- read_rds("../westcoast-networks/data/clean/Simulation/empiricalbenchmarks_meta.rds")$regionadjacency %>%
  select(-DistanceCostP)

trsplit <- read_rds("../westcoast-networks/data/clean/Simulation/empiricalbenchmarks_df_empnet.rds") %>%
  # filter(HomeRegion == "San Francisco",
  #        LANDING_YEAR == 2019) %>%
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




ggplot(allemp,
             aes(x = RevMed, y = NVessels)) +
  geom_point() +
  theme_minimal()
g1 <- ggplot(allemp,
       aes(x = RevMed, y = N_strategies/NVessels)) +
  geom_point() +
  theme_minimal()
g2 <- ggplot(allemp,
             aes(x = RevMed, y = MeanDiversification_All)) +
  geom_point() +
  theme_minimal()
g1+g2+plot_annotation(title = "Empirical Motivation",
                      subtitle = "Diversification and Revenue")


formulas_scall <- list(
  N_Fisheries ~ MeanDiversification_All + N_strategies,
  N_Edges ~ MeanDiversification_All + N_strategies,
  Mean_Weight ~ MeanDiversification_All + N_strategies,
  Modularity ~ MeanDiversification_All + N_strategies,
  ClusteringCoefficient_Global ~ MeanDiversification_All + N_strategies
)

run_models <- function(data, formulas) {
  lapply(formulas, function(f) {
    feols(f, data = data)
  })
}

# Run for scall
results_scall <- run_models(allemp %>%
                              distinct(Region, Year, .keep_all = T), formulas_scall)

names(results_scall) <- c("N_Fisheries","N_Edges","Mean_Weight",
                          "Modularity", "ClusteringCoefficient_Global")
# ,
#   "N_Fisheries", "N_Edges", "N_Edges", "Mean_Weight", "Mean_Weight",
#   "N_Fisheries", "ClusteringCoefficient_Global", "ClusteringCoefficient_Global",
#   "Modularity", "Modularity","FragMeasure","FragMeasure","H","H"
# )

options(modelsummary_factory_word = 'huxtable')
modelsummary(
  results_scall,
  stars = TRUE,           # adds significance stars
  coef_rename = NULL,    # or "html", "latex" depending on context
  gof_map = c("nobs", "r.squared")
)




