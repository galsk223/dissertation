library(fixest)
library(modelsummary)
library(fixest)
library(broom)
library(dplyr)
library(stringr)
library(ggplot2)
library(patchwork)

color_list <- c("#4c6085","#39a0ed","#f7b32b","#EC0868","#13c4a3","#C09BD8","#52050A",
                "#fe5f55","#161613","#A44A3F")

regsmall <- function(df){
  sc01 <- which(df$SwitchingCost < 1)
  drop_rows <- sample(sc01, 75)
  df <- df[-drop_rows, ]
}

rds <- "~/westcoast-networks/data/clean/Simulation/static_sc_a1.rds"
nodelevel <- function(rds){
  df <- read_rds(rds)[[1]] %>%
    filter(str_detect(Fishery,"Dungeness|Salmon"))
}

sc <- read_rds("~/westcoast-networks/data/clean/Simulation/static_sc_a1.rds")[[1]] %>%
  mutate(SwitchCostInc = "All",
         FishCostInc = "None") %>%
  distinct(iter, .keep_all = T) %>%
  regsmall() %>%
  mutate(Connectance = N_Edges/(N_Fisheries*N_Fisheries-1)/2)
sc_n <- nodelevel("~/westcoast-networks/data/clean/Simulation/static_sc_a1.rds") %>%
  mutate(SwitchCostInc = "All",
         FishCostInc = "None") %>%
  filter(iter %in% sc$iter)

kk <- read_rds("~/westcoast-networks/data/clean/Simulation/static_kk_a1.rds")[[1]] %>%
  mutate(SwitchCostInc = "Key x Key",
         FishCostInc = "None") %>%
  distinct(iter, .keep_all = T) %>%
  regsmall() %>%
  mutate(Connectance = N_Edges/(N_Fisheries*N_Fisheries-1)/2)
kk_n <- nodelevel("~/westcoast-networks/data/clean/Simulation/static_kk_a1.rds") %>%
  mutate(SwitchCostInc = "Key x Key",
         FishCostInc = "None") %>%
  filter(iter %in% kk$iter)

knk <- read_rds("~/westcoast-networks/data/clean/Simulation/static_knk_a1.rds")[[1]] %>%
  mutate(SwitchCostInc = "Key x Non-Key",
         FishCostInc = "None") %>%
  distinct(iter, .keep_all = T) %>%
  regsmall() %>%
  sample_n(125)%>%
  mutate(Connectance = N_Edges/(N_Fisheries*N_Fisheries-1)/2)
knk_n <- nodelevel("~/westcoast-networks/data/clean/Simulation/static_knk_a1.rds") %>%
  mutate(SwitchCostInc = "Key x Non-Key",
         FishCostInc = "None") %>%
  filter(iter %in% knk$iter)

nknk <- read_rds("~/westcoast-networks/data/clean/Simulation/static_nknk_a1.rds")[[1]] %>%
  mutate(SwitchCostInc = "Non-Key x Non-Key",
         FishCostInc = "None") %>%
  distinct(iter, .keep_all = T) %>%
  regsmall()%>%
  mutate(Connectance = N_Edges/(N_Fisheries*N_Fisheries-1)/2)
nknk_n <- nodelevel("~/westcoast-networks/data/clean/Simulation/static_nknk_a1.rds") %>%
  mutate(SwitchCostInc = "Non-Key x Non-Key",
         FishCostInc = "None") %>%
  filter(iter %in% nknk$iter)

fc <- read_rds("~/westcoast-networks/data/clean/Simulation/static_fc_a1.rds")[[1]] %>%
  mutate(SwitchCostInc = "None",
         FishCostInc = "All") %>%
  distinct(iter, .keep_all = T) %>%
  mutate(Connectance = N_Edges/(N_Fisheries*N_Fisheries-1)/2)
fc_n <- nodelevel("~/westcoast-networks/data/clean/Simulation/static_fc_a1.rds") %>%
  mutate(SwitchCostInc = "None",
         FishCostInc = "All") %>%
  filter(iter %in% fc$iter)

fc1 <- read_rds("~/westcoast-networks/data/clean/Simulation/static_fc1_a1.rds")[[1]] %>%
  mutate(SwitchCostInc = "None",
         FishCostInc = "Key") %>%
  distinct(iter, .keep_all = T) %>%
  mutate(Connectance = N_Edges/(N_Fisheries*N_Fisheries-1)/2)
fc1_n <- nodelevel("~/westcoast-networks/data/clean/Simulation/static_fc1_a1.rds") %>%
  mutate(SwitchCostInc = "None",
         FishCostInc = "Key") %>%
  filter(iter %in% fc2$iter)

fc2 <- read_rds("~/westcoast-networks/data/clean/Simulation/static_fc2_a1.rds")[[1]] %>%
  mutate(SwitchCostInc = "None",
         FishCostInc = "Non-Key") %>%
  distinct(iter, .keep_all = T) %>%
  mutate(Connectance = N_Edges/(N_Fisheries*N_Fisheries-1)/2)
fc2_n <- nodelevel("~/westcoast-networks/data/clean/Simulation/static_fc2_a1.rds") %>%
  mutate(SwitchCostInc = "None",
         FishCostInc = "Non-Key") %>%
  filter(iter %in% fc2$iter)

allsc <- bind_rows(sc, kk, knk, nknk)
allfc <- bind_rows(fc, fc1, fc2)
allsc_n <- bind_rows(sc_n, kk_n, knk_n, nknk_n)
allfc_n <- bind_rows(fc_n, fc1_n, fc2_n)

# datasets <- list(
#   All = sc,
#   `Inside x Inside` = kk,
#   `Inside x Outside` = knk,
#   `Outside x Outside` = nknk,
#   `Inside Costs` = fc1,
#   `Outside Costs` = fc2
# )


# switching costs ---------------------------------------------------------

out <- c("N_Fisheries", "N_Edges", "MeanDiversification_All", "MeanDiversification_Diverse", "N_strategies", "N_diversified2",
         "Mean_Weight", "Modularity", "ClusteringCoefficient_Global", "FragMeasure", "Connectance",
         "DistMean", "RevMean", "RevGini", "Rev10", "Rev90", "Size", "Strength","ClusteringCoefficient", "FragmentationCentrality")

color_list <- c("#4c6085","#39a0ed","#f7b32b","#EC0868","#13c4a3","#C09BD8","#52050A",
                "#fe5f55","#161613","#A44A3F")

plotsf <- list()
plotss <- list()

i <- 17
for(i in 1:length(out)){

  if(i >= 17){
    dffc <- allfc_n
    dfsc <- allsc_n
  } else {
    dffc <- allfc
    dfsc <- allsc
  }

  yvar <- sym(out[[i]])
  plotsf[[i]] <- ggplot(dffc, aes(x = FishingCost, y = !!yvar, color = FishCostInc)) +
    # geom_vline(xintercept = 1, color = "grey60") +
    geom_point(alpha = .4, size = 2) +
    geom_smooth(method = "lm",
                # formula = y ~ poly(x, 2),
                se = T, size = 1)  +
      scale_color_manual(values = c(color_list[1],color_list[2],color_list[4])) +
      theme_minimal() +
      labs(
        y = "",
        subtitle = paste0(out[[i]]),
        color = "Increasing fishing costs\nfor fisheries:")

  plotss[[i]] <- ggplot(dfsc, aes(x = SwitchingCost, y = !!yvar, color = SwitchCostInc)) +
    geom_vline(xintercept = 1, color = "grey60") +
    geom_point(alpha = .6) +
    geom_smooth() +
    scale_color_manual(values = c(color_list[1],color_list[2],
                                  color_list[8],color_list[10])) +
    theme_minimal() +
    labs(
      y = "",
      subtitle = paste0(out[[i]]),
      color = "Increasing switching costs\nbetween fisheries:")

}

plotsf[[1]]+plotsf[[2]]+plotsf[[11]]+guide_area()+
  plot_layout(guides="collect", axis_titles ="collect", axes = "collect")+
  plot_annotation(title = "Network Structure",
                  subtitle = "Fishing Cost Changes")

plotsf[[3]]+plotsf[[4]]+plotsf[[5]]+plotsf[[6]]+
  plot_layout(guides="collect", axis_titles ="collect", axes = "collect")+
  plot_annotation(title = "Diversification",
                  subtitle = "Fishing Cost Changes")  &
  theme(legend.position='bottom')

plotsf[[7]]+plotsf[[8]]+plotsf[[9]]+plotsf[[10]]+
  plot_layout(guides="collect", axis_titles ="collect", axes = "collect")+
  plot_annotation(title = "Network Outcomes",
                  subtitle = "Fishing Cost Changes")  &
  theme(legend.position='bottom')

plotsf[[12]]+plotsf[[13]]+plotsf[[14]]+plotsf[[15]]+plotsf[[16]]+guide_area()+
  plot_layout(guides="collect", axis_titles ="collect", axes = "collect")+
  plot_annotation(title = "Inequality Outcomes",
                  subtitle = "Fishing Cost Changes")

plotsf[[17]]+plotsf[[18]]+plotsf[[19]]+plotsf[[20]]+
  plot_layout(guides="collect", axis_titles ="collect", axes = "collect")+
  plot_annotation(title = "Node-level Outcomes",
                  subtitle = "Fishing Cost Changes") &
  theme(legend.position='bottom')



plotss[[1]]+plotss[[2]]+plotss[[11]]+guide_area()+
  plot_layout(guides="collect", axis_titles ="collect", axes = "collect")+
  plot_annotation(title = "Network Structure",
                  subtitle = "Switching Cost Changes")

plotss[[3]]+plotss[[4]]+plotss[[5]]+plotss[[6]]+
  plot_layout(guides="collect", axis_titles ="collect", axes = "collect")+
  plot_annotation(title = "Diversification",
                  subtitle = "Switching Cost Changes")  &
  theme(legend.position='bottom')

plotss[[7]]+plotss[[8]]+plotss[[9]]+plotss[[10]]+
  plot_layout(guides="collect", axis_titles ="collect", axes = "collect")+
  plot_annotation(title = "Network Outcomes",
                  subtitle = "Switching Cost Changes")  &
  theme(legend.position='bottom')

plotss[[12]]+plotss[[13]]+plotss[[14]]+plotss[[15]]+plotss[[16]]+guide_area()+
  plot_layout(guides="collect", axis_titles ="collect", axes = "collect")+
  plot_annotation(title = "Inequality Outcomes",
                  subtitle = "Switching Cost Changes")

plotss[[17]]+plotss[[18]]+plotss[[19]]+plotss[[20]]+
  plot_layout(guides="collect", axis_titles ="collect", axes = "collect")+
  plot_annotation(title = "Node-level Outcomes",
                  subtitle = "Fishing Cost Changes") &
  theme(legend.position='bottom')


plot(fc1$FishingCost, fc1$N_diversified4)
plot(fc2$FishingCost, fc2$N_diversified4)


# regressions -------------------------------------------------------------


out <- c("N_Fisheries", "N_Edges", "MeanDiversification_All", "MeanDiversification_Diverse", "N_strategies", "N_diversified2",
         "Mean_Weight", "Modularity", "ClusteringCoefficient_Global", "FragMeasure", "Connectance", "DistMean", "RevMean", "RevGini",
         "Size", "Strength","ClusteringCoefficient", "FragmentationCentrality")

formulas_sc <- list(
  N_Fisheries ~ SwitchingCost*SwitchCostInc,
  N_Edges ~ SwitchingCost*SwitchCostInc,
  Connectance ~ SwitchingCost*SwitchCostInc,

  MeanDiversification_All ~ SwitchingCost*SwitchCostInc,
  MeanDiversification_Diverse ~ SwitchingCost*SwitchCostInc,
  N_diversified2 ~ SwitchingCost*SwitchCostInc,
  N_strategies ~ SwitchingCost*SwitchCostInc,

  Mean_Weight ~ SwitchingCost*SwitchCostInc,
  Modularity ~ SwitchingCost*SwitchCostInc,
  ClusteringCoefficient_Global ~ SwitchingCost*SwitchCostInc,
  FragMeasure ~ SwitchingCost*SwitchCostInc
  # ,
  #
  # DistMean ~ SwitchingCost*SwitchCostInc,
  # RevMean ~ SwitchingCost*SwitchCostInc,
  # RevGini ~ SwitchingCost*SwitchCostInc
)

formulas_fc <- list(
  N_Fisheries ~ FishingCost*FishCostInc,
  N_Edges ~ FishingCost*FishCostInc,
  Connectance ~ FishingCost*FishCostInc,

  MeanDiversification_All ~ FishingCost*FishCostInc,
  MeanDiversification_Diverse ~ FishingCost*FishCostInc,
  N_diversified2 ~ FishingCost*FishCostInc,
  N_strategies ~ FishingCost*FishCostInc,

  Mean_Weight ~ FishingCost*FishCostInc,
  Modularity ~ FishingCost*FishCostInc,
  ClusteringCoefficient_Global ~ FishingCost*FishCostInc,
  FragMeasure ~ FishingCost*FishCostInc,

  Size ~ FishingCost*FishCostInc,
  Strength ~ FishingCost*FishCostInc
)

run_models <- function(data, formulas) {
  lapply(formulas, function(f) {
    feols(f, data = data)
  })
}

# results_sc <- lapply(formulas_sc, feols, data = allsc)
# names(results_sc) <- sapply(formulas_sc, function(f) all.vars(f[[2]]))
# vcovs_sc <- lapply(results_sc, vcov)
# i <- 1
# dfvcovs <- map_dfr(1:length(vcovs_sc), function(i){
#
#   df <- vcovs_sc[[i]] %>%
#     as.data.frame() %>%
#     rownames_to_column() %>%
#     pivot_longer(-rowname,
#                  names_to = "term",
#                  values_to = "cov") %>%
#     mutate(model = names(vcovs_sc)[[i]]) %>%
#     filter(rowname == "SwitchingCost")
#
# })
#
# tidy_sc <- bind_rows(
#   lapply(names(results_sc), function(name) {
#     broom::tidy(results_sc[[name]]) %>%
#       mutate(
#         model = name
#       )
#   })
# ) %>%
#   mutate(type = case_when(!str_detect(term, "^SwitchingCost(:|$)") ~ "Level",
#                           str_detect(term, "^SwitchingCost(:|$)") ~ "Slope",
#                           TRUE ~ NA_character_),
#          SwitchCostInc = case_when(term == "(Intercept)" ~ "All",
#                                    term == "SwitchingCost" ~ "All",
#                                    str_detect(term, "SwitchCostInc") ~ str_remove(term, ".*SwitchCostInc"),
#                                    TRUE ~ "All"),
#          SwitchCostInc = str_trim(SwitchCostInc)) %>%
#   select(model, type, SwitchCostInc, term, estimate, std.error) %>%
#   filter(type == "Slope") %>%
#   left_join(dfvcovs, by = c("model", "term")) %>%
#   group_by(model) %>%
#   mutate(baseline_term = "SwitchingCost",
#          baseline_est = estimate[SwitchCostInc == "All"],
#          baseline_se  = std.error[SwitchCostInc == "All"],
#          effect = ifelse(SwitchCostInc == "All", estimate,
#                          estimate+baseline_est),
#          se2 = if_else(
#            SwitchCostInc == "All",baseline_se,
#            sqrt(baseline_se^2 + std.error^2 + 2 * cov)),
#          se = ifelse(SwitchCostInc == "All", std.error,
#                      sqrt(baseline_se^2 + std.error^2))) %>%
#     ungroup() %>%
#     mutate(group = case_when(model %in% c("N_Edges", "Connectance") ~ "Network Structure",
#                            model %in% c("MeanDiversification_All", "MeanDiversification_Diverse",
#                                         "N_diversified2", "N_strategies") ~ "Diversification",
#                            model %in% c("Mean_Weight", "Modularity",
#                                         "ClusteringCoefficient_Global") ~ "Network Outcomes",
#                            model %in% c("DistMean", "RevMean",
#                                         "RevGini") ~ "Inequality Outcomes"))
#
#
# ggplot(tidy_sc %>%
#          filter(group == "Network Structure"),aes(x = estimate, y = SwitchCostInc, color = SwitchCostInc)) +
#   geom_vline(xintercept = 0, color =  "grey80") +
#   geom_point(size = 3) +
#   geom_errorbarh(aes(xmin = estimate - 1.96 * std.error,
#                      xmax = estimate + 1.96 * std.error),
#                  height = 0.2) +
#   facet_wrap(vars(model), scales = "free") +
#   theme_minimal() +
#   scale_color_manual(values = c(color_list[1],color_list[2],
#                                 color_list[8],color_list[10])) +
#   labs(
#     x = "Estimate",
#     y = "SwitchCostInc Level",
#     color = ""
#   )

results_fc <- lapply(formulas_fc, feols, data = allfc)
names(results_fc) <- sapply(results_fc, function(m) all.vars(formula(m))[1])
vcovs_fc <- lapply(results_fc, vcov)
i <- 1
dfvcovf <- map_dfr(1:length(vcovs_fc), function(i){

  df <- vcovs_fc[[i]] %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    pivot_longer(-rowname,
                 names_to = "term",
                 values_to = "cov") %>%
    mutate(model = names(vcovs_fc)[[i]]) %>%
    filter(rowname == "FishingCost")

})

tidy_fc <- bind_rows(
  lapply(names(results_fc), function(name) {
    broom::tidy(results_fc[[name]]) %>%
      mutate(
        model = name
      )
  })
) %>%
  mutate(type = case_when(!str_detect(term, "^FishingCost(:|$)") ~ "Level",
                          str_detect(term, "^FishingCost(:|$)") ~ "Slope",
                          TRUE ~ NA_character_),
         SwitchCostInc = case_when(term == "(Intercept)" ~ "All",
                                   term == "FishingCost" ~ "All",
                                   str_detect(term, "FishingCost") ~ str_remove(term, ".*FishingCostInc"),
                                   TRUE ~ "All"),
         SwitchCostInc = str_trim(SwitchCostInc)) %>%
  select(model, type, SwitchCostInc, term, estimate, std.error) %>%
  filter(type == "Slope") %>%
  left_join(dfvcovf, by = c("model", "term")) %>%
  group_by(model) %>%
  mutate(baseline_term = "FishingCost",
         baseline_est = estimate[SwitchCostInc == "All"],
         baseline_se  = std.error[SwitchCostInc == "All"],
         effect = ifelse(SwitchCostInc == "All", estimate,
                         estimate+baseline_est),
         se2 = if_else(
           SwitchCostInc == "All",baseline_se,
           sqrt(baseline_se^2 + std.error^2 + 2 * cov)),
         se = ifelse(SwitchCostInc == "All", std.error,
                     sqrt(baseline_se^2 + std.error^2))) %>%
  ungroup() %>%
  mutate(group = case_when(model %in% c("N_Fisheries", "N_Edges", "Connectance") ~ "Network Structure",
                           model %in% c("MeanDiversification_All", "MeanDiversification_Diverse",
                                        "N_diversified2", "N_strategies") ~ "Diversification",
                           model %in% c("Mean_Weight", "Modularity",
                                        "ClusteringCoefficient_Global") ~ "Network Outcomes",
                           model %in% c("DistMean", "RevMean",
                                        "RevGini") ~ "Inequality Outcomes"),
         y = str_remove(SwitchCostInc,"FishingCost:"))


ggplot(tidy_fc %>%
         filter(group == "Diversification"),aes(x = estimate, y = y, color = SwitchCostInc)) +
  geom_vline(xintercept = 0, color =  "grey80") +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = estimate - 1.96 * std.error,
                     xmax = estimate + 1.96 * std.error),
                 height = 0.2) +
  facet_wrap(vars(model), scales = "free") +
  theme_minimal() +
  scale_color_manual(values = c(color_list[1],color_list[2],color_list[4])) +
  labs(
    x = "Estimate",
    y = "Fishing Costs Increased for",
    color = ""
  ) +
  theme(axis.text.y = element_text(angle = 60),
        legend.position='bottom')




# other -------------------------------------------------------------------

out <- c("N_Fisheries", "N_Edges", "MeanDiversification_All", "MeanDiversification_Diverse", "N_strategies", "N_diversified2",
         "Mean_Weight", "Modularity", "ClusteringCoefficient_Global", "FragMeasure", "Connectance", "DistMean", "RevMean", "RevGini")

formulas_sc <- list(
  # N_Fisheries ~ SwitchingCost,
  N_Edges ~ SwitchingCost*SwitchCostInc | N_Fisheries,
  Connectance ~ SwitchingCost*SwitchCostInc | N_Fisheries,

  MeanDiversification_All ~ SwitchingCost*SwitchCostInc | N_Fisheries,
  MeanDiversification_Diverse ~ SwitchingCost*SwitchCostInc | N_Fisheries,
  N_diversified2 ~ SwitchingCost*SwitchCostInc | N_Fisheries,
  N_strategies ~ SwitchingCost*SwitchCostInc | N_Fisheries,

  Mean_Weight ~ SwitchingCost*SwitchCostInc | N_Fisheries,
  Modularity ~ SwitchingCost*SwitchCostInc | N_Fisheries,
  ClusteringCoefficient_Global ~ SwitchingCost*SwitchCostInc | N_Fisheries,
  FragMeasure ~ SwitchingCost*SwitchCostInc | N_Fisheries,

  DistMean ~ SwitchingCost*SwitchCostInc | N_Fisheries,
  RevMean ~ SwitchingCost*SwitchCostInc | N_Fisheries,
  RevGini ~ SwitchingCost*SwitchCostInc | N_Fisheries
)

formulas_fc <- list(
  # N_Fisheries ~ FishingCost,
  N_Edges ~ FishingCost*FishCostInc | N_Fisheries,
  Connectance ~ FishingCost*FishCostInc | N_Fisheries,

  MeanDiversification_All ~ FishingCost*FishCostInc | N_Fisheries,
  MeanDiversification_Diverse ~ FishingCost*FishCostInc | N_Fisheries,
  N_diversified2 ~ FishingCost*FishCostInc | N_Fisheries,
  N_strategies ~ FishingCost*FishCostInc | N_Fisheries,

  Mean_Weight ~ FishingCost*FishCostInc | N_Fisheries,
  Modularity ~ FishingCost*FishCostInc | N_Fisheries,
  ClusteringCoefficient_Global ~ FishingCost*FishCostInc | N_Fisheries,
  FragMeasure ~ FishingCost*FishCostInc | N_Fisheries,

  DistMean ~ FishingCost*FishCostInc | N_Fisheries,
  RevMean ~ FishingCost*FishCostInc | N_Fisheries,
  RevGini ~ FishingCost*FishCostInc | N_Fisheries
)
results_sc <- run_models(allsc, formulas_sc)
results_fc <- run_models(allfc, formulas_fc)

for (i in seq_along(results_fc)) {
  # Extract left-hand side (dependent variable) from formula
  dep <- all.vars(formulas_fc[[i]][[2]])
  # Store it so modelsummary can use it
  attr(results_fc[[i]], "depvar") <- dep
}

for (i in seq_along(results_sc)) {
  # Extract left-hand side (dependent variable) from formula
  dep <- all.vars(results_sc[[i]][[2]])
  # Store it so modelsummary can use it
  attr(results_sc[[i]], "depvar") <- dep
}




modelsummary(
  results_sc[1:3],
  stars = TRUE,           # adds significance stars
  coef_rename = NULL    # or "html", "latex" depending on context
)

modelsummary(
  results_sclog[1:7],
  stars = TRUE,           # adds significance stars
  coef_rename = NULL    # or "html", "latex" depending on context
)






