library(fixest)
library(modelsummary)
library(fixest)
library(broom)
library(dplyr)
library(stringr)
library(ggplot2)

color_list <- c("#4c6085","#39a0ed","#f7b32b","#EC0868","#13c4a3","#C09BD8","#52050A",
                "#fe5f55","#161613","#A44A3F")

sc <- read_rds("~/westcoast-networks/data/clean/Simulation/static_sc.rds")[[1]] %>%
  mutate(SwitchCostInc = "All",
         FishCostInc = "None")

kk <- read_rds("~/westcoast-networks/data/clean/Simulation/static_kk.rds")[[1]] %>%
  mutate(SwitchCostInc = "Key x Key",
         FishCostInc = "None")
kk1 <- read_rds("~/westcoast-networks/data/clean/Simulation/static_kk1.rds")[[1]] %>%
  mutate(SwitchCostInc = "Key x Key",
         FishCostInc = "Key")
kk2 <- read_rds("~/westcoast-networks/data/clean/Simulation/static_kk2.rds")[[1]] %>%
  mutate(SwitchCostInc = "Key x Key",
         FishCostInc = "Non-key")

knk <- read_rds("~/westcoast-networks/data/clean/Simulation/static_knk.rds")[[1]] %>%
  mutate(SwitchCostInc = "Non-Key x Key",
         FishCostInc = "None")
knk1 <- read_rds("~/westcoast-networks/data/clean/Simulation/static_knk1.rds")[[1]] %>%
  mutate(SwitchCostInc = "Non-Key x Key",
         FishCostInc = "Key")
knk2 <- read_rds("~/westcoast-networks/data/clean/Simulation/static_knk2.rds")[[1]] %>%
  mutate(SwitchCostInc = "Non-Key x Key",
         FishCostInc = "Non-key")

nknk <- read_rds("~/westcoast-networks/data/clean/Simulation/static_nknk.rds")[[1]] %>%
  mutate(SwitchCostInc = "Non-Key x Non-Key",
         FishCostInc = "None")
nknk1 <- read_rds("~/westcoast-networks/data/clean/Simulation/static_nknk1.rds")[[1]] %>%
  mutate(SwitchCostInc = "Non-Key x Non-Key",
         FishCostInc = "Key")
nknk2 <- read_rds("~/westcoast-networks/data/clean/Simulation/static_nknk2.rds")[[1]] %>%
  mutate(SwitchCostInc = "Non-Key x Non-Key",
         FishCostInc = "Non-key")

all <- bind_rows(sc, kk, kk1, kk2, knk, knk1, knk2, nknk, nknk1, nknk2)

ggplot(all, aes(x = MeanDiversification_All, y = ClusteringCoefficient_Global, color = N_strategies)) +
  geom_point() +
  facet_grid(rows = vars(SwitchCostInc),
             cols = vars(FishCostInc)) +
  theme_minimal() +
  labs(color = "# Strategies",
       x = "Switching Cost Multiplier",
       y = "Mean Fisheries / Vessel (Diversification)")

p +
  patchwork::plot_annotation(
    subtitle = "Increasing Fish Cost for:",
    theme = theme(plot.title = element_text(hjust = 0.5))
  ) &
  patchwork::inset_element(
    ggplot() + theme_void() +
      geom_text(aes(0, 0, label = "Inc Switching \nCost for:"), size = 4),
    left = 2.25, bottom = 1.85, right = 0, top = 0, align_to = "panel"
  )

ggplot(sc, aes(x = MeanDiversification_All, y = ClusteringCoefficient_Global, size = SwitchingCost)) +
  geom_point() +
  scale_size_continuous(range = c(1, 7), limits = c(1, 5)) +
  theme_minimal()

# list all datasets
datasets <- list(
  All = sc,
  `Inside x Inside` = kk,
  # `Inside x Inside \n(Inside Cost Inc)` = kk1,
  # `Inside x Inside \n(Outside Cost Inc)` = kk2,
  `Inside x Outside` = knk,
  # `Inside x Outside \n(Inside Cost Inc)` = knk1,
  # `Inside x Outside \n(Outside Cost Inc)` = knk2,
  `Outside x Outside` = nknk
  # `Outside x Outside \n(Inside Cost Inc)` = nknk1,
  # `Outside x Outside \n(Outside Cost Inc)` = nknk2
)

# run the same regression for each dataset
models <- lapply(datasets, function(d) {
  feols(ClusteringCoefficient_Global ~ SwitchingCost + FishingCost + N_Edges + Mean_Weight, data = d)
})

models <- lapply(datasets, function(d) {
  feols(ClusteringCoefficient_Global ~ SwitchingCost + MeanDiversification_All + N_strategies, data = d)
})

# name each model by dataset
names(models) <- names(datasets)

# show them together in one table
modelsummary(models,
             stars = TRUE,
             gof_omit = 'IC|Log|Adj|Within|F|Std.Errors',
             output = "markdown")

# results_df <- lapply(names(datasets), function(nm) {
#   model <- feols(H ~ SwitchingCost + N_Fisheries + N_Edges + Mean_Weight,
#                  data = datasets[[nm]])
#
#   broom::tidy(model, conf.int = TRUE) %>%
#     mutate(dataset = nm)
# }) %>%
#   bind_rows()
#
#
# results_df <- lapply(names(datasets), function(nm) {
#   model <- feols(ClusteringCoefficient_Global ~ SwitchingCost + N_Fisheries + MeanDiversification_All + N_strategies,
#                  data = datasets[[nm]])
#
#   broom::tidy(model, conf.int = TRUE) %>%
#     mutate(dataset = nm)
# }) %>%
#   bind_rows()
#
# results_df <- lapply(names(datasets), function(nm) {
#   model <- feols(N_Fisheries ~ SwitchingCost,
#                  data = datasets[[nm]])
#
#   broom::tidy(model, conf.int = TRUE) %>%
#     mutate(dataset = nm)
# }) %>%
#   bind_rows()

# define model formula (you can later loop through multiple dependent vars too)
formula <- ClusteringCoefficient_Global ~ SwitchingCost + N_Fisheries + N_Edges + Mean_Weight
formula <- ClusteringCoefficient_Global ~ SwitchingCost + N_Fisheries + MeanDiversification_All + N_strategies

# Run regressions and extract tidy results with dependent variable + dataset name
results_df <- imap_dfr(datasets, function(data, nm) {
  model <- feols(formula, data = data)
  broom::tidy(model, conf.int = TRUE) %>%
    mutate(
      dataset = nm,
      dependent = all.vars(formula)[1]  # extract dependent variable name
    )
})

# create a grouping variable based on the dataset prefix
results_df <- results_df %>%
  mutate(
    group = ifelse(dataset == "All","All",str_extract(dataset, "^[A-Za-z]+ x [A-Za-z]+")),
    dataset = factor(dataset, levels = rev(unique(names(datasets)))),
    term = factor(term,
                  levels = (c("SwitchingCost", "N_Fisheries", "N_Edges",
                              "Mean_Weight", "MeanDiversification_All", "N_strategies")),  # reverse variable order
                  labels = c("Switching Cost", "# Fishery Nodes",  "# Edges",
                             "Mean Edge Weight", "Mean Fisheries / Vessel \n(Diversification)", "# Unique Strategies"))
  )

# filter out intercept if you don’t want it plotted
results_df <- results_df %>% filter(term != "(Intercept)")

ggplot(results_df, aes(x = estimate, y = dataset, color = group)) +
  geom_vline(xintercept = 0, color = "grey70") +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~ term, scales = "free_x") +
  labs(
    # title = "Switching Costs and Density",
    title = paste0("Switching Costs and ",unique(results_df$dependent)),
    # subtitle = "Holding Diversification Constant",
    # subtitle = "Holding Network Dimensions Constant",
    x = "Estimate (with 95% CI)",
    y = paste0(unique(results_df$dependent), "\n (Dependant Variable)"),
    color = "Switching Cost Increased For:",
    caption = paste0("Each row is a single regression of ",unique(results_df$dependent)," ~ ",paste(unique(as.character(results_df$term)), collapse = " + "),"
    for each set of simulations. Simulations vary by the fisheries for which I vary switching costs.
    `Inside` refers to crab or salmon fisheries, so that `Inside x Inside` denotes that switching costs are only
    increased within the set of crab and salmon fisheries. `Outside` refers to all other fisheries.
    Color is redundant with row, to aid visual grouping of coefficients into each regression.")
  ) +
  scale_color_manual(values = color_list[1:4]) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

plot(kk$MeanDiversification_All, kk$ClusteringCoefficient_Global)
plot(kk$N_strategies, kk$ClusteringCoefficient_Global)

plot(nknk$MeanDiversification_All, nknk$ClusteringCoefficient_Global)
plot(nknk$N_strategies, nknk$ClusteringCoefficient_Global)

results_df <- lapply(names(datasets), function(nm) {
  model <- feols(ClusteringCoefficient_Global ~ SwitchingCost + N_Fisheries + N_Edges + Mean_Weight,
                 data = datasets[[nm]])

  broom::tidy(model, conf.int = TRUE) %>%
    mutate(dataset = nm)
}) %>%
  bind_rows()

# create a grouping variable based on the dataset prefix
results_df <- results_df %>%
  mutate(
    group = ifelse(dataset == "All","All",str_extract(dataset, "^[A-Za-z]+ x [A-Za-z]+")),
    dataset = factor(dataset, levels = rev(unique(names(datasets)))),
    term = factor(term,
                  levels = rev(c("SwitchingCost", "Mean_Weight", "N_Edges", "N_Fisheries")),  # reverse variable order
                  labels = c("# Nodes", "# Edges", "Mean Edge Weight", "Switching Cost"))
  )

