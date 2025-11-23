scalar <- read_rds("~/westcoast-networks/data/clean/Simulation/static_scalar500.rds")[[1]]
sc <- read_rds("~/westcoast-networks/data/clean/Simulation/static_sc_a1.rds")[[1]]
call <- scalar %>%
  distinct(iter, SwitchingCost, FishingCost, N_Fisheries, N_Edges, Mean_Weight,
           MeanDiversification_All, MeanDiversification_Diverse, N_strategies, N_diversified2, FragMeasure,
           Modularity, ClusteringCoefficient_Global, H, MedianToCentroid, HullAreaTrimmed) %>%
  filter(iter <= 100) %>%
  mutate(FisheryGroup = case_when(N_Fisheries <= 5 ~ "0-5",
                               N_Fisheries >= 6 &
                                 N_Fisheries <= 10  ~ "6-10",
                               N_Fisheries >= 11 &
                                 N_Fisheries <= 15  ~ "11-15",
                               N_Fisheries > 15 ~ "15+"),
         FisherySplit = floor(N_Fisheries/5),
         SC2 = SwitchingCost+SwitchingCost^2,
         Connectance = N_Edges/(N_Fisheries*N_Fisheries-1)/2)

scall <- sc %>%
  distinct(iter, SwitchingCost, FishingCost, N_Fisheries, N_Edges, Mean_Weight,
           MeanDiversification_All, MeanDiversification_Diverse, N_strategies, N_diversified2, FragMeasure,
           Modularity, ClusteringCoefficient_Global, H, MedianToCentroid, HullAreaTrimmed, DistMean, RevMean, RevGini) %>%
  # filter(iter <= 100) %>%
  mutate(FisheryGroup = case_when(N_Fisheries <= 5 ~ "0-5",
                                  N_Fisheries >= 6 &
                                    N_Fisheries <= 10  ~ "6-10",
                                  N_Fisheries >= 11 &
                                    N_Fisheries <= 15  ~ "11-15",
                                  N_Fisheries > 15 ~ "15+"),
         FisheryGroup2 = case_when(N_Fisheries <= 8 ~ "0-8",
                                   N_Fisheries >= 9 ~ "9+"),
         Connectance = N_Edges/(N_Fisheries*N_Fisheries-1)/2)

sc01 <- which(scall$SwitchingCost < 1)
drop_rows <- sample(sc01, 75)
scall <- scall[-drop_rows, ]

color_list <- c("#4c6085","#39a0ed","#f7b32b","#EC0868","#13c4a3","#C09BD8","#52050A",
                "#fe5f55","#161613","#A44A3F")

plots <- list()
out <- c("N_Fisheries", "N_Edges", "MeanDiversification_All", "MeanDiversification_Diverse", "N_strategies", "N_diversified2",
         "Mean_Weight", "Modularity", "ClusteringCoefficient_Global", "FragMeasure", "Connectance", "DistMean", "RevMean", "RevGini")
names <- c("# Fisheries", "# Edges", "Mean Diversification", "Mean Diversification \n(Diverse)", "# Strategies", "# Diversified Vessels",
         "Mean Weight", "Modularity", "Density", "Fragmentation", "Connectance", "DistMean", "RevMean", "RevGini")

i <- 1
for(i in 1:length(out)){

  yvar <- sym(out[[i]])
  # Fit
  mod <- lm(as.formula(paste(out[[i]], "~ SwitchingCost*FisheryGroup2")), data = scall)
  mod_sum <- tidy(mod)

  # Extract group names
  groups <- unique(scall$FisheryGroup2)
  g1 <- groups[2]
  g2 <- groups[1]

  # Extract slope for group 1 and difference term
  b1 <- mod_sum$estimate[mod_sum$term == "SwitchingCost"]
  se1   <- mod_sum$std.error[mod_sum$term == "SwitchingCost"]
  p_b1 <- mod_sum$p.value[mod_sum$term == "SwitchingCost"]
  b_diff <- mod_sum$estimate[grepl("SwitchingCost:FisheryGroup2", mod_sum$term)]
  seDiff <- mod_sum$std.error[grepl("SwitchingCost:FisheryGroup2", mod_sum$term)]
  p_diff <- mod_sum$p.value[grepl("SwitchingCost:FisheryGroup2", mod_sum$term)]

  # Compute slope for group 2
  b2 <- b1 + b_diff
  vc <- vcov(mod)
  cov12 <- vc["SwitchingCost", "SwitchingCost:FisheryGroup29+"]
  se2 <- sqrt(se1^2 + seDiff^2 + 2 * cov12)
  t2 <- b2 / se2
  df <- mod$df.residual
  p2 <- 2 * pt(-abs(t2), df)

  # Convert to stars
  stars <- function(p) ifelse(p < 0.001, "***",
                              ifelse(p < 0.01, "**",
                                     ifelse(p < 0.05, "*", "")))

  label_g1 <- sprintf("%s: β = %.3f%s", g1, b1, stars(p_b1))
  label_g2 <- sprintf("%s: β = %.3f%s", g2, b2, stars(p2))
  label_diff <- sprintf("Δβ = %.3f%s", b_diff, stars(p_diff))

  plots[[i]] <- ggplot(scall
                       # %>%filter(N_Fisheries <= 15)
           , aes(x = SwitchingCost, y = !!sym(out[[i]]), color = N_Fisheries)) +
    geom_vline(xintercept = 1, color = "grey60") +
    geom_point(size = 3) +
    scale_color_gradient(
      low = "#EDD174",   # color for smallest estimate E8E387
      high = "#ab2aed"    # color for largest estimate
    ) +
    # geom_smooth(method = "lm",
    #             # formula = y ~ poly(x, 2),
    #             se = T, size = 1)  +
    # annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 4, label = label_g1, color = color_list[3]) +
    # annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 2, label = label_g2, color = color_list[5]) +
    # annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 6, label = label_diff, color = "grey40") +
    # annotate("text", x = Inf, y = Inf, label = label_text, hjust = 1.1, vjust = 2, size = 5) +
    # scale_color_manual(values = c(color_list[3],color_list[5])) +
    theme_minimal() +
    # guides(color = "none") +
    labs(
      subtitle = paste0(names[[i]]),
      y = paste0(names[[i]]),
         color = "Network Size \n(fishery nodes)")

  if(i <= 6){
    mod <- feols(as.formula(paste(out[[i]], "~ SwitchingCost + SwitchingCost^2 + SwitchingCost^3")), data = scall)
    mod_sum <- tidy(mod)

    b1 <- mod_sum$estimate[mod_sum$term == "SwitchingCost"]
    p_b1 <- mod_sum$p.value[mod_sum$term == "SwitchingCost"]
    b2 <- mod_sum$estimate[mod_sum$term == "I(SwitchingCost^2)"]
    p_b2 <- mod_sum$p.value[mod_sum$term == "I(SwitchingCost^2)"]
    b3 <- mod_sum$estimate[mod_sum$term == "I(SwitchingCost^3)"]
    p_b3 <- mod_sum$p.value[mod_sum$term == "I(SwitchingCost^3)"]

    label_g1 <- sprintf("βSC = %.3f%s", b1, stars(p_b1))
    label_g2 <- sprintf("βSC^2 = %.3f%s", b2, stars(p_b2))
    label_g3 <- sprintf("βSC^2 = %.3f%s", b3, stars(p_b3))

    plots[[i]] <- ggplot(scall
                         # %>%filter(N_Fisheries <= 15)
                         , aes(x = SwitchingCost, y = !!sym(out[[i]]))) +
      geom_vline(xintercept = 1, color = "grey60") +
      geom_point() +
      # geom_smooth(method = "lm",
      #             formula = y ~ poly(x, 3),
      #             se = T, size = 1, color = color_list[6])  +
      # annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 6, label = label_g3) +
      # annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 4, label = label_g2) +
      # annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 2, label = label_g1) +
      # annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 6, label = label_diff, color = "grey40") +
      # annotate("text", x = Inf, y = Inf, label = label_text, hjust = 1.1, vjust = 2, size = 5) +
      # scale_color_manual(values = c(color_list[3],color_list[5])) +
      theme_minimal() +
      # guides(color = "none") +
      labs(
        subtitle = paste0(names[[i]]),
        y = paste0(names[[i]]))

  }

}

library(patchwork)
plots[[3]]+plots[[4]]+plots[[5]]+plots[[6]]+
  plot_layout(guides="collect", axis_titles ="collect", axes = "collect")+
  plot_annotation(title = "Diversification") &
  theme(legend.position='bottom')

plots[[7]]+plots[[8]]+plots[[9]]+plots[[10]]+
  plot_layout(guides="collect", axis_titles ="collect", axes = "collect")+
  plot_annotation(title = "Network Outcomes") &
  theme(legend.position='bottom')

plots[[1]]+plots[[2]]+plots[[11]]+guide_area()+
  plot_layout(guides="collect", axis_titles ="collect", axes = "collect")+
  plot_annotation(title = "Network Structure") &
  theme(legend.position='bottom')

plots[[12]]+plots[[13]]+plots[[14]]+guide_area()+
  plot_layout(guides="collect", axis_titles ="collect", axes = "collect")+
  plot_annotation(title = "Network Structure") &
  theme(legend.position='bottom')

ggplot(scall %>%
         filter(N_Fisheries <= 15), aes(x = SwitchingCost, y = FragMeasure, color = FisheryGroup2)) +
  geom_point() +
  geom_smooth(method = "lm",
              # formula = y ~ poly(x, 2),
              se = T, size = 1) +
  # ggforce::geom_mark_ellipse(aes(fill = factor(FisheryGroup), label = FisheryGroup),
  #                            alpha = 0.2, show.legend = FALSE) +
  theme_minimal() +
  labs()


# next --------------------------------------------------------------------

f1 <- feols(N_Fisheries ~ SwitchingCost, scall)
f2 <- feols(ClusteringCoefficient_Global ~ N_Fisheries, scall)
f3 <- feols(ClusteringCoefficient_Global ~ SwitchingCost, scall)
f4 <- feols(ClusteringCoefficient_Global ~ SwitchingCost + SwitchingCost^2, scall)
f5 <- feols(ClusteringCoefficient_Global ~ SwitchingCost + N_Fisheries, scall)
f6 <- feols(ClusteringCoefficient_Global ~ SwitchingCost | N_Fisheries, scall)

modelsummary(
  list("NF ~ SC" = f1,
       "Den ~ NF" = f2,
       "Den ~ SC" = f3,
       "Den ~ SC + SC2" = f4,
       "Den ~ SC + NF" = f5,
       "Den ~ SC | NF" = f6),
  stars = TRUE,           # adds significance stars
  coef_rename = NULL,    # or "html", "latex" depending on context
  gof_map = c("nobs", "r.squared")
)

f1 <- feols(N_Fisheries ~ SwitchingCost, scall)
f2 <- feols(Modularity ~ N_Fisheries, scall)
f3 <- feols(Modularity ~ SwitchingCost, scall)
f4 <- feols(Modularity ~ SwitchingCost + SwitchingCost^2, scall)
f5 <- feols(Modularity ~ SwitchingCost + N_Fisheries, scall)
f6 <- feols(Modularity ~ SwitchingCost | N_Fisheries, scall)

modelsummary(
  list("NF ~ SC" = f1,
       "Mod ~ NF" = f2,
       "Mod ~ SC" = f3,
       "Mod ~ SC + SC2" = f4,
       "Mod ~ SC + NF" = f5,
       "Mod ~ SC | NF" = f6),
  stars = TRUE,           # adds significance stars
  coef_rename = NULL,    # or "html", "latex" depending on context
  gof_map = c("nobs", "r.squared")
)


f1 <- feols(N_Fisheries ~ SwitchingCost, scall)
f2 <- feols(FragMeasure ~ N_Fisheries, scall)
f3 <- feols(FragMeasure ~ SwitchingCost, scall)
f4 <- feols(FragMeasure ~ SwitchingCost + SwitchingCost^2, scall)
f5 <- feols(FragMeasure ~ SwitchingCost + N_Fisheries, scall)
f6 <- feols(FragMeasure ~ SwitchingCost | N_Fisheries, scall)

modelsummary(
  list("NF ~ SC" = f1,
       "Frag ~ NF" = f2,
       "Frag ~ SC" = f3,
       "Frag ~ SC + SC2" = f4,
       "Frag ~ SC + NF" = f5,
       "Frag ~ SC | NF" = f6),
  stars = TRUE,           # adds significance stars
  coef_rename = NULL,    # or "html", "latex" depending on context
  gof_map = c("nobs", "r.squared")
)

# DAG ---------------------------------------------------------------------
library(dagitty)
library(ggdag)
library(tidySEM)
library(fixest)
library(parameters)

f1 <- feols(N_Fisheries ~ SwitchingCost, scall)
outcomes <- c("N_Edges","Connectance", "Mean_Weight", "ClusteringCoefficient_Global",
              "Modularity", "FragMeasure")
names <- c("# Edges","Connectance","Mean \nWeight", "Density", "Modularity", "Frag \nMeasure")
o <- 1

dags <- list()
for(o in 1:length(outcomes)){

  f2 <- feols(
    as.formula(paste0(outcomes[o], " ~ SwitchingCost + N_Fisheries")),
    scall
  )

  edges <- tibble(from = c("SwitchingCost", "SwitchingCost", "N_Fisheries"),
                  to = c("N_Fisheries", outcomes[o], outcomes[o]),
                  est = c(f1$coefficients[2], f2$coefficients[2], f2$coefficients[3]),
                  p = c(f1$coeftable[2,4],
                        f2$coeftable[2,4],
                        f2$coeftable[3,4])) %>%
    mutate(p = case_when(p < .001 ~ "***",
                         p < .01 ~ "**",
                         p < .05 ~ "*",
                         p < .1 ~ "+",
                         T ~ ""),
           label = paste0(round(est,3), p),
           direction = ifelse(est > 0, T, F))

  nodes <- tibble(
    name = c("SwitchingCost", "N_Fisheries", outcomes[o]),
    nlab = c("Switching \nCost", "# Fisheries", names[o]),
    x = c(0, 1, 2),
    y = c(0, .5, 0)
  )

  graph <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

  dags[[o]] <- ggraph(graph, layout = "manual", x = x, y = y) +
    geom_edge_link(aes(label = label, color = direction),
                   arrow = arrow(length = unit(5, "mm")),
                   end_cap = circle(12, "mm"),
                   start_cap = circle(12, "mm"),
                   label_push = unit(1, "pt"),
                   angle_calc = "along",
                   label_dodge = unit(2.5, "mm")) +
    geom_node_label(aes(label = nlab),
                    size = 4,
                    label = unit(5, "mm"),
                    label.padding = unit(4, "pt"),
                    label.r = unit(3, "pt"),
                    fill = "white",
                    color = "black") +
    theme_void() +
    scale_edge_color_manual(values = c("#fe5f55","#39a0ed"), guide = "none") +
    expand_limits(x = c(-0.3, 2.3), y = c(-0.1, .6))

}
# f5 <- feols(ClusteringCoefficient_Global ~ SwitchingCost + N_Fisheries, scall)

dags
