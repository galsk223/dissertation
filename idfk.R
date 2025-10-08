library(tidyverse)
library(broom)
library(fixest)
library(factoextra)
library(marginaleffects)
rm(list = ls())

resultfolder <- "MLResults_Meta3"
alldb <- read_rds("~/westcoast-networks/data/clean/Simulation/projectiondataforML_cleanedmeta3.rds")
vi_long <- read_rds(paste0("~/westcoast-networks/output/Simulation/",resultfolder,"/vi_long.rds"))

r <- alldb[[1]]
pnames <- names(r)[!str_detect(names(r),"v")]


# costs and variables -----------------------------------------------------

colelem <- c("SS_Vessels", "SS_Revenue", "SS_Weeks", "N_Fisheries")
colelem_node <- c("SS_Drop_Vessels", "SS_Drop_Revenue", "SS_Drop_Weeks")
colnodes <- c("Size", "Closeness", "Strength", "ClusteringCoefficient", "FragmentationCentrality", 
              "FragmentationCentrality_Intra", "FragmentationCentrality_Cross", "FragmentationCentrality_Total")
colnet <- c("FragMeasure", "SI", "H", "H2", "Mean_Weight", "MeanStrength", "N_Edges", "Modularity", "URF",
            "ClusteringCoefficient_Global", "NetworkCentralization", "Redundancy", "Complementarity",
            "HullArea", "HullAreaTrimmed", "MedianToCentroid", "MeanToCentroid")
t <- c("Vessels5")
c_all <- c(colelem,colelem_node,colnodes,colnet,t)
  

df_name <- pnames[[1]]
target <- c_all[[13]]
results <- map_dfr(pnames, function(df_name) {
  
    inner <- map_dfr(c_all,function(target) {
        
        print(target)
      if (!(target %in% colnames(r[[df_name]])) ||
          df_name == "network_pre_b" & target == "SI") {
        return(NULL)
      }
      
      model <- feols(as.formula(paste("log(", target, ") ~ FishCosts + SwitchCosts + Distparameter + ds")),
                     data = r[[df_name]])
      
      out <- tidy(model) %>%
        select(term, estimate, std.error, p.value) %>%
        mutate(dataset = df_name, target = target)
      # print(out)
      }
    )
  }
)

d <- "V5"
v <- vi_long %>% 
  filter(series == 6,
         Dep == d,
         DataSubset == "All") %>% 
  distinct(Variable, Projection)

checkout <- results %>% 
  filter(term != "(Intercept)", 
         p.value < .01) %>% 
  mutate(Projection = case_when(dataset == "network_pre_b" ~ "Bipartite",
                                dataset == "network_pre_e" ~ "Fishery Network (Co-Participation)",
                                dataset == "network_pre_ev" ~ "Vessel Network (Co-Fished)",
                                dataset == "network_pre_f" ~ "Fishery Network (Sum Product)",
                                dataset == "network_pre_fv" ~ "Vessel Network (Sum Product)",
                                dataset == "network_pre_tc" ~ "Fishery Network (Catch Correlation)",
                                dataset == "network_pre_tt" ~ "Fishery Network (Sequential Switching)")) %>% 
  inner_join(v, by = c("target" = "Variable", "Projection"))

p <- 1

pcostsandvars <- list()
for (p in 1:length(pnames)) {
  c <- checkout %>% 
    filter(dataset == pnames[[p]])
  pcostsandvars[[p]] <- ggplot(c, 
                               aes(y = estimate, x = target)) +
    geom_point() +
    geom_errorbar(aes(ymin = estimate-1.96*std.error,
                      ymax = estimate+1.96*std.error)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_hline(yintercept = 0) +
    facet_wrap(vars(term), scales = "free") +
    labs(title = c$Projection,
         x = "",
         y = "")
}

dv1 <- r[[pnames[[2]]]] 
model <- feols(log(Vessels1) ~ Mean_Weight + HullArea + MedianToCentroid + SI +
                 Closeness + FragmentationCentrality + SS_Vessels + N_Fisheries, dv1)

# more at outcomes --------------------------------------------------------

s <- 3
i <- pnames[[3]]
d <- "Return"
pick <- vi_long %>% 
  group_by(Dep, Projection) %>% 
  add_tally()

f <- 1:6
fileid <- "MLSizeCacheMeta2_"
files <- map(1:6,function(f){
  read_rds(paste0("~/westcoast-networks/data/clean/Simulation/projectiondataforML_",
                  fileid,f,".rds"))})
alldb <- map(1:6, function(a){
  # meta <- ifelse(a %in% 1:3, F, T)
  # if(a == 6){# dropped time only in 6 atm
    db2 <- dfbind(files[[a]][[1]], 3:9, meta, 500, a, F)
  # } else {
  #   db2 <- dfbind(files[[a]][[1]], 2:8, meta, 1000, a)
  # }
})

for (i in pnames) {
  
  bettername <- case_when(i == "network_pre_b" ~ "Bipartite",
                          i == "network_pre_e" ~ "Fishery Network (Co-Participation)",
                          i == "network_pre_ev" ~ "Vessel Network (Co-Fished)",
                          i == "network_pre_f" ~ "Fishery Network (Sum Product)",
                          i == "network_pre_fv" ~ "Vessel Network (Sum Product)",
                          i == "network_pre_tc" ~ "Fishery Network (Catch Correlation)",
                          i == "network_pre_tt" ~ "Fishery Network (Sequential Switching)")
  dpoint <- case_when(d == "V1" ~ "Vessels1",
                      d == "V5" ~ "Vessels5",
                      d == "V10" ~ "Vessels10",
                      d == "RegReturn"|
                        d == "Return" ~ "Return")
  
  vtight <- vi_long %>% 
    filter(series == s,
           Dep == d,
           Projection == bettername,
           DataSubset == "All") 
  vars <- paste(vtight$Variable, collapse = " * ")
  rhs_terms <- c(vars, colelem)
  rhs <- paste(rhs_terms, collapse = " + ")
  rhs <- paste(c(vtight$Variable, "SS_Vessels", "N_Fisheries"),
               collapse = " + ")
  rhs <- paste(c(vtight$Variable, colelem),
               collapse = " + ")
  
  data <- alldb[[s]][[i]] %>% 
    rowwise() %>% 
    mutate(ND = length(Drop)) %>% 
    filter(ND == 7) %>% 
    # select(all_of(c(vtight$Variable, dpoint, colelem, colelem_node))) %>%
    mutate(DropPortion = SS_Drop_Vessels/SS_Vessels)
  
  m <- feols()
  model <- feols(as.formula(paste(dpoint, " ~ ", rhs)),
                 data = data)
  model
  out <- tibble(Variable = names(cor_with_target),
                Correlation = cor_with_target,
                Projection = names(projections[p])) %>% 
    filter(abs(Correlation) > .05,
           Variable != "Return")
  
  # res.pca1 <- prcomp(data, scale = TRUE)
  # fviz_pca_var(res.pca1, 
  #              title = bettername,
  #              # geom = "ellipse",  
  #              # col.ind = as.factor(data$Recover),
  #              # addEllipses = TRUE, # Concentration ellipses
  #              # ellipse.type = "confidence",
  #              repel = TRUE)
  
  
}

