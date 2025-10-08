six <- read_rds("~/westcoast-networks/data/clean/Simulation/projectiondataforML_MLSizeCacheMeta1_6.rds") 
vi_long <- read_rds("~/westcoast-networks/output/Simulation/MLResults_Meta2/vi_long.rds")
networksix <- read_rds("~/westcoast-networks/data/clean/Simulation/projectiondataforML_cleanedmeta2.rds")[[6]]
# r <- read_rds("~/westcoast-networks/data/clean/Simulation/projectiondataforML_cleanedmeta2.rds")
# networksix2 <- r[[6]]
pnames <- names(networksix)[!str_detect(names(networksix),"v")]

# s <- six[[1]][[1]]
# db <- map_dfr(1:length(six[[1]]), function(s){
#   
#   if(is.null(six[[1]][[s]]$dropped_time) ||
#      nrow(six[[1]][[s]]$dropped_time)<=1){
#     return(NULL)
#   }
#   dt <- six[[1]][[s]]$dropped_time %>% 
#     mutate(i = s) 
#   
# }) 

colelem <- c("SS_Vessels", "SS_Revenue", "SS_Weeks", "N_Fisheries")
colelem_node <- c("SS_Drop_Vessels", "SS_Drop_Revenue", "SS_Drop_Weeks")

i <- pnames[[2]]
d <- "V5"
for (i in pnames) {
  
  bettername <- case_when(i == "network_pre_b" ~ "Bipartite",
                          i == "network_pre_e" ~ "Fishery Network (Co-Participation)",
                          i == "network_pre_ev" ~ "Vessel Network (Co-Fished)",
                          i == "network_pre_f" ~ "Fishery Network (Sum Product)",
                          i == "network_pre_fv" ~ "Vessel Network (Sum Product)",
                          i == "network_pre_tc" ~ "Fishery Network (Catch Correlation)",
                          i == "network_pre_tt" ~ "Fishery Network (Sequential Switching)")
  dpoint <- case_when(d == "V5" ~ "Vessels5",
                      d == "V10" ~ "Vessels10",
                      d == "Reg" ~ "Return")
  
  vtight <- vi_long %>% 
    filter(series == 6,
           Dep == d,
           Projection == bettername,
           DataSubset == "All") 
  
  rhs <- paste(c(vtight$Variable, "SS_Vessels", "DropPortion", "N_Fisheries"), collapse = " + ")
  
  data <- networksix[[i]] %>% 
    mutate(DropPortion = SS_Drop_Vessels/SS_Vessels)
  
  model <- feols(as.formula(paste(dpoint, " ~ ", rhs)),
                 data = data)
  
  out <- tidy(model) %>%
    select(term, estimate, std.error, p.value)
  
  datasamplesH <- data %>%
    filter(Mean_Weight > quantile(Mean_Weight, .5),
           Closeness > quantile(Closeness, .5),
           # HullArea > quantile(HullArea, .5),
           SI < quantile(SI, .5),
           MedianToCentroid > quantile(MedianToCentroid, .5),
           SS_Vessels < quantile(SS_Vessels, .5),
           DropPortion > quantile(DropPortion, .5),
           N_Fisheries > quantile(N_Fisheries, .4)
           ) %>%
    arrange(Vessels5)
  
  datasamplesL <- data %>%
    filter(Mean_Weight < quantile(Mean_Weight, .6),
           Closeness < quantile(Closeness, .6),
           # HullArea > quantile(HullArea, .5),
           SI > quantile(SI, .4),
           MedianToCentroid < quantile(MedianToCentroid, .6),
           SS_Vessels > quantile(SS_Vessels, .4),
           DropPortion < quantile(DropPortion, .5),
           N_Fisheries < quantile(N_Fisheries, .5)
    ) %>%
    arrange(Vessels5)
  
  sampleplot <- db %>% 
    filter(i %in% datasamplesL$iter) 
  uniqueiter <- unique(datasamplesL$iter)
    
  j <- 3
  fitplotshigh <- list()
  fitplots <- list()
  for (j in 1:length(uniqueiter)){
    
    fourteen <- sampleplot %>% 
      filter(i == uniqueiter[[j]]) %>% 
      filter(Year == 14,
             Dropped == 1) 
    
    plotsam <- sampleplot %>% 
      filter(i == uniqueiter[[j]]) %>% 
      mutate(DropRegion = ifelse(HomeRegion %in% stringr::str_extract(fourteen$FISHERY_ID, "(?<=, ).*"), 
                                 "Shocked Region", "")) %>% 
      distinct(Year, HomeRegion, DropRegion, NVessels.y, MeanRevenue)
    
    pt <- datasamples %>% 
      filter(iter == uniqueiter[[j]])
    
    fitplots[[j]] <- ggplot(plotsam, aes(x = Year, y = NVessels.y, color = DropRegion, group = HomeRegion)) +
      geom_line() +
      geom_vline(xintercept = 15) + 
      ggthemes::theme_tufte() +
      labs(y = "NVessels by Region",
           title = paste("Portion @ t=5", round(pt$Vessels5,2)),
           subtitle = paste("Iteration", uniqueiter[[j]]))
    
  }
  
  (fitplotshigh[[1]] + fitplotshigh[[2]] + fitplotshigh[[3]]) /
    (fitplotshigh[[4]] + fitplotshigh[[5]]) +
    (fitplots[[1]] + fitplots[[2]] + fitplots[[3]]) +
    plot_layout(guides = "collect", axes = "collect", axis_titles = "collect")
  
}

draws <- 


