i <- 1
p <- 4

# projections <- alldb[[i]]
# dfin <- projections[[p]]

outcome <- list("Return" = "Return", 
                "Recover" = "Recover",
                "Portion5" = "Portion5",
                "Portion10" = "Portion10")

colelem <- c("SS_Vessels", "SS_Revenue", "SS_Weeks", "N_Fisheries")
colelem_node <- c("SS_Drop_Vessels", "SS_Drop_Revenue", "SS_Drop_Weeks")

colnodes <- c("Size", "Closeness", "Strength", "ClusteringCoefficient", "FragmentationCentrality", 
              "FragmentationCentrality_Intra", "FragmentationCentrality_Cross", "FragmentationCentrality_Total")

colnet <- c("FragMeasure", "SI", "H", "H2", "Mean_Weight", "MeanStrength", "N_Edges", "Modularity", "URF",
            "ClusteringCoefficient_Global", "NetworkCentralization", "Redundancy", "Complementarity",
            "HullArea", "HullAreaTrimmed", "MedianToCentroid", "MeanToCentroid")

id <- c("iter","FishCosts","SwitchCosts","Drop","Subgraph","vessels_in",
        "Distparameter","Rand_Scale","Rand_Skill","CostbyFishery","ClosureResponse")

t <- c("Return", "Recover", "Vessels5", "Vessels10")

dfassemble <- function(dfin, t, outcome, colelem, colelem_node, colnodes, colnet, id){
  
  if(t == "Return"){
    
    df_interior <- dfin %>% 
      filter(Return < 35) %>% 
      mutate(Outcome = Return)
    
  } else if(t == "Recover") {
    
    df_interior <- dfin %>% 
      mutate(Recover = factor(ifelse(Return < 25, 1, 0)),
             Outcome = Recover) 
    
  } else if(t == "Vessels1") {
    
    df_interior <- dfin %>% 
      mutate(Outcome = Vessels1) 
    
  } else if(t == "Vessels5") {
    
    df_interior <- dfin %>% 
      mutate(Outcome = Vessels5) 
    
  } else if(t == "Vessels10") {
    
    df_interior <- dfin %>% 
      mutate(Outcome = Vessels10) 
    
  } else if(t == "Revenue1") {
    
    df_interior <- dfin %>% 
      mutate(Outcome = Revenue1) 
    
  } else if(t == "Revenue5") {
    
    df_interior <- dfin %>% 
      mutate(Outcome = Revenue5) 
    
  } else if(t == "Revenue10") {
    
    df_interior <- dfin %>% 
      mutate(Outcome = Revenue10) 
    
  } else if(t == "RV1") {
    
    df_interior <- dfin %>% 
      mutate(R1 = Revenue1*SS_Revenue,
             V1 = Vessels1*SS_Vessels,
             Outcome = (R1/V1)/(SS_Revenue/SS_Vessels)) 
    
  } else if(t == "RV5") {
    
    df_interior <- dfin %>% 
      mutate(R5 = Revenue5*SS_Revenue,
             V5 = Vessels5*SS_Vessels,
             Outcome = (R5/V5)/(SS_Revenue/SS_Vessels)) 
    
  } else if(t == "RV10") {
    
    df_interior <- dfin %>% 
      mutate(R10 = Revenue10*SS_Revenue,
             V10 = Vessels10*SS_Vessels,
             Outcome = (R10/V10)/(SS_Revenue/SS_Vessels)) %>% 
      filter(if_any(everything(), is.na))
    
  }
    
    # df_net <- df_interior %>% 
    #   select(any_of(c(Outcome, colnet)))
    # df_elem <- df_interior %>% 
    #   select(any_of(c(Outcome, colelem)))
    # df_all <- df_interior %>% 
    #   select(any_of(c(Outcome, colnet, colelem)))
    
    df_node <- df_interior %>% 
      select(any_of(c("Outcome", colnodes)))
    df_netdrop <- df_interior %>% 
      select(any_of(c("Outcome", colnet, colnodes)))
    df_elemdrop <- df_interior %>% 
      select(any_of(c("Outcome", colelem, colelem_node)))
    df_alldrop <- df_interior %>% 
      select(any_of(c("Outcome", colnet, colnodes, colelem, colelem_node)))
    
    df_id <- df_interior %>% 
      select(any_of(id))
    
    return(list(
      # All = df_all, 
      #           Network = df_net, 
      #           NoNetwork = df_elem,
                All_Drop = df_alldrop,
                Network_Drop = df_netdrop,
                NoNetwork_Drop = df_elemdrop,
                FisheryNode = df_node, 
                ID = df_id))
  
}


# set1:
# df_elem <- df_interior %>% 
#   select(any_of(c(outcome$Recover, colelem)))
# df_node <- df_interior %>% 
#   select(any_of(c(outcome$Recover, colnodes)))
# df_net <- df_interior %>% 
#   select(any_of(c(outcome$Recover, colnodes, colnet)))
# df_all <- df_interior %>% 
#   select(any_of(c(outcome$Recover, colelem, colnodes, colnet)))
# 
# 
# # dfin <- df_net_all$`all stochasticity, permanent shock`$Extensive
# dfassembleportion <- function(dfin){
#   
#     df_interior <- dfin %>% 
#       filter(!is.na(Closeness),
#              !is.na(ClusteringCoefficient)) %>% 
#       mutate(URF = replace_na(URF, 0)) 
#     
#     if("FragmentationCentrality_Total" %in% colnames(df_interior)){
#       
#       df_all <- df_interior %>% 
#         select(PortionVessels, UniqueVessels, Closeness, Strength, ClusteringCoefficient,
#                FragmentationCentrality_Total, H2, Modularity, URF,
#                ClusteringCoefficient_Global, NetworkCentralization, Mean_Weight, N_Edges,
#                N_Fisheries, SS_Vessels, SS_Revenue, SS_Weeks)
#       
#       df_regionnetwork <- df_interior %>%
#         select(PortionVessels, UniqueVessels, Closeness, Strength, ClusteringCoefficient,
#                FragmentationCentrality_Total, H2, Modularity, URF,
#                ClusteringCoefficient_Global, NetworkCentralization, Mean_Weight, N_Edges)
#       
#       df_fisherynode <- df_interior %>%
#         select(PortionVessels, UniqueVessels, Closeness, Strength, ClusteringCoefficient,
#                FragmentationCentrality_Total)
#       
#     } else {
#       
#       df_all <- df_interior %>% 
#         select(PortionVessels, UniqueVessels, Closeness, Strength, ClusteringCoefficient,
#                FragmentationCentrality, FragMeasure, H, Modularity, URF,
#                ClusteringCoefficient_Global, NetworkCentralization, Mean_Weight, N_Edges,
#                N_Fisheries, SS_Vessels, SS_Revenue, SS_Weeks)
#       
#       df_regionnetwork <- df_interior %>%
#         select(PortionVessels, UniqueVessels, Closeness, Strength, ClusteringCoefficient,
#                FragmentationCentrality, FragMeasure, H, Modularity, URF,
#                ClusteringCoefficient_Global, NetworkCentralization, Mean_Weight, N_Edges)
#       
#       df_fisherynode <- df_interior %>%
#         select(PortionVessels, UniqueVessels, Closeness, Strength, ClusteringCoefficient,
#                FragmentationCentrality)
#       
#     }
#     
#     df_nonetwork <- df_interior %>%
#       select(PortionVessels, UniqueVessels, 
#              N_Fisheries, SS_Vessels, SS_Revenue, SS_Weeks)
#     
#     return(list(All = df_all, 
#                 RegionNetwork = df_regionnetwork, 
#                 FisheryNode = df_fisherynode, 
#                 NoNetwork = df_nonetwork))
#   
# }

