# affiliation.mat <- df_choice %>% 
#   filter(FISHERY_ID != "Not Fishing") %>% 
#   group_by(FISHERY_ID, Vessel_ID) %>% 
#   summarise(NTrips = n()) %>% 
#   ungroup() %>% 
#   pivot_wider(names_from = FISHERY_ID,
#               values_from = NTrips,
#               values_fill = 0) %>% 
#   select(-Vessel_ID)
# 
# fragmentation.centrality.bipartite(affiliation.mat)

fragmentation.centrality.bipartite <- function(affiliation.mat){

  mafia <- as.matrix(affiliation.mat)
  fakenet <- graph_from_biadjacency_matrix(mafia, weighted = T)
  
  ###intramodal fragmentation--takes ~1 minute
  nact<-nrow(mafia)+ncol(mafia)
  # mafiafrag1<-vector(length=nact)
  mafiafrag1 <- vector(length = ncol(mafia))
  # i <- 2
  
  # for (i in 1:nrow(mafia)){
  #   print(i)
  #   projnet<-fakenet
  #   fragnet<-delete_vertices(projnet, i) ##delete vertex for focal actor, fragnet is subgraph without i
  # 
  #   fragnet<-get.edgelist(fragnet)
  #   fragnet<-as.tnet(fragnet, type="binary two-mode tnet")
  #   projfirstmode<-projecting_tm(fragnet, method="Newman") ##project with Newman weights
  #   distfirstproj<-distance_w(projfirstmode, directed=NULL, gconly=FALSE) #shortest path algorithm
  # 
  #   distnet<-(sum(1/distfirstproj, na.rm=TRUE)) ##sum 1/distances, all distances are counted twice, so no need to multiply by two (as in equation)
  #   distnet<-((nrow(mafia)^2)-nrow(mafia))/distnet ##numerator is total number of potential edges in projection, ensures increasing values indicate increasing vulnerability
  # 
  #   mafiafrag1[i]<-distnet
  #   }
  i <- 393
  for (i in nrow(mafia):nact){
    # print(i)
    if(i==nrow(mafia)){i<-i+1}
    projnet<-fakenet
    fragnet<-delete_vertices(projnet, i)
    
    fragnet<-t(as_biadjacency_matrix(fragnet))###transpose to project second mode
    projsecmode <- try(projecting_tm(fragnet, method = "Newman"), silent = TRUE)
    
    if (inherits(projsecmode, "try-error")) {
      # message("Projection error at i = ", i)
      next
    }
    distsecproj<-distance_w(projsecmode, directed=NULL, gconly=FALSE)

    distnet2<-(sum(1/distsecproj, na.rm=TRUE))
    distnet2<-((ncol(mafia)^2)-ncol(mafia))/distnet2
    
    mafiafrag1[i-nrow(mafia)]<-distnet2  
    }
  
  intramodalmafiafrag <- mafiafrag1
  names(intramodalmafiafrag) <- colnames(mafia)
  
  
  ###cross modal fragmentation
  crossmafia<-vector(length = ncol(mafia))
  i <- nrow(mafia)+1
  for (i in nrow(mafia):nact){
    # print(i)
    if(i==nrow(mafia)){i<-i+1}
    
    projnet<-fakenet
    fragnet<-delete_vertices(projnet, i) ##delete focal vertex
    
    fragnet<-as_edgelist(fragnet)
    # projfirstmode<-projecting_tm(fragnet, method="Newman") ##project first mode
    projfirstmode <- try(projecting_tm(fragnet, method = "Newman"), silent = TRUE)
    
    if (inherits(projfirstmode, "try-error")) {
      # message("Projection error at i = ", i)
      next
    }
    
    
    distfirstproj<-distance_w(projfirstmode, directed=NULL, gconly=FALSE)
    
    
    distnet<-(sum(1/distfirstproj, na.rm=TRUE))
    distnet<-((nrow(mafia)^2)-nrow(mafia))/distnet ###numerator is size of first mode network
    
    crossmafia[i-nrow(mafia)]<-distnet
    if(i==nact){break}
  }
  # 
  # for (i in 1:nrow(mafia)){##first mode
  #   
  #   projnet<-fakenet
  #   fragnet<-delete_vertices(projnet, i)
  #   
  #   fragnet<-t(get.incidence(fragnet))##take transpose to project second mode
  #   projsecmode<-projecting_tm(fragnet, method="Newman")
  #   distsecproj<-distance_w(projsecmode, directed=NULL, gconly=FALSE)
  #   
  #   #projsecmode$w
  #   
  #   distnet2<-(sum(1/distsecproj, na.rm=TRUE))
  #   distnet2<-((ncol(mafia)^2)-ncol(mafia))/distnet2 ##constrain by total potential edges in second mode network
  #   
  #   crossmafia[i]<-distnet2
  # }
  
  crossnodalmafia <- crossmafia
  names(crossnodalmafia) <- colnames(mafia)
  
  ##total fragmentation
  totalmafia<-(crossmafia+mafiafrag1)/2
  names(totalmafia) <- colnames(mafia)
  
  fragmentation.output<-tibble(FISHERY_ID = colnames(mafia),
                               Intramodal = intramodalmafiafrag,
                               Crossmodal = crossnodalmafia,
                               Overall = totalmafia)
  
  fragmentation.output
  
}



