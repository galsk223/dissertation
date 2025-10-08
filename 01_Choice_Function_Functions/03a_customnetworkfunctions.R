# nlinks <- nvessels
# nnodes <- nfisheries

sparsityindex <- function(ey, nlinks, nnodes, projection){
  
  ey_si <- ey %>% 
    group_by(weight) %>% 
    add_tally() %>% 
    select(weight,n) %>% 
    unique() %>% 
    arrange(weight)
  
  w <- ey_si$weight
  f <- ey_si$n
  
  sum <- matrix(nrow = length(w),
                ncol = 1)
  
  # i <- 22
  if(nrow(ey_si) > 0){
    
    sum[1,1] <- 0
    
    if(length(w) > 1){
      
      for (i in 2:(length(w)-1)){
        
        sum[i,1] <- w[i]*f[i]*(f[i]+
                                 2*sum(f[(i+1):length(f)]))
        
      }
      
    }
    
    sum[length(w),1] <- w[length(w)]*f[length(w)]*2*(f[length(w)])
    
    # nnodes <- nrow(vy)
    if(projection == "bipartite"){
      edges_possible <- nnodes*nlinks
      t1 <- edges_possible*53
    } else {
      edges_possible <- choose(nnodes,2)
      if(projection == "extensive"){
        t1 <- edges_possible*nlinks
      } else if(projection == "fuller"){
        w <- 53/nnodes
        t1 <- edges_possible*nlinks*(w*w)/53*(w+w)
      } else if(projection == "tc"){
        t1 <- edges_possible
      } else if(projection == "tt"){
        t1 <- edges_possible*nlinks*4
      } else if(projection == "ev"){
        t1 <- edges_possible*nlinks
      } else if(projection == "fv"){
        w <- 53/nnodes
        t1 <- edges_possible*nlinks*(w*w)/53*(w+w)
      }
      
    }
    max(1-sum(sum)/(edges_possible*t1),0)
    
  } else {
    0
  }
  
}

networkcentralization_bipartite <- function(strength, eb, nfisheries, nvessels){
  
  s <- strength
  sstar <- max(strength)
  wbar <- mean(eb$freq)
  n <- nfisheries+nvessels
  return(sum(sstar-s)/((n-2)*(n-1)*wbar))
  
}

networkcentralization <- function(gy){
  
  s <- igraph::strength(gy)
  sstar <- max(igraph::strength(gy))
  wbar <- mean(E(gy)$weight)
  n <- vcount(gy)
  return(sum(sstar-s)/((n-2)*(n-1)*wbar))
  
}

fragmentation <- function(Ainv){
  
  gd <- graph_from_adjacency_matrix(Ainv, mode = "undirected", weighted = TRUE, diag = FALSE)
  distances <- distances(gd, mode = "all", weights = E(gd)$weight)
  
  diag(distances) = Inf
  weights = 1/distances
  m = max(weights)
  sum = sum(weights)
  1 - sum/(ncol(distances) * (ncol(distances) - 1) * m)
  
}

fragmentation_bipartite <- function(affiliation.mat){
  
  mafia <- as.matrix(affiliation.mat)
  fakenet <- graph_from_biadjacency_matrix(mafia, weighted = T)
  fragnet<-t(as_biadjacency_matrix(fakenet))###transpose to project second mode
  projsecmode <- try(projecting_tm(fragnet, method = "Newman"), silent = TRUE)
  
  if (inherits(projsecmode, "try-error")) {
    # message("Projection error")
    return(NA)
  }
  distsecproj<-distance_w(projsecmode, directed=NULL, gconly=FALSE)
  
  distnet2<-(sum(1/distsecproj, na.rm=TRUE))
  distnet2<-((ncol(mafia)^2)-ncol(mafia))/distnet2
  
}



