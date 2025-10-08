pindex <- 2:8
n <- 20

bigi <- 1
# meta <- ifelse(a %in% 1:3, F, T)
# df <- files[[a]][[1]]
pindex <- 3:9
p <- 8
i <- 2
intersect <- F

dfbind <- function(df, pindex, meta, n, bigi, intersect){
  
  pname <- names(df[[1]])
  
  allproj <- map(pindex, function(p){
    
    # batch_results <- map(batches, function(bidx){
    # 
    df_out <- 
      suppressMessages(
        map(1:n, function(i){
          print(paste(bigi, pname[p],i))
          
          x <- df[[i]][[p]]
          if(is.null(x) ||
             nrow(x)<=1){
            return(NULL)
          }
          
          temp1 <- as_tibble(x) 
          # %>%
          #   drop_na()
          
          if(str_detect(pname[p],"v")){
            temp <- temp1 %>% 
              group_by(Component) %>% 
              mutate(MeanStrength = mean(Strength)) %>% 
              ungroup() %>% 
              distinct(across(-c(3:7,15))) %>% 
              select(-any_of("Cluster")) 
            # print(unique(temp$Component))
          } else if(meta == F){
            temp <- temp1 %>% 
              select(-any_of("Cluster")) %>% 
              filter(Fishery == Drop) 
          } else {
            temp <- temp1 %>% 
              select(-any_of("Cluster")) %>% 
              filter(map2_lgl(Fishery, Drop, ~ .x %in% .y)) 
          }
          
          
          
        }) %>% 
          compact() %>% 
          bind_rows()
      )
    
  })
  
  if(intersect == T){
    # get only the iter columns for all non-null dfs
    iter_lists <- map(allproj, ~ if(!is.null(.x)){ .x$iter} else {NULL} ) 
    iter_lists <- compact(iter_lists)  # remove NULLs
    
    # find intersection of iterations across all p
    all_iters <- reduce(iter_lists, intersect)
    allproj <- map(allproj, ~ filter(.x, iter %in% all_iters))
  }
  
  names(allproj) <- pname[pindex]
  
  return(allproj)
  
}

dfmedplus <- function()