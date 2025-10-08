transition5 <- function(cache_dfchoice, yi){

  i <- 4
  # y1 <- burnin-5
  allswitches <- list()
  for (i in 1:4){

    dft1 <- cache_dfchoice[[yi-5+i]] %>%
      select(Vessel_ID, FISHERY_ID) %>%
      filter(FISHERY_ID != "Not Fishing") %>%
      distinct() %>%
      mutate(Year = 1)
    dft2 <- cache_dfchoice[[yi-4+i]] %>%
      select(Vessel_ID, FISHERY_ID) %>%
      filter(FISHERY_ID != "Not Fishing") %>%
      distinct() %>%
      mutate(Year = 2)

    presentanddouble <- bind_rows(dft1, dft2) %>%
      group_by(Vessel_ID) %>%
      summarise(YearsPresent = n_distinct(Year), .groups = "drop") %>%
      filter(YearsPresent == 2)

    all <- bind_rows(dft1, dft2) %>%
      group_by(Vessel_ID, FISHERY_ID) %>%
      filter(n() == 1) %>%  # keep only FISHERIES those present in one year
      ungroup() %>%
      filter(Vessel_ID %in% presentanddouble$Vessel_ID)

    t1check <- dft1 %>%
      filter(Vessel_ID %in% all$Vessel_ID)

    t2check <- dft2 %>%
      filter(Vessel_ID %in% all$Vessel_ID)

    as_each <- full_join(t1check, t2check, by = c("Vessel_ID"),
                         relationship = "many-to-many") %>%
      filter(FISHERY_ID.x != FISHERY_ID.y) %>%
      rowwise() %>%
      mutate(pair = list(sort(c(FISHERY_ID.x, FISHERY_ID.y)))) %>%
      filter(length(pair) == 2)
    
    if(nrow(as_each) == 0){
      allswitches[[i]] <- NULL
      next
    }
    
    as_eachgo <- as_each %>%
      mutate(F1 = pair[[1]], F2 = pair[[2]]) %>%
      ungroup() %>%
      count(F1, F2, name = "weight")
    
    # as_eachgo <- as_each %>%
    #   rowwise() %>%
    #   mutate(pair = list(sort(c(FISHERY_ID.x, FISHERY_ID.y)))) %>%
    #   mutate(F1 = pair[[1]], F2 = pair[[2]]) %>%
    #   ungroup() %>%
    #   count(F1, F2, name = "weight")
    
    allswitches[[i]] <- as_eachgo

  }

  out <- bind_rows(allswitches) %>%
    group_by(F1, F2) %>%
    summarise(weight = sum(weight), .groups = "drop")

}



