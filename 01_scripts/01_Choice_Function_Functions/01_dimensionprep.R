# regions <- unique(start$df_filter$Region)
# 
# region <- regions[3]
# year <- 2017
# df_blanks <- start$df_filter
# df_edges <- start$df_edges
# nvessels <- 200

fcn_asc_fc <- function(df_blanks, region, year){

  prep_df_fc <- df_blanks %>%
    filter(Region == region,
           LANDING_YEAR == year) %>% 
    distinct(LengthBin, FISHERY_ID) %>% 
    arrange(LengthBin, FISHERY_ID)
  
  asc_fc_list <- prep_df_fc %>% 
    group_split(LengthBin)
  
  asc_fc_blanks_list <- map(asc_fc_list, ~ rep(0, nrow(.x)))
  
  return(list(prep_df_fc = prep_df_fc,
         asc_fc_blanks_list = asc_fc_blanks_list))
  
}

# df_edges <- start$df_edges
# nvessels <- 200
fcn_asc_sc <- function(df_edges, region, year, nvessels){
  
  prep_sc_mat <- df_edges %>% 
    filter(Region == region,
           LANDING_YEAR == year) %>% 
    mutate(EdgeNorm = NormUV*nvessels,
           InvEdge = ifelse(EdgeNorm < 0.5, Inf, 1/EdgeNorm)) %>% 
    distinct(Fishery1, Fishery2, InvEdge) %>% 
    mutate(SC = exp(InvEdge)*1000) %>% 
    select(-c(InvEdge)) %>% 
    filter(SC != Inf) %>%
    mutate(F1 = pmin(as.character(Fishery1), as.character(Fishery2)),
           F2 = pmax(as.character(Fishery1), as.character(Fishery2))) %>%
    as.data.table() %>% 
    distinct(F1, F2, SC) %>%
    rename(Fishery1 = F1, Fishery2 = F2) %>% 
    arrange(Fishery1, Fishery2) 
  
  asc_sc_0 <- prep_sc_mat$SC*0
  
  return(list(prep_sc_mat = prep_sc_mat,
         asc_sc_0 = asc_sc_0))
  
}





# meta --------------------------------------------------------------------

# df_blanks <- fishing_bench
fcn_asc_fc_meta <- function(df_blanks){
  
  prep_df_fc <- df_blanks %>% 
    distinct(LengthBin, RegionFishery) %>% 
    arrange(LengthBin, RegionFishery)
  
  asc_fc_list <- prep_df_fc %>% 
    group_split(LengthBin)
  
  asc_fc_blanks_list <- map(asc_fc_list, ~ rep(0, nrow(.x)))
  
  return(list(prep_df_fc = prep_df_fc,
              asc_fc_list = asc_fc_list,
              asc_fc_blanks_list = asc_fc_blanks_list))
  
}

# df_edges <- switching_bench
# nvessels <- 200
fcn_asc_sc_meta <- function(df_edges){
  
  prep_sc_mat <- df_edges %>%  
    mutate(InvEdge = ifelse(EdgeExp < 2, Inf, 1/EdgeExp)) %>% 
    distinct(Fishery1, Fishery2, InvEdge, EdgeExp) %>% 
    mutate(SC = exp(15*InvEdge)*100) %>% 
    select(-c(InvEdge)) %>% 
    filter(SC != Inf) %>%
    mutate(F1 = pmin(as.character(Fishery1), as.character(Fishery2)),
           F2 = pmax(as.character(Fishery1), as.character(Fishery2))) %>%
    as.data.table() %>% 
    distinct(F1, F2, SC) %>%
    rename(Fishery1 = F1, Fishery2 = F2) %>% 
    arrange(Fishery1, Fishery2) 
  
  asc_sc_0 <- prep_sc_mat$SC*0
  asc_sc <- prep_sc_mat$SC
  
  return(list(prep_sc_mat = prep_sc_mat,
              asc_sc_0 = asc_sc_0,
              asc_sc = asc_sc))
  
}

