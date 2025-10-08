# regions <- unique(start$df_filter$Region)
# 
# region <- regions[3]
# year <- 2017
# df_blanks <- start$df_filter
# df_edges <- start$df_edges
# nvessels <- 200
start <- read_rds("westcoast-networks/data/clean/Simulation/empiricalbenchmarks_region.rds")

fsmall <- start$df_filter %>% 
  group_by(LengthBin, LANDING_YEAR, Region) %>% 
  summarise(NF = n_distinct(FISHERY_ID)) %>% 
  filter(NF > 2)

vsmall <- start$df_vsets %>% 
  mutate(NUse = round(Prob*200)) %>% 
  filter(NUse > 10)

togetheruse <- inner_join(fsmall, vsmall) %>% 
  arrange(Region, LANDING_YEAR, LengthBin) 

write_rds(togetheruse, "westcoast-networks/data/clean/Simulation/empiricalbenchmarks_usereduced.rds")
df_blanks <- start$df_filter

fcn_asc_fc <- function(df_blanks, region, year){
  
  regionuse <- togetheruse %>% 
    filter(Region == region,
           LANDING_YEAR == year)
  
  prep_df_fc <- df_blanks %>%
    filter(Region == region,
           LANDING_YEAR == year,
           LengthBin %in% regionuse$LengthBin) %>% 
    distinct(LengthBin, FISHERY_ID) %>% 
    arrange(LengthBin, FISHERY_ID)
  
  asc_fc_list <- prep_df_fc %>% 
    group_split(LengthBin)
  
  asc_fc_blanks_list <- map(asc_fc_list, ~ rep(0, nrow(.x)))
  
  return(list(prep_df_fc = prep_df_fc,
         asc_fc_blanks_list = asc_fc_blanks_list))
  
}

df_edges <- start$df_edges
nvessels <- 200
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

fulleredges <- function(connectivity, bipmat_0){
  
  total_weeks <- rowSums(bipmat_0)
  
  for (i in 1:ncol(bipmat_0)) {
    for (j in 1:ncol(bipmat_0)) {
      
      f1 <- colnames(bipmat_0)[i]
      f2 <- colnames(bipmat_0)[j]
      
      if (i != j) {
        sum_k <- 0
        for (k in 1:nrow(bipmat_0)) {
          Wik <- bipmat_0[k, i]
          Wjk <- bipmat_0[k, j]
          Wk <- total_weeks[k]
          if (Wk > 0) {
            sum_k <- sum_k + ((Wik * Wjk) / Wk)*(Wik+Wjk)
          }
        }
        connectivity[f1, f2] <- sum_k
      }
    }
  }
  
  out <- (connectivity / nrow(bipmat_0)) %>% 
    as.data.frame() %>%
    rownames_to_column("Fishery1") %>%
    pivot_longer(-Fishery1, names_to = "Fishery2", values_to = "Weight") %>%
    filter(Fishery1 != Fishery2) %>% 
    mutate(F1 = pmin(as.character(Fishery1), as.character(Fishery2)),
           F2 = pmax(as.character(Fishery1), as.character(Fishery2))) %>%
    as.data.table() %>% 
    distinct(F1, F2, Weight) %>%
    rename(Fishery1 = F1, Fishery2 = F2) %>% 
    arrange(Fishery1, Fishery2) 
  
  return(out)
}
