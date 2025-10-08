library(patchwork)
library(tidyverse)
library(data.table)
library(evd)
library(nloptr)
library(DEoptim)

list.files("../westcoast-networks/scripts/02_Simulation/0_Port_Regions/00_CalibrationFunctions/",
           full.names = TRUE) %>%
  walk(source)
list.files("../westcoast-networks/scripts/02_Simulation/0_Port_Regions/01_Choice_Function_Functions/",
           full.names = TRUE) %>%
  walk(source)
source("../westcoast-networks/scripts/02_Simulation/0_Port_Regions/06_MetaNetwork/CalibrationBenchmarkReadin_MetaNetwork.R")
source("../westcoast-networks/scripts/02_Simulation/0_Port_Regions/01_Choice_Function_Functions/01_dimensionprep.R")
source("../westcoast-networks/scripts/02_Simulation/0_Port_Regions/01_Choice_Function_Functions/02a_AdjRevFcns.R")


# optimize ----------------------------------------------------------------

stol <- .1
dtol <- 10
max_iter_loop <- 5
max_iter_sc <- 100
max_iter_fc <- 25
vessels_in <- 350
scalet_1ev <- 1000
year_ref <- 2017
asc_fc_blanks_meta <- fcn_asc_fc_meta(fishing_bench)
prep_df_fc <- asc_fc_blanks_meta$prep_df_fc
asc_sc_blanks_meta <- fcn_asc_sc_meta(switching_bench)

guess <- start$df_exprev %>%
  mutate(RegionFishery = paste0(FISHERY_ID,", ",Region)) %>%
  semi_join(prep_df_fc, by = c("LengthBin", "RegionFishery")) %>%
  distinct(LengthBin, RegionFishery, ExpRev) %>%
  filter(ExpRev > 0) %>%
  group_by(RegionFishery, LengthBin) %>%
  summarise(Min = min(ExpRev)) %>%
  ungroup() %>%
  arrange(LengthBin, RegionFishery) %>%
  group_split(LengthBin)

asc_fc_0 <- map(guess, ~(.x$Min))
asc_fc_in <- asc_fc_0
set.seed(Sys.time())
asc_sc_0 <- runif(length(asc_sc_blanks_meta$asc_sc_0), 0, 10000)

ebench <- Inf
asc_sc_try <- asc_sc_0
# asc_sc_try
tryerror <- list()
tryscost <- list()
tryedges <- list()
for (try in 1:30){
  print(try)
  tryscost[[try]] <- asc_sc_try
  e <- obj_fcn1_e_meta(asc_fc_in, asc_sc_try,
                       switching_bench, targets_lower)
  tryerror[[try]] <- e
  tryedges[[try]] <- choices_asc_cal(start, year_ref, vessels_in,
                                  fishery_pairs, prep_df_fc, scale_t1ev,
                                  asc_sc = asc_sc_try, asc_fc = asc_fc_in)$edges_ext
  if(e < ebench){
    ebench <- e
    asc_sc_0 <- asc_sc_try
  }
  set.seed(Sys.time())
  asc_sc_try <- runif(length(asc_sc_blanks_meta$asc_sc_0), 0, 20000)
  print(e)
  # print(asc_sc_0)
}
tryflat <- map_dfr(1:30, function(i){

  ee_start_lower <- tryedges[[i]] %>%
    semi_join(targets_list, by = c("Fishery1", "Fishery2")) %>%
    arrange(as.character(Fishery1), as.character(Fishery2))
  out <- asc_sc_blanks_meta$prep_sc_mat %>%
    select(Fishery1, Fishery2) %>%
    mutate(EdgeN = 1:46,
           Costs = tryscost[[i]],
           Error = tryerror[[i]]) %>%
    left_join(ee_start_lower, by = c("Fishery1", "Fishery2")) %>%
    left_join(targets_list, by = c("Fishery1", "Fishery2")) %>%
    mutate(Dif = UniqueVessels-EdgeExp,
           AbsDif = abs(Dif))

})
trybest <- tryflat %>%
  group_by(EdgeN) %>%
  slice_min(order_by = AbsDif, with_ties = F)
use <- asc_sc_blanks_meta$prep_sc_mat %>%
  select(Fishery1, Fishery2) %>%
  left_join(trybest, by = c("Fishery1", "Fishery2"))
asc_sc_0 <- use$Costs
obj_fcn1_e_meta(asc_fc_in, asc_sc_0,
                switching_bench, targets_lower)

obs_0 <- choices_asc_cal(start, year_ref, vessels_in, fishery_pairs, prep_df_fc, scale_t1ev,
                         asc_sc = asc_sc_0, asc_fc = asc_fc_in)

shares_0 <- fishing_bench %>%
  left_join(obs_0$fishing, by = join_by(LengthBin, RegionFishery)) %>%
  mutate(Shares.y = replace_na(Shares.y, 0))
plot(shares_0$Shares.x, shares_0$Shares.y)
cor(shares_0$Shares.x, shares_0$Shares.y)
targets_list <- switching_bench %>%
  select(Fishery1, Fishery2, EdgeExp) %>%
  filter(EdgeExp > 2) %>%
  arrange(as.character(Fishery1), as.character(Fishery2))
ee_start_lower <- obs_0$edges_ext %>%
  semi_join(targets_list, by = c("Fishery1", "Fishery2")) %>%
  arrange(as.character(Fishery1), as.character(Fishery2))
plot(targets_list$EdgeExp, ee_start_lower$UniqueVessels)
cor(targets_list$EdgeExp, ee_start_lower$UniqueVessels)

asc_fc_in <- asc_fc_0
asc_sc_in <- asc_sc_0

cache_edges <- matrix(nrow = nrow(targets_list), ncol = max_iter_loop + 2)
cache_shares <- matrix(nrow = nrow(shares_0), ncol = max_iter_loop + 2)

cache_sc <- matrix(nrow = length(asc_sc_0), ncol = max_iter_loop + 1)
cache_fc <- matrix(nrow = length(unlist(asc_fc_0)), ncol = max_iter_loop + 1)
cache_alg_fc <- matrix(nrow = length(asc_fc_0), ncol = max_iter_loop)
cache_alg_sc <- matrix(nrow = 2, ncol = max_iter_loop)

cache_edges[,1] <- targets_list$EdgeExp
cache_shares[,1] <- fishing_bench$Shares
cache_sc[,1] <- c(asc_sc_0)
cache_fc[,1] <- unlist(asc_fc_0)
cache_edges[,2] <- ee_start_lower$UniqueVessels
cache_shares[,2] <- shares_0$Shares.y


# The real thing ----------------------------------------------------------

i <- 1
for (i in 1:max_iter_loop){

  fc_benchmark <- unlist(asc_fc_in)
  sc_benchmark <- asc_sc_in

  result <- nloptr(
    x0 = asc_sc_in,
    eval_f = function(x) {
      obj_fcn1_e_meta(asc_fc_in, asc_sc_in = x, switching_bench, targets_lower)
    },
    opts = list(
      algorithm = "NLOPT_LN_NELDERMEAD",
      maxeval = max_iter_sc,
      ftol_rel = .05,
      xtol_rel = .05,
      print_level = 1
    )
    ,
    lb = rep(0, length(asc_sc_0)),
    ub = rep(25000, length(asc_sc_0))
  )

  asc_sc_in <- result$solution
  cache_alg_sc[1,i] <- result$iterations
  cache_alg_sc[2,i] <- result$objective

  # length(asc_fc_0)
  j <- 1
  for (j in 1:length(asc_fc_0)) {
    cat("Optimizing ASC for LengthBin", j, "\n")
    S_obs <- targetshares[[j]]$Shares
    delta_costs <- asc_fc_in[[j]]

    # max_iter_fc
    for (iter in 1:max_iter_fc) {
      # delta_costs <- delta_new
      S_sim <- share_sim_j_meta(delta_costs, j, prep_df_fc, asc_fc_in, asc_sc_in)

      # if(max(abs(S_sim - S_obs)) > .5){
      # delta_new <- delta_costs - log(pmax(S_obs, 1e-8) / pmax(S_sim, 1e-8)) * 10
      # } else {
      delta_new <- pmax(delta_costs - log(pmax(S_obs, 1e-8) / pmax(S_sim, 1e-8)) * 50,100)
      # }

      # S_obs
      # S_sim
      # delta_costs
      # delta_new

      if (max(abs(S_sim - S_obs)) < stol
          || max(abs(log(pmax(S_obs, 1e-8) / pmax(S_sim, 1e-8)))) < 1
      ) {
        cat("  Converged in", iter, "iterations\n Costs: $",
            round(delta_new), "\n")
        break
      }

      delta_costs <- delta_new
      print(paste0(iter,": ",sort(abs(round(S_obs-S_sim,4)))))
    }

    # Store updated delta
    asc_fc_in[[j]] <- delta_costs
    cache_alg_fc[[j,i]] <- iter
  }

  cache <- choices_asc_cal(start, year_ref, vessels_in, fishery_pairs, prep_df_fc, scale_t1ev,
                           asc_sc = asc_sc_in, asc_fc = asc_fc_in)

  shares_cache <- asc_fc_blanks$prep_df_fc %>%
    left_join(cache$fishing) %>%
    filter(RegionFishery != "Not Fishing") %>%
    mutate(Shares = replace_na(Shares, 0))
  ee_cache <- cache$edges_ext %>%
    semi_join(targets_list, by = c("Fishery1", "Fishery2")) %>%
    arrange(as.character(Fishery1), as.character(Fishery2))

  cache_edges[,i+2] <- ee_cache$UniqueVessels
  cache_shares[,i+2] <- shares_cache$Shares
  cache_sc[,i+1] <- c(asc_sc_in)
  cache_fc[,i+1] <- unlist(asc_fc_in)

  if ((max(abs(cache_sc[,i]-cache_sc[,i+1]))<200)*
      (max(abs(cache_fc[,i]-cache_fc[,i+1]))<200)*
      (max(abs(cache_edges[,1]-cache_edges[,i+2]))<5)*
      (max(abs(cache_shares[,1]-cache_shares[,i+2]))<.05)) {
    cat("  Converged in", i, "\n")
    break
  } else {
    cat("Completed iter",i,"\n")
  }
}

cache_all <- list(cache_alg_fc = cache_alg_fc, cache_alg_sc = cache_alg_sc,
                  cache_edges = cache_edges, cache_sc = cache_sc,
                  cache_shares = cache_shares, cache_fc = cache_fc)
write_rds(cache_all, paste0("../westcoast-networks/data/Simulation/ASC_Calibration_RegionalMeta.rds"))
cache_all <- read_rds("../westcoast-networks/data/Simulation/ASC_Calibration_RegionalMeta.rds")

cache <- choices_asc_cal(start, year_ref, vessels_in, fishery_pairs, prep_df_fc, scale_t1ev = 1000,
                         asc_sc = cache_all$cache_sc[,5], asc_fc = cache_all$cache_fc[,5])


plot(cache_shares[,1],cache_shares[,5])
cor(cache_shares[,1],cache_shares[,2])
cor(cache_shares[,1],cache_shares[,5])

plot(cache_edges[,1],cache_edges[,7])
cor(cache_edges[,1],cache_edges[,7])
cor(cache_edges[,2],cache_edges[,7])


