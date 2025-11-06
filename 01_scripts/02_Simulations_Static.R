rm(list = ls())
source("/home/gkoss/dissertation/01_scripts/01_Choices_Function_MetaExpansion.R")


# ingest and static parameters --------------------------------------------

year_ref <- 2017
scale_t1ev <- 1000
cache_all <- read_rds("/home/gkoss/westcoast-networks/data/Simulation/ASC_Calibration_RegionalMeta.rds")
asc_sc_start <- cache_all$cache_sc[,5]
asc_fc_start <- cache_all$cache_fc[,5]
fisherynet <- read_rds("~/westcoast-networks/data/clean/Simulation/topregionfisherylist.rds")
fisherylistin <- fisherynet$FISHERY_ID
start <- startmet
vessels_in <- 320
subgraph_use <- "Meta"
skillrand <- F
dropves <- F
costbyfishery <- F
entry_opt <- "portion"
drop <- NA
ds <- NA
shockpermanent <- F
closureresponse <- F
scalerand <- F

# sc and fc indices -------------------------------------------------------

asc_fc <- asc_fc_start
asc_sc <- asc_sc_start
fleetnull <- 350
vset <- start$df_vsets %>%
  filter(LANDING_YEAR == 2017) %>%
  mutate(SimFleet = fleetnull,
         RegionExp = RegionAll/YearAll*SimFleet,
         LengthRegionExp = RegionExp*Prob) %>%
  filter(LengthRegionExp > 5)
df_all <- start$df_filter %>%
  filter(LANDING_YEAR %in% 2017,
         LengthBin %in% vset$LengthBin &
           Region%in% vset$Region) %>%
  mutate(RegionFishery = paste0(FISHERY_ID,", ",Region)) %>%
  distinct(LengthBin, RegionFishery)
asc_fc_blanks <- fcn_asc_fc_meta(df_all)
asc_sc_blanks <- fcn_asc_sc_meta(start$df_edges_meta %>%
                                   filter(LANDING_YEAR == 2017)%>%
                                   mutate(SimFleet = vessels_in,
                                          EdgeExp = SimFleet*NormUV))
prep_sc_mat <- asc_sc_blanks$prep_sc_mat
prep_df_fc <- asc_fc_blanks$prep_df_fc

df_fc <- prep_df_fc %>%
  arrange(LengthBin, RegionFishery) %>%
  rownames_to_column() %>%
  mutate(FC = unlist(asc_fc)) %>%
  rename(FISHERY_ID = RegionFishery)

fcindex <- df_fc %>%
  filter(str_detect(FISHERY_ID, "Crab|Salmon"))

scindex <- prep_sc_mat %>%
  rownames_to_column() %>%
  mutate(KeyKey = ifelse(str_detect(Fishery1, "Crab|Salmon") &
                           str_detect(Fishery2, "Crab|Salmon"),1,0),
         KeyNotKey = ifelse((str_detect(Fishery1, "Crab|Salmon") &
                               !str_detect(Fishery2, "Crab|Salmon")) |
                              (!str_detect(Fishery1, "Crab|Salmon") &
                                 str_detect(Fishery2, "Crab|Salmon")),1,0),
         NotKeyNotKey = ifelse(!str_detect(Fishery1, "Crab|Salmon") &
                                 !str_detect(Fishery2, "Crab|Salmon"),1,0))


# set remaining parameter  ------------------------------------------------

nyears <- 1
burnin <- 1
distparameter <- .3
vessels_in <- 320
options(future.rng.onMisuse = "ignore")
log_dir <- "/home/gkoss/westcoast-networks/data/Simulation/StaticSimulationOutcomes/Logs/"

# Bootstrap (Inner) -------------------------------------------------------

jall <- c("scalar", "sc", "kk", "knk", "nknk",
          "kk1", "kk2",
          "knk1", "knk2",
          "nknk1", "nknk2")

j <- 2
for(j in 4:10){

  log_file <- file.path(log_dir, sprintf("worker_%02d.log", j))
  if (file.exists(log_file)) file.remove(log_file)
  write(sprintf("=== Starting worker j = %d at %s ===\n", j, Sys.time()),
        file = log_file, append = TRUE)

  plan(sequential)
  plan(multisession, workers = 16)
  cache_simulation_par <- furrr::future_map(1:100,function(s){

    # cache_iter <- list()
    # for(s in setwrite[1:3]){
    msg <- sprintf("j = %d, s = %d : starting at %s", j, s, Sys.time())
    write(msg, file = log_file, append = TRUE)

    cat(paste("\n Bootstrap", s,"\n"))

    set.seed(s)
    rscalef <- 1
    asc_fc <- asc_fc_start

    if(jall[[j]] == "scalar"){
      rscalef <- runif(1,1,3)
      asc_fc <- asc_fc_start*rscalef
      rscales <- runif(1,1,3)
      asc_sc <- asc_sc_start*rscales
    }

    if(jall[[j]] == "sc"){
      rscales <- runif(1,1,5)
      asc_sc <- asc_sc_start*rscales
    }

    if(str_detect(jall[[j]],"1")){
      rscalef <- runif(1,1,3)
      asc_fc <- asc_fc_start
      asc_fc[as.numeric(fcindex$rowname)] <- asc_fc[as.numeric(fcindex$rowname)]*rscalef
    } else if(str_detect(jall[[j]],"2")) {
      rscalef <- runif(1,1,3)
      asc_fc <- asc_fc_start
      asc_fc[-as.numeric(fcindex$rowname)] <- asc_fc[-as.numeric(fcindex$rowname)]*rscalef
    }

    if(str_detect(jall[[j]],"kk")) {

      rscales <- runif(1,1,5)
      asc_sc <- asc_sc_start
      asc_sc[as.numeric(scindex$rowname[scindex$KeyKey == 1])] <- asc_sc[as.numeric(scindex$rowname[scindex$KeyKey == 1])]*rscales

    } else if(str_detect(jall[[j]],"knk")) {

      rscales <- runif(1,1,5)
      asc_sc <- asc_sc_start
      asc_sc[as.numeric(scindex$rowname[scindex$KeyNotKey == 1])] <- asc_sc[as.numeric(scindex$rowname[scindex$KeyNotKey == 1])]*rscales

    } else if(str_detect(jall[[j]],"nknk")) {

      rscales <- runif(1,1,5)
      asc_sc <- asc_sc_start
      asc_sc[as.numeric(scindex$rowname[scindex$NotKeyNotKey == 1])] <- asc_sc[as.numeric(scindex$rowname[scindex$NotKeyNotKey == 1])]*rscales

    }

    sim_run <- choices_asc_meta(start, subgraph_use, year_ref, nyears, vessels_in, burnin,
                                scale_t1ev, asc_sc, asc_fc, fisherylistin, drop,
                                skillrand, costbyfishery, shockpermanent,
                                distparameter, closureresponse, entry_opt, dropves, ds)

    msg <- sprintf("j = %d, s = %d : finished at %s", j, s, Sys.time())
    write(msg, file = log_file, append = TRUE)

    sim_id <- tibble(
      iter = s,
      rscalef = rscalef,
      rscales = rscales,
      Subgraph = subgraph_use,
      vessels_in = vessels_in,
      Drop = drop,
      Distparameter = distparameter,
      Rand_Scale = scalerand,
      Rand_Skill = skillrand,
      CostbyFishery = costbyfishery,
      ClosureResponse = closureresponse,
      EntryOpt = entry_opt,
      dropves = dropves,
      Ds = ds
    )

    cache_iter <- list(sim_run = sim_run,
                       sim_id = sim_id)

    # }
  })

  write_rds(cache_simulation_par,
            paste0("/home/gkoss/westcoast-networks/data/Simulation/StaticSimulationOutcomes/Static_adjustment",jall[[j]],".rds"))

  plan(sequential)
  gc()

}

s1 <- read_rds("/home/gkoss/westcoast-networks/data/Simulation/StaticSimulationOutcomes/Static_adjustmentscalar.rds")
s2 <- read_rds("/home/gkoss/westcoast-networks/data/Simulation/StaticSimulationOutcomes/Static_adjustmentscalar_2.rds")
si <- s2[[1]]
sreiter <- map(s2, function(si){

  sout <- si$sim_id %>%
    mutate(iter = iter+100)
  sall <- list(sim_run = sim_run,
               sim_id = sout)

})
s <- c(s1,sreiter)
write_rds(s,
          paste0("/home/gkoss/westcoast-networks/data/Simulation/StaticSimulationOutcomes/Static_adjustmentscalar500.rds"))
