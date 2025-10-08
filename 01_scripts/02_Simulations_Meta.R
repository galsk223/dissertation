rm(list = ls())
source("01_scripts/01_Choices_Function_MetaExpansion.R")
library(furrr)
# library(foreach)
# library(doParallel)
library(parallel)

year_ref <- 2017
scale_t1ev <- 1000
nyears <- 40
burnin <- 14
e17 <- read_rds("westcoast-networks/data/Simulation/ASC_Calibration_Regional/Eureka_2017_fuller.rds")
cache_all <- read_rds("~/westcoast-networks/data/Simulation/ASC_Calibration_RegionalMeta.rds")

# run conditions 1 and 2
# runconditions <- tibble(
#   subgraph_use = c("Region","Region","Region","Meta","Meta","Meta"),
#   scalerand = c(F,T,T,F,T,T),
#   skillrand = c(F,T,T,F,T,T),
#   costbyfishery = c(F,F,T,F,F,T),
#   # shockpermanent = c(),
#   closureresponse = c(F,F,T,F,F,T)
# )

# run conditions 3
runconditions <- tibble(
  entry_opt = c("portion","sluggish","random","portion","sluggish","random"),
  dropves = c(F,F,F,T,T,T)
)

costbyfishery <- T
closureresponse <- T
subgraph_use <- "Meta"
shockpermanent <- F
scalerand <- T
skillrand <- T
entry_opt <- "flat"
dropves <- F

# entry_opt <- c("portion", "sluggish", "random")
# entry <- entry_opt[[3]]
# dropves <- F

# >> only run once <<
# base_name <- "~/westcoast-networks/data/Simulation/DynamicSimulationOutcomes/MLSizeCacheMeta2_"
# numbers <- 1:nrow(runconditions)
# for (i in numbers) {
#   dir.create(file.path(paste0(base_name, i)))
# }
log_dir <- "/home/gkoss/westcoast-networks/data/Simulation/DynamicSimulationOutcomes/MLSizeCacheMeta2_Logs/"
# dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)


# Loop Outer --------------------------------------------------------------

j <- 7
# nyears <- 8
# burnin <- 5
for(j in 1:nrow(runconditions)){

  print(j)
  # entry_opt <- runconditions$entry_opt[[j]]
  entry_opt <- "flat"
  # dropves <- runconditions$dropves[[j]]
  dropves <- F

  log_file <- file.path(log_dir, sprintf("worker_%02d.log", j))
  if (file.exists(log_file)) file.remove(log_file)
  write(sprintf("=== Starting worker j = %d at %s ===\n", j, Sys.time()),
        file = log_file, append = TRUE)

  currentlf <- as.numeric(
    str_extract_all(
      list.files(
        paste0("/home/gkoss/westcoast-networks/data/Simulation/DynamicSimulationOutcomes/MLSizeCacheMeta2_",j,"/"))
      ,"[0-9]+"))
  setwrite <- setdiff(1:500,currentlf)

  if(length(setwrite) == 0) {
    next
  }
# Bootstrap (Inner) -------------------------------------------------------

  s <- 1
  # plan(sequential)
    plan(multisession, workers = 6)
    # # options(future.rng.onMisuse="ignore")
    cache_simulation_par <- furrr::future_map(setwrite,function(s){

    # for(s in setwrite[1:3]){
      msg <- sprintf("j = %d, s = %d : starting at %s", j, s, Sys.time())
      write(msg, file = log_file, append = TRUE)

      cat(paste("\n Bootstrap", s,"\n"))

      if(subgraph_use == "Region"){
        asc_fc_reg <- e17$cache_fc[,ncol(e17$cache_fc)]
        asc_sc_reg <- e17$cache_sc[,ncol(e17$cache_sc)]
        start <- startreg
        asc_sc_start <- asc_sc_reg
        asc_fc_start <- asc_fc_reg
        drop <- "CA Dungeness Crab"
        vessels_in <- 200
      } else {
        asc_sc_met <- cache_all$cache_sc[,5]
        asc_fc_met <- cache_all$cache_fc[,5]
        start <- startmet
        asc_sc_start <- asc_sc_met
        asc_fc_start <- asc_fc_met
        fisherylistuse <- unique(startmet$df_exprev$RegionFishery)
        drop_candidate <- fisherylistuse[str_detect(fisherylistuse, "Dungeness|Salmon")]
        distparameter <- .3
        vessels_in <- 350
      }

      set.seed(1000 + s)
      rscalef <- runif(1,.2,2)
      rscales <- runif(1,.2,2)
      asc_fc <- asc_fc_start*rscalef
      asc_sc <- asc_sc_start*rscales
      distparameter <- .3

      if(scalerand == T){
        rscalef <- runif(length(asc_fc_start),0,1.5)
        rscales <- runif(length(asc_sc_start),0,1.5)
        asc_fc <- asc_fc_start*rscalef
        asc_sc <- asc_sc_start*rscales
        distparameter <- runif(1,0,10)
        vessels_in <- round(runif(1,125,425))
      }

      if(closureresponse == T){
        vessels_in <- round(runif(1,125,425))
      }
      ds <- NA
      if(dropves == T){
        ds <- runif(1,.5,1)
      }

      if(subgraph_use == "Region"){
        drop <- "CA Dungeness Crab"} else {
          # drop <- sample(drop_candidate,round(runif(1,6,7)))
          drop <- drop_candidate
        }

      if(dropves == F){
        cat(paste("\n Dropping", drop,"\n"))
      } else {
        cat(paste("\n Dropping", round(ds,2), "Vessels \n"))
      }

      sim_run <- list()
      cache_iter <- list()

      out <- tryCatch({
        # s <- Sys.time()
        sim_run <- choices_asc_meta(start, subgraph_use, year_ref, nyears, vessels_in, burnin,
                                    scale_t1ev, asc_sc, asc_fc, drop,
                                    skillrand, costbyfishery, shockpermanent,
                                    distparameter, closureresponse, entry, dropves, ds)
        # s - Sys.time()

        # log finish
        msg <- sprintf("j = %d, s = %d : finished at %s", j, s, Sys.time())
        write(msg, file = log_file, append = TRUE)
        sim_run
      }, error = function(e) {
        msg <- sprintf("j = %d, s = %d : error -- %s", j, s, e$message)
        write(msg, file = log_file, append = TRUE)
        NULL
      })

      sim_id <- tibble(
        iter = s,
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
                         sim_id = sim_id,
                         rscalef = rscalef,
                         rscales = rscales)

      write_rds(cache_iter,
                paste0("/home/gkoss/westcoast-networks/data/Simulation/DynamicSimulationOutcomes/MLSizeCacheMeta2_",j,"/",
                       subgraph_use,"_",s,".rds"))

      # return(list(sim_run = sim_run,
      #             iter = s,
      #             rscalef = rscalef,
      #             rscales = rscales))
      # }, mc.cores = 64)
    # }
        })

    plan(sequential)
    gc()

  write(sprintf("=== Finished worker j = %d at %s ===\n", j, Sys.time()),
        file = log_file, append = TRUE)
}


t <- read_rds("/home/gkoss/westcoast-networks/data/Simulation/DynamicSimulationOutcomes/MLSizeCacheMeta1_4/Meta_99.rds")
t2 <- read_rds("/home/gkoss/westcoast-networks/data/Simulation/DynamicSimulationOutcomes/MLSizeCacheMeta1_6/Meta_1.rds")
# t2e <- t2$sim_run$cache_ttran[[1]]
# te <- t$sim_run$cache_e[[1]]
#
# t$sim_id
# av <- bind_rows(t$sim_run$cache_dfchoice, .id = "Year") %>%
#   group_by(Year, FISHERY_ID) %>%
#   tally()
#
# waldo::compare(t2$sim_run$cache_e,
#           t$sim_run$cache_e)

