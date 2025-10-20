library(tidyverse)
library(zoo)
library(tidymodels)
library(themis)
library(vip)
library(shapviz)
library(kernelshap)
library(ranger)
library(xgboost)
library(earth)
library(factoextra)
library(fastshap)
library(NeuralNetTools)
library(tidytext)
library(brulee)
library(future)

rm(list = ls())
list.files("/home/gkoss/dissertation/01_scripts/04_ML_Functions/",
           full.names = TRUE) %>%
  walk(source)

# f <- 1:10
fileid <- "MLSizeCacheMeta2_"
files <- map(1:10,function(f){
  read_rds(paste0("~/westcoast-networks/data/clean/Simulation/projectiondataforML_",
                      fileid,f,".rds"))})

a <- 1
meta <- T
alldb <- map(1:10, function(a){
  # meta <- ifelse(a %in% 1:3, F, T)
  # if(a == 6){# dropped time only in 6 atm
    db2 <- dfbind(files[[a]][[1]], 3:9, meta, 500, a, F)
  # } else {
  #   db2 <- dfbind(files[[a]][[1]], 2:8, meta, 1000, a)
  # }
})

write_rds(alldb, "~/westcoast-networks/data/clean/Simulation/projectiondataforML_cleanedmeta3.rds")
alldb <- read_rds("~/westcoast-networks/data/clean/Simulation/projectiondataforML_cleanedmeta3.rds")

log_dir <- "/home/gkoss/westcoast-networks/data/clean/Simulation/projectiondataforML_MetaLogs/"
mlresultfolder <- "/home/gkoss/westcoast-networks/data/clean/Simulation/MLResults_Meta3/"
dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(mlresultfolder, showWarnings = FALSE, recursive = TRUE)
log_msg <- function(log_file, worker, proj, step) {
  msg <- sprintf("[%s] simulation series %02d | projection %s | %s for all data groupings",
                 format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                 worker, proj, step)
  write(msg, file = log_file, append = TRUE)
}
outcomes <- c("Vessels1","Return","YearPVesLost")

# no network loop ---------------------------------------------------------


i <- 1
o <- 4
plan(multisession, workers = 16)
outnonet <- furrr::future_map(1:length(alldb), function(i){

  print(i)

  log_file <- file.path(log_dir, sprintf("worker_%02d.log", i))
  if (file.exists(log_file)) file.remove(log_file)
  write(sprintf("=== Starting worker i = %d at %s ===\n", i, Sys.time()),
        file = log_file, append = TRUE)

  pname <- names(alldb[[1]][1])
  print(pname)

  for(o in 3:length(outcomes)){

    df_subdata_r <- dfassemble(alldb[[i]][[1]], outcomes[[o]],
                               outcome, colelem, colelem_node, colnodes, colnet, id)
    df_subdata_rnn <- list(NoNetwork_Drop = df_subdata_r$NoNetwork_Drop)

    ML_achievement_R <- out_reg(df_subdata_rnn, models_r, pname, log_file)
    log_msg(log_file, i, pname, paste("finished regression ml:", outcomes[[o]]))
    write_rds(ML_achievement_R, paste0(mlresultfolder,"Series",i,"_NoNet_",outcomes[[o]],".rds"))

  }

  })

t <- read_rds(paste0(mlresultfolder,"Series",i,"_NoNet_",outcomes[[o]],".rds"))

# network loop ------------------------------------------------------------

i <- 8
p <- 1
o <- 4
plan(multisession, workers = 16)
outwithnet <- map(1:length(alldb), function(i){ # each simulation

  print(i)
  projections <- alldb[[i]]
  # names(projections)

  log_file <- file.path(log_dir, sprintf("worker_%02d.log", i))
  if (file.exists(log_file)) file.remove(log_file)
  write(sprintf("=== Starting worker i = %d at %s ===\n", i, Sys.time()),
        file = log_file, append = TRUE)

  # plan(sequential)
  pout <- furrr::future_map(1:7, function(p){ # each projection

    pname <- names(projections[p])
    print(pname)

      log_msg(log_file, i, pname, "starting ml")

      for(o in 1:length(outcomes)){

        df_subdata_r <- dfassemble(projections[[p]], outcomes[[o]],
                                   outcome, colelem, colelem_node, colnodes, colnet, id)
        if (str_detect(pname, "v")) {
          df_subdata_rn <- list(All_Drop = df_subdata_r$All_Drop,
                                Network_Drop = df_subdata_r$Network_Drop)
        } else {
          df_subdata_rn <- list(All_Drop = df_subdata_r$All_Drop,
                                Network_Drop = df_subdata_r$Network_Drop,
                                FisheryNode = df_subdata_r$FisheryNode)
        }

        ML_achievement_R <- out_reg(df_subdata_rn, models_r, pname, log_file)
        log_msg(log_file, i, pname, "finished regression ml")
        write_rds(ML_achievement_R,
                  paste0(mlresultfolder,"Series",i,"_Projection",p,"_",outcomes[[o]],".rds"))

      }

      # close -------------------------------------------------------------------

        # df_id <- df_subdata$ID
        #
        # writeout <- list(ID = df_id)
        #
        # write_rds(writeout, paste0(mlresultfolder,"Series",i,"_Projection",p,".rds"))

  })

  gc()
  log_msg(log_file, i, pname, "finished regression ml")
  # return(pout)

})

# t1 <- read_rds(paste0(mlresultfolder,"Series",i,"NoNet_Class.rds"))


