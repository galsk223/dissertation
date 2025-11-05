

fileid <- "MLSizeCacheMeta2_"
base_name <- "~/westcoast-networks/data/Simulation/DynamicSimulationOutcomes/"

allfolders <- list(fl1 = list.files(paste0(base_name, fileid, 1, "/"), full.names = T),
                   fl2 = list.files(paste0(base_name, fileid, 2, "/"), full.names = T),
                   fl3 = list.files(paste0(base_name, fileid, 3, "/"), full.names = T),
                   fl4 = list.files(paste0(base_name, fileid, 4, "/"), full.names = T),
                   fl5 = list.files(paste0(base_name, fileid, 5, "/"), full.names = T),
                   fl6 = list.files(paste0(base_name, fileid, 6, "/"), full.names = T),
                   fl7 = list.files(paste0(base_name, fileid, 7, "/"), full.names = T),
                   fl8 = list.files(paste0(base_name, fileid, 8, "/"), full.names = T),
                   fl9 = list.files(paste0(base_name, fileid, 9, "/"), full.names = T),
                   fl10 = list.files(paste0(base_name, fileid, 10, "/"), full.names = T))
folder <- allfolders[[8]]

c <- 1

fisherychoices <- map_dfr(1:length(folder), function(c){
  d <- read_rds(folder[c])
  print(c)
  # d <- read_rds(fs)
  if(length(d$sim_run) == 0){return(NULL)}
  fisheries <- tibble(Iter = unique(d$sim_id$iter),
                      FISHERY_ID = unique(d$sim_run$cache_e[[1]]$Fishery))
})

t <- fisherychoices %>%
  group_by(FISHERY_ID) %>%
  tally() %>%
  filter(!is.na(FISHERY_ID))

outfile <- paste0("~/westcoast-networks/data/clean/Simulation/topregionfisherylist.rds")
write_rds(t, outfile)


