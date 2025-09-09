##
## Process Simulation experiment model results and save
##
## -- hyporheic_flow_path_data must first be downloaded and unzipped from: 
## -- https://doi.org/10.5061/dryad.2v6wwq026
##
## -- WARNING: FILES TAKE A LONG TIME TO PROCESS 
##

if(!require("HGSReader", character.only = TRUE)){
  devtools::install_github("FluvialLandscapeLab/HGSReader")
}

library(tidyr)
library(stringr)
library(gstat)
library(HGSReader)
source("./functions/calc_daily_mean.R")

## -- *may need to change if you did not unzip to the project working directory
file_directory <- "./hyporheic_flow_path_data/"

kvals <- rep(c("100", "400"), each = 12)
bcvals <- rep(rep(c("sunny", "shady", "riveronly"), each = 4), times = 2)
soilvals <- rep(c("0.5", "1.0", "2.0", "3.0"), times = 6)

eachmodel <- numeric(24)
for (i in 1:24){
  eachmodel[i] <- paste0("k", kvals[i], "_", soilvals[i], "_", bcvals[i])
}

ptm <- proc.time()
for(i in seq_along(eachmodel)){
  
  modelname <- eachmodel[i]
  
  f <- HGSFile(paste0(file_directory, 
                      modelname, "o.pm.dat"))
  
  g <- HGSGetData(f, 
                  variables = c("X", "Y", "Z", "temp"),
                  blockNumbers = 1:length(HGSQueryBlocks(f)),
                  asArray = TRUE)
  
  first100m <- g[1:95,,,,]
  saveRDS(first100m,
          file(paste0("./data/processed_model_output/", modelname, "_first100m.RData")))
  
  gmeans <- calc_daily_mean(g)
  saveRDS(gmeans,
          file = paste0("./data/processed_model_output/", modelname, "_dailymeans.RData"))
  rm(gmeans)
  
  modelstructure <- g[,,,1,c("X","Y","Z")]
  saveRDS(modelstructure, 
          file = paste0("./data/processed_model_output/", modelname, "_modelstructure.RData"))
  rm(modelstructure)
  rm(g)
  rm(f)
}
proc.time() - ptm