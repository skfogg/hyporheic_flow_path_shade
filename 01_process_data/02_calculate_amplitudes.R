##
## Calculate the annual temperature amplitude at every model node
## -- and interpolate to a raster (1 cm resolution)
## -- WARNING: INTERPOLATION OF RASTERS TAKES A LONG TIME TO RUN
## -- Data is pre-processed and located in the ./data/processed_model_output directory
##

## -- read in model data
file_directory <- "./data/processed_model_output/"

## -- read in boundary condition data
river <- read.table("./data/boundary_conditions/river.txt", skip = 1, col.names = c("s", "e", "temp"))
shady <- read.table("./data/boundary_conditions/shady.txt", skip = 1, col.names = c("s", "e", "temp"))
sunny <- read.table("./data/boundary_conditions/sunny.txt", skip = 1, col.names = c("s", "e", "temp"))

## -- calculate boundary condition temrpeature annual amplitudes
riverx <- xts(zoo(river$temp, order.by = ymd_hms("2020-01-01 00:00:00")+river$s))
riveramp <- (max(apply.daily(riverx["2020"], colMeans)) - min(apply.daily(riverx["2020"], colMeans)))/2

shadyx <- xts(zoo(shady$temp, order.by = ymd_hms("2020-01-01 00:00:00")+shady$s))
shadyamp <- (max(apply.daily(shadyx["2020"], colMeans)) - min(apply.daily(shadyx["2020"], colMeans)))/2

sunnyx <- xts(zoo(sunny$temp, order.by = ymd_hms("2020-01-01 00:00:00")+sunny$s))
sunnyamp <- (max(apply.daily(sunnyx["2020"], colMeans)) - min(apply.daily(sunnyx["2020"], colMeans)))/2

## -- calculate boundary condition damping depths
dd_river <- riveramp*exp(-1)
dd_shady <- shadyamp*exp(-1)
dd_sunny <- sunnyamp*exp(-1)

## -- save bc amplitudes
save(riveramp, file = "./data/processed_model_output/riveramp.RData")
save(shadyamp, file = "./data/processed_model_output/shadyamp.RData")
save(sunnyamp, file = "./data/processed_model_output/sunnyamp.RData")

## -- read in daily mean and model structure data
kvals <- rep(c("100", "400"), each = 12)
bcvals <- rep(rep(c("sunny", "shady", "riveronly"), each = 4), times = 2)
soilvals <- rep(c("0.5", "1.0", "2.0", "3.0"), times = 6)

eachmodel <- numeric(24)
for (i in 1:24){
  eachmodel[i] <- paste0("k", kvals[i], "_", soilvals[i], "_", bcvals[i])
}

for (i in 1:length(eachmodel)){
  assign(eachmodel[i], readRDS(paste0(file_directory, eachmodel[i], "_dailymeans.RData")))
  assign(paste0(eachmodel[i], "_s"), readRDS(paste0(file_directory, eachmodel[i], "_modelstructure.RData")))
}

## -- function that calculates the annual amplitude at each model node
all_amplitudes <- function(x, x_structure){
  ampsdf <- data.frame(x = rep(x_structure[,1,1,"X"], times = dim(x_structure)["Z"]), 
                       y = rep(round(rev(x_structure[1,1,,"Z"])-rev(x_structure[1,1,,"Z"])[1],2), each = 413), 
                       z_amps = NA)
  for(i in 1:dim(x)["Z"]){
    for(j in 1:413){
      ampsdf$z_amps[j+(413*(i-1))] <- (max(x[i,j,1:24]) - min(x[i,j,1:24]))/2
    }
  }
  return(ampsdf)
}

allamps <- list(24L)
for(i in 1:24){
  allamps[[i]] <- all_amplitudes(x = get(eachmodel[i]), 
                                 x_structure = get(paste0(eachmodel[i], "_s")))
}

## -- interpolate the amplitudes to create a smooth raster

interpolate_amps <- function(df){
  vi <- interp(x = df$x,
               y = df$y,
               z = df$z_amps,
               xo = seq(0,2000,by = 0.1),
               yo = seq(min(df$y),0, by = 0.1))
  return(raster(vi))
}

amplitude_rasters <- lapply(allamps, interpolate_amps)

## -- save
save(amplitude_rasters, file = "./data/processed_model_output/amplitude_rasters.RData")
