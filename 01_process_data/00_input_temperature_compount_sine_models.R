## 
## Compound Sine Wave models for HGS model input
##

library(xts)
library(zoo)
library(lubridate)
source("./functions/include_na_times.R")

## -- read in data
shadysoil <- read.csv("./data/meacham_creek_data/shadysoil.csv")
sunnysoil <- read.csv("./data/meacham_creek_data/sunnysoil.csv")
watertemp <- read.csv("./data/meacham_creek_data/watertemp.csv")

## -- convert to xts/zoo, and include_na_times
shadysoilx <- include_na_times(xts(zoo(shadysoil$Temp, order.by = mdy_hms(shadysoil$DateTime))))
sunnysoilx <- include_na_times(xts(zoo(sunnysoil$Temp, order.by = mdy_hms(sunnysoil$DateTime))))
watertempx <- include_na_times(xts(zoo(watertemp$Temp, order.by = ymd_hms(watertemp$DateTime))))

## --  compile list of data
rawdata <- list(shadysoil = data.frame(DateTime = index(shadysoilx),
                                       Temp = coredata(shadysoilx)), 
                sunnysoil = data.frame(DateTime = index(sunnysoilx),
                                       Temp = coredata(sunnysoilx)), 
                watertemp = data.frame(DateTime = index(watertempx),
                                       Temp = coredata(watertempx)))

## -- add rows to data.frames
for (i in 1:3){
  rawdata[[i]]$PosixTime <- rawdata[[i]]$DateTime
  rawdata[[i]]$DateTime <- as.character(rawdata[[i]]$DateTime)
  rawdata[[i]]$NumTime <- as.numeric(rawdata[[i]]$PosixTime)
  rawdata[[i]]$NumTimeOfYr <- rawdata[[i]]$NumTime - as.numeric(mdy_hms("01/01/20 00:00:00"))
  colnames(rawdata[[i]])[2] <- "Temp"
}

## -- compile list of xts/zoo data
zoodata <- list(shadysoilx = shadysoilx, sunnysoilx = sunnysoilx, watertempx = watertempx)

## -- calc daily means of xts/zoo data
shadysoildailymeanx <- apply.daily(shadysoilx, colMeans)
sunnysoildailymeanx <- apply.daily(sunnysoilx, colMeans)
watertempdailymeanx <- apply.daily(watertempx, colMeans)

## -- compile daily mean data into list
rawdatameans <- list(shadysoildailymean = data.frame(DateTime = as.character(index(shadysoildailymeanx)),
                                                     PosixTime = index(shadysoildailymeanx),
                                                     NumTime = as.numeric(index(shadysoildailymeanx)),
                                                     NumTimeOfYr = as.numeric(index(shadysoildailymeanx)) - as.numeric(mdy_hms("01/01/20 00:00:00")),
                                                     DailyMean = coredata(shadysoildailymeanx)), 
                     sunnysoildailymean = data.frame(DateTime = as.character(index(sunnysoildailymeanx)),
                                                     PosixTime = index(sunnysoildailymeanx),
                                                     NumTime = as.numeric(index(sunnysoildailymeanx)),
                                                     NumTimeOfYr = as.numeric(index(sunnysoildailymeanx)) - as.numeric(mdy_hms("01/01/20 00:00:00")),
                                                     DailyMean = coredata(sunnysoildailymeanx)), 
                     watertempdailymean = data.frame(DateTime = as.character(index(watertempdailymeanx)),
                                                     PosixTime = index(watertempdailymeanx),
                                                     NumTime = as.numeric(index(watertempdailymeanx)),
                                                     NumTimeOfYr = as.numeric(index(watertempdailymeanx)) - as.numeric(mdy_hms("01/01/20 00:00:00")),
                                                     DailyMean = coredata(watertempdailymeanx)))
for (i in 1:3){
  names(rawdatameans[[i]])[5] <- "DailyMean"
}

## -- add daily mean data to rawdata list
for (i in 1:3){
  rawdata[[i]]$DailyMean <- rep(coredata(rawdatameans[[i]]$DailyMean), each = 24) 
}

saveRDS(rawdata, "./data/meacham_creek_data/compiled_raw_data.rds")

## -- create a meaningless list to put the model data into 
meachammodels <- list(shadysoil = 1, sunnysoil = 2, watertemp = 3)

for (i in 1:3){
  
  ## -- DAILY MEAN MODEL
  # -- start val for m:
  meanstart <- mean(rawdatameans[[i]]$DailyMean, na.rm = T)
  # -- start val for a:
  ampstart <- (max(rawdatameans[[i]]$DailyMean, na.rm = T)-min(rawdatameans[[i]]$DailyMean, na.rm = T))/2
  
  dailymeanmodel <- nls(DailyMean ~ m + a*sin(2*pi*(1/31536000)*NumTimeOfYr + p),
                        rawdatameans[[i]],
                        start = list(m = meanstart,
                                     a = ampstart,
                                     p = 0.1))
 
  ## -- DAILY AMPLITUDE MODEL
  amps <- apply.daily(zoodata[[i]], function(x) (max(x)-min(x))/2)

  # -- start val for m:
  meanstart <- mean(rawdatameans[[i]]$DailyAmp, na.rm = T)
  # -- start val for a:
  ampstart <- (max(rawdatameans[[i]]$DailyAmp, na.rm = T)-min(rawdatameans[[i]]$DailyAmp, na.rm = T))/2
  
  dailyampmodel <- nls(DailyAmp ~ m + a*sin(2*pi*(1/31536000)*NumTimeOfYr + p),
                       rawdatameans[[i]],
                       start = list(m = meanstart,
                                    a = ampstart,
                                    p = 0.1))
  
  ## -- DAILY FREQUENCY MODEL
  dailyfreqmodel <- nls(I(Temp-DailyMean) ~ a*sin(2*pi*(1/86400)*NumTimeOfYr + p),
                        rawdata[[i]],
                        start = list(a = 1,
                                     p = 0.1))
 
  ## -- COMPOUND MODEL 
  t10 <- seq(0, 31536000*10, by = 3600)
  means <- coef(dailymeanmodel)[1] + coef(dailymeanmodel)[2]*sin(2*pi*(1/31536000)*t10 + coef(dailymeanmodel)[3])
  amps <- coef(dailyampmodel)[1] + coef(dailyampmodel)[2]*sin(2*pi*(1/31536000)*t10 + coef(dailyampmodel)[3])
  
  meachammodels[[i]] <- means + amps*sin(2*pi*(1/86400)*t10 + coef(dailyfreqmodel)[2])
}

## -- make 10 years of repeating data to use as model input
meachamshadysoil <- meachammodels[[1]]
meachamsunnysoil <- meachammodels[[2]]
meachamwatertemp <- meachammodels[[3]]

meachamshadydf <- data.frame(s = t10, e = t10+3600, temp = meachamshadysoil)
meachamsunnydf <- data.frame(s = t10, e = t10+3600, temp = meachamsunnysoil)
meachamwaterdf <- data.frame(s = t10, e = t10+3600, temp = meachamwatertemp)

write.table(meachamshadydf, "./data/meacham_creek_data/shady_model.txt",
            row.names = FALSE,
            col.names = FALSE)
write.table(meachamsunnydf, "./data/meacham_creek_data/sunny_model.txt",
            row.names = FALSE,
            col.names = FALSE)
write.table(meachamwaterdf, "./data/meacham_creek_data/river_model.txt",
            row.names = FALSE,
            col.names = FALSE)