##
## Create model temperature input plots
##

library(xts)
library(zoo)
library(lubridate)

## -- import data
rawdata <- readRDS("./data/meacham_creek_data/compiled_raw_data.rds")
shady_model <- read.table("./data/meacham_creek_data/shady_model.txt",
                          col.names = c("start", "end", "temperature"))
sunny_model <- read.table("./data/meacham_creek_data/sunny_model.txt",
                          col.names = c("start", "end", "temperature"))
river_model <- read.table("./data/meacham_creek_data/river_model.txt",
                          col.names = c("start", "end", "temperature"))

## -- convert to xts/zoo
riverT <- xts(zoo(rawdata$watertemp$Temp, order.by = rawdata$watertemp$PosixTime)) 
sunnyT <- xts(zoo(rawdata$sunnysoil$Temp, order.by = rawdata$sunnysoil$PosixTime))
shadyT <- xts(zoo(rawdata$shadysoil$Temp, order.by = rawdata$shadysoil$PosixTime))

modelriverT <- xts(zoo(river_model$temperature, order.by = seq(mdy_hms("01-01-2019 00:00:00"), by = 3600, length.out = 87601)))
modelsunnyT <- xts(zoo(sunny_model$temperature, order.by = seq(mdy_hms("01-01-2019 00:00:00"), by = 3600, length.out = 87601)))
modelshadyT <- xts(zoo(shady_model$temperature, order.by = seq(mdy_hms("01-01-2019 00:00:00"), by = 3600, length.out = 87601)))


png("./plots/model_temperature_inputs.png", width = 900*5, height = 1200*5, res = 72*5)
par(mfrow = c(3,1),
    oma = c(4,4,0,0),
    mar = c(1,2,0,0),
    cex.axis = 1.3,
    cex.lab = 1.3,
    cex = 1.3,
    bty = "l")
plot.zoo(modelriverT,
         xlim = c(index(modelriverT["2020"][1]), index(last(modelriverT["2021"]))),
         ylim = c(-1,35),
         col = "gray",
         xaxt = "n",
         lwd = 2)
lines(as.zoo(riverT), lwd = 2)
axis(side = 1, at = c(ymd_hms("2020-01-01 00:00:00", "2021-01-01 00:00:00", "2022-01-01 00:00:00")), labels = F)
text(ymd_hms("2020-01-01 00:00:00"), 34, labels = "River", cex = 2, pos = 4)

plot.zoo(modelsunnyT,
         xlim = c(index(modelriverT["2020"][1]), index(last(modelriverT["2021"]))),
         ylim = c(-1,35),
         col = "gray",
         xaxt = "n", lwd = 2)
lines(as.zoo(sunnyT), lwd = 2)
axis(side = 1, at = c(ymd_hms("2020-01-01 00:00:00", "2021-01-01 00:00:00", "2022-01-01 00:00:00")), labels = F)
text(ymd_hms("2020-01-01 00:00:00"), 34, labels = "Sunny Soil", cex = 2, pos = 4)


plot.zoo(modelshadyT,
         xlim = c(index(modelriverT["2020"][1]), index(last(modelriverT["2021"]))),
         ylim = c(-1,35),
         col = "gray",
         lwd = 2)
lines(as.zoo(shadyT), lwd = 2)
axis(side = 1, at = c(ymd_hms("2020-01-01 00:00:00", "2021-01-01 00:00:00", "2022-01-01 00:00:00")), 
     labels = c("2020", "2021", "2022"))
text(ymd_hms("2020-01-01 00:00:00"), 34, labels = "Shady Soil", cex = 2, pos = 4)

mtext(expression(paste("Temperature ( ", degree, "C)")), side = 2, outer = T, line = 1, cex = 2)
mtext("Time (y)", side = 1, outer = T, line = 2, cex = 2)
dev.off()
