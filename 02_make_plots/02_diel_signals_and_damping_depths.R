## 
## plot the diel signals with damping depths
##

library(lubridate)
library(zoo)
library(xts)
library(stringr)

## -- read in data
file_directory <- "./data/processed_model_output/"

kvals <- rep(c("100", "400"), each = 12)
bcvals <- rep(rep(c("sunny", "shady", "riveronly"), each = 4), times = 2)
soilvals <- rep(c("0.5", "1.0", "2.0", "3.0"), times = 6)

eachmodel <- numeric(24)
for (i in 1:24) {
  eachmodel[i] <- paste0("k", kvals[i], "_", soilvals[i], "_", bcvals[i])
}

for (i in seq_along(eachmodel)) {
  assign(paste0(eachmodel[i], "_100m"), 
         readRDS(paste0(file_directory, eachmodel[i], "_first100m.RData")))
}

## -- read in boundary condition data
river <- read.table("./data/boundary_conditions/river.txt", skip = 1, col.names = c("s", "e", "temp"))

## -- calculate boundary condition temperature annual amplitudes
riverx <- xts(zoo(river$temp, order.by = ymd_hms("2020-01-01 00:00:00")+river$s))

# -- compile into lists
shady_100 <- list(k100_0.5_shady_100m, k100_1.0_shady_100m, 
                  k100_2.0_shady_100m, k100_3.0_shady_100m)
sunny_100 <- list(k100_0.5_sunny_100m, k100_1.0_sunny_100m, 
                  k100_2.0_sunny_100m, k100_3.0_sunny_100m)
riveronly_100 <- list(k100_0.5_riveronly_100m, k100_1.0_riveronly_100m, 
                      k100_2.0_riveronly_100m, k100_3.0_riveronly_100m)

shady_400 <- list(k400_0.5_shady_100m, k400_1.0_shady_100m, 
                  k400_2.0_shady_100m, k400_3.0_shady_100m)
sunny_400 <- list(k400_0.5_sunny_100m, k400_1.0_sunny_100m, 
                  k400_2.0_sunny_100m, k400_3.0_sunny_100m)
riveronly_400 <- list(k400_0.5_riveronly_100m, k400_1.0_riveronly_100m, 
                      k400_2.0_riveronly_100m, k400_3.0_riveronly_100m)

# -- time index calculations
hours_idx <- seq(2, 24, 5)
starts <- hours_idx * 24 - 23
ends <- starts + 23

daytimes <- mapply(
  function(s) seq(s, s + 23, by = 5),
  starts
)

daily_time_idx <- c(daytimes[, 1], 
                    daytimes[, 2], 
                    daytimes[, 3], 
                    daytimes[, 4], 
                    daytimes[, 5])

these_days <- c(
  "2021-01-16", 
  "2021-04-01",
  "2021-06-15", 
  "2021-08-29",
  "2021-11-12"
)

## -- X and Z dimension indexes for diel signal
x_idx <- 2:95
aquifer_z_idx <- 18:47

## -- plot controls
plotlwd <- 3

## -- calculate river temperature diel amplitude for correct days
riveramps <- numeric(5)
for (i in 1:5) {
  riveramps[i] <- (max(riverx[these_days[i]]) -
                     min(riverx[these_days[i]])) / 2
}

## -- plotting function
plotacrosstime_daily <- function(x, modelrun) {
  x_amps <- numeric(50)
  x_amp_loc <- data.frame(
    day_index = numeric(5),
    e1_amp_loc = numeric(5),
    e2_amp_loc = numeric(5),
    e3_amp_loc = numeric(5),
    e4_amp_loc = numeric(5),
    e5_amp_loc = numeric(5))
  for (j in 1:5) {
    for (i in 1:50) {
      x_amps[i] <- (max(colMeans(x[i, 1, 18:47, starts[j]:ends[j], "temp"])) -
                      min(colMeans(x[i, 1, 18:47, starts[j]:ends[j], "temp"]))) / 2
    }
    x_amp_df <- data.frame(
      amps = x_amps,
      idx = x[1:50, 2, 30, 1, "X"],
      e1 = abs(x_amps - riveramps[j] * exp(-1)),
      e2 = abs(x_amps - riveramps[j] * exp(-2)),
      e3 = abs(x_amps - riveramps[j] * exp(-3)),
      e4 = abs(x_amps - riveramps[j] * exp(-4)),
      e5 = abs(x_amps - riveramps[j] * exp(-5))
    )
    x_amp_loc$e1_amp_loc[j] <- x_amp_df[x_amp_df$e1 == 
                                          min(abs(x_amps - (riveramps[j] * exp(-1)))), "idx"]
    x_amp_loc$e2_amp_loc[j] <- x_amp_df[x_amp_df$e2 == 
                                          min(abs(x_amps - (riveramps[j] * exp(-2)))), "idx"]
    x_amp_loc$e3_amp_loc[j] <- x_amp_df[x_amp_df$e3 == 
                                          min(abs(x_amps - (riveramps[j] * exp(-3)))), "idx"]
    x_amp_loc$e4_amp_loc[j] <- x_amp_df[x_amp_df$e4 == 
                                          min(abs(x_amps - (riveramps[j] * exp(-4)))), "idx"]
    x_amp_loc$e5_amp_loc[j] <- x_amp_df[x_amp_df$e5 == 
                                          min(abs(x_amps - (riveramps[j] * exp(-5)))), "idx"]
    x_amp_loc$day_index[j] <- j
  }

  plot(x[x_idx, 2, 30, 1, "X"],
    rowMeans(x[x_idx, 2, aquifer_z_idx, daily_time_idx[1], "temp"]),
    type = "l",
    col = "white",
    ylim = c(0, 22),
    lwd = plotlwd,
    ylab = expression(paste("Temperature (", degree, "C)")),
    xlab = "Flow Path Length (m)",
    xlim = c(0, 10)
  )

  mapply(
    function(t, c) lines(x[x_idx, 2, 30, 1, "X"], 
                         rowMeans(x[x_idx, 2, aquifer_z_idx, t, "temp"]), 
                         col = c, 
                         lwd = plotlwd),
    daily_time_idx,
    rep(hcl.colors(24, "Fall")[hours_idx], each = 5)
  )
  abline(v = mean(x_amp_loc$e1_amp_loc), col = "gray45", lwd = 2)
  abline(v = mean(x_amp_loc$e2_amp_loc), col = "gray45", lwd = 2)
  abline(v = mean(x_amp_loc$e3_amp_loc), col = "gray45", lwd = 2)

  text(mean(x_amp_loc$e1_amp_loc) - 0.2, 19.6, 
       labels = paste(round(exp(-1) * 100, 1), "%"), srt = 90, adj = 0)
  text(mean(x_amp_loc$e2_amp_loc) - 0.2, 19.6, 
       labels = paste(round(exp(-2) * 100, 1), "%"), srt = 90, adj = 0)
  text(mean(x_amp_loc$e3_amp_loc) - 0.2, 19.6, 
       labels = paste(round(exp(-3) * 100, 1), "%"), srt = 90, adj = 0)
}

thickness <- c("Soil 0.5m", "Soil 1.0m", "Soil 2.0m", "Soil 3.0m")

## -- k100 diel plots
png("./plots/diel_signals_and_damping_depths_k100.png",
    height = 1000 * 5,
    width = 900 * 5,
    res = 72 * 5)
par(mfcol = c(4, 3),
    oma = c(4, 4, 4, 0),
    mar = c(2, 2, 2, 1),
    cex.main = 2,
    cex.axis = 1.8)
## -- river only
mapply(plotacrosstime_daily,
       riveronly_100,
       thickness)
## -- shady 
mapply(plotacrosstime_daily,
       shady_100,
       thickness)
## -- sunny
mapply(plotacrosstime_daily,
       sunny_100,
       thickness)
mtext(expression(paste("Temperature (", degree, " C)")),
  side = 2,
  line = 1, outer = TRUE,
  cex = 2)
mtext("Flow Path Length (m)",
  side = 1, line = 2, outer = TRUE,
  cex = 2)
dev.off()

## -- k400 diel plots
png("plots/diel_signals_and_damping_depths_k400.png",
    height = 1000 * 5,
    width = 900 * 5,
    res = 72 * 5)
par(mfcol = c(4, 3),
    oma = c(4, 4, 4, 0),
    mar = c(2, 2, 2, 1),
    cex.main = 2,
    cex.axis = 1.8)
## -- river only
mapply(plotacrosstime_daily,
       riveronly_400,
       thickness)
## -- shady
mapply(plotacrosstime_daily,
       shady_400,
       thickness)
## -- sunny 
mapply(plotacrosstime_daily,
       sunny_400,
       thickness)
mtext(expression(paste("Temperature (", degree, " C)")),
      side = 2,
      line = 1, outer = TRUE,
      cex = 2)
mtext("Flow Path Length (m)",
      side = 1, line = 2, outer = TRUE,
      cex = 2)
dev.off()

## -- single plot per K value:
png("./plots/diel_signals_and_damping_depths_simple.png",
  height = 1000 * 5,
  width = 600 * 5,
  res = 72 * 5)
par(mfrow = c(2, 1),
  oma = c(4, 4, 0, 0),
  mar = c(2, 2, 2, 1),
  cex.main = 2,
  cex.axis = 1.8)
plotacrosstime_daily(riveronly_100[[2]], thickness[2])
plotacrosstime_daily(riveronly_400[[2]], thickness[2])
mtext(expression(paste("Temperature (", degree, " C)")),
  side = 2,
  line = 1, outer = TRUE,
  cex = 2)
mtext("Flow Path Length (m)",
  side = 1, line = 2, outer = TRUE,
  cex = 2)
dev.off()