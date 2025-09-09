##
## Plots to compare effects of floodplain soil thickness
## K 100 m day-1
##

library(lubridate)

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
  assign(paste0(eachmodel[i]), 
         readRDS(paste0(file_directory, eachmodel[i], "_dailymeans.RData")))
  assign(paste0(eachmodel[i], "_s"), 
         readRDS(paste0(file_directory, eachmodel[i], "_modelstructure.RData")))
}

## -- compile into lists
structure_list <- list(k100_0.5_shady_s, k100_1.0_shady_s, k100_2.0_shady_s, k100_3.0_shady_s)
temp_shady_k100 <- list(k100_0.5_shady, k100_1.0_shady, k100_2.0_shady, k100_3.0_shady)
temp_sunny_k100 <- list(k100_0.5_sunny, k100_1.0_sunny, k100_2.0_sunny, k100_3.0_sunny)
temp_riveronly_k100 <- list(k100_0.5_riveronly, k100_1.0_riveronly, k100_2.0_riveronly, k100_3.0_riveronly)

temp_shady_k400 <- list(k400_0.5_shady, k400_1.0_shady, k400_2.0_shady, k400_3.0_shady)
temp_sunny_k400 <- list(k400_0.5_sunny, k400_1.0_sunny, k400_2.0_sunny, k400_3.0_sunny)
temp_riveronly_k400 <- list(k400_0.5_riveronly, k400_1.0_riveronly, k400_2.0_riveronly, k400_3.0_riveronly)

## -- plotting function
annual_plot_across_time <- function(x, temp, modelrun) {
  plot(x[x_idx,2,30,"X"], 
       colMeans(temp[aquifer_z_idx,x_idx,time_idx[1]]),
       type = "l",
       col = plotcols[1],
       ylim = c(3,19),
       lwd = plotlwd,
       ylab = expression(paste("Temperature (", degree, "C)")),
       xlab = "Flow Path Length (m)",
       main = modelrun,
       xlim = c(0,2000))
  mapply(function(t,c) lines(x[x_idx,2,30,"X"], colMeans(temp[aquifer_z_idx,x_idx,t]), col = c, lwd = plotlwd),
         time_idx,
         hcl.colors(12,"Fall"))
}

## -- plot controls
originalpar <- par()
plotcols <- hcl.colors(5, "Oranges", rev = F)[1:4] 
plotlwd <- 3
x_idx <- 1:413 
thickness <- c("Soil 0.5m", "Soil 1.0m", "Soil 2.0m", "Soil 3.0m")
time_idx <- c(1,3,5,7,9,11,13,15,17,19,21,23)
aquifer_z_idx <- 17:47

## -- k100 longitudinal temperature plots
png("./plots/annual_longitudinal_signal_k100.png",
    height = 1000*5,
    width = 900*5,
    res = 72*5)
par(mfcol = c(4,3),
    oma = c(4,4,4,0),
    mar = c(2,2,3,1),
    cex.main = 2,
    cex.axis = 1.5)
## -- river only
mapply(annual_plot_across_time,
       structure_list,
       temp_riveronly_k100,
       thickness)
## -- shady 
mapply(annual_plot_across_time,
       structure_list,
       temp_shady_k100,
       thickness)
## -- sunny
mapply(annual_plot_across_time,
       structure_list,
       temp_sunny_k100,
       thickness)
mtext(expression(paste("Temperature (", degree, " C)")), side = 2,
      line = 1, outer = T,
      cex = 2)
mtext("Flow Path Length (m)", side = 1, line = 2, outer = T,
      cex = 2)
dev.off()

## -- k400 longitudinal temperature plots
png("./plots/annual_longitudinal_signal_k400.png",
    height = 1000*5,
    width = 900*5,
    res = 72*5)
par(mfcol = c(4,3),
    oma = c(4,4,4,0),
    mar = c(2,2,3,1),
    cex.main = 2,
    cex.axis = 1.5)
## -- river only
mapply(annual_plot_across_time,
       structure_list,
       temp_riveronly_k400,
       thickness)
## -- shady 
mapply(annual_plot_across_time,
       structure_list,
       temp_shady_k400,
       thickness)
## -- sunny
mapply(annual_plot_across_time,
       structure_list,
       temp_sunny_k400,
       thickness)
mtext(expression(paste("Temperature (", degree, " C)")), side = 2,
      line = 1, outer = T,
      cex = 2)
mtext("Flow Path Length (m)", side = 1, line = 2, outer = T,
      cex = 2)
dev.off()


## -- plot legend; appended later
par(originalpar)
png("plots/annual_longitudinal_signal_legend.png",
    height = 230*5,
    width = 400*5,
    res = 72*5)
plot(rep(5, times = 12), 
     0:11, 
     bty = "n",
     col = hcl.colors(12,"Fall", rev = T),
     pch = 175,
     cex = 5,
     xaxt = "n",
     yaxt = "n",
     ylab = "",
     xlab = "")
text(rep(4.72, times = 6), seq(11.1, 0.1, by = -2), 
     c("Jan", "Mar", "May", "Jul", "Sep", "Nov"), 
     cex = 0.65,
     adj = 1)
mtext("Month", side = 3, line = 0)
dev.off()

