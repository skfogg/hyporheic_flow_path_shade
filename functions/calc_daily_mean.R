##
## Calculate daily mean temperature from HGS output
##
calc_daily_mean <- function(HGSdata){
  
  message("Calculating Daily Means...")
  
  xs <- HGSdata[,2,1,1,"X"]
  ys <- HGSdata[1,2,1,1,"Y"]
  zs <- HGSdata[1,2,,1,"Z"]
  tlength <- length(HGSdata[1,1,1,,"temp"])
  
  sID <- seq(1, tlength-23, by = 24) # start ID
  eID <- seq(24, tlength, by = 24) # end ID
  
  daily_means <- array(NA, dim = c(Z = length(zs), X = length(xs), Time = 24))
  
  xmeans <- numeric(length(xs))
  for (t in 1:24){
    s <- sID[t] 
    e <- eID[t] 
    
    for(z in 1:length(zs)){
      for(x in 1:length(xs)){
        xmeans[x] <- mean(HGSdata[x,2,seq(length(zs), 1, by = -1)[z],s:e,"temp"], na.rm = TRUE)
      }
      daily_means[z,,t] <- xmeans
    }
  }
return(daily_means)
}






