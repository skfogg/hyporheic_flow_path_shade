##
## Make amplitude heat map plots with damping depth contour lines
##

## -- read in data
load("./data/processed_model_output/amplitude_rasters.RData")
load("./data/processed_model_output/riveramp.RData")
load("./data/processed_model_output/shadyamp.RData")
load("./data/processed_model_output/sunnyamp.RData")

for(i in 1:length(amplitude_rasters)){
  
  png(paste0("./plots/amp_heat_map_", eachmodel[i],".png"),
      width = 800*5,
      height = 500*5,
      res=72*5)
  par(cex.lab = 4,
      cex.axis = 1.5,
      mar = c(3,3,3,6))
  
  # -- determine the water table level (i.e. where the vadose zone starts)
  vadose_line <- as.numeric(str_split(eachmodel[i], "_")[[1]][2])
  
  # -- plot heat map
  plot(amplitude_rasters[[i]], ylim = c(-15,0), 
       main = paste0("Model Scenario: K = ",
                     str_remove(str_split(eachmodel[i], "_")[[1]][1], "k"),
                     "; vadose = ",
                     str_split(eachmodel[i], "_")[[1]][2],
                     "m; floodplain = ",
                     ifelse(str_split(eachmodel[i], "_")[[1]][3] == "riveronly", 
                            "Control",
                            ifelse(str_split(eachmodel[i], "_")[[1]][3] == "sunny", 
                                   "Unshaded",
                                   ifelse(str_split(eachmodel[i], "_")[[1]][3] == "shady", 
                                          "Shaded")
                            )
                     )
       ), 
       col = hcl.colors(15,"Plasma"),
       asp = NA,
       breaks = c(0:14))
  
  # -- add water table line
  abline(h = vadose_line*(-1), lty = 2, col = "white", lwd = 2)
  
  # -- add bedrock line
  abline(h = (vadose_line+3)*(-1), lty = 2, col = "white", lwd = 2)
  
  # -- add damping depths based off of input river amplitude
  contour(amplitude_rasters[[i]], 
          add = TRUE, 
          levels = c(round(riveramp*exp(-5:-1),2)), 
          col = "white", 
          lwd = 2, 
          cex = 4,
          labels = paste0(round(exp(-5:-1),3)*100,"%"))
  
  # -- add damping depths based off of surface temperature bc (either sunny or shady)
  if(str_split(eachmodel[i], "_")[[1]][3] == "sunny"){
    contour(amplitude_rasters[[i]], 
            add = TRUE, 
            levels = c(round(sunnyamp*exp(-5:-1),2)), 
            col = "greenyellow", 
            lwd = 3, 
            cex = 4, 
            lty = 1,
            labels = paste0(round(exp(-5:-1),3)*100,"%"))
  }
  
  if(str_split(eachmodel[i], "_")[[1]][3] == "shady"){
    contour(amplitude_rasters[[i]], 
            add = TRUE, 
            levels = c(round(shadyamp*exp(-5:-1),2)), 
            col = "greenyellow", 
            lwd = 3, 
            cex = 4, 
            lty = 1,
            labels = paste0(round(exp(-5:-1),3)*100,"%"))
  }
  
  dev.off()
}