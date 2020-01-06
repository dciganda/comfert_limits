
plot_asfr <- function(year, sim, obs){
  
  year <- as.character(year)
  
  cols <- c("black", "red")
  
  plot(as.numeric(gsub("age", "", colnames(obs))),
       obs[year,], 
       xlab="age", ylab="asfr",
       pch=20, col=cols[1], cex=1.5, 
       ylim = c(0, 0.20), # c(0, 1.4*max(as.numeric(obs[year,])) ) 
       main = year  )
  
  mean_asfr = Reduce("+", sim) / length(sim)
  
  points(as.numeric(gsub("age", "", colnames(mean_asfr))), 
         mean_asfr[year,], pch=20, col=cols[2], cex=1.5)
  
  legend("topright", legend = c("observed", "simulated"), 
         col=cols, cex=1, pch=20, pt.cex = 2, bty="n")
  
  return(0)
}

