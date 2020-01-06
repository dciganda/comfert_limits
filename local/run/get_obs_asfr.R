# Observed ASFR
get_obs_asfr <- function(cntry){
  obs_asfr <- read.table(paste0("data/", cntry ,"/out/asfrs_hfd.txt"), skip = 2, header=T, stringsAsFactors = F)
  ey <- max(obs_asfr$Year)
  obs_asfr[,2] <- ifelse(obs_asfr[,2] == "12-", "12", obs_asfr[,2]) 
  obs_asfr[,2] <- ifelse(obs_asfr[,2] == "55+", "55", obs_asfr[,2]) 
  obs_asfr[,2] <- as.numeric(obs_asfr[,2])
  obs_asfr <- obs_asfr[obs_asfr$Year>=1960, c(1,2,3)] # 1960 - Year in which Simulated ASFR are grown entirely from model (past initialization)
  obs_asfr <- reshape(obs_asfr, idvar = "Year" , timevar = "Age", direction="wide")
  obs_asfr <- as.matrix(obs_asfr[,-c(1:4,40:45)])
  colnames(obs_asfr) <- paste0("age", 15:49)
  rownames(obs_asfr) <- 1960:ey
  
  return(obs_asfr)  
}  



