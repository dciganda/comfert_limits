get_obs <- function(country, ysd){
  
  dir_o <- paste0("../run/data/", country ,"/out/")
  # Observed ASFR
  obs_asfr_0 <- read.table(paste0(dir_o,"asfrs_hfd.txt"),
                           skip = 2, header=T, stringsAsFactors = F)
  obs_asfr_0[,2] <- ifelse(obs_asfr_0[,2] == "12-", "12", obs_asfr_0[,2]) 
  obs_asfr_0[,2] <- ifelse(obs_asfr_0[,2] == "55+", "55", obs_asfr_0[,2]) 
  obs_asfr_0[,2] <- as.numeric(obs_asfr_0[,2])
  obs_asfr <- obs_asfr_0[obs_asfr_0$Year >= ysd, c(1,2,3)] # 
  obs_asfr <- reshape(obs_asfr, idvar = "Year" , timevar = "Age", direction="wide")
  obs_asfr <- as.matrix(obs_asfr[,-c(1:4,40:45)])
  colnames(obs_asfr) <- paste0("age", 15:49)
  rownames(obs_asfr) <- ysd:max(obs_asfr_0$Year)
  
  obs_data <- list(obs_asfr = obs_asfr) 
  
  # Observed Unplanned Births
  file_oo <- paste0(dir_o, "unplanned.csv")
  if(file.exists(file_oo)){
  obs_unp_0 <- read.table(file_oo,
                          sep = ",", skip = 3, header = T,
                          stringsAsFactors = F)
  obs_unp_1 <- as.matrix(obs_unp_0[,2])  
  rownames(obs_unp_1) <- obs_unp_0$year
  obs_unplanned <- as.matrix(obs_unp_1[!is.na(obs_unp_1),])
  colnames(obs_unplanned) <- "proportion.unplanned"
  obs_data <- c(obs_data, list(obs_unplanned = obs_unplanned))
  }

  # Observed Unwanted Births
  file_o <- paste0(dir_o, "unwanted.csv")
  if (file.exists(file_o)){
    obs_unw_0 <- read.table(file_o, 
                            sep = ",", skip = 3, header = T,
                            stringsAsFactors = F)
    obs_unw_1 <- as.matrix(obs_unw_0[,2]/100)  
    rownames(obs_unw_1) <- obs_unw_0$year
    obs_unwanted <- as.matrix(obs_unw_1[!is.na(obs_unw_1),])
    colnames(obs_unwanted) <- "prop.unwanted"
    obs_data <- c(obs_data, list(obs_unwanted = obs_unwanted))
  }
  
  # Observed D
  file_ooo <- paste0(dir_o, "ideal_sit.csv")
  if (file.exists(file_ooo)){
    if(country == "FR"){
      obs_1 <- read.table(paste0(dir_o,"ideal_sit.csv"), 
                          sep = ",", skip = 0, header = T)
      obs_1 <- obs_1[!is.na(obs_1[,5]),c(2,5)]
    }else{
      obs_1 <- read.table(paste0(dir_o,"ideal_sit.csv"), 
                          sep = "", skip = 0, header = T)
     # if(country == "ES"){
       # obs_1 <- obs_1[-1,]
      #}
      
    }
  obsD_3 <- predict(smooth.spline(obs_1[,1], obs_1[,2], spar = 0.5),
                    x = seq(min(obs_1$year), max(obs_1$year),1))
  obs_desired <- as.matrix(obsD_3$y)
  rownames(obs_desired) <- obsD_3$x
  colnames(obs_desired) <- "d"
  obs_data <- c(obs_data, list(obs_desired = obs_desired))
  }
  
  return(obs_data)  
}  



