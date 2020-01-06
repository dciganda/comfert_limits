
# get simulated rates
get_sim_asfr <- function(cntry, nrpops, popsize, obs_rates,res_path, iniY, endY){

  get_sim_results <- function(path){ 
    
    res_names <- sapply(path, function(x) {list.files(x, "RData", full.names = TRUE)})
    
    asfr <- list()
    
    if (length(res_names) == 1){
      
      return(list()) # no results. empty list
      
    } else{
      
      for(i in res_names){
        out <- readRDS(i)
        asfr <- c(asfr, out["asfrt"])
        rm(out)
      }
      
      asfr <- lapply(asfr,
                     function(x){colnames(x) <- paste0("age", 14:50);
                     x <- x[-1,]
                     rownames(x) <- iniY:(endY-1);
                     return(as.matrix(x))})
      
      asfr <- lapply(asfr,
                     function(x){x[rownames(x) %in% rownames(obs_rates),
                                   colnames(x) %in% colnames(obs_rates)]})
      
      return(asfr)
      
    }
  }
  
  npop_asfr <- get_sim_results(res_path)

  return(npop_asfr)
  
}