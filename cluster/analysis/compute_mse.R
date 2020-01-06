

compute_mse <-  function(sim, obs, multi_obj_mse, weights){
  
  if(multi_obj_mse){
    
    cat(paste(".\n Weights:\n\n", paste(weights, collapse = "-")))
    
    sse <- list()
    
    for(i in sub(".*_", "", names(obs))){
      
      sim_std <- lapply(sim[paste0("sim_", i)][[1]], function(x) (x - mean(x))/sd(x))
      obs_std <- (obs[paste0("obs_", i)][[1]] - 
                    mean(obs[paste0("obs_", i)][[1]])) / sd(obs[paste0("obs_", i)][[1]])
      
      sse[[i]] <- sapply(sim_std, function(x){norm(x - obs_std, type="F")^2 / ncol(x)}) 
      
    }
    
    mse <- sapply(sse, mean)
    
    mses <- cbind(mse, weights = weights[names(mse)])
    
    mse <- sum(mses[,"mse"] * mses[,"weights"])
    
  }else{
    
    if(class(obs) == "list"){
      obs <- obs$obs_asfr
      sim <- sim[[1]] 
    }
    
    se <- sapply(sim, function(x){norm(x-obs, type="F")^2})
    mse <- mean(se)
    
  }
  
  return(mse)
  
}
