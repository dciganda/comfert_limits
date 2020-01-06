real_mse <- function(country, params, multi_obj_mse, unplanned, unwanted, desired, weights){
  
  source("get_sim.R")
  source("get_obs.R")
  source("compute_mse.R")
  
  ysd <- params[params$country ==  country, "ysd"]
  iniY <- params[params$country ==  country, "iniY"]
  endY <- params[params$country ==  country, "endY"]
  npop <- params[params$country ==  country, "npop"]
  N <- params[params$country ==  country, "N"]
  
  # get observed rate
  obs <- get_obs(country, ysd)
  # get asfr for base scenario
  base_sim <- get_sim(params, country, obs, estimated_params = T,
                      unplanned = unplanned, unwanted = unwanted,
                      desired = desired, multi_obj_mse = multi_obj_mse,
                      weights = weights)
  #Compute real MSE for optim params
  base_real_mse <- compute_mse(base_sim, obs, multi_obj_mse, weights = weights)
  # Compare with model MSE
  # Get Estimated values
  dir_o <- paste0("../results/", country, "/N_POP_", npop, "/N_", N)
  if(multi_obj_mse){
    results <- readRDS(paste0(paste0(dir_o,"/gp/",
                                     paste(weights, collapse = "_")),
                              "/gp_results_multi_mse.rds"))
  }else{
    results <- readRDS(paste0(paste0(dir_o,"/gp/"), "gp_results.rds"))
  }
  
  estim_params <- results$best_combis[which.min(results$best_combis$mse),]
  cat("\n",paste("GP MSE", country, ":",  as.numeric(estim_params[length(estim_params)])),"\n")
  cat("\n",paste("Real MSE",country, ":", base_real_mse), "\n")
  
}