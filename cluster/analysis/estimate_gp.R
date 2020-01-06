estimate_gp <- function(country, params, parallel = F, multi_obj_mse = F,
                        weights = c(asfr = 0.25, unplanned = 0.25,
                                    unwanted = 0.25, desired = 0.25)){
  
  if(parallel){
    gp <- estimate_gp_parallel(country, params, multi_obj_mse)
  }else{
    gp <- estimate_gp_sequential(country, params, multi_obj_mse, weights)
  }
  return(gp)
}

estimate_gp_sequential <- function(country, params, multi_obj_mse, weights){
  
  if(sum(weights)!=1){stop("Weights don't add up to 1")}
  
  source("compute_mse.R") 
  source("get_obs.R")
  source("get_sim.R") 
  source("plot_asfr.R")
  
  iniY <- params[params$country ==  country, "iniY"]
  endY <- params[params$country ==  country, "endY"]
  ysd <- params[params$country ==  country, "ysd"]
  npop <- params[params$country ==  country, "npop"]
  N <- params[params$country ==  country, "N"]
  
  # main dir
  dir_o <- paste0("../results/", country, "/N_POP_", npop, "/N_", N)
  
  # get data.frame of limits
  limits <- fread(paste0(dir_o, "/param_sample/limits.csv"))
  
  # get data.frame of sample points
  em_data <- fread(paste0(dir_o,"/param_sample/sample.csv"))
  
  # get observed fertility rates
  obs <- get_obs(country, ysd)
  
  # simulated rates
  sim <- get_sim(params, country, obs, unplanned = length(obs$obs_unplanned)>0, 
                 unwanted = length(obs$obs_unwanted)>0,
                 desired = length(obs$obs_desired)>0)
  
  # compute mse
  em_data <- em_data[1:length(sim)]
  em_data[, mse := sapply(sim, compute_mse, obs, multi_obj_mse, weights)]  
  
  # training gp
  cat(".\n training gp...\n\n")
  gpe <- mlegp(em_data[purpose=="training" & !is.na(mse), 
                       ! names(em_data) %in% c("mse", "purpose"), with = F], 
               em_data[purpose=="training" & !is.na(mse), 
                       mse], 
               nugget = T)
  
  pred_gp = function(x){
    predict.gp(object = gpe, newData = x)
  }
  
  # Save gp 
  cat("\n saving gp...\n\n")
  dir.create(paste0(dir_o,"/gp"), showWarnings = FALSE)
  
  if(multi_obj_mse){
  dir_multi <- paste0(dir_o,"/gp/",paste(weights, collapse = "_")) 
  dir.create(dir_multi, showWarnings = FALSE)
  saveRDS(gpe, file = paste0(dir_multi,"/gp_multi_mse.rds"))
  }else{
  saveRDS(gpe, file = paste0(paste0(dir_o,"/gp/"), "gp.rds"))
  }

  # Validation
  gp_pred_valid <- pred_gp(em_data[,
                                   ! names(em_data) %in% c("mse", "purpose"),
                                   with=F])
  em_data[,pred_mse := gp_pred_valid]
  
  if(multi_obj_mse){
    saveRDS(em_data, file = paste0(dir_multi, "/sample_multi_mse.rds"))
  }else{
    saveRDS(em_data, file = paste0(paste0(dir_o,"/gp/"), "sample_mse.rds"))
  }
  
  # Optimize gp
  lower = as.vector(limits[1,])
  upper = as.vector(limits[2,])
  
  # several starting points to check the same minimum is found
  start_pts <- as.data.frame(lhs::improvedLHS(100, ncol(limits)))
  start_pts <- mapply(function(x, multiplier,location) {(x*multiplier) + location},
                      start_pts, (limits[2, ] - limits[1, ]), limits[1, ])
  start_pts <- data.table(start_pts)
  setnames(start_pts, old = names(start_pts), new = colnames(limits))
  
  best_combi <- matrix(NA, nrow(start_pts), ncol(start_pts) )
  
  for(I in 1:(nrow(start_pts))){
    optim_gp <- optim(par =  start_pts[I],  # starting point
                      method = "L-BFGS-B",
                      fn = pred_gp, 
                      lower = lower, upper = upper) 
    for(J in 1:length(optim_gp$par)){
      best_combi[I, J] =  optim_gp$par[J]
    }
  }
  
  best_combi_mse <- apply(best_combi, 1, pred_gp)
  
  best_combi <- as.data.frame(best_combi)
  names(best_combi) <- names(start_pts)
  
  best_combi$mse <- best_combi_mse
  
  # Best Combis
  cat("\n best combination in training sample:\n\n")
  lowest_mse <- em_data[purpose=="training", min(mse, na.rm=T)]
  position_optim <- em_data[mse==lowest_mse & purpose=="training", which = TRUE]
  print(em_data[position_optim]) 
  cat("\n best combination gp:\n\n")
  print(best_combi[which.min(best_combi$mse),])
  
  
  gp_results <- list(gp_data  = em_data, best_combis = best_combi)
  
  if(multi_obj_mse){
    saveRDS(gp_results, file = paste0(dir_multi, "/gp_results_multi_mse.rds"))
  }else{
    saveRDS(gp_results, file = paste0(paste0(dir_o,"/gp/"), "gp_results.rds"))
  }
  
  return(gp_results)
  
}  

# run estimate_gp fn in parallel
estimate_gp_parallel <- function(country, params, multi_obj_mse){
  
  o_file <- "estimate_gp_out.txt"
  
  if (file.exists(o_file)){file.remove(o_file)} 
  
  cl <- makeCluster(length(params), type = "PSOCK", outfile = o_file)
  
  clusterEvalQ(cl, library(mlegp))
  clusterEvalQ(cl, library(data.table))

  clusterExport(cl, "country_params", envir =.GlobalEnv)
  
  clusterCall(cl, function() {
    source("estimate_gp.R") 
    source("compute_mse.R") 
    source("get_obs.R")
    source("get_sim.R") 
  })
  
  gp_out <- parLapply(cl, params$country, "estimate_gp_sequential", params, multi_obj_mse)
  names(gp_out) <- levels(params$country)
  
  stopCluster(cl)
  
  return(gp_out)
  
}





