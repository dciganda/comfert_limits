compute_no_info_contraception <- function(country = "FR", params, weights){
  
  iniY <- params[params$country ==  country, "iniY"]
  endY <- params[params$country ==  country, "endY"]
  foreY <- params[params$country ==  country, "foreY"]
  ysd <- params[params$country ==  country, "ysd"]
  npop <- params[params$country ==  country, "npop"]
  N <- params[params$country ==  country, "N"]
  
  # Get Estimated values
  dir_o <- paste0("../results/", country, "/N_POP_", npop, "/N_", N)
  results <- readRDS(paste0(paste0(dir_o,"/gp/"), paste(weights, collapse = "_"),
                            "/gp_results_multi_mse.rds"))
  
  estim_params <- results$best_combis[which.min(results$best_combis$mse),]
  # Compute results for best combination of parameters 
  no_info <- do.call("rbind", replicate(npop, estim_params, simplify = F))
  
  no_info$modName <- c("no_info_contraception")
  
  source("parallel_comfert.R")
  # start parallel computations -- this takes time!
  s <- Sys.time()
  out_scenario <- parallel_comfert(models = no_info, country, N, iniY, foreY)
  e <- Sys.time()
  cat("finished parallel processes \n")
  cat(paste(round(e-s,2), "elapsed"))
  

  # save results
  no_info_dir <- paste0(dir_o,"/results/no_info_contraception/")
  dir.create(no_info_dir, showWarnings = F)
  invisible(lapply(1:npop, function(x) saveRDS(out_scenario[[x]],
                                               file = paste0(no_info_dir,"no_info", x,".RData"))))
  
  cat('\nResults obtained with combination of parameters : \n', 
      file = paste0(no_info_dir, '/Info.txt'))
  
  for(I in 1:ncol(no_info)){
    cat(names(no_info)[I], " : ", no_info[1, I], "\n", 
        file = paste0(no_info_dir, '/Info.txt'),
        append = T)
  }
  
  return(0)
  
}