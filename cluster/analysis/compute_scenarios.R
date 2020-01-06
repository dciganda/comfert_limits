compute_scenarios <- function(country, params, multi_obj_mse, weights = c(0.25,0.25,0.25,0.25)){
  
  iniY <- params[params$country ==  country, "iniY"]
  endY <- params[params$country ==  country, "endY"]
  foreY <- params[params$country ==  country, "foreY"]
  ysd <- params[params$country ==  country, "ysd"]
  npop <- params[params$country ==  country, "npop"]
  N <- params[params$country ==  country, "N"]
  
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
  # Compute results for best combination of parameters and reduced models 
  # (models excluding effects, while other parameters are fixed at optimal values) 
  scenarios <- do.call("rbind", replicate(4, estim_params, simplify = F))
  scenarios[2, "th_edu"] <- 50 # no edu push
  scenarios[3, "th_edu"] <- 12 # full edu push
  scenarios[4, "w_pnty"] <- 0 # no w penalty
  
  scenarios$modName <- c("base","no_edu_effect", "full_edu_effect", "no_w_pnty")
  scenarios_all <- rbind(scenarios[rep(1, npop), ],
                         scenarios[rep(2, npop), ],
                         scenarios[rep(3, npop), ],
                         scenarios[rep(4, npop), ])
  
  source("parallel_comfert.R")
  # start parallel computations -- this takes time!
  s <- Sys.time()
  out_scenarios <- parallel_comfert(models = scenarios_all, country, N = N, iniY, foreY)
  e <- Sys.time()
  cat("finished parallel processes \n")
  cat(paste(round(e-s,2), "elapsed"))
  
  # save results
  base_res <- out_scenarios[c(1:npop)]
  base_dir <- paste0(dir_o,"/results/base/",paste(weights, collapse = "_"))
  dir.create(base_dir, showWarnings = F, recursive = T)
  invisible(lapply(1:npop, function(x) saveRDS(base_res[[x]],
                                               file = paste0(base_dir,"/", x,".RData"))))
  
  cat('\nResults obtained with combination of parameters : \n', 
      file = paste0(base_dir, '/Info.txt'))
  
  for(I in 1:ncol(scenarios_all)){
    cat(names(scenarios_all)[I], " : ", scenarios_all[1, I], "\n", 
        file = paste0(base_dir, '/Info.txt'),
        append = T)
  }
  
  scenarios_dir <- paste0(dir_o,"/results/scenarios/", paste(weights, collapse = "_"), "/")
  dir.create(scenarios_dir, showWarnings = F, recursive = T)
  invisible(lapply((npop+1):nrow(scenarios_all), function(x) saveRDS(out_scenarios[[x]],              
                                                                   file = paste0(scenarios_dir, out_scenarios[[x]]$modName,
                                                                                 "_",
                                                                                 x %% ((floor((x-1)/npop) * npop)+1) +1, 
                                                                                 ".RData"))))
  
  source("plot_out.R")
  sim_res <-  list.files(base_dir, ".RData", full.names = T)
  plot_out(sim = sim_res, country = country, params = params, iniY = iniY)
  return(0)
  
}