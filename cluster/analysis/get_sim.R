
# get simulated rates
get_sim <- function(params, country, obs, weights,
                    multi_obj_mse = T,
                    estimated_params = F,
                    all_sim = F,
                    sample = F,
                    no_info = F,
                    unplanned = F,
                    unwanted = F,
                    desired = F){
  
  iniY <- params[params$country ==  country, "iniY"]
  endY <- params[params$country ==  country, "endY"]
  ysd <- params[params$country ==  country, "ysd"]
  npop <- params[params$country ==  country, "npop"]
  N <- params[params$country ==  country, "N"]
  
  path_o <- paste0("../results/", country,"/N_POP_",
                   npop,"/N_", N, "/results")
  res_path <- dir(path_o, pattern = "param_set", full.names = T)
  
  # reorder results in numerical order (1-nrpops)
  order_res <- sapply(X = strsplit(res_path, split = "param_set_"), 
                      FUN = function(x){rev(x)[1]})
  
  res_path <- res_path[match(1:length(res_path), order_res)]
  
  # path to results for best combination
  if (estimated_params){
    if(multi_obj_mse){
    res_path <- paste0(path_o,"/base/", paste(weights, collapse = "_"), "/")
    } else {
    res_path <- paste0(path_o,"/base/")
    }
  }
  
  if(sample){
    dir_o <- paste0("../results/", country, "/N_POP_", npop, "/N_", N)
    results <- readRDS(paste0(paste0(dir_o,"/gp/"), "gp_results.rds"))
    em_data <- results$gp_data
    lowest_mse <- em_data[purpose=="training", min(mse, na.rm=T)]
    position_optim <- em_data[mse==lowest_mse & purpose=="training", which = TRUE]
    res_path <- paste0(path_o,"/param_set_",position_optim)
  }
  
  # path to results for combination without info on contraception
  if(no_info){
    res_path <- paste0(path_o,"/no_info_contraception")
  }
  
  get_sim_results <- function(path, iniY, endY, obs){ 
    
    res_list <- lapply(path, function(x) {list.files(x, "RData", full.names = TRUE)})
    empty <- which(sapply(res_list, length)!= npop)
    
    if(length(empty)>0){
      cat(paste("No results for combination(s)"), as.vector(empty), 
          "\n removing directory from list..")
      path <- path[-as.numeric(empty)]
    } 
    
    res_names <- sapply(path, function(x) {list.files(x, "RData", full.names = TRUE)})
    
    if(class(obs) == "matrix"){
      obs_rnames <- rownames(obs)
      obs_cnames <- colnames(obs)
    }else{
      obs_rnames <- rownames(obs$obs_asfr)
      obs_cnames <- colnames(obs$obs_asfr)
    }
    
    # asfr
    asfr <- sapply(res_names, function(x) readRDS(x)["asfrt"])
    names(asfr) <- rep("asfr", length(asfr))
    
    sim_asfr_0 <- lapply(asfr,
                         function(x){colnames(x) <- paste0("age", 14:50);
                         x <- x[-1,];
                         x <- x[1:length(iniY:(endY-1)),];
                         rownames(x) <- iniY:(endY-1);
                         return(as.matrix(x))})
    
    
    sim_asfr <- lapply(sim_asfr_0,
                       function(x){x[rownames(x) %in% obs_rnames,
                                     colnames(x) %in% obs_cnames]})
    
    sim_data <- list(sim_asfr = sim_asfr)
    
    # unplanned births
    if(unplanned){
      if(length(obs$obs_unplanned)>0){
        c_names <- colnames(obs$obs_unplanned) 
      }else{
        c_names <- "proportion.unplanned"  
      }
        unp <- sapply(res_names, function(x) readRDS(x)["ppBirths"])
        names(unp) <- rep("un", length(unp))
        
        sim_unp_0 <- lapply(unp,
                            function(x) {prop <- (x[,1] - x[,2]) / x[,1];
                            return(as.matrix(prop))})
        sim_unp <- lapply(sim_unp_0,
                          function(x){x <- as.matrix(x[1:length(iniY:(endY-1)),]);
                          colnames(x) <- c_names;
                          rownames(x) <- iniY:(endY-1);
                          return(as.matrix(x))})
        
        if(all_sim){
          
          sim_unplanned <- sim_unp
          
        }else{
          
          sim_unplanned <- lapply(sim_unp,
                                  function(x){x <- x[rownames(x) %in% rownames(obs$obs_unplanned),
                                                     colnames(x) %in% colnames(obs$obs_unplanned)];
                                  x <- as.matrix(x);
                                  colnames(x) <- colnames(obs$obs_unplanned);
                                  return(x)})
        }
        
        sim_data <- c(sim_data, list(sim_unplanned = sim_unplanned))
    }
    # unwanted
    if(unwanted){
      if(length(obs$obs_unwanted)>0){
        c_names <- colnames(obs$obs_unwanted) 
      }else{
        c_names <- "prop.unwanted"  
      }
        sim_unw_0 <- lapply(unp,
                            function(x) {prop <- (x[,1] - x[,3]) / x[,1];
                            return(as.matrix(prop))})
        sim_unw <- lapply(sim_unw_0,
                          function(x){x <- as.matrix(x[1:length(iniY:(endY-1)),]);
                          colnames(x) <- c_names;
                          rownames(x) <- iniY:(endY-1);
                          return(as.matrix(x))})
        if(all_sim){
          
          sim_unwanted <- sim_unw
          
        }else{
          sim_unwanted <- lapply(sim_unw,
                                 function(x){x <- x[rownames(x) %in% rownames(obs$obs_unwanted),
                                                    colnames(x) %in% colnames(obs$obs_unwanted)];
                                 x <- as.matrix(x);
                                 colnames(x) <- colnames(obs$obs_unwanted);
                                 return(x)})
        }
        sim_data <- c(sim_data, list(sim_unwanted = sim_unwanted))
    }
    #desired
    if(desired){
      if(length(obs$obs_desired)>0){
        c_names <- colnames(obs$obs_desired) 
      }else{
        c_names <- "d"  
      }
        #dKids <- sapply(res_names, function(x) readRDS(x)["dKids"])
        dKids <- sapply(res_names, function(x) readRDS(x)["dKids_all"])
      
        names(dKids) <- rep("desired", length(dKids))
        
        sim_dkids_0 <- lapply(dKids, as.matrix)
        sim_dkids <- lapply(sim_dkids_0,
                            function(x){x <- as.matrix(x[1:length(iniY:(endY-1)),]);
                            colnames(x) <- colnames(obs$obs_desired);
                            rownames(x) <- iniY:(endY-1);
                            return(as.matrix(x))})
        
        if(all_sim){
          
          sim_desired <- sim_dkids
          
        }else{
          sim_desired <- lapply(sim_dkids,
                                function(x){x <- x[rownames(x) %in% rownames(obs$obs_desired),
                                                   colnames(x) %in% colnames(obs$obs_desired)];
                                x <- as.matrix(x);
                                colnames(x) <- colnames(obs$obs_desired);
                                return(x)})
        }
        sim_data <- c(sim_data, list(sim_desired = sim_desired))
    }
    # results
    return(sim_data)
  }
  
  if (estimated_params){
    npop_sim_results <- get_sim_results(res_path, iniY, endY, obs)
  }else{
    npop_sim_results <- lapply(res_path, get_sim_results, iniY, endY, obs)
  }
  
  return(npop_sim_results)
  
}
