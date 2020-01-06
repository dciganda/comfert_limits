
parallel_comfert <- function(models, country, N, iniY, endY) {

  path_to <- function(file){
    paste0("../run/data/", country, "/in/", file)}
  
  o_file <- paste0("parallel_comfert_", country ,"_", Sys.Date(),".txt")
  
  if (file.exists(o_file)){file.remove(o_file)} 
  
  cl <- makeCluster(nrow(models), type = "PSOCK", outfile = o_file)
  
  clusterEvalQ(cl, library(lubridate))
  clusterEvalQ(cl, library(data.table))
  clusterEvalQ(cl, library(truncdist))

  clusterExport(cl, "country")
  clusterExport(cl, "path_to", envir = environment())
  clusterExport(cl,"N", envir = environment())
  clusterExport(cl,"models", envir = environment())
  clusterExport(cl,"iniY", envir = environment())
  clusterExport(cl,"endY", envir = environment())
  
  clusterCall(cl, function() {
    source(path_to("fixed_param.R"))  # load fixed parameters (only local run)
    source("../run/aux_vars.R") # load auxiliar variables (only local run) 
    source("../run/data_&_funs.R") # load some functions
    source("../run/comfert.R") # load function running the simulation
  })
  
  output <- parLapply(cl, 1:nrow(models), "comfert", optim_run = T, cluster = F)
  
  stopCluster(cl)

  return(output)
}