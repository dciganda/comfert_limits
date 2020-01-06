#### Retrieve Arguments
library(parallel)
# country on which simulations are performed
country <- commandArgs(TRUE)[1]
# years for which the simulations are performed
iniY <- as.numeric(commandArgs(TRUE)[2])
endY <- as.numeric(commandArgs(TRUE)[3])
# size of the initial population
N <- as.numeric(commandArgs(TRUE)[4])
# get index (in sample.csv) of parameters' combination used
paramSetIndex <- as.numeric(commandArgs(TRUE)[5]) 
# nb of populations
npop <- as.numeric(commandArgs(TRUE)[6])

#### Retrieve sample of combinations

# get data.frame of design points
param_df <- read.csv(paste0("../results/", country, "/N_POP_",
                            npop, "/N_", N, "/param_sample/sample.csv"))

# path where results will be saved
resultsPath <- paste0("../results/", country, "/N_POP_",
                      npop, "/N_", N, "/results/param_set_", paramSetIndex)


cat('\nResults obtained with combination of parameters : \n',
    file = paste0(resultsPath, '/Info.txt'))

for(I in 1:ncol(param_df)){

 cat(names(param_df)[I], " : ", param_df[paramSetIndex, I], "\n",
     file = paste0(resultsPath, '/Info.txt'),
     append=T)
}

cat("\n\nCountry : ", country, "\n\nSize of the initial population : ", N, "\n\n")

cat("Simulation with set of parameters : \n")
print(param_df[paramSetIndex, ])
cat("\n")

ini_d <- param_df[paramSetIndex, "ini_d"]  
diff_d <- param_df[paramSetIndex, "diff_d"] 
d_norm <- param_df[paramSetIndex, "d_norm"] 
d <- param_df[paramSetIndex, "d"]
un_year <- param_df[paramSetIndex, "un_year"]
un_max <- param_df[paramSetIndex, "un_max"]
un_min <- param_df[paramSetIndex, "un_min"]
uk <- param_df[paramSetIndex, "uk"]
th_edu <- param_df[paramSetIndex, "th_edu"]
w_pnty <- param_df[paramSetIndex, "w_pnty"]
post_edu <- param_df[paramSetIndex, "post_edu"]
gamma <- param_df[paramSetIndex, "gamma"]
kappa <- param_df[paramSetIndex, "kappa"]


free_par_ls <- param_df[paramSetIndex, ]
  
# Parallelization of repetitions
cat("Launching cluster\n")

cl <- makeCluster(npop, type = "PSOCK")

# Load libraries
clusterEvalQ(cl, library(lubridate))
clusterEvalQ(cl, library(data.table))
clusterEvalQ(cl, library(truncdist))

# Source country files
path_to = function(file){
  paste0("data/", country, "/in/", file)}

cat("Exporting objects\n")

clusterExport(cl, "path_to", envir = environment())
clusterExport(cl,"country", envir = environment())
clusterExport(cl,"N", envir = environment())
clusterExport(cl,"iniY", envir = environment())
clusterExport(cl,"endY", envir = environment())
clusterExport(cl,"ini_d", envir = environment())
clusterExport(cl,"diff_d", envir = environment())
clusterExport(cl,"d_norm", envir = environment())
clusterExport(cl,"d", envir = environment())
clusterExport(cl,"un_year", envir = environment())
clusterExport(cl,"un_max", envir = environment())
clusterExport(cl,"un_min", envir = environment())
clusterExport(cl,"uk", envir = environment())
clusterExport(cl,"th_edu", envir = environment())
clusterExport(cl,"w_pnty", envir = environment())
clusterExport(cl,"post_edu", envir = environment())
clusterExport(cl,"gamma", envir = environment())
clusterExport(cl,"kappa", envir = environment())
clusterExport(cl,"free_par_ls", envir = environment())

cat("Sourcing on slaves\n")

clusterCall(cl, function() {
  source(path_to("fixed_param.R"))  # load fixed parameters
  source("data_&_funs.R") # load some functions
  source("comfert.R") # load function running the simulation
})

cat("runing comfert\n")

s <- Sys.time()
output <- parLapply(cl, 1:npop, "comfert", optim_run = F, cluster = T) # only requirement on the argument : should be unique = Not the case in this version!
e <- Sys.time()
print(e-s)
cat("finished parallel processes \n")
stopCluster(cl)

cat(".....saving results....")
invisible(lapply(1:npop, function(x) saveRDS(output[[x]],
                                             file = paste0(resultsPath,"/full_results",x,".RData"))))
rm(output)

