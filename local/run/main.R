library(parallel)
# country on which simulations are performed
country <- "IR"
# years for which the simulations are performed
iniY <- 1910
endY <- 2017
# size of the initial population
N <- 500
# nr of populations
npop <- 30

o_file <- paste0("out_", country, "_", Sys.Date(), ".txt")

if (file.exists(o_file)){file.remove(o_file)} 
  
cl <- makeCluster(npop, type = "PSOCK", outfile = o_file)

# Load libraries
clusterEvalQ(cl, library(lubridate))
clusterEvalQ(cl, library(data.table))
clusterEvalQ(cl, library(truncdist))

# Source country files
path_to = function(file){
  paste0("data/", country, "/in/", file)}

clusterExport(cl, "path_to", envir = environment())
clusterExport(cl,"country", envir = environment())
clusterExport(cl,"N", envir = environment())
clusterExport(cl,"iniY", envir = environment())
clusterExport(cl,"endY", envir = environment())

clusterCall(cl, function() {
  source(path_to("fixed_param.R"))  # load fixed parameters (only local run)
  source("aux_vars.R") # load auxiliar variables (only local run) 
  source(path_to("free_param.R"))
  source("data_&_funs.R") # load some functions
  source("comfert.R") # load function running the simulation
})

s <- Sys.time()
output <- parLapply(cl, 1:npop, function (x) comfert(x, optim_run = F, cluster = F))
e <- Sys.time()
print(e-s)
cat("finished parallel processes \n")
stopCluster(cl)


# save results
out_dir <- file.path("results", paste(country),
                     paste0("N_POP_",npop),
                     paste0("/N_", N),"results")
dir.create(out_dir, recursive = TRUE, showWarnings = F)
invisible(lapply(1:npop,
                 function(x) saveRDS(output[[x]],
                                             file = paste0(out_dir,
                                                           "/Results_run_",
                                                           x,
                                                           ".RData"))))
source("get_obs_asfr.R")
source("get_sim_asfr.R")
source("plot_asfr.R")
source("plot_out.R")

# get observed fertility rates
obs_asfr <- get_obs_asfr(country)
# Simulated rates
sim_asfr <- get_sim_asfr(country, npop, N, obs_asfr, out_dir, iniY, endY)

# compare sim vs. obs asfr in selected years 
par(mfrow=c(1,1), mar=c(4, 4, 3, 1) )
plot_years = seq(1960, 2015, 5)

# plot the asfr in selected years for the best combination in the training sample
sapply(plot_years, plot_asfr, 
       sim = sim_asfr, 
       obs = obs_asfr)

# plot observed vs simulated outcomes 
sim_res <-  list.files(out_dir, ".RData", full.names = T)
par(mfrow=c(1,1), mar=c(4, 4, 3, 1) )
plot_out(sim_res, country, iniY)



