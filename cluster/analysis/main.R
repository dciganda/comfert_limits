library(lubridate); library(data.table); library(parallel);
library(ggplot2); library(mlegp); library(lhs); library(forecast); 

country <- c("ES","FR", "IR")
iniY <- rep(1910, length(country))
endY <- rep(2017, length(country))
foreY <- rep(2037, length(country))
ysd <- rep(1960, length(country))
npop <- rep(10, length(country))
N <- c(1101, 1101, 600)
country_params <- cbind.data.frame(country = country,
                                   iniY = iniY,
                                   endY = endY,
                                   foreY = foreY,
                                   ysd = ysd,
                                   npop = npop,
                                   N = N)
weights <- c(asfr = 0.60, unplanned = 0.00, unwanted = 0.00, desired = 0.40)

source("estimate_gp.R")
# This functions estimate the gp, predict the mse of test set,
# optimizes the gp and returns the original data (training + test)
# and a set with best predicted combinations.
# For more than 1 country it can be run in parallel (with parallel = T).
source("compute_scenarios.R")
source("real_mse.R")
source("plots.R")

#### FRANCE ######################################################################
weights <- c(asfr = 0.40, unplanned = 0.20, unwanted = 0.00, desired = 0.40)
gp_results <- estimate_gp(country = "FR", params =  country_params,
                          parallel = F, multi_obj_mse = T,
                          weights = weights) 

compute_scenarios(country = "FR", params = country_params, multi_obj_mse = T,
                  weights = weights)

real_mse(country = "FR", params = country_params,
         multi_obj_mse = T, 
         weights = weights,
         unplanned = T, unwanted = T, desired = T)

plot_gp_validation(country = "FR", 
                   params = country_params,
                   multi_obj_mse = T,
                   weights = weights,
                   save = T)
plot_fit_asfr(country = "FR", params = country_params,
              weights = weights, multi_obj_mse = T, save = T)

plot_fit(country = "FR", params = country_params, multi_obj_mse = T,
         weights = weights, unplanned = T, unwanted = T, desired = T,
         save = T)

plot_sim_contra(country = "ES", params = country_params, weights = weights, save = T)

plot_tfr(country = "FR", params = country_params, estimated_params = T,
         weights = weights, save = T)

plot_mab(country = "FR", params = country_params,
         estimated_params = T,
         weights = weights, save = T)

plot_ccf(country = "FR", params = country_params,
         estimated_params = T,
         weights = weights, save = T)

plot_gap(country = "FR", params = country_params, estimated_params = T,
         weights = weights, save = T)

plot_analysis_scenarios(country = "FR",params = country_params, weights = weights, save = T)

plot_no_info_contra(country = "FR", params = country_params, weights = weights, save = T)


#### SPAIN #########################################################################
weights <- c(asfr = 0.10, unplanned = 0.00, unwanted = 0.00, desired = 0.90)

gp_results <- estimate_gp(country = "ES", params =  country_params,
                          parallel = F, multi_obj_mse = T,
                          weights = weights) 

compute_scenarios(country = "ES", country_params,weights = weights, multi_obj_mse = T)

real_mse(country = "ES", params = country_params,
         multi_obj_mse = T, 
         weights = weights,
         unplanned = T, unwanted = T, desired = T)

plot_gp_validation(country = "ES",weights = weights,
                   params = country_params,
                   multi_obj_mse = T,
                   save = F)

plot_fit_asfr(country = "ES", params = country_params,
              weights = weights, multi_obj_mse = T, save = T)


plot_fit(country = "ES", params = country_params, multi_obj_mse = T,
         weights = weights, unplanned = F, unwanted = F, desired = T,
         save = T)

plot_sim_contra(country = "ES", params = country_params, weights = weights, save = T)

plot_tfr(country = "ES", params = country_params, estimated_params = T,
         weights = weights, save = T)

plot_ccf(country = "ES", params = country_params,
         estimated_params = T,
         weights = weights, save = T)


plot_mab(country = "ES", params = country_params,
         estimated_params = T,
         weights = weights, save = T)

plot_gap(country = "ES", params = country_params, estimated_params = T,
         weights = weights, save = T)

plot_analysis_scenarios(country = "ES",params = country_params,
                        weights = weights, save = T)

#### IRELAND ###########################################################
weights <- c(asfr = 0.40, unplanned = 0.00, unwanted = 0.00, desired = 0.60)
gp_results <- estimate_gp(country = "IR", params =  country_params,
                          parallel = F, multi_obj_mse = T,
                          weights = weights) 
compute_scenarios(country = "IR", country_params,weights = weights, multi_obj_mse = T)


plot_gp_validation(country = "IR",weights = weights,
                   params = country_params,
                   multi_obj_mse = T,
                   save = T)

plot_fit_asfr(country = "IR", params = country_params,
              weights = weights, multi_obj_mse = T, save = T)


plot_fit(country = "IR", params = country_params, multi_obj_mse = T,
         weights = weights, unplanned = F, unwanted = F, desired = T,
         save = T)

plot_sim_contra(country = "IR", params = country_params, weights = weights, save = T)

plot_tfr(country = "IR", params = country_params, estimated_params = T,
         weights = weights, save = T)

plot_mab(country = "IR", params = country_params,
         estimated_params = T,
         weights = weights, save = T)

plot_ccf(country = "IR", params = country_params,
         estimated_params = T,
         weights = weights, save = T)

plot_gap(country = "IR", params = country_params, estimated_params = T,
         weights = weights, save = T)

plot_analysis_scenarios(country = "IR",params = country_params,
                        weights = weights, save = T)


########################################################################