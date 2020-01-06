###########################################################
#         FIXED PARAMETERS & AUXILIARY VARIABLES          #  
###########################################################
#                      PARAMETERS                         #
###########################################################
fixed_param <- function(){
  ###############
  lambda <- 2.5e-08# rate of the exponential decrease of intention with age last kid
  rEduInt <- 0.53 # rate effect education
  sd_lnrm <- 0.19 # stdrd dev lognorm
  diffEduUn <- 0.14 # Difference in contraceptive use by edu
  ###############
  ini_mean_d <- 1940 # year from which D_bar is computed from real average 
  c_d <- 1960 # last cohort for which mean age at union decreases
  ini_mau <- 28 # initial value of the mean age at union formation
  end_mau <- 23.5 # end value of the mean age at union  
  uprob <- c(seq(0.25, 0.11, length.out = 1940 - iniY), seq(0.11, 0.28, length.out = (endY + 1) - 1940)) # union probability
  mau_trend <- seq(ini_mau, end_mau, length.out = c_d-iniY)
  fixed_param_list <- mget(ls())
  return(fixed_param_list)
}
# Sending objcts to the Global Environment   #
fix_par_ls <- fixed_param()
list2env(fix_par_ls, envir = .GlobalEnv)