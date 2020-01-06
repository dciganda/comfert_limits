###########################################################
#                    FREE PARAMETERS                      #
###########################################################
free_param <- function(){
post_edu <- 4 # wt union after edu
un_year <- 1978 # inflection point of the contraceptive transition
un_max <- 0.9 # maximum risk of unplaned birth (+ minUn)
un_min <- 0.07
uk <- 0.1 # speed contraceptive difusion
th_edu <- 16.6 # Threshold after which years of education reduces the penalty of working on intentions
w_pnty <- 0.35 # penalty on intention for working women
ini_d <- 5.3 # initial value desired family size
diff_d <- 0.28 # delta D
d_norm <- 2.35 # inflection point resistance of D
gamma <- 38
kappa <- 0.5
d <- 0.055
free_par_ls <- mget(ls())
return(free_par_ls)
}

# Sending objcts to the Global Environment
free_par_ls <- free_param()
list2env(free_par_ls, envir = .GlobalEnv)
