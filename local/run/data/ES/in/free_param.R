###########################################################
#                    FREE PARAMETERS                      #
###########################################################
free_param <- function(){
post_edu <- 7.5 # wt union after edu
un_year <- 1976 # inflection point of the contraceptive transition
un_max <- 0.30 # maximum risk of unplaned birth (+ minUn)
un_min <- 0.032
uk <- 0.1943520 # speed contraceptive difusion
th_edu <- 17.20977 # Threshold after which years of education reduces the penalty of working on intentions
w_pnty <- 0.45 # penalty on intention for working women
ini_d <- 3.3 # initial value desired family size
diff_d <- 0.25 # delta D
d_norm <- 1.7 # inflection point resistance of D
gamma <- 38
kappa <- 0.5
d <- 0.09 
free_par_ls <- mget(ls())
return(free_par_ls)
}

# Sending objcts to the Global Environment
free_par_ls <- free_param()
list2env(free_par_ls, envir = .GlobalEnv)

