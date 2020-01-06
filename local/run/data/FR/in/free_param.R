###########################################################
#                    FREE PARAMETERS                      #
###########################################################
free_param <- function(){
ini_d <- 2.8  # initial value desired family size
post_edu <- 3.13502# wt union after edu
rEduInt <- 0.53  # Rate function effect years of education
lambda <- 3e-08 # rate of the exponential decrease of intention with age last kid
un_year <- 1972.2 # inflection point of the contraceptive transition
un_max <- 0.42 # maximum risk of unplaned birth (+ minUn)
d <- 0.076 # reduction risk unplanned after achieve D
uk <- 0.2602583  # speed contraceptive difusion
th_edu <- 16.9051 # Threshold after which years of education reduces the penalty of working on intentions
w_pnty <- 0.45 # penalty on intention for working women
gamma <- 38.7 # Fecundability age
kappa <- 0.4 # Fecundability rate
#############
d_norm <- 2.4 # inflection point resistance of D
diff_d <- 0.17 # delta D
un_min <- 0.03  # minimum risk of unplanned birth
############
free_par_ls <- mget(ls())
return(free_par_ls)
}

# Sending objcts to the Global Environment   
free_par_ls <- free_param()
list2env(free_par_ls, envir = .GlobalEnv)