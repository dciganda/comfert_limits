##############################################################################################

# The first three arguments determine where the sample will be saved
# country sample is asked for
country <- commandArgs(TRUE)[1]
# number of populations
npop <- as.numeric(commandArgs(TRUE)[2])
# size of initial populations
N <- as.numeric(commandArgs(TRUE)[3])

# number of parameters' combinations that must be generated
ncombi <- as.numeric(commandArgs(TRUE)[4])

# fraction of sample that goes to validation
# NB : ncombi * frac must be an integer
frac <- 0.1

# min and max of parameters we allow to vary
param_limits <- data.frame(un_year = c(1971, 1976),         # Year inflexion point diffusion of contraception.
                           un_max = c(0.25, 0.45),          # Maximum Risk Unplanned births
                           un_min = c(0.005, 0.045),        # minimum risk of unplanned birth
                           uk = c(0.19, 0.28),              # Speed of diffusion contraception
                           th_edu = c(14, 24),              # Threshold years of education
                           w_pnty = c(0.2, 0.6),            # Max effect work
                           post_edu = c(3, 4.5),            # years after end of education for family formation
                           gamma = c(35, 45),               # Fecundability age
                           kappa = c(0.1, 0.9),             # Fecundability rate
                           d = c(0.03, 0.09),               # reduction risk unplanned after achieve D
                           ini_d = c(2.6, 3),               # initial value desired family size
                           diff_d = c(0.14, 0.20),          # delta D
                           d_norm = c(2.2, 2.6)             # inflection point resistance of D
)

locations <- param_limits[1, ]
multipliers <-param_limits[2, ] - locations

training_sample <- lhs::improvedLHS(ncombi*(1-frac), ncol(param_limits))

full_sample <- as.data.frame(lhs::optAugmentLHS(training_sample, ncombi*frac))

full_sample <- mapply(function(x, multiplier,location) (x*multiplier) + location,
                      full_sample, multipliers, locations)

full_sample <- data.frame(full_sample)

colnames(full_sample) <- colnames(param_limits)

full_sample$purpose <- ifelse(1:nrow(full_sample)<=(1-frac)*ncombi, "training", "validation")


#############################################################################################
#################################### Saving to csv files ####################################

loc_path <- paste0("../results/", country, "/N_POP_", npop, "/N_", N, "/param_sample","/")

if(!dir.exists(loc_path)){
      dir.create(loc_path)
}

write.csv(param_limits, paste0(loc_path, "limits.csv"), row.names=FALSE)
write.csv(full_sample, paste0(loc_path, "sample.csv"), row.names=F)
