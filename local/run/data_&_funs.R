# Function to recover the data
get.data <- function(ini, end) {
  
  # EDUCATIONAL ATTAINMENT OF COHORTS 1925-2016 - From Census Data | IPUMS
  eduDist <- read.table(path_to("edu_dist"), header = T)
  #°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
  # ACTIITY STATUS COHORTS by EDUCATION 1925-2016 - From Census Data | IPUMS
  workDist <- as.data.table(read.table(path_to("work_dist"), header = T))
  #°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
  # AGE-SPECIFIC MORTALITY RATES by Cohort 1925-2025 | HMD
  wts_death <- readRDS(path_to("wts_death"))
  #°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
  return(list(eduDist = eduDist, workDist = workDist , wts_death = wts_death))
}

##########################################
#            AUX FUNCTIONS               #  
##########################################
# Years of Education #################################################################
yEdu <- function(eduLev, cYear) {
  
  afMeanLow <- approxfun(c(iniYear, endYear), c(3, 6)) # from 4 6
  afMeanMed <- approxfun(c(iniYear, endYear), c(8, 12)) # from 10 12
  afMeanHigh <- approxfun(c(iniYear, endYear), c(16, 22)) # from 18 22
  
  if(eduLev == 1){
    yrs <- rtrunc(1, spec = "norm", a = minEduYears,
                  b = 6, mean = afMeanLow(cYear))
  }
  if(eduLev == 2){
    yrs <- rtrunc(1, spec = "norm", a = 6, b = 12,
                  mean = afMeanMed(cYear))
  }
  if(eduLev == 3){
    yrs <- rtrunc(1, spec = "norm", a = 12, b = maxEduYears,
                  mean = afMeanHigh(cYear), sd = 6)
  }
  return(round(yrs))
}

# Function to compute Ages from dates  ##################################################
age_calc <- function(from, to) {
  from_lt <-  as.POSIXlt(from, origin = "1970-01-01")
  to_lt <-  as.POSIXlt(to, origin = "1970-01-01")

  age <-  to_lt$year - from_lt$year

  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1L, age)
}
# Value of shape parameters truncated Gamma #############################################
get_e0 = function(k_a, theta_a, a_a, b_a){
  
  res <-  k_a*theta_a*(pgamma(q = c(a_a, b_a), shape = k_a+1, scale = theta_a) %*% c(1,-1) )/
    (pgamma(q = c(a_a, b_a), shape = k_a, scale = theta_a) %*% c(1, -1) )
  
  return(as.numeric(res))
}


###########################################################
# CONSTANTS                                               #
###########################################################
max_fec <- .24
r <- 5 # rate of resistance of D
theta <- 0.7 # scale of truncated Gamma (D)
###########################################################

###########################################################
#                     AUX  VARIABLES                      #
###########################################################
Sys.setenv(TZ = "UTC") # Time is handled with POSIXct (seconds from 01.01.1970)
iniTime <- as.POSIXct(paste0(iniY,"-01-01 00:00:00"))
endTime <- as.POSIXct(paste0(endY,"-01-01 00:00:00"))
iniYear <- year(iniTime)
endYear <- year(endTime)
julys <- seq(iniTime + months(6), endTime, "years") # Aux var to identify the month of July each year.
years <- iniYear:endYear # Aux var to identify position in duration vector
time <- iniTime
elapsed <- 0L
year <- year(time)
intervalAux <- 0L
minAge <- 14L # Min age to have kids
maxAge <- 50L # Max age
srb <- 0.515 # Sex ratio at birth
pregnancy <- 270*86400L # 270 Days in seconds
tw_m <- 12*2592000L # twelve months in seconds
six_m <- 6*2592000L
secs_m <- 2592000L
secs_y <- 31536000L
mfa <- 15L # Maximum failed attempts at conception
maxKids <- 6L # Max Nr of Desired kids
maxEduYears <- 28L # Max nr of years of education yEdu Function
minEduYears <- 0L # Min nr of years of education
afMeanLogNorm <- approxfun(c(maxEduYears, minEduYears), c(3.25, 3.17)) # Aux funs to assign Age at first birth
age2 <-  seq(minAge+.5, maxAge+.5 , 1) 
asfrt  <- data.frame(t(rep(NA, maxAge-minAge+1)))
mau_cohort <- vector()
trend_d <- vector() # 
prop_w_trend <- matrix(NA,1,2) # 
dKids <- vector()
dKids_all <- vector()
ppBirths <- vector()
k_v = seq(1, 30, 0.01)
e0_v <- sapply(X = k_v, FUN = get_e0, theta_a=theta, a_a=0, b_a=maxKids)
k_vals = data.table(mean=e0_v, k=k_v) 






