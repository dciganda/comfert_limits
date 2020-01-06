# -------------------------------------------------------- #
# This script generates waiting times from                 #
# piece-wise constant hazard models (time-specific rates)  #
# -------------------------------------------------------- # 

# --- Breakpoints and coefficients  -----------------

wtg <- function(rates, n){

# Define breaks and levels of the PW model
# The levels are the hazard (conditional prob.)
my.lower <- seq(0, length(rates)-1,1)
my.upper <- seq(1, length(rates))
my.coeff <- rates
# ---------------------------------------------------

# ----- Define function for h.pw ---------------
h.pw <- function(t, lower, upper, coeff){
  max(coeff * as.numeric (((t-lower)>=0) & ((t-upper)<0)))
}
# ----------------------------------------------

x <- seq(0,length(rates), by=0.2)
h <- rep(NA, length(x))

for (i in 1:length(x)){
 h[i] <- h.pw(x[i], lower=my.lower, upper=my.upper, coeff=my.coeff)
}

# ------- intergrated hazard pw-const model  ----- #
H.pw <- function(t, lower, upper, coeff){  
 p1 <-  pmax((t-lower), 0)
 p2 <-  pmin(p1, upper-lower)
 return(sum(coeff*p2)) 
}

H <- rep(NA, length(x))

for (i in 1:length(x)){
  H[i] <- H.pw(x[i], lower=my.lower, upper=my.upper, coeff=my.coeff)
}

# --------- Now for pw-const H-inversion    --------
pw.root <- function(t, low, up, coef, u){
    return(H.pw(t, lower=low, upper=up, coeff=coef) + log(u))
}

#-------- Proportion not experiencing the event -----#
ne <- exp(-H)[length(exp(-H))]
nn <- round(n - (n*ne),0)
if (nn==0){nn <- nn+1}

# -----------------------------------------------------
r.pw <- function(nn, lower, upper, coeff){
   u <- runif(nn, min=exp(-H)[length(exp(-H))])
    times <- rep(NA, nn)
    for(i in 1:nn){
    result <- uniroot(pw.root, interval=c(0, 101), 
                  u=u[i], low=lower, up=upper, coef=coeff)
    times[i] <- result$root
  }
  return(times)
}
# -----------------------------------------------------

# --------------------------------------- #
pw.sample <- r.pw(nn, lower=my.lower, upper=my.upper, coeff=my.coeff)

if(n==1)  {
  all.sample <- sample(c(pw.sample,Inf), 1, prob=c(1-ne, ne))
  }else{
  all.sample <- sample(c(pw.sample, rep(Inf, n-length(pw.sample))))
  }
return(all.sample)
}


