comfert <- function(j, optim_run = F, cluster = F){
  setDTthreads(1)
  
  if(optim_run){
   param_list <- c("ini_d", "diff_d", "lambda", "un_year",
      "un_max", "un_min", "uk", "th_edu", "w_pnty",
      "post_edu", "rEduInt", "d_norm",  "modName")
   for(i in param_list){
     if(length(models[j, i]) > 0){
       assign(i, models[j, i])
     }
   }
  }else{
    modName <- "regular"
  }
  
  set.seed(j)
  
  ##########################################
  # FUNCTIONS                              #  
  ##########################################
  # Effect of Contraception on Fecundability #################
  cnt <- function(eduYrs, yr){
    eduYrs <- eduYrs + 1 
    un_max/(eduYrs^diffEduUn) / (1 + exp(uk*(yr-(un_year-(eduYrs))))) + un_min
  }
  # Effect of years of edu on Intention ######################
  eduInt <- function(eduYears){
    ei <- w_pnty / (1 + exp(rEduInt*(eduYears-th_edu)))  
    return(ei)
  }
  ###########################################################
  # LOAD DATA                                               #  
  ###########################################################
  data <- get.data(ini = iniYear, end = endYear)
  cat("Data loaded ok\n") ###################################
  eduDist <- data$eduDist
  workDist <- data$workDist
  wts_death <- data$wts_death
  ###########################################################
  # INITIAL BIRTHS                                          #  
  ###########################################################
  wtb <- runif(N)
  swtb <- wtb/max(wtb) * secs_y 
  ###########################################################
  # POP                                                     #  
  ###########################################################
  pop <- data.table(wtDeath = Inf,
                    wtUnion = Inf,
                    wtBirth = Inf,
                    wtEvalBirth = Inf,
                    age = 50L,
                    ageSec = 1576800000L,
                    tob = -2997907200,
                    edu = 3L,
                    yearsEdu = 20L,
                    activity = 0L,
                    union = 0L,
                    unionAge = 0L,
                    desiredKids = 0L,
                    kids = 0L,
                    ageBirth = 0L,
                    timeBirth = 0,
                    kidsLeft = 0L,
                    birthYear = 0L,
                    pregnant = 0L,
                    unionYear = 0L,
                    baseline = 0,
                    intentBirth = 0,
                    failedAttempts = 0L,
                    unplanned = 0L
  )
  
  ###################
  # Aux Vars        # 
  ###################
  eventNames <- c("death", "union", "birth", "evalBirth")
  wt_col <- paste0("wt", toupper(substr(eventNames, 1, 1)), substr(eventNames, 2, 1000L))
  eventCount <- setNames(as.list(rep(0, length(eventNames))), eventNames)
  mau <- mau_trend[1]

  cat("Initialisation ok, 'entering' while loop\n")  ######################################## 
  
  #****************************************************************************************************************************************
  #-------------------------------------------------------- RUNNING THE SIMULATION -------------------------------------------------------#
  #****************************************************************************************************************************************
  while (time < endTime){
    
    min_idx <- pop[, arrayInd(which.min(as.matrix(.SD)), .dim = dim(.SD)), .SDcols = wt_col]
    timeNextEvent <- pop[[min_idx[1], min_idx[2]]]
    nextEvent <- eventNames[min_idx[2]]
    rid <- min_idx[1]
    
    # next event*
    if(length(swtb) > 0){  
      if(timeNextEvent > min(swtb)){
        nextEvent <- "birth"
        timeNextEvent <- min(swtb)
        rid <- NA
        swtb <- swtb[-which.min(swtb)]
      }
      swtb <- swtb - timeNextEvent
    }
    eventCount[[nextEvent]] <- eventCount[[nextEvent]] + 1

    #****************************************************************************************************************************************
    #--------- UPDATING ---------------------------------------------------------------------------------------------------------------------
    #****************************************************************************************************************************************
    # Clock.
    time <- time + timeNextEvent
    elapsed <- elapsed + timeNextEvent
    # Update Age of all agents.
    pop[, ageSec := ageSec + timeNextEvent]
    # Update WTs of all agents
    pop[, c(wt_col):= lapply(.SD, function(x){x-timeNextEvent}), .SDcols = wt_col]

    ########################
    ##       UNION        ##
    ########################
    if (nextEvent=="union") {
      pop[rid, age := age_calc(tob, time)]
      pop[rid, `:=`(union = 1L, 
                    unionAge = age,
                    wtUnion = Inf,
                    unionYear = 1L)]
      
      mean_gamma <- mean_d * (1 + ((diff_d / (1 + exp((-1)^pop[rid,activity] * r *
                                                        (mean_d-(d_norm))))) * (-1)^pop[rid,activity] * 
                                     (1-prop_w[pop[rid,activity]+1])))
      k <- k_vals[which.min(abs(mean_gamma-mean)), k]
      
      pop[rid, desiredKids := round(rtrunc(1,
                                           spec ="gamma",
                                           a = 0, b = maxKids,
                                           shape = k,
                                           scale = theta),
                                    0)]
      pop[rid, kidsLeft := desiredKids]
      pop[rid, baseline := ifelse(kidsLeft <= 0L, 0, 0.8)]
      pop[rid, intentBirth := baseline - eduInt(eduYears = yearsEdu) * activity -
            (baseline - eduInt(eduYears = yearsEdu) * activity) * exp(-lambda * (elapsed - timeBirth))]  
      
      have_child <-  runif(1) < pop[rid, intentBirth]
      pop[rid, unplanned := 0L]
      
      if(have_child){ # go for planned conception
        
        wtConception <- rexp(1, rate = max_fec / (1 + exp(kappa*(pop[rid, age] - gamma)))) * secs_m
        
        if(wtConception < tw_m){ # conception within the year
          
          pop[rid, `:=` (pregnant = 1, wtBirth = wtConception + pregnancy)]  
          
        }else{ # failed to conceive
          
          pop[rid, `:=` (failedAttempts = failedAttempts + 1, wtEvalBirth = tw_m)]
          
        }
      }else{ # unplanned risk
        
        wtConception <- rexp(1, rate = cnt(eduYrs = pop[rid, yearsEdu], yr = year(time)) * 
                               max_fec / (1 + exp(kappa*(pop[rid, age]-gamma))) * d) * secs_m # unplanned wt
        
        if(wtConception < tw_m){ # unplanned conception within the year
          
          pop[rid, `:=` (pregnant = 1, unplanned = 1L, wtBirth = wtConception + pregnancy)]  
          
        }else{ # no unplanned conception
          
          pop[rid, `:=` (failedAttempts = failedAttempts + 1, wtEvalBirth = tw_m)]
          
        }
      }
    } 
    
    ########################
    ##     EVAL BIRTH     ##
    ########################
    if (nextEvent == "evalBirth") {

      pop[rid, intentBirth := baseline - eduInt(eduYears = yearsEdu) * activity -
            (baseline - eduInt(eduYears = yearsEdu) * activity) * exp(-lambda * (elapsed - timeBirth))]

      have_child <-  runif(1) < pop[rid, intentBirth]
      pop[rid, unplanned := 0L]
      
      if(have_child){ # go for planned conception
        
        wtConception <- rexp(1, rate = max_fec / (1 + exp(kappa*(pop[rid, age]-gamma)))) * secs_m
        
        if(wtConception < tw_m){ # conception within the year
          
          pop[rid, `:=` (pregnant = 1, wtBirth = wtConception + pregnancy, wtEvalBirth = Inf)]  
          
        }else{ # failed to conceive
          
          pop[rid, failedAttempts := failedAttempts + 1]
          pop[rid, `:=` (wtEvalBirth = ifelse(failedAttempts == mfa, Inf, tw_m))]
          
        }
      }else{ # unplanned risk
        
        wtConception <- rexp(1, rate = cnt(eduYrs = pop[rid, yearsEdu], yr = year(time)) * 
                               max_fec / (1 + exp(kappa*(pop[rid, age]-gamma))) * d^pop[rid, kidsLeft<=0] ) * secs_m # unplanned wt
        
        if(wtConception < tw_m){ # unplanned conception within the year
          
          pop[rid, `:=` (pregnant = 1, unplanned = 1L, wtBirth = wtConception + pregnancy, wtEvalBirth = Inf)]  
          
        }else{ # no unplanned conception
          
          pop[rid, failedAttempts := failedAttempts + 1]
          pop[rid, `:=` (wtEvalBirth = ifelse(failedAttempts == mfa, Inf, tw_m))]
          
        }
      }
    }
    
    ########################
    ##       BIRTH        ##
    ########################
    if (nextEvent == "birth") {
      if(is.na(rid)){
        girl <- Inf
      }else{
        girl <- runif(1)
      }
      if (girl > srb){
        pop <- rbindlist(list(pop, pop[.N+1]), fill = T)
        pop[.N, `:=` (age = 0L,
                      ageSec = 0,
                      tob=as.numeric(time),
                      union = 0L,
                      kids = 0L,
                      timeBirth = 0,
                      birthYear = 0L,
                      pregnant = 0L,
                      unionYear = 0L,
                      failedAttempts = 0L,
                      wtBirth = Inf,
                      baseline = NA_real_,
                      intentBirth = NA_real_,
                      ageBirth = NA_integer_,
                      wtEvalBirth = Inf,
                      unplanned = 0L,
                      desiredKids = NA_integer_,
                      kidsLeft = NA_integer_
                      )]
        
        pop[.N, edu := findInterval(runif(1),
                                    eduDist[eduDist$year == year(time),
                                                 1:3],
                                    rightmost.closed = TRUE) + 1]
        pop[.N, yearsEdu := yEdu(edu, year(time))]
        pop[.N, activity := findInterval(runif(1), 
                                         workDist[year == year(time), 
                                                  ((edu*2)-1):(((edu*2)-1)+1)],
                                         rightmost.closed = FALSE)]

        unionProb <- uprob[year(time)-(year(iniTime)-1)]
        if(runif(1) > unionProb){
          
          if(year(time) >= c_d){
           input_age_union <- max(end_mau, 6 + pop[.N, yearsEdu] + post_edu)
          }else{
           input_age_union <- mau
          }
          
          wtunionaux <- rlnorm(1,
                               meanlog = log(mean(c(mau, input_age_union))),
                               sdlog = sd_lnrm) 
          
          pop[.N, unionAge := floor(wtunionaux)]
          pop[.N, wtUnion := wtunionaux*secs_y] # seconds from years
          
        }else{
          pop[.N, wtUnion := Inf]  
          pop[.N, unionAge := NA_integer_]
        }
  
        wtd <- sample(wts_death[[year(time) - (iniY-1)]],size = 1)
        wts_death[[year(time) - (iniY-1)]] <- wts_death[[year(time) - (iniY-1)]][-as.numeric(names(wtd))]
        pop[.N, wtDeath := wtd*secs_y] 
        
      }
      
      # Change Indicators for mother
      if(!is.na(rid)){
        pop[rid, age := age_calc(tob, time)]
        pop[rid, `:=`(ageBirth = age,
                      timeBirth = elapsed,
                      pregnant = 0,
                      wtBirth = Inf,
                      kids = kids + 1L)]
        pop[rid, birthYear := ifelse(unplanned == 1, 2, 1)] # 2 = unplanned birth
        pop[rid, kidsLeft := desiredKids - kids]
        pop[rid, baseline := ifelse(kidsLeft <= 0, 0, baseline)]
        # Redefine intention: 
        pop[rid, wtEvalBirth := six_m]
      }
    }
    
    ########################
    ##       DEATH        ##
    ########################
    if (nextEvent == "death") {
      pop <- pop[-rid]
    }
    
    #****************************************************************************************************************************************#
    #--------- INDICATORS--------------------------------------------------------------------------------------------------------------------#
    #****************************************************************************************************************************************#
    if (intervalAux != findInterval(time, julys)){
      # Number of women by age to compute ASFR
      pop[ageSec > 13L*secs_y, age := age_calc(tob, time)]
      womenAge <- tabulate(pop[age %in% minAge:maxAge, age])
      womenAge <- womenAge[minAge:maxAge]
      womenAge[which(is.na(womenAge))] <- 0
      intervalAux <- intervalAux + 1
      
      pop <- pop[age < 52]
    }
    
    if (year != year(time)){
      print(time)
      # Gereating new births for burn-in period
      if (year <= iniYear + minAge){
        wtb <- runif(N)
        swtb <- wtb/max(wtb) * secs_y # seconds from years
      }else{
        if (year > iniYear + minAge & year <= iniYear + (maxAge-minAge-1)){
          wtb <- runif(N)
          swtb <- wtb/max(wtb) * secs_y 
          rw <- (year-(iniYear + minAge))/(iniYear + maxAge-(iniYear + minAge))
          rwt <- sample(1:length(swtb),round(length(swtb)*rw,0))
          swtb <- swtb[-rwt]
        }else{
          wtb <- NULL
          swtb <- Inf
        }
      }
      
      # Mean D of younger cohort
      trend_d <- c(trend_d, mean(pop[age %in% minAge:29, desiredKids], na.rm = T))
      mean_d <- mean(trend_d[length(trend_d)])
      if(year(time)<ini_mean_d){mean_d <- ini_d}
      prop_w <- as.numeric(prop.table(table(factor(pop[age %in% minAge:maxAge,activity], levels = 0:1))))
      
      # Mean D
      dKids <- c(dKids, pop[age %in% 25:34, mean(desiredKids, na.rm = T)]) 
      dKids_all <- c(dKids_all, pop[age > 19, mean(desiredKids, na.rm = T)]) 
      
      # Unions
      yob <- year(as.POSIXlt(pop[,tob], origin="1970-01-01"))

      id_last_year <- which(yob == year(time) - 1) # born last year
      union_ages <- pop[id_last_year, unionAge]
      mau_cohort <- c(mau_cohort, mean(union_ages, na.rm = T))
      
      # Number of births by age to compute ASFR
      birthsAge <- tabulate(pop[age %in% minAge:maxAge & birthYear > 0, ageBirth])
      birthsAge <- birthsAge[minAge:maxAge]
      birthsAge[which(is.na(birthsAge))] <- 0
      # ASFR
      asfr  <-  birthsAge/womenAge # Age-specific Fertility Rates
      asfrt  <-  rbind(asfrt, data.frame(t(birthsAge/womenAge))) # ASFR
      
      # Unwanted Births
      wBirthsAge <- tabulate(pop[age %in% minAge:maxAge & kidsLeft >= 0 & birthYear > 0, ageBirth])
      wBirthsAge <- wBirthsAge[minAge:maxAge]
      wBirthsAge[which(is.na(wBirthsAge))] <- 0
      # Unplanned Births
      pBirthsAge <- tabulate(pop[age %in% minAge:maxAge & birthYear == 1, ageBirth])
      pBirthsAge <- pBirthsAge[minAge:maxAge]
      pBirthsAge[which(is.na(pBirthsAge))] <- 0
      # Births by type
      ppBirths <- rbind(ppBirths, c(sum(birthsAge), sum(pBirthsAge), sum(wBirthsAge))) 
      
      if(!cluster){
        if(optim_run){
          source("../run/monitor.R", local = T)
        }else{source("monitor.R", local = T)}
      }

      # Reset indicators
      if (year(time) >= c_d) {
          mau <- mau_cohort[length(mau_cohort)]
      }else{
        mau <- mau_trend[year(time) - (iniYear-1)]
      }
      pop[,birthYear := 0]
      pop[, unionYear := 0]
      year <-year + 1
    }
    
  }
  
  # Paramerters
  if(optim_run){
    params <- list(fix_par_ls, c(sd_lnrm, lambda, th_edu, w_pnty, modName, un_year, un_max , uk))  
  }else{
    params <- list(free_par_ls, fix_par_ls)
  }
  if(cluster){
    out <- list(iniYear = iniYear, endYear = endYear, asfrt = asfrt,
                dKids = dKids, dKids_all = dKids_all, ppBirths = ppBirths)  
  }else{
    out <- list(iniYear = iniYear, endYear = endYear, cohort = cohort, cohort1 = cohort1, cohort2 = cohort2, cohort3 = cohort3,
                tfr = tfr, asfrt = asfrt, dKids = dKids, dKids_all = dKids_all, meanAgeBirth = meanAgeBirth, meanAgeBirth1 = meanAgeBirth1,
                meanAgeBirth2 = meanAgeBirth2, meanAgeUnion = meanAgeUnion, childless = childless, unionless = unionless,
                ppBirths = ppBirths, meanIntentB = meanIntentB, failed = failed, totPop = totPop, gapKids = gapKids,
                gapKids3 = gapKids3, gapEdu1 = gapEdu1, gapEdu2 = gapEdu2, gapEdu3 = gapEdu3,
                cohortEduLow = cohortEduLow, cohortEduMed = cohortEduMed, cohortEduHigh = cohortEduHigh, exposure = exposure,
                cohortBirths = cohortBirths, modName = modName, params = params, mean_age_union = mean_age_union, aux_mau = aux_mau, 
                aux_myedu = aux_myedu, aux_myedu_2 = aux_myedu_2, mau_cohort = mau_cohort, one_child = one_child,
                two_children = two_children, three_plus_children = three_plus_children, prop_w_trend = prop_w_trend)
  }
  
  return(out)
  
} 
