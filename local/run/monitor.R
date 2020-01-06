############### MONITOR ###################################################
prop_w_trend <- rbind(prop_w_trend, prop_w)

# Cohort Fertility - Method 1.
cId <- which(yob==(year(time)-1)-50) # Turned 51 in year t.
eduId1 <- which(pop[,edu == 1])
eduId2 <- which(pop[,edu == 2])
eduId3 <- which(pop[,edu == 3])

if(!is.na(yob[cId[1]]) & yob[cId[1]] >= iniYear){
  
  cohort[!is.na(cohort[,1]) & cohort[,1] == paste(yob[cId[1]]),2] <- pop[cId, mean(kids, na.rm = T)] # Mean number of kids cohort
  cohort1[!is.na(cohort1[,1]) & cohort1[,1] == paste(yob[cId[1]]),2] <- pop[intersect(eduId1,cId), mean(kids, na.rm = T)]
  cohort2[!is.na(cohort1[,1]) & cohort2[,1] == paste(yob[cId[1]]),2] <- pop[intersect(eduId2,cId), mean(kids, na.rm = T)] 
  cohort3[!is.na(cohort1[,1]) & cohort3[,1] == paste(yob[cId[1]]),2] <- pop[intersect(eduId3,cId), mean(kids, na.rm = T)]
}

if(!is.na(yob[cId[1]]) & yob[cId[1]] >= 1900){
  childless[childless[,1] == paste(yob[cId[1]]),2] <- pop[cId, prop.table(table(kids))[1]]
  one_child[one_child[,1] == paste(yob[cId[1]]),2] <- pop[cId, prop.table(table(kids))[2]]
  two_children[two_children[,1] == paste(yob[cId[1]]),2] <- pop[cId, prop.table(table(kids))[3]]
  three_plus_children[three_plus_children[,1] == paste(yob[cId[1]]),2] <- sum(pop[cId, prop.table(table(kids))[4]],
                                                                                pop[cId, prop.table(table(kids))[5]],
                                                                                      pop[cId, prop.table(table(kids))[6]],
                                                                                            pop[cId, prop.table(table(kids))[7]], na.rm = T)
  unionless[unionless[,1] == paste(yob[cId[1]]),2] <- pop[cId, prop.table(table(union))[1]]
}

mean_age_union <- c(mean_age_union, mean(pop[age %in% minAge:maxAge & unionYear == 1, unionAge])) # period
unionsAge <- tabulate(pop[age %in% minAge:maxAge & unionYear == 1, unionAge])
unionsAge <- unionsAge[minAge:maxAge]
unionsAge[which(is.na(unionsAge))] <- 0

birthsAge1 <- tabulate(pop[age %in% minAge:maxAge & birthYear > 0 & kids == 1, ageBirth])
birthsAge1 <- birthsAge1[minAge:maxAge]
birthsAge1[which(is.na(birthsAge1))] <- 0

birthsAge2 <- tabulate(pop[age %in% minAge:maxAge & birthYear > 0 & kids == 2, ageBirth])
birthsAge2 <- birthsAge2[minAge:maxAge]
birthsAge2[which(is.na(birthsAge2))] <- 0

icf <- sum(asfr) # TFR
if (is.na(icf)){icf <- 1}
tfr <- c(tfr, icf)

asfr1  <-  birthsAge1/womenAge # Age-specific Fertility Rates (1st)
asfr2  <-  birthsAge2/womenAge # Age-specific Fertility Rates (2nd)
icf1 <- sum(asfr1) # TFR 1 
icf2 <- sum(asfr2) # TFR 2
asfrAge  <- asfr %*% age2 # Mean Age
meanAgeBirth <- c(meanAgeBirth, asfrAge / icf)
asfrAge1  <- asfr1 %*% age2 # Mean Age 1st
meanAgeBirth1 <- c(meanAgeBirth1, asfrAge1 / icf1)
asfrAge2  <- asfr2 %*% age2 # Mean Age 2st
meanAgeBirth2 <- c(meanAgeBirth2,asfrAge2 / icf2)

asnr <- unionsAge/womenAge
icn <- sum(asnr)
if (is.na(icn)){icn <- 1}
asnrAge  <- asnr %*% age2 # Mean Age
meanAgeUnion <- c(meanAgeUnion, asnrAge / icn)

meanIntentB <- c(meanIntentB, pop[age %in% 14:51, mean(intentBirth, na.rm = T)]) # mean intention

failed <- c(failed, pop[age %in% 45:51, mean(failedAttempts, na.rm = T)])

totPop <- c(totPop, pop[,.N]) # Total Population

gapKids <- c(gapKids, pop[age %in% 48:51, mean(kidsLeft, na.rm = T)]) # Gap Kids
gapAux <- pop[age %in% 48:51 & !is.na(kidsLeft), kidsLeft]
gapAux <- ifelse(gapAux < 0, 0, gapAux)
gapKids3 <- c(gapKids3, mean(gapAux, na.rm = T))
gapEdu1 <- c(gapEdu1,pop[age %in% 48:51 & edu == 1 & !is.na(kidsLeft), mean(kidsLeft, na.rm = T)])
gapEdu2 <- c(gapEdu2,pop[age %in% 48:51 & edu == 2 & !is.na(kidsLeft), mean(kidsLeft, na.rm = T)])
gapEdu3 <- c(gapEdu3,pop[age %in% 48:51 & edu == 3 & !is.na(kidsLeft), mean(kidsLeft, na.rm = T)])

cohortEduLow <- c(cohortEduLow, pop[age == 0, prop.table(tabulate(edu))[1]]) # Education of cohorts
cohortEduMed <- c(cohortEduMed, pop[age == 0, prop.table(tabulate(edu))[2]]) 
cohortEduHigh <- c(cohortEduHigh, pop[age == 0, prop.table(tabulate(edu))[3]])

eCount[[year(time)-(year(iniTime)-1)]] <- eventCount

aux_mau <- c(aux_mau, mau)
aux_myedu <- c(aux_myedu, mean(pop[age %in% minAge:maxAge & unionYear == 1, yearsEdu], na.rm = T))
aux_myedu_2 <- c(aux_myedu_2, mean(pop[age %in% minAge:maxAge, yearsEdu], na.rm = T))

eventCount <- setNames(as.list(rep(0, length(eventNames))), eventNames)


