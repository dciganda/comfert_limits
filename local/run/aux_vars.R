############### AUX VARS ##################################################
cohort <- cohort1 <- cohort2 <- cohort3 <- data.frame(year = iniY:(endY-51), val = -1)
childless <- unionless <- one_child <- two_children <- three_plus_children <- data.frame(year = 1900:(endY-51), val = -1)
tfr <- meanAgeBirth <- meanAgeBirth1 <- meanAgeBirth2 <- meanAgeUnion <- dKids <- ppBirths <- vector()
meanIntentB  <- totPop <- meanNrDate <- gapKids <- gapKids3 <- gapEdu1 <- gapEdu2 <- gapEdu3 <- vector()
cohortEduLow <- cohortEduMed <- cohortEduHigh <- failed <- mean_age_union <-aux_mau <- aux_myedu <- aux_myedu_2 <- vector()
eCount <- exposure <-  list()
cohortExpo <- matrix(nrow = length(seq(14, 50,1)), ncol = length(seq(iniY, (endY-52), 1)))
colnames(cohortExpo) <- seq(iniY, (endY-52),1) 
row.names(cohortExpo) <- seq(14, 50,1) 
# Births Matrix for cohorts 1925 to 1965
cohortBirths <- matrix(data = 0, nrow = length(seq(14, 50,1)), ncol = length(seq(iniY-1, (endY-51), 1)))
colnames(cohortBirths) <- seq(iniY-1, (endY-51), 1) 
row.names(cohortBirths) <- seq(14, 50,1) 
