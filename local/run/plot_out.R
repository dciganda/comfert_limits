plot_out <- function(sim, cntry, iniY) {
  
  # ASFR ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sim
  asfr <- list()
  
    for(i in sim){
      out <- readRDS(i)
      asfr <- c(asfr, out["asfrt"])
      rm(out)
    }
  
  asfr <- lapply(asfr,
                 function(x){colnames(x) <- paste0("age", 14:50);
                 x <- x[-1,]
                 rownames(x) <- iniY:(endY-1);
                 return(as.matrix(x))})
  
  # Obs
  obs_asfr <- get_obs_asfr(cntry)
  
  asfr <- lapply(asfr,
                 function(x){x[rownames(x) %in% rownames(obs_asfr),
                               colnames(x) %in% colnames(obs_asfr)]})
  
  mean_asfr <-  Reduce("+", asfr) / length(asfr)
  
  # color pallette
  tm <- 1
  npoints <- nrow(obs_asfr)
  colfunc <- colorRampPalette(c("orange", "red"))
  redCol <- colfunc(npoints)
  dest <- c(1, 12, 27, nrow(obs_asfr)) 
  
  # PLOT
  # Obs

  plot(obs_asfr[1,], type="l", col=redCol[1], lwd=4, cex.lab=tm, cex.axis=tm,
       ylab="Age Specific Fertility Rates", xlab="Age", xaxt='n',
       frame.plot=F, ylim=c(0, 0.2),
       main = paste("Observed ASFR", cntry))
  for(i in 2:nrow(obs_asfr)){
    if (i %in% dest[c(2:4)]) {
      lines(obs_asfr[i,], col=redCol[i - 1], lwd=4)  
    } else lines(obs_asfr[i,], col=redCol[i - 1])  
  }
  axis(side=1, at=seq(1, 46, 5), label= seq(15, 60, 5), cex.axis=tm)
  legend(22, .18, legend = rownames(obs_asfr)[dest], 
         col=c(redCol[1], redCol[12], redCol[27], redCol[npoints]),
         lwd=4, bty="n", cex=1.4, y.intersp=1.5)

  # Sim
  nlines <- nrow(mean_asfr)-1

  # PLOT
  plot(mean_asfr[1,], type="l", col=redCol[1], lwd=4, cex.lab=tm, cex.axis=tm,
       ylab="Age Specific Fertility Rates", xlab="Age", xaxt='n', frame.plot=F,
       ylim = c(0,0.2),
       main = paste("Simulated ASFR", cntry))
  for(i in 2:nrow(mean_asfr)){
    if (i %in% c(12, 27, nrow(mean_asfr))) {
      lines(mean_asfr[i,], col=redCol[i - 1], lwd=4)  
    } else lines(mean_asfr[i,], col=redCol[i - 1])  
  }
  axis(side=1, at=seq(1, 46, 5), label= seq(15, 60, 5), cex.axis=tm)
  legend(22, .20, legend=c(1975, 1986, 2001, 2016), 
         col=c(redCol[1], redCol[11], redCol[26], redCol[npoints]),
         lwd=4, bty="n", cex=1.4, y.intersp=1.5)

  # TFR ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Years for the Plot
  y1 <- 1960
  y2 <- endY
  # Obs
  obsTfr <- read.table(paste0("../run/data/",cntry,"/out/tfr_hfd.txt"), skip = 2, header = T,
                       stringsAsFactors = F)
  
  if(cntry != "FR"){
  obs_ta_tfr <- read.table(paste0("../run/data/",cntry,"/out/ta_tfr_hfd.txt"), skip = 2, header = T,
                         stringsAsFactors = F)
  }
  
  # Sim
  tfr <- list()
  
  for(i in sim){
    out <- readRDS(i)
    tfr <- c(tfr, out["tfr"])
    rm(out)
  }
  
  mean_tfr <-  Reduce("+", tfr) / length(tfr)
  
  #pdf("../../Latex/sim_workshop_uni_rostock/Plots/ovs_tfr.pdf",width=6, height=6) 
  par(mar= c(5, 4, 1, 2))
  plot(seq(y1,max(obsTfr$Year),1),c(obsTfr[obsTfr >= y1,2]),
       xlab="Year", ylim=c(0.8,4),
       cex.main=1, cex.axis=tm, cex.lab=tm ,type="l", pch=20, 
       ylab="TFR", xaxt='n', frame.plot=F,
       xlim = c(y1, endY))
  lines(seq(y1,endY-1,1), mean_tfr[-c(1:c(y1-iniY))], cex=1, pch=20, col = "red", lwd = 4)
  for (i in 1:length(tfr)){
    lines(seq(y1,endY-1,1), tfr[[i]][-c(1:c(y1-iniY))], lty = 3, col = "red")
  }
  if(cntry != "FR"){
    lines(obs_ta_tfr$Year, obs_ta_tfr[,2], lty = 2, lwd =3)
  }
  axis(side=1, at=seq(y1, y2+10, 5), label= seq(y1, y2 + 10, 5), cex.axis=tm)
  legend("topright", c("Observed","Tempo Adjusted", "Simulated"), pch=20, pt.cex = 2, bty="n", cex=tm,
         col = c("black","black","red"), lty = c(1,2,3))

  # CCF ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sim
  ccf <- list()
  
  for(i in sim){
    out <- readRDS(i)
    ccf <- c(ccf, out["cohort"])
    rm(out)
  }
  
  mean_ccf <-  Reduce("+", ccf) / length(ccf)
  
  # Obs
    obs_ccf <- read.table(paste0("../run/data/",cntry,"/out/ccf_hfd.txt"), skip = 2, header = T,
                         stringsAsFactors = F)
    obs_ccf$CCF<- ifelse(obs_ccf$CCF==".", NA, obs_ccf$CCF)
    obs_ccf$CCF <- as.numeric(obs_ccf$CCF)
  if (cntry == "FR"){
    obs_ccf <- rbind(setNames(as.data.frame(matrix(c(iniY:1930,
                                                     rep(NA, 1931-iniY)),
                                                   ncol = 2)),names(obs_ccf[,1:2])), obs_ccf[,1:2])
  }

  #pdf("../../Latex/sim_workshop_uni_rostock/Plots/ovs_ccf.pdf",width=6, height=6) 
  plot(obs_ccf[obs_ccf[,1] %in% iniY:(endY -51),1], obs_ccf[obs_ccf[,1] %in% iniY:(endY -51),2],
       xlab="Year", ylim=c(1.5,3.5),
       cex.main=1,cex.axis=tm, cex.lab=tm, type="l", cex=1, pch=20,
       ylab="Cohort Completed Fertility", xaxt='n',
       frame.plot = F)
  lines(mean_ccf$year,mean_ccf$val, cex=1, pch=20, col = "red", lwd = 4) 
  for (i in 1:length(ccf)){
    lines(seq(iniY,(endY -51),1), ccf[[i]]$val, lty = 3, col = "red")
  }
  axis(side = 1, at = seq(iniY, y2+10, 5), label= seq(iniY, y2 + 10, 5), cex.axis=tm)
  legend("topright", c("Observed", "Simulated"),pch=20,
         pt.cex = 2, bty="n", cex=tm, col = c("black","red"))
  
  
  # MAB ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Obs
  mab0 <- read.table(paste0("../run/data/",cntry,"/out/mab_hfd.txt"), skip = 2, header = T,
                     stringsAsFactors = F)
  obs_mab <- mab0[mab0>=1960,]
  
  if(cntry %in% c("ES", "IR")){
  mabp <- read.table(paste0("../run/data/",cntry,"/out/mab_parity_hfd.txt"), skip = 2, header = T,
                     stringsAsFactors = F)
  obs_mab1 <- mabp[,c("MAB1","Year")]
  obs_mab2 <- mabp[,c("MAB2","Year")]
  }
  
  # Sim
  mab <- list()
  mab1 <- list()
  mab2 <- list()
  
  for(i in sim){
    out <- readRDS(i)
    mab <- c(mab, out["meanAgeBirth"])
    rm(out)
  }
  
  for(i in sim){
    out <- readRDS(i)
    mab1 <- c(mab1, out["meanAgeBirth1"])
    rm(out)
  }
  
  for(i in sim){
    out <- readRDS(i)
    mab2 <- c(mab2, out["meanAgeBirth2"])
    rm(out)
  }
  
  
  mean_mab <-  Reduce("+", mab) / length(mab)
  mean_mab1 <-  Reduce("+", mab1) / length(mab1)
  mean_mab2 <-  Reduce("+", mab2) / length(mab2)
  
  # Plot
  par(mar= c(5, 4, 1, 2))
  plot(obs_mab[,1], obs_mab[,2],
       xlab="Year", ylim=c(25,35),
       cex.main=1,cex.axis=tm, cex.lab=tm, type="p",
       lwd = 2, lty = 1, ylab = "Mean Age at Birth", xaxt = 'n', frame.plot = F)
  if(cntry != "FR"){
  lines(obs_mab1[,2], obs_mab1[,1])
  lines(obs_mab1[,2], obs_mab2[,1])
  }
  points(seq(1960, endY-1,1), mean_mab[(1960-iniY+1):length(mean_mab)], lty = 1, lwd = 2, col = "red")  
  for (i in 1:length(mab)){
    lines(seq(1960, endY-1,1), mab[[i]][(1960-iniY+1):length(mean_mab)], lty = 3, col = "red")
  }
  points(seq(1960, endY-1,1), mean_mab1[(1960-iniY+1):length(mean_mab1)], lty = 1, lwd = 2, col = "orange")  
  for (i in 1:length(mab1)){
    lines(seq(1960, endY-1,1), mab1[[i]][(1960-iniY+1):length(mean_mab1)], lty = 3, col = "orange")
  }
  points(seq(1960, endY-1,1), mean_mab2[(1960-iniY+1):length(mean_mab2)], lty = 1, lwd = 2, col = "violet")
  for (i in 1:length(mab2)){
    lines(seq(1960, endY-1,1), mab2[[i]][(1960-iniY+1):length(mean_mab2)], lty = 3, col = "violet")
  }
  axis(side = 1, at = seq(1955, y2+10, 5), label= seq(1955, y2 + 10, 5), cex.axis = tm)
  legend(1986,28, c("Observed", "Simulated"),
         lty = c(1,1),lwd=2, bty="n", cex=tm, col = c("black","red"))
  
  # MAB ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Obs
  mab0 <- read.table(paste0("../run/data/",cntry,"/out/mab_hfd.txt"), skip = 2, header = T,
                     stringsAsFactors = F)
  obs_mab <- mab0[mab0>=iniY,1:2]
  
  # Sim
  mab <- list()
  
  for(i in sim){
    out <- readRDS(i)
    mab <- c(mab, out["meanAgeBirth"])
    rm(out)
  }
  
  mean_mab <-  Reduce("+", mab) / length(mab)

    
  # Plot
  par(mar= c(5, 4, 1, 2))
  plot(obs_mab[,1], obs_mab[,2],
       xlab="Year", ylim = c(15,35), xlim = c(iniY, endY),
       cex.main=1,cex.axis=tm, cex.lab=tm, type="p",
       lwd = 2, lty = 1, ylab = "Mean Age at Birth", xaxt = 'n', frame.plot = F)
  points(seq(iniY, endY-1,1), mean_mab, lty = 1, lwd = 2, col = "red")  
  for (i in 1:length(mab)){
    lines(seq(iniY,endY-1,1), mab[[i]], lty = 3, col = "red")
  }
  axis(side = 1, at = seq(iniY, endY, 5), label= seq(iniY, endY, 5), cex.axis = tm)
  legend(1986,28, c("Observed", "Simulated"),
         lty = c(1,1),lwd=2, bty="n", cex=tm, col = c("black","red"))
  
  
  # MA UNION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # obs 
  #obs_mau <- read.csv(paste0("../run/data/",cntry,"/out/mean_age_marriage_period.csv"), header = T,
  #                   stringsAsFactors = F)
  # Sim
  mau <- list()
  
  for(i in sim){
    out <- readRDS(i)
    mau <- c(mau, out["meanAgeUnion"])
    rm(out)
  }
  
  mean_mau <-  Reduce("+", mau) / length(mau)
  
  maul <- list()
  
  for(i in sim){
    out <- readRDS(i)
    maul <- c(maul, out["mean_age_union"])
    rm(out)
  }
  
  mean_maul <-  Reduce("+", maul) / length(maul)
  
  aux_mau <- list()
  
  for(i in sim){
    out <- readRDS(i)
    aux_mau <- c(aux_mau, out["aux_mau"])
    rm(out)
  }
  
  mean_aux_mau <-  Reduce("+", aux_mau) / length(aux_mau)
  
  
  aux_mau_cohort <- list()
  
  for(i in sim){
    out <- readRDS(i)
    aux_mau_cohort <- c(aux_mau_cohort, out["mau_cohort"])
    rm(out)
  }
  
  mean_aux_mau_cohort <-  Reduce("+", aux_mau_cohort) / length(aux_mau_cohort)
  
  
  # Plot
  lines(seq(iniY, endY-1, 1), mean_mau,lwd = 2, lty = 1)
  points(seq(iniY, endY-1,1), mean_mau, lty = 1, lwd = 2, col = "red") 
  lines(seq(iniY, endY-1, 1), mean_maul, col = "violet")
  lines(seq(iniY, endY-1, 1), mean_aux_mau)
  lines(seq(iniY, endY-1, 1), mean_aux_mau_cohort, col = "orange")
  
  

# UNPLANNED ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Obs - NO DATA FOR SPAIN
  obs_unw_0 <- read.table("../run/data/FR/out/unwanted.csv",sep = ",", skip = 3, header = T,
                          stringsAsFactors = F)
  aux_frame_1 <- as.data.frame(cbind(seq(iniY, 2016,1), NA))
  names(aux_frame_1) <- names(obs_unw_0)
  obs_unw <- merge(obs_unw_0, aux_frame_1, by = "year", all.y = T)
  obs_unw[,2] <- obs_unw[,2]/100
  obs_unw <- obs_unw[1:2] 
  
  obs_unp_0 <- read.table("../run/data/FR/out/unplanned.csv",sep = ",", skip = 3, header = T,
                          stringsAsFactors = F)
  obs_unp <- merge(obs_unp_0[,c(1:2)], aux_frame_1, by = "year", all.y = T)
  obs_unp <- obs_unp[1:2] 
  
  # Sim
  unp <- list()
  
  for(i in sim){
    out <- readRDS(i)
    unp <- c(unp, out["ppBirths"])
    rm(out)
  }
  
  prop_unp <- lapply(unp, function(x) (x[,1] - x[,2]) / x[,1])
  prop_unw <- lapply(unp, function(x) (x[,1] - x[,3]) / x[,1])
  sim_unp <- do.call(cbind, prop_unp)
  mean_sim_unp <- rowMeans(sim_unp, na.rm = T)
  sim_unw <- do.call(cbind, prop_unw)
  mean_sim_unw <- rowMeans(sim_unw, na.rm = T)
  

  # Plot
  par(mar= c(5, 4, 1, 2))
  plot(obs_unp[,1], obs_unp[,2],
       xlab="Year", ylim=c(0,.60), xlim = c(iniY, endY-1),
       cex.main=1,cex.axis=tm, cex.lab=tm, type="l",
       lwd = 2, lty = 1, ylab = "% Unplanned Births", xaxt = 'n', frame.plot = F)
  lines(seq(iniY, endY-1,1), mean_sim_unp[c((iniY-iniY+1):length(mean_sim_unp))], lty = 1, lwd = 2, col = "red")  
  for (i in 1:ncol(sim_unp)){
    lines(seq(iniY,endY-1,1), sim_unp[,i][c((iniY-iniY+1):nrow(sim_unp))], lty = 3, col = "red")
  }
  axis(side = 1, at = seq(iniY, endY-1+10, 10), label= seq(iniY, endY-1 + 10, 10), cex.axis = tm)
  legend(1986,.18, c("Observed FRANCE!", "Simulated"),
         lty = c(1,1),lwd=2, bty="n", cex=tm, col = c("black","red"))
  
  plot(obs_unp[,1], obs_unp[,2],
       xlab="Year", ylim=c(0,.60), xlim = c(1960, endY-1),
       cex.main=1,cex.axis=tm, cex.lab=tm, type="l",
       lwd = 2, lty = 1, ylab = "% Unplanned Births", xaxt = 'n', frame.plot = F)
  lines(seq(iniY, endY-1,1), mean_sim_unp, lty = 1, lwd = 2, col = "red")  
  for (i in 1:ncol(sim_unp)){
    lines(seq(iniY,endY-1,1), sim_unp[,i], lty = 3, col = "red")
  }
  axis(side = 1, at = seq(iniY, endY-1+10, 5), label= seq(iniY, endY-1 + 10, 5), cex.axis = tm)
  legend(1986,.18, c("Observed FRANCE!", "Simulated"),
         lty = c(1,1),lwd=2, bty="n", cex=tm, col = c("black","red"))
  
  # Plot
  par(mar= c(5, 4, 1, 2))
  plot(obs_unw[,1], obs_unw[,2],
       xlab="Year", ylim=c(0,.20),
       cex.main=1,cex.axis=tm, cex.lab=tm, type="l",
       lwd = 2, lty = 1, ylab = "% Unplanned Births", xaxt = 'n', frame.plot = F)
  lines(seq(1960, endY-1,1), mean_sim_unw[c((1960-iniY+1):length(mean_sim_unw))], lty = 1, lwd = 2, col = "red")  
  for (i in 1:ncol(sim_unw)){
    lines(seq(1960,endY-1,1), sim_unw[,i][c((1960-iniY+1):nrow(sim_unw))], lty = 3, col = "red")
  }
  axis(side = 1, at = seq(1955, endY-1+10, 5), label= seq(1955, endY-1 + 10, 5), cex.axis = tm)
  legend(1986,.18, c("Observed FRANCE!", "Simulated"),
         lty = c(1,1),lwd=2, bty="n", cex=tm, col = c("black","red"))
  
  plot(obs_unw[,1], obs_unw[,2],
       xlab="Year", ylim=c(0,.60),
       cex.main=1,cex.axis=tm, cex.lab=tm, type="l",
       lwd = 2, lty = 1, ylab = "% Unpwanted Births", xaxt = 'n', frame.plot = F)
  lines(seq(iniY, endY-1,1), mean_sim_unw, lty = 1, lwd = 2, col = "red")  
  for (i in 1:ncol(sim_unw)){
    lines(seq(iniY,endY-1,1), sim_unw[,i], lty = 3, col = "red")
  }
  axis(side = 1, at = seq(iniY, endY-1+10, 5), label= seq(iniY, endY-1 + 10, 5), cex.axis = tm)
  legend(1986,.18, c("Observed FRANCE!", "Simulated"),
         lty = c(1,1),lwd=2, bty="n", cex=tm, col = c("black","red"))
  
  
  
  # DESIRED ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  if(cntry == "FR"){
  obsD1 <- read.table(paste0("../run/data/",country,"/out/ideal_sit.csv"), 
                             sep = ",", skip = 0, header = T)
  obsD1 <- obsD1[!is.na(obsD1[,5]),c(2,5)]
  }else{
  obsD1 <- read.table(paste0("../run/data/",country,"/out/ideal_sit.csv"), 
                      sep = "", skip = 0, header = T)
    
  }
  #obsD1 <- obsD1[!is.na(obsD1[,2]),]

  
  
  if(cntry == "FR"){
  obsD <- read.table(paste0("../run/data/",cntry,"/out/ideal.csv"), sep = ",", skip = 0, header = T)
  obsD <- obsD[!is.na(obsD[,5]),c(2,5)]
  }else{obsD <- read.table(paste0("../run/data/",cntry,"/out/ideal.csv"), sep = ",", skip = 2, header = F)
  }
  
  # Sim
  dKids <- list()
  
  for(i in sim){
    out <- readRDS(i)
    dKids <- c(dKids, out["dKids_all"])
    rm(out)
  }
  
  prop_w <- list()
  
  for(i in sim){
    out <- readRDS(i)
    prop_w <- c(prop_w, out["prop_w_trend"])
    rm(out)
  }
  
  mean_dKids <-  Reduce("+", dKids) / length(dKids)
  mean_prop_w <-  Reduce("+", prop_w) / length(prop_w)
  mean_prop_w <- mean_prop_w[-1,]
  mean_prop_w <- cbind(mean_prop_w, seq(iniY,endY-1,1))
  #dKids1 <- smooth.spline(dKids,spar = 0.8)
  #dKids1 <- predict(dKids1)
  
  par(mfrow=c(1, 1))
  plot(seq(iniY,endY-1,1),mean_dKids, ylim=c(1,4),
       cex.main=1, cex.axis=tm, 
       cex.lab=tm, type="l", lty=1,
       lwd=2, cex.lab=tm,
       ylab="Desired Family Size", xlab="Year", xaxt='n', frame.plot=F)
  for (i in 1:length(dKids)){
    lines(seq(iniY,endY-1,1), dKids[[i]], lty = 3, col = "red")
  }
  lines(mean_prop_w[,3], mean_prop_w[,1], lwd= 2)
  lines(mean_prop_w[,3], mean_prop_w[,2], lwd= 2)
  
  lines(obsD[,1], obsD[,2], type = "l", lty = 3)
  points(obsD[,1], obsD[,2], ylim = c(1.8,2.7), lwd = 1)
  points(obsD1[,1], obsD1[,2], lwd = 4)
  abline(h = 2.5)
  
  axis(side=1, at=seq(iniY, endY-1, 10), label= seq(iniY, endY-1, 10), cex.axis=tm)
  legend(1990, 2.8,c("Simulated Ideal","Observed Ideal"),
         lty=c(1,NA),pch = c(NA, 1),lwd=2, bty="n", cex=tm, col = c("black","black"))
  
  


  # PARITY DISTRIBUTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Obs css
  css  <- read.table(paste0("../run/data/",cntry,"/out/childlessness.csv"), sep = ",", skip = 3, header = T)
  
  # Sim
  sim_css <- list()
  sim_one <- list()
  sim_two <- list()
  sim_three <- list()
  
  for(i in sim){
    out <- readRDS(i)
    sim_css <- c(sim_css, out["childless"])
    rm(out)
  }
  
  for(i in sim){
    out <- readRDS(i)
    sim_one <- c(sim_one, out["one_child"])
    rm(out)
  }
  for(i in sim){
    out <- readRDS(i)
    sim_two <- c(sim_two, out["two_children"])
    rm(out)
  }
  for(i in sim){
    out <- readRDS(i)
    sim_three <- c(sim_three, out["three_plus_children"])
    rm(out)
  }
  
  
  sim_uss <- list()
  
  for(i in sim){
    out <- readRDS(i)
    sim_uss <- c(sim_uss, out["unionless"])
    rm(out)
  }
  
  mean_sim_css <-  Reduce("+", sim_css) / length(sim_css)
  mean_sim_one <- Reduce("+", sim_one) / length(sim_one)
  mean_sim_two <- Reduce("+", sim_two) / length(sim_two)
  mean_sim_three <- Reduce("+", sim_three) / length(sim_three)
  
  mean_sim_uss <-  Reduce("+", sim_uss) / length(sim_uss)
  
  plot(seq(iniY,max(mean_sim_css[,1]),1), mean_sim_css[mean_sim_css$year>=iniY,2]*100,
     xlab="Year", ylim=c(0,60),
     cex.main=1, cex.axis=1, 
     cex.lab=1,type="l", col="red",
     xlim = c(min(css$year),(endY - 51)),
     lwd=2, lty=1,ylab="Childless")
  lines(css[,1],css[,2],col="Black", lwd = 2)
  lines(seq(iniY,max(mean_sim_one[,1]),1), mean_sim_one[mean_sim_one$year>=iniY,2]*100)
  lines(seq(iniY,max(mean_sim_two[,1]),1), mean_sim_two[mean_sim_two$year>=iniY,2]*100, col = "orange", lwd = 2)
  lines(seq(iniY,max(mean_sim_three[,1]),1), mean_sim_three[mean_sim_three$year>=iniY,2]*100)
  
  lines(seq(iniY,max(mean_sim_uss[,1]),1), mean_sim_uss[mean_sim_uss$year>=iniY,2]*100, col = "red", lwd=2, lty=2)
  legend(1945, 10,c("Obs % Childless at 50","Sim Childless","Sim Unionless", "Sim parity2"), col=c("Black","red", "red", "orange"),
       lty=c(1,1,2), bty="n", cex=1)


# GAP KIDS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sim
sim_gap <- list()
sim_gap3 <- list()

for(i in sim){
  out <- readRDS(i)
  sim_gap <- c(sim_gap, out["gapKids"])
  rm(out)
}

for(i in sim){
  out <- readRDS(i)
  sim_gap3 <- c(sim_gap3, out["gapKids3"])
  rm(out)
}

mean_sim_gap <-  Reduce("+", sim_gap) / length(sim_gap)
mean_sim_gap3 <-  Reduce("+", sim_gap3) / length(sim_gap3)

plot(seq(iniY,endY-1,1), mean_sim_gap,
     xlab="Year", ylim=c(-0.5,1),
     cex.main=1, cex.axis=1, 
     cex.lab=1,type="l", col="red",
     lwd=2, lty=1,ylab="Gap")
lines(seq(iniY,endY-1,1), mean_sim_gap3,col="Black", lwd = 2)
legend(1935, 0.5,c("Gap","Gap3"), col=c("red","black"),
       lty=c(1,1), bty="n", cex=1)
}

# ecount <- list()
# for(i in sim){
#   out <- readRDS(i)
#   ecount <- c(ecount, out["eCount"])
#   rm(out)
# }
# 
# 
# eventCount <- sapply(ecount, function(x) x)
# eventCount <- eventCount[-1,]  
# events <- sapply(apply(eventCount, 1, function(x) sapply(x, function(y) y)), function(x) apply(x, 1, function(y) Reduce("+", y) / ncol(x)))
# 


