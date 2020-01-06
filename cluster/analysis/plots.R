plot_gp_validation <- function(country, params, multi_obj_mse,
                               weights = c(0.25, 0.25, 0.25, 0.25), save = F){
  
  npop <- params[params$country ==  country, "npop"]
  N <- params[params$country ==  country, "N"]
  
  dir_o <- paste0("../results/", country, "/N_POP_", npop, "/N_", N)
  
  if(multi_obj_mse){
    results <- readRDS(paste0(paste0(dir_o,"/gp/",
                                     paste(weights, collapse = "_")),
                              "/gp_results_multi_mse.rds"))
    lims <- c(0.5,6)
  }else{
    results <- readRDS(paste0(paste0(dir_o,"/gp/"), "gp_results.rds"))
    if(country == "ES"){
    lims <- c(0,1.4)
    }else{
    lims <- c(0.3,0.8) 
    }
  }
  
  
  dat <- results$gp_data
  
  par(mfrow=c(1, 1))
  par(bg=NA)
  p <- ggplot(dat,
              aes(x = mse,
                  y = pred_mse)) +
    geom_point(aes(col = purpose), size = 1) +
    theme_bw() +
    #ylim(lims) +
    #xlim(lims) +
    theme(legend.position = c(0.2,0.8),
          legend.title=element_blank())+
    theme(legend.text = element_text(size=25,
                                     margin = margin(l = -10, unit = "pt")),
          legend.key.height=unit(5,"point"),
          legend.key.size = unit(1.5, "cm"),
          legend.key = element_rect(colour = "transparent", fill = NA),
          legend.background=element_blank())+
    scale_colour_manual(values = c("darkgrey", "black"),
                        labels = c("training set","test set"))+
    guides(colour = guide_legend(override.aes = list(size = 5)))+
    ylab("Predicted MSE") +
    xlab("Observed MSE") +
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 20))+
    theme(plot.background = element_rect(fill = "transparent",colour = NA),
          panel.background = element_rect(fill = "transparent",colour = NA))
  if(save){
    pdf(paste0("../../../latex/pdr/plots/",country,
               "/gp_valid.pdf"), width=9, height=9) 
    print(p)
    dev.off()
  }
  p
}


plot_fit_asfr <- function(country, params, multi_obj_mse, weights, save = F){
  
  source("get_obs.R")
  source("get_sim.R") 
  
  iniY <- params[params$country ==  country, "iniY"]
  endY <- params[params$country ==  country, "endY"]
  foreY <- params[params$country ==  country, "foreY"]
  ysd <- params[params$country ==  country, "ysd"]
  npop <- params[params$country ==  country, "npop"]
  N <- params[params$country ==  country, "N"]
  
  # main dir
  dir_o <- paste0("../results/", country, "/N_POP_", npop, "/N_", N)
  # get data.frame of limits
  limits <- fread(paste0(dir_o, "/param_sample/limits.csv"))
  # get data.frame of sample points
  em_data <- fread(paste0(dir_o,"/param_sample/sample.csv"))
  # get observed fertility rates
  obs <- get_obs(country, ysd)
  # simulated rates
  sim <- get_sim(params, country, obs, estimated_params = T, weights = weights, multi_obj_mse = multi_obj_mse)
  
  if(class(obs) == "list"){
    obs_asfr <- obs$obs_asfr
  }else{
    obs_asfr <- obs
  }
  
  plot_years = seq(1960, (endY), 5)
  
  if(country == "IR"){
    plot_years[length(plot_years)] <- 2014 
  }

  
  plot_year_asfr <- function(year){
    
  dat_obs <- as.data.frame(cbind(as.numeric(obs_asfr[as.character(year),]),
                                 as.numeric(gsub("age", "", colnames(obs_asfr)))))
  
  mean_sim_asfr = Reduce("+", sim[[1]]) / length(sim[[1]])
  dat_sim <- as.data.frame(cbind(as.numeric(mean_sim_asfr[as.character(year),]),
                                 as.numeric(gsub("age", "", colnames(mean_sim_asfr)))))
  
  
  dat <- cbind(rbind(dat_obs, dat_sim), c(rep("obs", nrow(dat_obs)), rep("sim", nrow(dat_sim))))
  names(dat) <- c("val", "age", "type")

  # max value for ylim
  max_obs <- obs_asfr[which(obs_asfr == max(obs_asfr), arr.ind = T)]
  max_sim <- mean_sim_asfr[which(mean_sim_asfr == max(mean_sim_asfr), arr.ind = T)]
  max_b <- max(max_obs, max_sim)  
  
  par(mfrow=c(1, 1))
  par(bg=NA)
  p <- ggplot(dat,
              aes(x = age,
                  y = val)) +
    geom_point(aes(col = type), size = 3) +
    theme_bw() +
    theme(legend.position = c(0.7,0.8),
          legend.title=element_blank())+
    theme(legend.text = element_text(size=25,
                                     margin = margin(l = -10,
                                                     unit = "pt")),
          legend.key.height=unit(5,"point"),
          legend.key.size = unit(1.5, "cm"),
          legend.key = element_rect(colour = "transparent", fill = NA),
          legend.background=element_blank())+
    scale_colour_manual(values = c("black", "grey50"),
                        labels = c("Observed","Simulated"))+
    guides(linetype = guide_legend(reverse = TRUE))+
    ylim(0, (max_b + 0.05))+
    theme(axis.text = element_text(size = 20),
          axis.title = element_blank())+
    theme(plot.background = element_rect(fill = "transparent",colour = NA),
          panel.background = element_rect(fill = "transparent",colour = NA))
  if(year != 1960){
    p <- p + theme(legend.position = "none")
  }
  if(save){
  pdf(paste0("../../../latex/pdr/plots/",country,"/asfr_", year,".pdf"), width=5, height=5) 
  print(p)
  dev.off()
  }
  return(p)
  }
  
  lapply(plot_years, plot_year_asfr)
  
}

plot_fit <- function(country, params, weights, multi_obj_mse,
                     save = F, ysd = 1960, unplanned = F, unwanted = F, desired = F){
  
  source("get_obs.R")
  source("get_sim.R") 
  
  npop <- params[params$country ==  country, "npop"]
  ysd <- params[params$country ==  country, "ysd"]
  
  # get observed fertility rates
  obs <- get_obs(country, ysd)
  # simulated rates
  sim <- get_sim(params, country, obs = obs, weights = weights,
                 estimated_params = T, all_sim = T, sample = F,
                 multi_obj_mse = multi_obj_mse,
                 unplanned = unplanned,unwanted = unwanted, desired = desired)
  

  shape_plot <- function(x, npop, save, ylims, p_names){
  obs_d <- obs[[x]]
  sim_d <- sim[[x]] 
  ylims <- ylims[[x-1]]

    
  dat_obs <- as.data.frame(obs_d)
  dat_obs$year <- rownames(obs_d)
  
  mean_dat_sim <- as.data.frame(Reduce("+", sim_d) / length(sim_d))
  dat_sim <- as.data.frame(do.call("cbind", sim_d))
  dat_sim$year <- as.numeric(rownames(dat_sim))
  dat_sim <- cbind(dat_sim, mean_dat_sim)
  names(dat_sim) <- c(rep("Simulated", npop),"year", "Mean Simulations")
  
  dat_aux_0 <- merge(dat_obs, dat_sim, by = "year", all = T)
  dat_aux_1 <- dat_aux_0[dat_aux_0$year %in% c(1960:2011),]
  names(dat_aux_1)[2] <- c("Observed")
  
  #### REMOVE WHEN SIMS WITH BIGGER N AVAILABLE
  #sm_sims <- sapply(dat_aux_1[3:ncol(dat_aux_1)], smooth.spline, spar = 0.2)
  #sm_sims_dat <- as.data.frame(do.call("cbind", sm_sims[2,]))
  #dat_aux_1[3:ncol(dat_aux_1)] <- sm_sims_dat
  #############################################
  
  dat_long <- reshape(dat_aux_1, dir = "long", idvar = "year",
                      names(dat_aux_1)[c(2:ncol(dat_aux_1))], v.names = "vals",
                      timevar = "type", times = names(dat_aux_1)[c(2:ncol(dat_aux_1))])
  
  dat_long$mean <- ifelse(dat_long$type %in% c("Mean Simulations", "Observed"), 1, 0)
  dat_long$mean <- ifelse(dat_long$type == "Mean Simulations", 2, dat_long$mean )
  dat_long$year <- as.numeric(dat_long$year)
  
  lbs <- c("Simulated", "Observed", "Mean Simulations")
  
  par(mfrow=c(1, 1))
  par(bg=NA)
  p <- ggplot(dat_long,
              aes(x = year,
                  y = vals,
                  group = as.factor(type),
                  size = as.factor(mean),
                  linetype = as.factor(mean),
                  shape = as.factor(mean),
                  colour = as.factor(mean))) +
    geom_line() +
    geom_point() +
    ylim(ylims) +
    scale_colour_manual(values = c("grey65", "grey0", "grey65"),
                        labels = lbs)+
    scale_shape_manual( values = c(NA,16,16), labels = lbs)+
    scale_linetype_manual( values = c(3,NA,NA), labels = lbs)+
    scale_size_manual( values = c(0.5, 2.5, 2.5), labels = lbs)+
    guides(shape = guide_legend(override.aes = list(size=c(1.2, 5, 5))))+
    theme_bw() +
    theme(legend.position = c(0.7,0.8),
          legend.title=element_blank()) +
    theme(legend.text = element_text(size = 20,
                                     margin = margin(l = 1, unit = "pt")),
          legend.key.height=unit(5,"point"),
          legend.key.size = unit(1.2, "cm"),
          legend.key = element_rect(colour = "transparent", fill = NA),
          legend.background=element_blank())+
    theme(axis.text = element_text(size = 20),
          axis.title = element_blank())+
    theme(plot.background = element_rect(fill = "transparent",colour = NA),
          panel.background = element_rect(fill = "transparent",colour = NA))
  if(p_names[x-1] == "/d_fit"){
    xlims <- c(as.numeric(dat_obs$year[1]),
              as.numeric(dat_obs$year[length(dat_obs$year)]))
    p <- p + xlim(xlims)
  }
  
  if(save){
  pdf(paste0("../../../latex/pdr/plots/",country, p_names[x-1],".pdf"), width=6, height=6) 
  print(p)
  dev.off()
  }  
  p 
  return(p)
  }
  
  if(country == "FR"){
  lapply(2:4, shape_plot, npop = npop, save,
         ylim = list(c(0,0.6), c(0,0.4), c(1.5,4)),
         p_names = c("/unp_fit", "/unw_fit", "/d_fit"))
  }else{
  lapply(2, shape_plot, npop = npop, save, ylim = list(c(1.5,4)),
         p_names = "/d_fit")
  }
}

plot_no_info_contra <- function(country = "FR", params, weights, save = F){
  
  source("get_obs.R")
  source("get_sim.R") 
  
  # get observed fertility rates
  obs <- get_obs(country, ysd = 1960)
  
  # simulated rates
  sim <- get_sim(params, country, obs, 
                 estimated_params = T, 
                 all_sim = T, sample = F, no_info = F,
                 multi_obj_mse = T,
                 weights = c(asfr = 0.40, unplanned = 0.20, unwanted = 0.00, desired = 0.40), 
                 unplanned = T)
  
  sim_no <- get_sim(params, country, obs,
                    estimated_params = T, 
                    all_sim = T, sample = F, no_info = T,
                    multi_obj_mse = T, weights = weights, unplanned = T)
  
  info_u <- sim$sim_unplanned
  no_info_u <- sim_no$sim_unplanned 
  
  mean_info_u <- as.data.frame(Reduce("+", info_u) / length(info_u))
  info_us <- as.data.frame(do.call("cbind", info_u))
  info_us$year <- as.numeric(rownames(info_us))
  info_us <- cbind(info_us, mean_info_u)
  names(info_us) <- c(rep("Simulated Info", length(info_u))
                      ,"year", "Mean Simulations Info")
  
  mean_no_info_u <- as.data.frame(Reduce("+", no_info_u) / length(no_info_u))
  no_info_us <- as.data.frame(do.call("cbind", no_info_u))
  no_info_us$year <- as.numeric(rownames(no_info_us))
  no_info_us <- cbind(no_info_us, mean_no_info_u)
  names(no_info_us) <- c(rep("Simulated No Info", length(no_info_u))
                         ,"year", "Mean Simulations No Info")
  
  all_dat_1 <- cbind(info_us$year, info_us[,names(info_us) != "year"],
                   no_info_us[,names(no_info_us) != "year"]) 
  names(all_dat_1)[1] <- "year"
    
  all_dat_1 <- all_dat_1[all_dat_1$year %in% c(1960:2005),]
    
  #### REMOVE WHEN SIMS WITH BIGGER N AVAILABLE
  sm_dat <- sapply(all_dat_1[2:ncol(all_dat_1)], smooth.spline, spar = 0.2)
  sm_sim_dat <- as.data.frame(do.call("cbind", sm_dat[2,]))
  all_dat_1[2:ncol(all_dat_1)] <- sm_sim_dat
  #############################################
    
  dat_long <- reshape(all_dat_1, dir = "long", idvar = "year",
                      names(all_dat_1)[c(2:ncol(all_dat_1))], v.names = "vals",
                      timevar = "type", times = names(all_dat_1)[c(2:ncol(all_dat_1))])
  
  dat_long$mean <- ifelse(dat_long$type %in% c("Mean Simulations Info","Mean Simulations No Info"), 1, 0)
  dat_long$scenario <- rep(c("Original Estimation","Without Information"), each = nrow(dat_long)/2)
  dat_long$year <- as.numeric(dat_long$year)
  dat_long$grp <- paste(dat_long[,2],dat_long[,4])
  
  lbs <- c("Original Estimation", "Estimation Without Info on Contraception")
    
    par(mfrow=c(1, 1))
    par(bg=NA)
    p <- ggplot(dat_long,
                aes(x = year,
                    y = vals,
                    group = as.factor(grp),
                    size = as.factor(mean),
                    linetype = as.factor(mean),
                    shape = as.factor(mean),
                    colour = as.factor(scenario))) +
      geom_line() +
      geom_point() +
      ylim(c(0,0.65)) +
      #xlim(c(1957, 2016)) +
      scale_colour_manual(values = c("grey65", "grey30"))+
      scale_shape_manual( values = c(NA,16), labels = lbs)+
      scale_linetype_manual( values = c(3,NA), labels = lbs)+
      scale_size_manual( values = c(0.5, 4), labels = lbs)+
      guides(shape = F, linetype = F, size = F)+
      theme_bw() +
      theme(legend.position = c(0.6,0.8),
            legend.title=element_blank()) +
      theme(legend.text = element_text(size = 20,
                                       margin = margin(l = 1, unit = "pt")),
            legend.key.height=unit(5,"point"),
            legend.key.size = unit(1.2, "cm"),
            legend.key = element_rect(colour = "transparent", fill = NA),
            legend.background=element_blank())+
      theme(axis.text = element_text(size = 20),
            axis.title = element_blank())+
      theme(plot.background = element_rect(fill = "transparent",colour = NA),
            panel.background = element_rect(fill = "transparent",colour = NA))
    if(save){
      pdf(paste0("../../../latex/pdr/plots/",country,"/no_info_contra.pdf"), width=8, height=8) 
      print(p)
      dev.off()
    }  
    p 
    
}

plot_tfr <- function(country, params, estimated_params = T,
                     scenarios = F,
                     save = F,
                     weights = c(0.25,0.25,0.25,0.25)){
  
  iniY <- params[params$country ==  country, "iniY"]
  endY <- params[params$country ==  country, "endY"]
  foreY <- params[params$country ==  country, "foreY"]
  ysd <- params[params$country ==  country, "ysd"]
  npop <- params[params$country ==  country, "npop"]
  N <- params[params$country ==  country, "N"]
  
  dir_o <- paste0("../results/", country, "/N_POP_", npop, "/N_", N)
  
  if(estimated_params){
  dir <- paste0(dir_o, "/results/base/", paste(weights, collapse = "_"))
  res_names <- list.files(dir, pattern = ".RData", full.names = TRUE)
  }
  
  if(scenarios){
    dir <- paste0(dir_o, "/results/scenarios/",paste(weights, collapse = "_"))
    res_names <- sapply(dir, function(x) {list.files(x, pattern, full.names = TRUE)})
  }
  
  obsTfr <- read.table(paste0("../run/data/",
                              country,"/out/tfr_hfd.txt"),
                       skip = 2, header = T,
                       stringsAsFactors = F)
  # Years for the Plot
  y1 <- ysd
  y2 <- max(obsTfr$Year)
  
  # Sim
  tfr <- list()
  
    for(i in res_names){
      out <- readRDS(i)
      tfr <- c(tfr, out["tfr"])
      rm(out)
    }

  mean_tfr <-  Reduce("+", tfr) / length(tfr)
  sim_tfr_vals <- sapply(tfr, function(x) x[1:length(iniY:(endY-1))])
  
  
  sim_tfr <- as.data.frame(cbind(Year = iniY:(endY-1),
                                 sim_tfr_vals, Mean_Sim_TFR = mean_tfr[1:length(iniY:(endY-1))]))
  fore_tfr <- as.data.frame(cbind(endY:(foreY-1),
                                  mean_tfr[length(iniY:endY):length(iniY:(foreY-1))]))
  for(i in 2:(ncol(sim_tfr)-1)){
  colnames(sim_tfr)[i] <- paste0(names(obsTfr[2]),"_", i-1)
  }
  
  names(fore_tfr) <- names(obsTfr[1:2])
  sim_obs <- merge(sim_tfr, obsTfr[1:2], by = "Year", all = T)
  sim_obs_fore <- merge(sim_obs, fore_tfr, by = "Year", all = T)
  tfr_aux <- sim_obs_fore[sim_obs_fore$Year >= ysd,]
  names(tfr_aux) <- c("year", rep("Simulated", npop), "Mean Simulations", "Observed", "Forecasted") 
  
  for(i in 2:11){
    colnames(tfr_aux)[i] <- paste0(names(tfr_aux)[i],"_", i-1)
  }
  
  tfr_long <- reshape(tfr_aux, dir = "long", idvar = "year",
          names(tfr_aux)[c(2:ncol(tfr_aux))], v.names = "vals",
          timevar = "type", times = names(tfr_aux)[c(2:ncol(tfr_aux))])
  tfr_sim_obs <- tfr_long[tfr_long$type %in% names(tfr_aux)[c(2:(ncol(tfr_aux)-1))],]
  tfr_sim_obs$mean <- ifelse(tfr_sim_obs$type %in% c("Mean Simulations", "Observed"), 1, 0)
  tfr_sim_obs$mean <- ifelse(tfr_sim_obs$type == "Mean Simulations", 2, tfr_sim_obs$mean )
  tfr_sim_obs$r_type <- ifelse(tfr_sim_obs$type %in% c("Mean Simulations", "Simulated"), 0, 1)
  tfr_sim_obs$r_type <- ifelse(tfr_sim_obs$type == "Mean Simulations", 2, tfr_sim_obs$r_type)

  #lim_y_up <- max(tfr_sim_obs[3], na.rm = T) + 0.3
  #lim_y_down <- min(tfr_sim_obs[3], na.rm = T) - 0.3

  lbs <- c("Simulated", "Observed", "Mean Simulations")
  
  par(mfrow=c(1, 1))
  par(bg=NA)
  p <- ggplot(tfr_sim_obs,
              aes(x = year,
                  y = vals,
                  group = as.factor(type),
                  size = as.factor(mean),
                  linetype = as.factor(mean),
                  shape = as.factor(mean),
                  colour = as.factor(mean))) +
    geom_line() +
    geom_point() +
    xlim(1958, 2018) +
    #ylim(lim_y_down, lim_y_up) +
    ylim(1,4.5) +
    scale_colour_manual(values = c("grey65", "grey0", "grey65"),
                        labels = lbs)+
    scale_shape_manual( values = c(NA,16,16), labels = lbs)+
    scale_linetype_manual( values = c(3,NA,NA), labels = lbs)+
    scale_size_manual( values = c(0.5, 2.5, 2.5), labels = lbs)+
    guides(shape = guide_legend(override.aes = list(size=c(1.2, 5, 5))))+
    theme_bw() +
    theme(legend.position = c(0.7,0.8),
          legend.title=element_blank()) +
    theme(legend.text = element_text(size = 20,
                                     margin = margin(l = 1, unit = "pt")),
          legend.key.height=unit(5,"point"),
          legend.key.size = unit(1.2, "cm"),
          legend.key = element_rect(colour = "transparent", fill = NA),
          legend.background=element_blank())+
    theme(axis.text = element_text(size = 20),
          axis.title = element_blank())+
    theme(plot.background = element_rect(fill = "transparent",colour = NA),
          panel.background = element_rect(fill = "transparent",colour = NA))
  
  if(save){
  pdf(paste0("../../../latex/pdr/plots/",country,"/tfr.pdf"), width=7, height=7) 
  print(p)
  dev.off()
  }
  
  return(p)
  
}

plot_mab <- function(country, params, estimated_params = T,
                     scenarios = F,
                     save = F,
                     weights = c(0.25,0.25,0.25,0.25)){
  
  iniY <- params[params$country ==  country, "iniY"]
  endY <- params[params$country ==  country, "endY"]
  foreY <- params[params$country ==  country, "foreY"]
  ysd <- params[params$country ==  country, "ysd"]
  npop <- params[params$country ==  country, "npop"]
  N <- params[params$country ==  country, "N"]
  
  dir_o <- paste0("../results/", country, "/N_POP_", npop, "/N_", N)
  
  if(estimated_params){
    dir <- paste0(dir_o, "/results/base/", paste(weights, collapse = "_"))
    res_names <- list.files(dir, pattern = ".RData", full.names = TRUE)
  }
  
  if(scenarios){
    dir <- paste0(dir_o, "/results/scenarios/",paste(weights, collapse = "_"))
    res_names <- sapply(dir, function(x) {list.files(x, pattern, full.names = TRUE)})
  }
  
  obs_mab <- read.table(paste0("../run/data/",
                              country,"/out/mab_hfd.txt"),
                       skip = 2, header = T,
                       stringsAsFactors = F)
  # Years for the Plot
  y1 <- ysd
  y2 <- max(obs_mab$Year)
  
  # Sim
  mab <- list()
  
  for(i in res_names){
    out <- readRDS(i)
    mab <- c(mab, out["meanAgeBirth"])
    rm(out)
  }
  
  mean_mab <-  Reduce("+", mab) / length(mab)
  sim_mab_vals <- sapply(mab, function(x) x[1:length(iniY:(endY-1))])
  
  
  sim_mab <- as.data.frame(cbind(Year = iniY:(endY-1),
                                 sim_mab_vals, Mean_Sim_MAB = mean_mab[1:length(iniY:(endY-1))]))
  fore_mab <- as.data.frame(cbind(endY:(foreY-1),
                                  mean_mab[length(iniY:endY):length(iniY:(foreY-1))]))
  for(i in 2:(ncol(sim_mab)-1)){
    colnames(sim_mab)[i] <- paste0(names(obs_mab[2]),"_", i-1)
  }
  
  names(fore_mab) <- names(obs_mab[1:2])
  sim_obs <- merge(sim_mab, obs_mab[1:2], by = "Year", all = T)
  sim_obs_fore <- merge(sim_obs, fore_mab, by = "Year", all = T)
  mab_aux <- sim_obs_fore[sim_obs_fore$Year >= ysd,]
  names(mab_aux) <- c("year", rep("Simulated", npop), "Mean Simulations", "Observed", "Forecasted") 
  
  for(i in 2:11){
    colnames(mab_aux)[i] <- paste0(names(mab_aux)[i],"_", i-1)
  }
  
  mab_long <- reshape(mab_aux, dir = "long", idvar = "year",
                      names(mab_aux)[c(2:ncol(mab_aux))], v.names = "vals",
                      timevar = "type", times = names(mab_aux)[c(2:ncol(mab_aux))])
  mab_sim_obs <- mab_long[mab_long$type %in% names(mab_aux)[c(2:(ncol(mab_aux)-1))],]
  mab_sim_obs$mean <- ifelse(mab_sim_obs$type %in% c("Mean Simulations", "Observed"), 1, 0)
  mab_sim_obs$mean <- ifelse(mab_sim_obs$type == "Mean Simulations", 2, mab_sim_obs$mean )
  mab_sim_obs$r_type <- ifelse(mab_sim_obs$type %in% c("Mean Simulations", "Simulated"), 0, 1)
  mab_sim_obs$r_type <- ifelse(mab_sim_obs$type == "Mean Simulations", 2, mab_sim_obs$r_type)
  
  #lim_y_up <- max(tfr_sim_obs[3], na.rm = T) + 0.3
  #lim_y_down <- min(tfr_sim_obs[3], na.rm = T) - 0.3
  
  lbs <- c("Simulated", "Observed", "Mean Simulations")
  
  par(mfrow=c(1, 1))
  par(bg=NA)
  p <- ggplot(mab_sim_obs,
              aes(x = year,
                  y = vals,
                  group = as.factor(type),
                  size = as.factor(mean),
                  linetype = as.factor(mean),
                  shape = as.factor(mean),
                  colour = as.factor(mean))) +
    geom_line() +
    geom_point() +
    xlim(1958, 2018) +
    #ylim(lim_y_down, lim_y_up) +
    ylim(20,40) +
    scale_colour_manual(values = c("grey65", "grey0", "grey65"),
                        labels = lbs)+
    scale_shape_manual( values = c(NA,16,16), labels = lbs)+
    scale_linetype_manual( values = c(3,NA,NA), labels = lbs)+
    scale_size_manual( values = c(0.5, 2.5, 2.5), labels = lbs)+
    guides(shape = guide_legend(override.aes = list(size=c(1.2, 5, 5))))+
    theme_bw() +
    theme(legend.position = c(0.7,0.8),
          legend.title=element_blank()) +
    theme(legend.text = element_text(size = 20,
                                     margin = margin(l = 1, unit = "pt")),
          legend.key.height=unit(5,"point"),
          legend.key.size = unit(1.2, "cm"),
          legend.key = element_rect(colour = "transparent", fill = NA),
          legend.background=element_blank())+
    theme(axis.text = element_text(size = 20),
          axis.title = element_blank())+
    theme(plot.background = element_rect(fill = "transparent",colour = NA),
          panel.background = element_rect(fill = "transparent",colour = NA))
  
  if(save){
    pdf(paste0("../../../latex/pdr/plots/",country,"/mab.pdf"), width=6, height=6) 
    print(p)
    dev.off()
  }
  
  return(p)
  
}

plot_gap <- function(country, params, estimated_params = T,
                     scenarios = F,
                     save = F,
                     weights){
  
  iniY <- params[params$country ==  country, "iniY"]
  endY <- params[params$country ==  country, "endY"]
  foreY <- params[params$country ==  country, "foreY"]
  ysd <- params[params$country ==  country, "ysd"]
  npop <- params[params$country ==  country, "npop"]
  N <- params[params$country ==  country, "N"]
  
  dir_o <- paste0("../results/", country, "/N_POP_", npop, "/N_", N)
  
  if(estimated_params){
    dir <- paste0(dir_o, "/results/base/", paste(weights, collapse = "_"))
    res_names <- list.files(dir, pattern = ".RData", full.names = TRUE)
  }
  
  if(scenarios){
    dir <- paste0(dir_o, "/results/scenarios/",paste(weights, collapse = "_"))
    res_names <- sapply(dir, function(x) {list.files(x, pattern, full.names = TRUE)})
  }
  
  # Years for the Plot
  y1 <- ysd
  y2 <- endY
  
  # Sim
  gap <- list()
  
  for(i in res_names){
    out <- readRDS(i)
    gap <- c(gap, out["gapKids"])
    rm(out)
  }
  
  mean_gap <-  Reduce("+", gap) / length(gap)
  sim_gap_vals <- sapply(gap, function(x) x[1:length(iniY:(endY-1))])
  
  sim_gap <- as.data.frame(cbind(Year = iniY:(endY-1),
                                 sim_gap_vals, Mean_Sim_gap = mean_gap[1:length(iniY:(endY-1))]))
  fore_gap <- as.data.frame(cbind(endY:(foreY-1),
                                  mean_gap[length(iniY:endY):length(iniY:(foreY-1))]))
  
  for(i in 2:(ncol(sim_gap)-1)){
    colnames(sim_gap)[i] <- paste0("gapKids","_", i-1)
  }
  
  names(fore_gap) <- c("Year","fore_gap")
  #sim_obs <- merge(sim_gap, obs_gap[1:2], by = "Year", all = T)
  sim_fore <- merge(sim_gap, fore_gap, by = "Year", all = T)
  gap_aux <- sim_fore[sim_fore$Year >= ysd,]
  names(gap_aux) <- c("year", rep("Simulated", npop), "Mean Simulations", "Forecasted") 
  
  for(i in 2:11){
    colnames(gap_aux)[i] <- paste0(names(gap_aux)[i],"_", i-1)
  }
  
  gap_long <- reshape(gap_aux, dir = "long", idvar = "year",
                      names(gap_aux)[c(2:ncol(gap_aux))], v.names = "vals",
                      timevar = "type", times = names(gap_aux)[c(2:ncol(gap_aux))])
  gap_sim_obs <- gap_long[gap_long$type %in% names(gap_aux)[c(2:(ncol(gap_aux)-1))],]
  gap_sim_obs$mean <- ifelse(gap_sim_obs$type %in% c("Mean Simulations"), 1, 0)
  gap_sim_obs$mean <- ifelse(gap_sim_obs$type == "Mean Simulations", 2, gap_sim_obs$mean )
  gap_sim_obs$r_type <- ifelse(gap_sim_obs$type %in% c("Mean Simulations", "Simulated"), 0, 1)
  gap_sim_obs$r_type <- ifelse(gap_sim_obs$type == "Mean Simulations", 2, gap_sim_obs$r_type)
  
  #lim_y_up <- max(tfr_sim_obs[3], na.rm = T) + 0.3
  #lim_y_down <- min(tfr_sim_obs[3], na.rm = T) - 0.3
  
  lbs <- c("Simulated","Mean Simulations")
  
  par(mfrow=c(1, 1))
  par(bg=NA)
  p <- ggplot(gap_sim_obs,
              aes(x = year,
                  y = vals,
                  group = as.factor(type),
                  size = as.factor(mean),
                  linetype = as.factor(mean),
                  shape = as.factor(mean),
                  colour = as.factor(mean))) +
    geom_line() +
    geom_point() +
    xlim(1958, 2018) +
    #ylim(lim_y_down, lim_y_up) +
    ylim(-1,0.5) +
    scale_colour_manual(values = c("grey65", "grey65"),
                        labels = lbs)+
    scale_shape_manual( values = c(NA,16), labels = lbs)+
    scale_linetype_manual( values = c(3,NA), labels = lbs)+
    scale_size_manual( values = c(0.5, 2.5), labels = lbs)+
    guides(shape = guide_legend(override.aes = list(size=c(1.2, 5))))+
    theme_bw() +
    theme(legend.position = c(0.7,0.2),
          legend.title=element_blank()) +
    theme(legend.text = element_text(size = 20,
                                     margin = margin(l = 1, unit = "pt")),
          legend.key.height=unit(5,"point"),
          legend.key.size = unit(1.2, "cm"),
          legend.key = element_rect(colour = "transparent", fill = NA),
          legend.background=element_blank())+
    theme(axis.text = element_text(size = 20),
          axis.title = element_blank())+
    theme(plot.background = element_rect(fill = "transparent",colour = NA),
          panel.background = element_rect(fill = "transparent",colour = NA))
  
  if(save){
    pdf(paste0("../../../latex/pdr/plots/",country,"/gap.pdf"), width=7, height=7) 
    print(p)
    dev.off()
  }
  
  return(p)
  
}


plot_fore_tfr <- function(country, params, estimated_params = T, scenarios = F, save = F){
  
  iniY <- params[params$country ==  country, "iniY"]
  endY <- params[params$country ==  country, "endY"]
  foreY <- params[params$country ==  country, "foreY"]
  ysd <- params[params$country ==  country, "ysd"]
  npop <- params[params$country ==  country, "npop"]
  N <- params[params$country ==  country, "N"]
  
  dir_o <- paste0("../results/", country, "/N_POP_", npop, "/N_", N)
  
  if(estimated_params){
    dir <- paste0(dir_o, "/results/base/")
    res_names <- list.files(dir, pattern = ".RData", full.names = TRUE)
  }
  
  if(scenarios){
    dir <- paste0(dir_o, "/results/scenarios/")
    res_names <- sapply(dir, function(x) {list.files(x, pattern, full.names = TRUE)})
  }
  
  obsTfr <- read.table(paste0("../run/data/",country,"/out/tfr_hfd.txt"), skip = 2, header = T,
                       stringsAsFactors = F)
  # Years for the Plot
  y1 <- ysd
  y2 <- max(obsTfr$Year)
  
  # Sim
  tfr <- list()
  
  for(i in res_names){
    out <- readRDS(i)
    tfr <- c(tfr, out["tfr"])
    rm(out)
  }
  
  mean_tfr <-  Reduce("+", tfr) / length(tfr)
  sim_tfr_vals <- sapply(tfr, function(x) x)
  sim_tfr <- as.data.frame(cbind(Year = iniY:(foreY-1),
                                 sim_tfr_vals,
                                 Mean_Sim_TFR = mean_tfr))
  for(i in 2:(ncol(sim_tfr)-1)){
    colnames(sim_tfr)[i] <- paste0(names(obsTfr[2]),"_", i-1)
  }
  fore_tfr <- sim_tfr[length(iniY:endY):length(iniY:(foreY-1)),]
  sim_tfr[length(iniY:endY):length(iniY:(foreY-1)),2:ncol(sim_tfr)] <- NA
  sim_obs <- merge(sim_tfr, obsTfr[1:2], by = "Year", all = T)
  sim_obs_fore <- merge(sim_obs, fore_tfr, by = "Year", all = T)
  tfr_aux <- sim_obs_fore[sim_obs_fore$Year >= ysd,]
  names(tfr_aux) <- c("year", rep("Simulated", npop), "Mean Simulations", "Observed", rep("Forecasted", npop), "Mean Forecast") 
  for(i in 2:11){
    colnames(tfr_aux)[i] <- paste0(names(tfr_aux)[i],"_", i-1)
  }
  for(i in 14:(ncol(tfr_aux)-1)){
    colnames(tfr_aux)[i] <- paste0(names(tfr_aux)[i],"_", i-1)
  }
  tfr_long <- reshape(tfr_aux, dir = "long", idvar = "year",
                      names(tfr_aux)[c(2:ncol(tfr_aux))], v.names = "vals",
                      timevar = "type", times = names(tfr_aux)[c(2:ncol(tfr_aux))])
  tfr_long$mean <- ifelse(tfr_long$type %in% c("Mean Simulations", "Observed"), 1, 0)
  tfr_long$mean <- ifelse(tfr_long$type == "Mean Simulations", 2, tfr_long$mean )
  tfr_long$mean <- ifelse(tfr_long$type %in% colnames(tfr_aux)[grep("Forecasted", colnames(tfr_aux))], 3, tfr_long$mean)
  tfr_long$mean <- ifelse(tfr_long$type == "Mean Forecast", 4, tfr_long$mean)
  
  lbs <- c("Simulated", "Observed", "Mean Simulations", "Forecasted", "Mean Forecast")
  
  par(mfrow=c(1, 1))
  par(bg=NA)
  p <- ggplot(tfr_long,
              aes(x = year,
                  y = vals,
                  group = as.factor(type),
                  size = as.factor(mean),
                  linetype = as.factor(mean),
                  shape = as.factor(mean),
                  colour = as.factor(mean))) +
    geom_line() +
    ylim(1,4) +
    geom_point() + 
    scale_colour_manual(values = c("grey50", "grey0", rep("grey50", 3)),
                        labels = lbs)+
    scale_linetype_manual( values = c(3,NA,NA,4,NA), labels = lbs)+
    scale_shape_manual( values = c(NA,16,16,NA,18), labels = lbs)+
    scale_size_manual( values = c(0.5, 3, 3, 0.5, 3), labels = lbs)+
    theme_bw() +
    theme(legend.position = c(0.7,0.8),
          legend.title=element_blank()) +
    theme(legend.text = element_text(size = 20,
                                     margin = margin(l = 1, unit = "pt")),
          legend.key.height=unit(5,"point"),
          legend.key.size = unit(1.5, "cm"),
          legend.key = element_rect(colour = "transparent", fill = NA),
          legend.background=element_blank())+
    theme(axis.text = element_text(size = 20),
          axis.title = element_blank())+
    theme(plot.background = element_rect(fill = "transparent",colour = NA),
          panel.background = element_rect(fill = "transparent",colour = NA))
  if(save){
  pdf(paste0("../../../latex/pdr/plots/",country,"/tfr_fore.pdf"), width=9, height=9) 
  print(p)
  dev.off()
  }
  
  return(p)
  
}

plot_ccf <- function(country, params, estimated_params = T, scenarios = F, save = F,
                     weights = c(0.25,0.25,0.25,0.25)){
  
  iniY <- params[params$country ==  country, "iniY"]
  endY <- params[params$country ==  country, "endY"]
  foreY <- params[params$country ==  country, "foreY"]
  ysd <- params[params$country ==  country, "ysd"]
  npop <- params[params$country ==  country, "npop"]
  N <- params[params$country ==  country, "N"]
  
  dir_o <- paste0("../results/", country, "/N_POP_", npop, "/N_", N)
  
  if(estimated_params){
    dir <- paste0(dir_o, "/results/base/", paste(weights, collapse = "_"))
    res_names <- list.files(dir, pattern = ".RData", full.names = TRUE)
  }
  
  if(scenarios){
    dir <- paste0(dir_o, "/results/scenarios/",paste(weights, collapse = "_"))
    res_names <- sapply(dir, function(x) {list.files(x, pattern, full.names = TRUE)})
  }
  
  obs_ccf <- read.table(paste0("../run/data/",
                              country,"/out/ccf_hfd.txt"),
                       skip = 2, header = T,
                       stringsAsFactors = F)
  # Years for the Plot
  y1 <- ysd
  y2 <- max(obs_ccf$Cohort)
  
  names(obs_ccf)[1] <- "Year"
  # Sim
  ccf <- list()
  
  for(i in res_names){
    out <- readRDS(i)
    ccf <- c(ccf, out["cohort"])
    rm(out)
  }
  
  ccf_aux <- lapply(ccf, function(x) x[,2])
  mean_ccf <-  Reduce("+", ccf_aux) / length(ccf_aux)
  sim_ccf_vals <- sapply(ccf_aux, function(x) x[1:length(iniY:(endY-51))])
  
  sim_ccf <- as.data.frame(cbind(Year = iniY:(endY-51),
                                 sim_ccf_vals, Mean_Sim_CCF = mean_ccf[1:length(iniY:(endY-51))]))
  for(i in 2:(ncol(sim_ccf)-1)){
    colnames(sim_ccf)[i] <- paste0(names(obs_ccf[2]),"_", i-1)
  }
  
  sim_obs <- merge(sim_ccf[sim_ccf$Year %in% 1931:1966,],
                   obs_ccf[obs_ccf$Year %in% 1931:1966,1:2], by = "Year", all = T)
  names(sim_obs) <- c("year", rep("Simulated", npop), "Mean Simulations", "Observed") 
  
  for(i in 2:11){
    colnames(sim_obs)[i] <- paste0(names(sim_obs)[i],"_", i-1)
  }
  
  ccf_long <- reshape(sim_obs, dir = "long", idvar = "year",
                      names(sim_obs)[c(2:ncol(sim_obs))], v.names = "vals",
                      timevar = "type", times = names(sim_obs)[c(2:ncol(sim_obs))])
  ccf_sim_obs <- ccf_long[ccf_long$type %in% names(sim_obs)[c(2:(ncol(sim_obs)))],]
  ccf_sim_obs$mean <- ifelse(ccf_sim_obs$type %in% c("Mean Simulations", "Observed"), 1, 0)
  ccf_sim_obs$mean <- ifelse(ccf_sim_obs$type == "Mean Simulations", 2, ccf_sim_obs$mean )
  
  #lim_y_up <- max(tfr_sim_obs[3], na.rm = T) + 0.3
  #lim_y_down <- min(tfr_sim_obs[3], na.rm = T) - 0.3
  
  lbs <- c("Simulated", "Observed", "Mean Simulations")
  
  par(mfrow=c(1, 1))
  par(bg=NA)
  p <- ggplot(ccf_sim_obs,
              aes(x = year,
                  y = as.numeric(vals),
                  group = as.factor(type),
                  size = as.factor(mean),
                  linetype = as.factor(mean),
                  shape = as.factor(mean),
                  colour = as.factor(mean))) +
    geom_line() +
    geom_point() +
    xlim(1928, 1968) +
    #ylim(lim_y_down, lim_y_up) +
    ylim(1.5,3.5) +
    scale_colour_manual(values = c("grey65", "grey0", "grey65"),
                        labels = lbs)+
    scale_shape_manual( values = c(NA,16,16), labels = lbs)+
    scale_linetype_manual( values = c(3,NA,NA), labels = lbs)+
    scale_size_manual( values = c(0.5, 2.5, 2.5), labels = lbs)+
    guides(shape = guide_legend(override.aes = list(size=c(1.2, 5, 5))))+
    theme_bw() +
    theme(legend.position = c(0.7,0.8),
          legend.title=element_blank()) +
    theme(legend.text = element_text(size = 20,
                                     margin = margin(l = 1, unit = "pt")),
          legend.key.height=unit(5,"point"),
          legend.key.size = unit(1.2, "cm"),
          legend.key = element_rect(colour = "transparent", fill = NA),
          legend.background=element_blank())+
    theme(axis.text = element_text(size = 20),
          axis.title = element_blank())+
    theme(plot.background = element_rect(fill = "transparent",colour = NA),
          panel.background = element_rect(fill = "transparent",colour = NA))
  
  if(save){
    pdf(paste0("../../../latex/pdr/plots/",country,"/ccf.pdf"), width=7, height=7) 
    print(p)
    dev.off()
  }
  
  return(p)

}


plot_ideal_desired <- function(country, save = F){
  
  if (country=="FR"){
  ideal_sit0 <- read.table(paste0("../run/data/",country ,"/out/ideal_sit.csv"),
                           sep = ",", skip = 0, header = T)
  ideal_sit <- ideal_sit0[!is.na(ideal_sit0[,2]),c(2,5)]
  }else{
  ideal_sit <- read.table(paste0("../run/data/",country ,"/out/ideal_sit.csv"),
                           sep = "", skip = 0, header = T)
  }
  names(ideal_sit) <- c("year", "d")  
  
  if (country=="FR"){
  ideal0 <- read.table(paste0("../run/data/",country ,"/out/ideal.csv"),
                       sep = ",", header = T)
  ideal <- as.data.frame(ideal0[c(2,5)])
  }else{
  ideal0 <- read.table(paste0("../run/data/",country,"/out/ideal.csv"),
             sep = ",",skip = 1, header = T)
  ideal <- as.data.frame(ideal0[c(1:2)])
  }

  names(ideal) <- c("year", "d")  
  ideals <- merge(ideal, ideal_sit, by = "year", all = T)
  ideals <- ideals[complete.cases(ideals),]
  year <- as.data.frame(seq(min(ideals[1]), max(ideals[1]), 1))
  names(year) <- "year"
  ideals_all <- merge(ideals, year, by = "year", all = T)
  names(ideals_all) <- c("year", "d", "d")  
  id_all <- rbind(ideals_all[,c(1:2)], ideals_all[, c(1,3)])
  if(country == "FR"){
  id_all$type <- c(rep("Ideal Family Size", nrow(ideals_all)), 
                   rep("Ideal <<en situation>>", nrow(ideals_all)))
  greys <- c("grey65","grey0")
  }else{
  id_all$type <- c(rep("Ideal Family Size", nrow(ideals_all)), 
                     rep("Retrieved Desired Family Size", nrow(ideals_all)))  
  greys <- c("grey0","grey65")
  }
  
  all <- id_all[complete.cases(id_all),]
  
  par(mfrow=c(1, 1))
  par(bg=NA)
  p <- ggplot(all,
              aes(x = year,
                  y = d,
                  colour = type,
                  shape = type)) +
    geom_point(size = 5) +
    geom_line(size = 0.7)+
    theme_bw() +
    ylim(c(1.5,4))+
    scale_shape_manual(values = c(1,1))+
    scale_colour_manual(values = greys)+
    theme(legend.position = c(0.6,0.9),
          legend.title=element_blank())+
    theme(legend.text = element_text(size=20,
                                     margin = margin(l = 10, unit = "pt")),
          legend.key.height=unit(10,"point"),
          legend.key.size = unit(1.2, "cm"),
          legend.key = element_rect(colour = "transparent", fill = NA),
          legend.background=element_blank())+
    guides(colour = guide_legend(override.aes = list(size = 1.2)))+
    theme(axis.text = element_text(size = 20),
          axis.title = element_blank())+
    theme(plot.background = element_rect(fill = "transparent",colour = NA),
          panel.background = element_rect(fill = "transparent",colour = NA))
  print(p)
  
  if(save){
  pdf(paste0("../../../latex/pdr/plots/",country,"/ideal_desired.pdf"), width=7, height=7) 
  print(p)
  dev.off()
  }
}

plot_contra <- function(country = "FR", save = F){
  
  unp_0 <- read.table("../run/data/FR/out/unplanned.csv", sep = ",", skip = 3, header = T)
  unp_0[,2] <- 1 - unp_0[,2] 
  names(unp_0) <- c("year", "obs_unp_births")
  fcstd_unp <- as.data.frame(cbind(1940:2016,fcst(unp_0[,2], 1940:2016, unp_0[,1])))
  names(fcstd_unp) <- c("year", "fore_unp_births")
  unp <- merge(unp_0, fcstd_unp, by = "year", all.y = T)
  unp_long <- reshape(unp, dir="long", idvar="year",
                           names(unp)[c(3,2)], v.names="vals",
                      timevar = "type", times = c("Forecasted", "Observed")) 
  
  par(mfrow=c(1, 1))
  par(bg=NA)
  p <- ggplot(unp_long, aes(x = year,
                              y = 1 - vals,
                              shape = as.factor(type),
                              linetype = as.factor(type),
                              size = as.factor(type))) +
    geom_point() +
    geom_line() +
    scale_linetype_manual(values=c(4,NA,1))+
    scale_shape_manual(values=c(NA,1,NA)) +
    scale_size_manual(values = c(1,5,1)) +
    theme_bw() + scale_colour_grey(start = 0, end = .5) +
    theme(legend.position = c(0.8,0.8),
          legend.title=element_blank())+
    theme(legend.text = element_text(size=20,
                                     margin = margin(l = 10, unit = "pt")),
          legend.key.height=unit(-10,"point"),
          legend.key.size = unit(1.2, "cm"),
          legend.key = element_rect(colour = "transparent", fill = NA, size = 0),
          legend.background=element_blank())+
    guides(colour = F ,size = F, shape = guide_legend(override.aes = list(size=c(1.2,5))))+
    theme(axis.text = element_text(size = 20),
          axis.title = element_blank())+
    theme(plot.background = element_rect(fill = "transparent",colour = NA),
          panel.background = element_rect(fill = "transparent",colour = NA))
  if(save){
  pdf(paste0("../../../latex/pdr/plots/",country,"/contraceptive_ts.pdf"), width=7, height=7) 
  print(p)
  dev.off()
  }
  
  return(p)
  
}

plot_dists <- function(country, save = F){
  
  dir_o <- paste0("../../../latex/data/",country,"/")
  
  dists <- list.files(dir_o, pattern = "original")
  
  dists_new <- list()
  for(i in 1:length(dists)){
  dist_0 <- data.table::fread(paste0(dir_o, dists[i]), drop = 1)
  dist_fore <- dist_0[which(is.na(dist_0[,2])),.SD, .SDcols = names(dist_0) %like% "forecasted"]
  dist_fore$years <- dist_0[which(is.na(dist_0[,2])), "years"]
  dist_1 <- merge(dist_0, dist_fore, by= "years", all.x = T)
  fore_x_cols <- names(dist_1) %like% "forecasted.x"
  dist <- as.data.frame(dist_1)
  dist[fore_x_cols][which(is.na(dist_1[,2])),] <- NA
  dists_new[[i]] <- dist
  }
  
  colnames(dists_new[[2]])[2] <- "work_obs" 

  split_and_plot <- function(x){
  dist_observed <- reshape(x[,c(TRUE, grepl( "obs" , names(x))[-1])],
                          dir="long", idvar="years",
                          names(x)[grepl( "obs", names(x))], v.names="vals")  
  dist_observed$type <- "Observed"
  dist_smooth <- reshape(x[,c(TRUE, grepl( "forecasted.x" , names(x))[-1])],
                            dir="long", idvar="years",
                            names(x)[grepl( "forecasted.x", names(x))], v.names="vals") 
  dist_smooth$type <- "Smoothed"
  dist_fore <- reshape(x[,c(TRUE, grepl( "forecasted.y" , names(x))[-1])],
                           dir="long", idvar="years",
                           names(x)[grepl( "forecasted.y", names(x))], v.names="vals")
  dist_fore$type <- "Forecasted"
  dist_final <- rbind(dist_observed,dist_smooth, dist_fore)
  names(dist_final)[2] <- gsub("\\_.*","",names(x[2]))

  par(mfrow=c(1, 1))
  par(bg=NA)
  p <- ggplot(dist_final, aes(x = years,
                       y = vals,
                       shape = type,
                       linetype = type,
                       colour = as.factor(dist_final[,2]),
                       size = type)) + ylim(0, 1) +
           geom_point() +
           geom_line() +
           scale_linetype_manual(values=c(4,NA,1))+
           scale_shape_manual(values=c(NA,1,NA)) +
           scale_size_manual(values = c(1,3,1)) +
           theme_bw() + scale_colour_grey(start = 0, end = .5) +
    theme(legend.position = c(0.75,0.5),
          legend.title=element_blank())+
    theme(legend.text = element_text(size=12,
                                     margin = margin(l = 10, unit = "pt")),
          legend.key.height=unit(-10,"point"),
          legend.key.size = unit(1.5, "cm"),
          legend.key = element_rect(colour = "transparent", fill = NA, size = 0),
          legend.background=element_blank())+
    guides(size = F, colour = F)+
    theme(axis.text = element_text(size = 12),
          axis.title = element_blank())+
    theme(plot.background = element_rect(fill = "transparent",colour = NA),
          panel.background = element_rect(fill = "transparent",colour = NA))
  if(save){
  pdf(paste0("../../../latex/pdr/plots/",country,"/",gsub("\\_.*","",names(dist_final[2])),"_dist.pdf"), width=5, height=5) 
  print(p)
  dev.off()
  }
  p
  }
  
  ps <- lapply(dists_new, split_and_plot)
  return(ps)
}

plot_single_dists <- function(country, end_year, save = F){
  
  dir_o <- paste0("../../../latex/data/", country,"/")
  
  dists <- list.files(dir_o, pattern = "original")
  
  dists_new <- list()
  for(i in 1:length(dists)){
    dist_0 <- data.table::fread(paste0(dir_o, dists[i]), drop = 1)
    dists_new[[i]] <- as.data.frame(dist_0[dist_0$years <= end_year,])
    
  }
  
  colnames(dists_new[[2]])[2] <- "work_obs" 
  
  split_and_plot <- function(x, save){
    
    dist_observed <- reshape(x[,c(TRUE, grepl( "obs" , names(x))[-1])],
                             dir="long", idvar="years",
                             names(x)[grepl( "obs", names(x))], v.names="vals")  
    dist_observed$type <- "Observed"
    
    dist_fore <- reshape(x[,c(TRUE, grepl( "forecasted" , names(x))[-1])],
                         dir="long", idvar="years",
                         names(x)[grepl( "forecasted", names(x))], v.names="vals")
    dist_fore$type <- "Forecasted"
    dist_final <- rbind(dist_observed, dist_fore)
    names(dist_final)[2] <- gsub("\\_.*","",names(x[2]))
    
    # select only one level
    dist_final <- dist_final[dist_final[,2]==1,]
    
    par(mfrow=c(1, 1))
    par(bg=NA)
    p <- ggplot(dist_final, aes(x = years,
                                y = vals,
                                shape = type,
                                linetype = type,
                                size = type)) + 
      geom_point() +
      geom_line() +
      scale_linetype_manual(values=c(4,NA,1))+
      scale_shape_manual(values=c(NA,1,NA)) +
      scale_size_manual(values = c(1,5,1)) +
      theme_bw() + 
     # coord_cartesian(ylim = c(0, 1)) +
      scale_colour_grey(start = 0, end = .5) +
      theme(legend.position = c(0.8,0.15),
            legend.title=element_blank())+
      theme(legend.text = element_text(size=20,
                                       margin = margin(l = 10, unit = "pt")),
            legend.key.height=unit(-10,"point"),
            legend.key.size = unit(1.2, "cm"),
            legend.key = element_rect(colour = "transparent", fill = NA, size = 0),
            legend.background=element_blank())+
      guides(colour = F ,size = F, shape = guide_legend(override.aes = list(size=c(1.2,5))))+
      theme(axis.text = element_text(size = 20),
            axis.title = element_blank())+
      theme(plot.background = element_rect(fill = "transparent",colour = NA),
            panel.background = element_rect(fill = "transparent",colour = NA))
    if(save){
    pdf(paste0("../../../latex/pdr/plots/",country,"/",gsub("\\_.*","",names(dist_final[2])),"_single_dist.pdf"), width=7, height=7) 
    print(p)
    dev.off()
    }
    
    return(p)
    
  }
  
  ps <- lapply(dists_new, split_and_plot, save)
  return(ps)
}

plot_analysis_scenarios <- function(country, params, weights,  save = F){
  
  iniY <- params[params$country ==  country, "iniY"]
  foreY <- params[params$country ==  country, "foreY"]
  ysd <- params[params$country ==  country, "ysd"]
  npop <- params[params$country ==  country, "npop"]
  N <- params[params$country ==  country, "N"]
  
  # Get Estimated values
  dir_o <- paste0("../results/", country, "/N_POP_", npop, "/N_", N)
  
  base_dir <- paste0(dir_o, "/results/base/",paste(weights, collapse = "_"),"/")
  scenarios_dir <- paste0(dir_o, "/results/scenarios/",paste(weights, collapse = "_"),"/")
  
  get_sim_results <- function(path, pattern, iniY, foreY){ 
    
    res_names <- sapply(path, function(x) {list.files(x, pattern, full.names = TRUE)})
    
    # CCF ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ccf <- list()
    for(i in res_names){
      out <- readRDS(i)
      ccf <- c(ccf, out["cohort"])
      rm(out)
    }
    ccf <- Reduce(function(...) merge(..., all=T, by = "year"), ccf)
    # TFR ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tfr <- list()
    for(i in res_names){
      out <- readRDS(i)
      tfr <- c(tfr, out["tfr"])
      rm(out)
    }
    # MAB ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    mab <- list()
    for(i in res_names){
      out <- readRDS(i)
      mab <- c(mab, out["meanAgeBirth"])
      rm(out)
    }
    # GAP ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    gap <- list()
    for(i in res_names){
      out <- readRDS(i)
      gap <- c(gap, out["gapKids"])
      rm(out)
    }
    
    tfr <- do.call("cbind", tfr)
    tfr <- as.data.frame(cbind(iniY:(foreY-1), tfr))
    mab <- do.call("cbind", mab)
    mab <- as.data.frame(cbind(iniY:(foreY-1), mab))
    gap <- do.call("cbind", gap)
    gap <- as.data.frame(cbind(iniY:(foreY-1), gap))
    
    return(list(tfr = tfr, ccf = ccf, mab = mab, gap = gap))
    
  }
  
  scs <- c("full_edu_effect", "no_edu_effect", "no_w_pnty")
  
  base <- get_sim_results(path = base_dir, pattern =  "RData", iniY = iniY, foreY = foreY)
  
  s1 <- get_sim_results(path = scenarios_dir, pattern = scs[1], iniY, foreY)
  s2 <- get_sim_results(path = scenarios_dir, scs[2], iniY, foreY)
  s3 <- get_sim_results(path = scenarios_dir, scs[3], iniY, foreY)
  
  
  long_dat <- function(x){
    
    if(x[nrow(x),1] == foreY-1){
      fore_period <- length(iniY:(endY-1)):length(iniY:(foreY-1)) 
      sim_period <- length(iniY:endY):length(iniY:(foreY-1)) 
      ysd <- 1960
    }else{
      fore_period <- length(iniY:(endY-1)):length(iniY:(foreY-1))- 50 
      sim_period <- length(iniY:endY):length(iniY:(foreY-1)) - 50
      ysd <- 1920
    } 
    
    mean <-  rowMeans(x[-1])
    sim <- as.data.frame(cbind(x, Mean_Sim = mean))
    names(sim)[1] <- "year"
    fore <- sim[fore_period,]
    sim[sim_period,2:ncol(sim)] <- NA
    sim_fore <- merge(sim, fore, by = "year", all = T)
    aux <- sim_fore[sim_fore$year >= ysd,]
    names(aux) <- c("year", rep("Simulated", npop), "Mean Simulations", rep("Forecasted", npop), "Mean Forecast") 
    for(i in 2:11){
      colnames(aux)[i] <- paste0(names(aux)[i],"_", i-1)
    }
    for(i in 13:(ncol(aux)-1)){
      colnames(aux)[i] <- paste0(names(aux)[i],"_", i-1)
    }
    long <- reshape(aux, dir = "long", idvar = "year",
                    names(aux)[c(2:ncol(aux))], v.names = "vals",
                    timevar = "type", times = names(aux)[c(2:ncol(aux))])
    long$mean <- ifelse(long$type == "Mean Simulations", 1, 0)
    long$mean <- ifelse(long$type %in% colnames(aux)[grep("Forecasted", colnames(aux))], 2, long$mean)
    long$mean <- ifelse(long$type == "Mean Forecast", 3, long$mean)
    
    return(long)
    
  }
  
  base_long <- lapply(base, long_dat)
  s1_long <- lapply(s1, long_dat)
  s2_long <- lapply(s2, long_dat)
  s3_long <- lapply(s3, long_dat)
  
  
  all_sim <- list()
  for(i in 1:length(base)){
    all <- rbind(base_long[[i]], s1_long[[i]], s2_long[[i]], s3_long[[i]])
    all$sc <- c(rep("base", nrow(base_long[[i]])),
                rep(scs[1], nrow(base_long[[i]])),
                rep(scs[2], nrow(base_long[[i]])),
                rep(scs[3], nrow(base_long[[i]])))
    all_sim[[i]] <- all
    names(all_sim[[i]]) <- c("year","type","vals","mean","sc")
    all_sim[[i]]$type <- paste(all_sim[[i]][,2], all_sim[[i]][,5])
  }
  
  tfr <- all_sim[[1]] 
  ccf <- all_sim[[2]]
  mab <- all_sim[[3]]
  gap <- all_sim[[4]]
  
  
  lbs <- c("Simulated", "Mean simulations", "Forecasted", "Mean forecast")
  
  lim_y_up <- max(tfr$vals, na.rm = T) + 0.4
  lim_y_down <- min(tfr$vals, na.rm = T) - 0.4
  
  p <- ggplot(tfr[tfr$sc != "no_w_pnty",],
         aes(x = year,
             y = vals,
             group = as.factor(type),
             size = as.factor(mean),
             linetype = as.factor(mean),
             shape = as.factor(mean),
             colour = as.factor(sc))) +
    ylim(lim_y_down, lim_y_up) +  
    geom_line() +
    geom_point() +
    scale_colour_manual(values = c("grey60", "grey0", "grey40"), 
                        labels = c("Base scenario","Strong educ. effect",
                                "No effect"))+
    scale_linetype_manual( values = c(3,NA,3,NA), labels = lbs)+
    scale_shape_manual( values = c(NA,16,NA,18), labels = lbs)+
    scale_size_manual( values = c(0.3,5,0.3,3), labels = lbs)+
    theme_bw() +
    theme(legend.position = c(0.7,0.7),
          legend.title=element_blank()) +
    theme(legend.text = element_text(size = 20,
                                     margin = margin(l = 1, unit = "pt")),
          legend.key.height=unit(0.5,"cm"),
          legend.key.size = unit(0.5, "cm"),
          legend.key = element_rect(colour = "transparent", fill = NA),
          legend.background=element_blank())+
    theme(axis.text = element_text(size = 20),
          axis.title = element_blank())+
    theme(plot.background = element_rect(fill = "transparent",colour = NA),
          panel.background = element_rect(fill = "transparent",colour = NA))
  
  lim_y_up <- max(mab$vals, na.rm = T) + 0.4
  lim_y_down <- min(mab$vals, na.rm = T) - 0.4
  
  g <- ggplot(mab[mab$sc != "no_w_pnty",],
              aes(x = year,
                  y = vals,
                  group = as.factor(type),
                  size = as.factor(mean),
                  linetype = as.factor(mean),
                  shape = as.factor(mean),
                  colour = as.factor(sc))) +
    ylim(lim_y_down, lim_y_up) +  
    geom_line() +
    geom_point() +
    scale_colour_manual(values = c("grey60", "grey0", "grey40"), 
                        labels = c("Base scenario","Strong educ. effect",
                                   "No effect"))+
    scale_linetype_manual( values = c(3,NA,3,NA), labels = lbs)+
    scale_shape_manual( values = c(NA,16,NA,18), labels = lbs)+
    scale_size_manual( values = c(0.3,5,0.3,3), labels = lbs)+
    theme_bw() +
    theme(legend.position = "none") +
    theme(axis.text = element_text(size = 20),
          axis.title = element_blank())+
    theme(plot.background = element_rect(fill = "transparent",colour = NA),
          panel.background = element_rect(fill = "transparent",colour = NA))

  lim_y_up <- max(gap$vals, na.rm = T) + 0.4
  lim_y_down <- min(gap$vals, na.rm = T) - 0.4
  
    
  l <- ggplot(gap[gap$sc != "no_w_pnty",],
              aes(x = year,
                  y = vals,
                  group = as.factor(type),
                  size = as.factor(mean),
                  linetype = as.factor(mean),
                  shape = as.factor(mean),
                  colour = as.factor(sc))) +
    ylim(lim_y_down, lim_y_up) +  
    geom_line() +
    geom_point() +
    scale_colour_manual(values = c("grey60", "grey0", "grey40"), 
                        labels = c("Base scenario","Strong educ. effect",
                                   "No effect"))+
    scale_linetype_manual( values = c(3,NA,3,NA), labels = lbs)+
    scale_shape_manual( values = c(NA,16,NA,18), labels = lbs)+
    scale_size_manual( values = c(0.3,5,0.3,3), labels = lbs)+
    theme_bw() +
    theme(legend.position = "none") +
    theme(axis.text = element_text(size = 20),
          axis.title = element_blank())+
    theme(plot.background = element_rect(fill = "transparent",colour = NA),
          panel.background = element_rect(fill = "transparent",colour = NA))

  lim_y_up <- max(ccf$vals, na.rm = T) + 0.4
  lim_y_down <- min(ccf$vals, na.rm = T) - 0.4
  
  k <- ggplot(ccf[ccf$sc != "no_w_pnty",],
              aes(x = year,
                  y = vals,
                  group = as.factor(type),
                  size = as.factor(mean),
                  linetype = as.factor(mean),
                  shape = as.factor(mean),
                  colour = as.factor(sc))) +
    ylim(lim_y_down, lim_y_up) +  
    geom_line() +
    geom_point() +
    scale_colour_manual(values = c("grey60", "grey0", "grey40"), 
                        labels = c("Base scenario","Strong educ. effect",
                                   "No effect"))+
    scale_linetype_manual( values = c(3,NA,3,NA), labels = lbs)+
    scale_shape_manual( values = c(NA,16,NA,18), labels = lbs)+
    scale_size_manual( values = c(0.3,5,0.3,3), labels = lbs)+
    theme_bw() +
    theme(legend.position = "none") +
    theme(axis.text = element_text(size = 20),
          axis.title = element_blank())+
    theme(plot.background = element_rect(fill = "transparent",colour = NA),
          panel.background = element_rect(fill = "transparent",colour = NA))

  
  if(save){
    
    pdf(paste0("../../../latex/pdr/plots/",country,"/fore_tfr.pdf"), width=7, height=7) 
    print(p)
    dev.off()
    
    pdf(paste0("../../../latex/pdr/plots/",country,"/fore_ccf.pdf"), width=7, height=7) 
    print(k)
    dev.off()
    
    pdf(paste0("../../../latex/pdr/plots/",country,"/fore_mab.pdf"), width=7, height=7) 
    print(g)
    dev.off()
    
    pdf(paste0("../../../latex/pdr/plots/",country,"/fore_gap.pdf"), width=7, height=7) 
    print(l)
    dev.off()
    
  }
  
  print(p)
  print(g)
  print(l)
  print(k)
  
  return(0)
  
}

plot_sim_contra <- function(country, params, weights, save = F){
  
  source("get_sim.R") 
  source("get_obs.R")
  
  #ovserved
  obs <- get_obs(country = country, ysd = 1960)
  
  # simulated 
  sim <- get_sim(params, country, obs, 
                 estimated_params = T, 
                 all_sim = T, 
                 unplanned = T, weights = weights)
  
  
  sim_un <- sim$sim_unplanned
  
  mean_sim_un <- as.data.frame(Reduce("+", sim_un) / length(sim_un))
  sim_uns <- as.data.frame(do.call("cbind", sim_un))
  sim_uns$year <- as.numeric(rownames(sim_uns))
  sim_unp <- cbind(sim_uns, mean_sim_un)
  names(sim_unp) <- c(rep("Simulated", length(sim_un))
                      ,"year", "Mean Simulations")
  
  all_dat_1 <- cbind(sim_unp$year, sim_unp[,names(sim_unp) != "year"]) 
  names(all_dat_1)[1] <- "year"
  
  all_dat_1 <- all_dat_1[all_dat_1$year %in% c(1960:2005),]
  
  #### REMOVE WHEN SIMS WITH BIGGER N AVAILABLE
  sm_dat <- sapply(all_dat_1[2:ncol(all_dat_1)], smooth.spline, spar = 0.2)
  sm_sim_dat <- as.data.frame(do.call("cbind", sm_dat[2,]))
  all_dat_1[2:ncol(all_dat_1)] <- sm_sim_dat
  #############################################
  
  dat_long <- reshape(all_dat_1, dir = "long", idvar = "year",
                      names(all_dat_1)[c(2:ncol(all_dat_1))], v.names = "vals",
                      timevar = "type", times = names(all_dat_1)[c(2:ncol(all_dat_1))])
  
  dat_long$mean <- ifelse(dat_long$type == "Mean Simulations", 1, 0)
  dat_long$year <- as.numeric(dat_long$year)
  dat_long$grp <- paste(dat_long[,2],dat_long[,4])
  dat_long$scs <- 1
  
  lbs <- c("Proportion\nUnplanned Births")
  
  par(mfrow=c(1, 1))
  par(bg=NA)
  p <- ggplot(dat_long,
              aes(x = year,
                  y = vals,
                  size = as.factor(mean),
                  linetype = as.factor(mean),
                  shape = as.factor(mean),
                  group = as.factor(grp),
                  colour = as.factor(scs))) +
    geom_line() +
    geom_point() +
    ylim(c(0,0.65)) +
    scale_shape_manual(values = c(NA,16), labels = c("","Proportion\nUnplanned Births"))+
    scale_linetype_manual( values = c(3,NA), labels = c("","Proportion\nUnplanned Births"))+
    scale_size_manual(values = c(0.5, 4), labels = c("","Proportion\nUnplanned Births"))+
    scale_colour_manual(values = "grey0", labels = c("","Proportion\nUnplanned Births")) +
    guides( linetype = F, size = F, colour = F)+
    theme_bw() +
    theme(legend.position = c(0.7,0.8),
          legend.title=element_blank()) +
    theme(legend.text = element_text(size = 20,
                                     margin = margin(l = 1, unit = "pt")),
          legend.key.height=unit(5,"point"),
          legend.key.size = unit(1.2, "cm"),
          legend.key = element_rect(colour = "transparent", fill = NA),
          legend.background=element_blank())+
    theme(axis.text = element_text(size = 20),
          axis.title = element_blank())+
    theme(plot.background = element_rect(fill = "transparent",colour = NA),
          panel.background = element_rect(fill = "transparent",colour = NA))
  if(save){
    pdf(paste0("..//../../latex/pdr/plots/",country,"/sim_contra.pdf"), width=6, height=6) 
    print(p)
    dev.off()
  }  
  p 
  
}

# Forecasting function
library(forecast)
fcst <- function(series, period, series_years){
  
  y <- log((series)/(1-series))
  
  dif <- setdiff(period, series_years)
  if(length(dif) > 0){
    f <- which(dif > max(series_years))
    if(length(f) > 0){
      fit <- ets(y)  
      fore <- forecast(fit, h = length(f))
    }
    b <- which(dif < min(series_years))
    if(length(b) > 0){  
      fit <- ets(rev(y))
      back <- forecast(fit, h = length(b))
    }
  }
  if(exists("back")){
    back_mean <- exp(back$mean)/(1+exp(back$mean))
    fore_mean <- exp(fore$mean)/(1+exp(fore$mean)) 
    n_series <- c(rev(back_mean), series, fore_mean)
    rm(back)
    
  }else{
    fore_mean <- exp(fore$mean)/(1+exp(fore$mean)) 
    n_series <- c(series, fore_mean)
  }
  
  plot(period, n_series, type = "l")
  
  return(n_series)
}

