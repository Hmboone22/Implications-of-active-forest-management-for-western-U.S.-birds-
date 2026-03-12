

  run_gam_plots <- function(species) {
 
    setwd("C:/Users/hmboo/Box/02. BAD Lab/Projects/eBirdTreatment/IndividualBird_Paper_Writing_Figures/Submission_Materials")
    
    library(mgcv)
    library(dplyr)
    library(tidyr)
    library(readr)
    library(arrow)
    library('gratia')
    library(ggplot2)
    library(gridExtra)
    library(grid)
    
    cb_palette <- c("#fdb462", "#fb8072", "#80b1d3", "#b3de69") 
    
    #different colorblind friendly colors
    
    
    names(cb_palette) <- c("Prescribed Fire", "Clearcut", "Mastication", "Thinning")
    treat_types <- c("rx_fire", "clear", "mast", "thin")
    time_periods <- c("recent", "old")
    prop_seq <- seq(0, 1, length.out = 100)
    
    #Load GAM Models and data
    mod <- read_rds(paste0("data/",species, "/",species,"_management_gams.rds"))
    gams <- list(
      rx_model = mod[["rx_model"]],
      clearcut_model = mod[["clearcut_model"]],
      mastication_model = mod[["mastication_model"]],
      thinning_model = mod[["thinning_model"]]
    )
    d.rx <- read_parquet(paste0("data/",species, "/",species,"_rxfire_modeldata.parquet"))
    d.cl <- read_parquet(paste0("data/",species, "/",species,"_clearcut_modeldata.parquet"))
    d.ma <- read_parquet(paste0("data/",species, "/",species,"_mastication_modeldata.parquet"))
    d.th <- read_parquet(paste0("data/",species, "/",species,"_thinning_modeldata.parquet"))
   
    #Standardize data and create grid with all combinations of recent values and treatment prop
    
     d.rx.new <- d.rx %>% 
      modelr::data_grid(recent = unique(recent),
                        Prescribed_Fire = prop_seq,
                        hours_of_day = 0,
                        is_stationary = "stationary",
                        effort_hrs = 0,
                        effort_distance_km = 0,
                        effort_speed_kmph = 0,
                        #cci = 0, not included in GAMs for privacy reasons but was in original models
                        year = 0,
                        elevation_30m_median = 0,
                        num_observers = 0,
                        Propwater = 0,
                        road_density_all = 0,
                        NA_L2NAME = 0,
                        TCC2001 = 0
      ) %>% 
      mutate(
        old = ifelse(recent == 1, 0, 1),
        #Scale treatment parameters using data's mean and standard deviation
        Prescribed_Firescale = (Prescribed_Fire - mean(d.rx$Prescribed_Fire)) / sd(d.rx$Prescribed_Fire)
      )
    d.cl.new <- d.cl %>% 
      modelr::data_grid(recent = unique(recent),
                        Clearcut = prop_seq,
                        hours_of_day = 0,
                        is_stationary = "stationary",
                        effort_hrs = 0,
                        effort_distance_km = 0,
                        effort_speed_kmph = 0,
                        #cci = 0,
                        year = 0,
                        elevation_30m_median = 0,
                        num_observers = 0,
                        Propwater = 0,
                        road_density_all = 0,
                        NA_L2NAME = 0,
                        TCC2001 = 0
      ) %>%
      mutate(
        old = ifelse(recent == 1, 0, 1),
        Clearcutscale = (Clearcut - mean(d.cl$Clearcut)) / sd(d.cl$Clearcut)
      )
    d.ma.new <- d.ma %>% 
      modelr::data_grid(recent = unique(recent),
                        Mastication = prop_seq,
                        hours_of_day = 0,
                        is_stationary = "stationary",
                        effort_hrs = 0,
                        effort_distance_km = 0,
                        effort_speed_kmph = 0,
                      #  cci = 0,
                        year = 0,
                        elevation_30m_median = 0,
                        num_observers = 0,
                        Propwater = 0,
                        road_density_all = 0,
                        NA_L2NAME = 0,
                        TCC2001 = 0
      ) %>%
      mutate(
        old = ifelse(recent == 1, 0, 1),
        Masticationscale = (Mastication - mean(d.ma$Mastication)) /
          sd(d.ma$Mastication)
      )
    d.th.new <- d.th %>%
      modelr::data_grid(recent = unique(recent),
                        Thinning = prop_seq,
                        hours_of_day = 0,
                        is_stationary = "stationary",
                        effort_hrs = 0,
                        effort_distance_km = 0,
                        effort_speed_kmph = 0,
                        #cci = 0,
                        year = 0,
                        elevation_30m_median = 0,
                        num_observers = 0,
                        Propwater = 0,
                        road_density_all = 0,
                        NA_L2NAME = 0,
                        TCC2001 = 0
      ) %>%
      mutate(
        old = ifelse(recent == 1, 0, 1),
        Thinningscale = (Thinning - mean(d.th$Thinning)) / sd(d.th$Thinning)
      )
#### Fire Model Results, GAM Effect Plots, and Proportion Significant calculations ####
    firemod <- gams$rx_model
    fm0 <- smooth_estimates(firemod, data = d.rx.new) %>%
      add_confint()
    d.rx <- d.rx.new[, c("Prescribed_Fire","Prescribed_Firescale")]
    #merge fm0 and d.rx.new to get treatment values for plotting
    fm0 <- merge(fm0, d.rx, by=c("Prescribed_Firescale"), all.x=T)
    
    fm0 <- fm0 %>% filter(recent == 1 | old == 1)
    nrow(fm0)
    
    #Calculate significance proportions using confidence interval method
    fm0$CI_add <- fm0$.upper_ci + fm0$.lower_ci
    fm0$pos_test <- as.numeric(fm0$CI_add > fm0$.upper_ci)
    fm0$neg_test <- as.numeric(fm0$CI_add < fm0$.lower_ci)
    fm0$signtest <- fm0$pos_test + fm0$neg_test
    fm0$issignificant <- as.integer(fm0$signtest > 0)
    neg_prop.dat <- sum(fm0$neg_test, na.rm = TRUE) /nrow(fm0)
    pos_prop.dat <- sum(fm0$pos_test, na.rm = TRUE) /nrow(fm0)
    neg_prop_check <- sum(fm0$neg_test, na.rm = TRUE) /nrow(fm0)
    pos_prop_check <- sum(fm0$pos_test, na.rm = TRUE) /nrow(fm0)
    all.equal(neg_prop_check, neg_prop.dat)
    all.equal(pos_prop_check, pos_prop.dat)
    fm0_recent <- fm0 %>% filter(recent == 1)
    fm0_old <- fm0 %>% filter(old == 1)
    neg_prop_recent <- sum(fm0_recent$neg_test, na.rm = TRUE) /nrow(fm0_recent)
    pos_prop_recent <- sum(fm0_recent$pos_test, na.rm = TRUE) /nrow(fm0_recent)
    neg_prop_old <- sum(fm0_old$neg_test, na.rm = TRUE) /nrow(fm0_old)
    pos_prop_old <- sum(fm0_old$pos_test, na.rm = TRUE) /nrow(fm0_old)
    prop_table <- data.frame(
      Treatment = "Prescribed Fire",
      species = species,
      Total_Negative_Proportion = neg_prop.dat,
      Total_Positive_Proportion = pos_prop.dat,
      recent_Negative_Proportion = neg_prop_recent,
      recent_Positive_Proportion = pos_prop_recent,
      old_Negative_Proportion = neg_prop_old,
      old_Positive_Proportion = pos_prop_old
    )
    
    #### Clearcut Model Results, GAM Effect Plots, and Proportion Significant calculations ####
    clearcutmod <- gams$clearcut_model
    clm0 <- smooth_estimates(clearcutmod, data = d.cl.new) %>%
      add_confint()
    d.cl <- d.cl.new[, c("Clearcutscale","Clearcut")]
    #merge fm0 and d.rx.new to get treatment values for plotting
    clm0 <- merge(clm0, d.cl, by=c("Clearcutscale"), all.x=T)
    
    clm0 <- clm0 %>% filter(recent == 1 | old == 1)
    nrow(clm0)
    clm0$CI_add <- clm0$.upper_ci + clm0$.lower_ci
    clm0$pos_test <- as.numeric(clm0$CI_add > clm0$.upper_ci)
    clm0$neg_test <- as.numeric(clm0$CI_add < clm0$.lower_ci)
    clm0$signtest <- clm0$pos_test + clm0$neg_test
    clm0$issignificant <- as.integer(clm0$signtest > 0)
    neg_prop.dat <- sum(clm0$neg_test, na.rm = TRUE) /nrow(clm0)
    pos_prop.dat <- sum(clm0$pos_test, na.rm = TRUE) /nrow(clm0)
    neg_prop_check <- sum(clm0$neg_test, na.rm = TRUE) / nrow(clm0)
    pos_prop_check <- sum(clm0$pos_test, na.rm = TRUE) / nrow(clm0)
    all.equal(neg_prop_check, neg_prop.dat)
    all.equal(pos_prop_check, pos_prop.dat)
    clm0_recent <- clm0 %>% filter(recent == 1)
    clm0_old <- clm0 %>% filter(old == 1)
    neg_prop_recent <- sum(clm0_recent$neg_test, na.rm = TRUE) /nrow(clm0_recent)
    pos_prop_recent <- sum(clm0_recent$pos_test, na.rm = TRUE) /nrow(clm0_recent)   
    neg_prop_old <- sum(clm0_old$neg_test, na.rm = TRUE) /nrow(clm0_old)
    pos_prop_old <- sum(clm0_old$pos_test, na.rm = TRUE) /nrow(clm0_old)
    prop_table2 <- data.frame(
      Treatment = "Clearcut",
      species = species,
      Total_Negative_Proportion = neg_prop.dat,
      Total_Positive_Proportion = pos_prop.dat,
      recent_Negative_Proportion = neg_prop_recent,
      recent_Positive_Proportion = pos_prop_recent,
      old_Negative_Proportion = neg_prop_old,
      old_Positive_Proportion = pos_prop_old
    )
    # Combine with fire proportion table
    prop_table <- rbind(prop_table, prop_table2)
    
    #### Mastication Model Results, GAM Effect Plots, and Proportion Significant calculations ####    
    mastmod <- gams$mastication_model
    mam0 <- smooth_estimates(mastmod, data = d.ma.new) %>%
      add_confint() 
    d.ma <- d.ma.new[, c("Masticationscale","Mastication")]
    #merge fm0 and d.rx.new to get treatment values for plotting
    mam0 <- merge(mam0, d.ma, by=c("Masticationscale"), all.x=T)
    
    mam0 <- mam0 %>% filter(recent == 1 | old == 1)
    nrow(mam0)
    mam0$CI_add <- mam0$.upper_ci + mam0$.lower_ci
    mam0$pos_test <- as.numeric(mam0$CI_add > mam0$.upper_ci)
    mam0$neg_test <- as.numeric(mam0$CI_add < mam0$.lower_ci)
    mam0$signtest <- mam0$pos_test + mam0$neg_test
    mam0$issignificant <- as.integer(mam0$signtest > 0)
    neg_prop.dat <- sum(mam0$neg_test, na.rm = TRUE) /nrow(mam0)
    pos_prop.dat <- sum(mam0$pos_test, na.rm = TRUE) /nrow(mam0)
    neg_prop_check <- sum(mam0$neg_test, na.rm = TRUE) / nrow(mam0)
    pos_prop_check <- sum(mam0$pos_test, na.rm = TRUE) /nrow(mam0)
    all.equal(neg_prop_check, neg_prop.dat)
    all.equal(pos_prop_check, pos_prop.dat)
    mam0_recent <- mam0 %>% filter(recent == 1)
    mam0_old <- mam0 %>% filter(old == 1)
    neg_prop_recent <- sum(mam0_recent$neg_test, na.rm = TRUE) /nrow(mam0_recent)
    pos_prop_recent <- sum(mam0_recent$pos_test, na.rm = TRUE)/nrow(mam0_recent)
    neg_prop_old <- sum(mam0_old$neg_test, na.rm = TRUE)/ nrow(mam0_old)
    pos_prop_old <- sum(mam0_old$pos_test, na.rm = TRUE)/ nrow(mam0_old)
    prop_table3 <- data.frame(
      Treatment = "Mastication",
      species = species,
      Total_Negative_Proportion = neg_prop.dat,
      Total_Positive_Proportion = pos_prop.dat,
      recent_Negative_Proportion = neg_prop_recent,
      recent_Positive_Proportion = pos_prop_recent,
      old_Negative_Proportion = neg_prop_old,
      old_Positive_Proportion = pos_prop_old
    )
    prop_table <- rbind(prop_table, prop_table3)  
    
    #### Thinning Model Results, GAM Effect Plots, and Proportion Significant calculations ####
    thinmod <- gams$thinning_model
    thm0 <- smooth_estimates(thinmod, data = d.th.new) %>%
      add_confint()
    d.th <- d.th.new[, c("Thinningscale","Thinning")]
    #merge fm0 and d.rx.new to get treatment values for plotting
    thm0 <- merge(thm0, d.th, by=c("Thinningscale"), all.x=T)
    
    thm0 <- thm0 %>% filter(recent == 1 | old == 1)
    nrow(thm0)
    thm0$CI_add <- thm0$.upper_ci + thm0$.lower_ci
    thm0$pos_test <- as.numeric(thm0$CI_add > thm0$.upper_ci)
    thm0$neg_test <- as.numeric(thm0$CI_add < thm0$.lower_ci)
    thm0$signtest <- thm0$pos_test + thm0$neg_test
    thm0$issignificant <- as.integer(thm0$signtest > 0)
    neg_prop.dat <- sum(thm0$neg_test, na.rm = TRUE) /nrow(thm0)
    pos_prop.dat <- sum(thm0$pos_test, na.rm = TRUE) /nrow(thm0)
    neg_prop_check <- sum(thm0$neg_test, na.rm = TRUE) / nrow(thm0)
    pos_prop_check <- sum(thm0$pos_test, na.rm = TRUE) / nrow(thm0)
    all.equal(neg_prop_check, neg_prop.dat)
    all.equal(pos_prop_check, pos_prop.dat)
    thm0_recent <- thm0 %>% filter(recent == 1)
    thm0_old <- thm0 %>% filter(old == 1)
    neg_prop_recent <- sum(thm0_recent$neg_test, na.rm = TRUE)/ nrow(thm0_recent)
    pos_prop_recent <- sum(thm0_recent$pos_test, na.rm = TRUE)/ nrow(thm0_recent)
    neg_prop_old <- sum(thm0_old$neg_test, na.rm = TRUE)/ nrow(thm0_old)
    pos_prop_old <- sum(thm0_old$pos_test, na.rm = TRUE)/ nrow(thm0_old)
    prop_table4 <- data.frame(
      Treatment = "Thinning",
      species = species,
      Total_Negative_Proportion = neg_prop.dat,
      Total_Positive_Proportion = pos_prop.dat,
      recent_Negative_Proportion = neg_prop_recent,
      recent_Positive_Proportion = pos_prop_recent,
      old_Negative_Proportion = neg_prop_old,
      old_Positive_Proportion = pos_prop_old
    ) 
    prop_table <- rbind(prop_table, prop_table4)  
    

    ### Combine all model results into one dataframe for saving
    
    fm0$treatment <- "Prescribed Fire"
    clm0$treatment <- "Clearcut"
    mam0$treatment <- "Mastication"
    thm0$treatment <- "Thinning"
    colnames(fm0)[colnames(fm0) == "Prescribed_Fire"] <- "treatment_proportion"
    colnames(clm0)[colnames(clm0) == "Clearcut"] <- "treatment_proportion"
    colnames(mam0)[colnames(mam0) == "Mastication"] <- "treatment_proportion"
    colnames(thm0)[colnames(thm0) == "Thinning"] <- "treatment_proportion"
    colnames(fm0)[colnames(fm0) == "Prescribed_Firescale"] <- "Treatmentscale"
    colnames(clm0)[colnames(clm0) == "Clearcutscale"] <- "Treatmentscale"
    colnames(mam0)[colnames(mam0) == "Masticationscale"] <- "Treatmentscale"
    colnames(thm0)[colnames(thm0) == "Thinningscale"] <- "Treatmentscale"
    
    all_mods <- rbind(fm0, clm0, mam0, thm0)
    all_mods$species <- species

   
    
    write.csv(all_mods, paste0("data/",species, "/",species,"_all_treatment_smooth_estimates.csv"), row.names = FALSE)
    write.csv(prop_table, paste0("data/",species, "/",species,"_treatment_proportions.csv"), row.names = FALSE)
    
    ### Make GAM Raw plots
    
    ### Fire Plots ####
    d.rx.newcov <-   d.rx.new[, c("Prescribed_Fire","Prescribed_Firescale")]
    colnames(d.rx.newcov)[colnames(d.rx.newcov) == "Prescribed_Firescale"] <- "Treatmentscale"
    fm0 <- merge(fm0, d.rx.newcov, by=c("Treatmentscale"), all.x=T)
    fm0$.by <- factor(fm0$.by, levels = c("recent", "old"))
    #change recent to Recent
    levels(fm0$.by) <- c("Recent", "Old")
    
    fireplot <- ggplot(fm0, aes(x = Prescribed_Fire, y = .estimate)) +  geom_line (color=cb_palette["Prescribed Fire"], size=1) +
      xlab("Proportion of prescribed fire") + ylab("Response estimate") +  coord_cartesian(ylim = c(-4, 4),xlim = c(0, 1.0)) +
      geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci),
                  alpha = 0.5, fill=cb_palette["Prescribed Fire"]) + 
      facet_grid(.by~.,switch = "y")+ 
      geom_hline(yintercept=0, linetype="dashed", linewidth=1) +
      theme_classic() +
      theme(
        strip.placement = "outside",
        text = element_text(size = 18), # Increase base font size
        axis.title = element_text(size = 20), # Increase axis title size  
        axis.text = element_text(size = 18), # Increase axis text size
        strip.text = element_text(size = 18) # Increase facet label size
      )
    fireplot
    ggsave(
      filename = paste0("data/", species, "/", species, "_PrescribedFire.png"),
      plot = fireplot,
      width = 8, height = 6, units = "in"
    )
    
    ### Clearcut Plots ####
        d.cl.newcov <-   d.cl.new[, c("Clearcut","Clearcutscale")]
    colnames(d.cl.newcov)[colnames(d.cl.newcov) == "Clearcutscale"] <- "Treatmentscale"
    clm0 <- merge(clm0, d.cl.newcov, by=c("Treatmentscale"), all.x=T)
    clm0$.by <- factor(clm0$.by, levels = c("recent", "old"))
    #change recent to Recent
    levels(clm0$.by) <- c("Recent", "Old")
    
    clearplot <-ggplot(clm0, aes(x = Clearcut, y = .estimate)) +  geom_line (color=cb_palette["Clearcut"], size=1) +
      xlab("Proportion of clearcut") + ylab("Response estimate") +  coord_cartesian(ylim = c(-4, 4),xlim = c(0, 1.0)) +
      geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci),
                  alpha = 0.5, fill=cb_palette["Clearcut"]) + 
      facet_grid(.by~.,switch = "y")+ 
      geom_hline(yintercept=0, linetype="dashed", linewidth=1) +
      theme_classic() +
      theme(
        strip.placement = "outside",
        text = element_text(size = 18), # Increase base font size
        axis.title = element_text(size = 20), # Increase axis title size  
        axis.text = element_text(size = 18), # Increase axis text size
        strip.text = element_text(size = 18) # Increase facet label size
      )
    
    clearplot
    ggsave(
      filename = paste0("data/", species, "/", species, "_Clearcut.png"),
      plot = clearplot,
      width = 8, height = 6, units = "in"
    )
    
    ### Mastication Plots ####
    
    d.ma.newcov <-   d.ma.new[, c("Mastication","Masticationscale")]
    colnames(d.ma.newcov)[colnames(d.ma.newcov) == "Masticationscale"] <- "Treatmentscale"
    mam0 <- merge(mam0, d.ma.newcov, by=c("Treatmentscale"), all.x=T)
    mam0$.by <- factor(mam0$.by, levels = c("recent", "old"))
    #change recent to Recent
    levels(mam0$.by) <- c("Recent", "Old")
      mastplot <- ggplot(mam0, aes(x = Mastication, y = .estimate)) +  geom_line (color=cb_palette["Mastication"], size=1) +
      xlab("Proportion of mastication") + ylab("Response estimate") +  coord_cartesian(ylim = c(-4, 4),xlim = c(0, 1.0)) +
      geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci),
                  alpha = 0.5, fill=cb_palette["Mastication"]) + 
      facet_grid(.by~.,switch = "y")+ 
      geom_hline(yintercept=0, linetype="dashed", linewidth=1) +
      theme_classic() +
      theme(
        strip.placement = "outside",
        text = element_text(size = 18), # Increase base font size
        axis.title = element_text(size = 20), # Increase axis title size  
        axis.text = element_text(size = 18), # Increase axis text size
        strip.text = element_text(size = 18) # Increase facet label size
      )
    
    mastplot
    ggsave(
      filename = paste0("data/", species, "/", species, "_Mastication.png"),
      plot = mastplot,
      width = 8, height = 6, units = "in"
    )
    
    ### Thinning Plots ####
    
    d.th.newcov <-   d.th.new[, c("Thinning","Thinningscale")]
    colnames(d.th.newcov)[colnames(d.th.newcov) == "Thinningscale"] <- "Treatmentscale"
    thm0 <- merge(thm0, d.th.newcov, by=c("Treatmentscale"), all.x=T)
    thm0$.by <- factor(thm0$.by, levels = c("recent", "old"))
    #change recent to Recent
    levels(thm0$.by) <- c("Recent", "Old")
       thinplot <- ggplot(thm0, aes(x = Thinning, y = .estimate)) +  geom_line (color=cb_palette["Thinning"], size=1) +
      xlab("Proportion of thinning") + ylab("Response estimate") +  coord_cartesian(ylim = c(-4, 4),xlim = c(0, 1.0)) +
      geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci),
                  alpha = 0.5, fill=cb_palette["Thinning"]) + 
      facet_grid(.by~.,switch = "y")+ 
      geom_hline(yintercept=0, linetype="dashed", linewidth=1) +
      theme_classic() +
      theme(
        strip.placement = "outside",
        text = element_text(size = 18), # Increase base font size
        axis.title = element_text(size = 20), # Increase axis title size  
        axis.text = element_text(size = 18), # Increase axis text size
        strip.text = element_text(size = 18) # Increase facet label size
      )
    thinplot
    ggsave(
      filename = paste0("data/", species, "/", species, "_Thinning.png"),
      plot = thinplot,
      width = 8, height = 6, units = "in"
    )
    ### Combine all plots into one figure ####
    png(paste0("data/",species, "/",species,"_All_Treatments.png"), width = 1000, height = 800)
    my_title <- textGrob(
      species,
      gp = gpar(fontsize = 25) # Increase font size here
    )
    grid.arrange(fireplot, clearplot, mastplot, thinplot, ncol=2, top=my_title)
    dev.off()
    
    #### Save all species proportion information and raw gam effects into master files for later use
    
    totalSPProp <- read.csv("C:/Users/hmboo/Box/02. BAD Lab/Projects/eBirdTreatment/IndividualBird_Paper_Writing_Figures/Submission_Materials/data/figures/treatment_proportions.csv", header=T) 
    totalSPProp <- rbind(totalSPProp, prop_table)
    write.csv(totalSPProp, "C:/Users/hmboo/Box/02. BAD Lab/Projects/eBirdTreatment/IndividualBird_Paper_Writing_Figures/Submission_Materials/data/figures/treatment_proportions.csv", row.names = FALSE)
 
    allspeciesresults <-read.csv("C:/Users/hmboo/Box/02. BAD Lab/Projects/eBirdTreatment/IndividualBird_Paper_Writing_Figures/Submission_Materials/data/figures/all_treatment_smooth_estimates_allspecies.csv", header=T)
    allspeciesresults <- rbind(allspeciesresults, all_mods)
    write.csv(allspeciesresults, "C:/Users/hmboo/Box/02. BAD Lab/Projects/eBirdTreatment/IndividualBird_Paper_Writing_Figures/Submission_Materials/data/figures/all_treatment_smooth_estimates_allspecies.csv", row.names = FALSE)
     }
  
  
  

