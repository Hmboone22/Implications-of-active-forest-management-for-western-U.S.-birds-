## PurposE: Run GAM models
## Project: ebird_management
## Upstream: prep_spp_data.R
## Downstream: pred_treatments.R

bird_gams <- function(species)
{
  setwd("D:/Submission_Materials")
  #species <- "yebmag"
  library(tidyverse)
  library(mgcv)
  library(arrow)
  
  #### Prescribed Fire Model ####
  ## read in species model data
  d.rx <- read_parquet(paste0("/data/",species, "/",species,"_rxfire_modeldata.parquet"))
  
  ## run rx burm model
  ## If we use dummy variables for recent and old treatment this avoids trying to fit a smooth to untreated
  ## for privacy reasons, we can't share eBird's CCI variables in all GAMs but these indexes were in the original models. 
    m.rx <- gam(obs_detected ~ s(Prescribed_Firescale, by = recent) + s(Prescribed_Firescale, by = old) + #This should estimate sepparate intercepts for treated and untreated but not try to estimate splines for untreated
                (hours_of_day)+ is_stationary +(effort_hrs)+ (effort_distance_km)+ (effort_speed_kmph)+ year + NA_L2NAME +
                s(elevation_30m_median) + (num_observers) + (Propwater) + (road_density_all) + s(TCC2001), 
              data = d.rx,
              method = "REML", family = binomial("logit"))
  
  ## check to make sure nobs is equal to rows of input data; stop with error message if not
  if(nobs(m.rx) != nrow(d.rx)) stop("Number of observations in rx fire model does not equal number of rows in input data")
  
  #### Clearcut model ####
  
  d.cl <- read_parquet(paste0("/data/",species, "/",species,"_clearcut_modeldata.parquet"))
  
  ## run clearcut model
  m.cl <- gam(obs_detected ~ s(Clearcutscale, by = recent) + s(Clearcutscale, by = old) + 
                hours_of_day + is_stationary + effort_hrs + effort_distance_km + effort_speed_kmph  + year + NA_L2NAME +
                s(elevation_30m_median) +num_observers + Propwater + road_density_all + s(TCC2001), 
              data = d.cl,
              method = "REML", family = binomial("logit"))
  
  ## check to make sure nobs is equal to rows of input data; stop with error message if not
  if(nobs(m.cl) != nrow(d.cl)) stop("Number of observations in clearcut model does not equal number of rows in input data")
  
  
  #### Mastiction model ####
  
  d.ma <- read_parquet(paste0("/data/",species, "/",species,"_mastication_modeldata.parquet"))
  
  ## run mastication model
  m.ma <- gam(obs_detected ~ s(Masticationscale, by = recent) + s(Masticationscale, by = old) + 
                hours_of_day + is_stationary + effort_hrs + effort_distance_km + effort_speed_kmph  + year + NA_L2NAME+
                s(elevation_30m_median) +num_observers + Propwater + road_density_all + s(TCC2001), 
              data = d.ma,
              method = "REML", family = binomial("logit"))
  
  ## check to make sure nobs is equal to rows of input data; stop with error message if not
  if(nobs(m.ma) != nrow(d.ma)) stop("Number of observations in mastication model does not equal number of rows in input data")
  
  #### Thinning Model ####
  
  d.th <- read_parquet(paste0("/data/",species, "/",species,"_thinning_modeldata.parquet"))
  
  ## run thinning model
  m.th <- gam(obs_detected ~ s(Thinningscale, by = recent) + s(Thinningscale, by = old) + 
                hours_of_day + is_stationary + effort_hrs + effort_distance_km + effort_speed_kmph  + year + NA_L2NAME +
                s(elevation_30m_median) +num_observers + Propwater + road_density_all + s(TCC2001), 
              data = d.th,
              method = "REML", family = binomial("logit"))
  
  ## check to make sure nobs is equal to rows of input data; stop with error message if not
  if(nobs(m.th) != nrow(d.th)) stop("Number of observations in thinning model does not equal number of rows in input data")
  
  ## save models for later use using a named list and rds
  write_rds(list(rx_model = m.rx, 
               clearcut_model = m.cl, 
               mastication_model = m.ma, 
               thinning_model = m.th),
          paste0("/data/",species, "/",species,"_management_gams.rds"),
          compress = "xz")
  
  
  
}