#### Western ebird forest treatment response 
#### Script Dash Part 1
#### Organize Data and run birds through GAM Models
#### Next Script Dash: updated_plot_results_generation_dash.R
#### Last Update: September 28th 2025
## Project: ebird_management (housed in ebird_pyodiv repo)

setwd("C:/Submission_Materials")

 source("scripts/functions/bird_gams.R")
 
# bird_gams(species = "houwre")
 
 library(future)
 library(furrr)
 
species_code <- c("amerob","batpig1","bkcchi")

plan(multisession, workers = 3)

future_map(species_code, ~bird_gams(.x), .options = furrr_options(seed = 123))

## shut down workers
## shut down workers
plan(sequential)

#### Calculating proportion significance for birds

source("scripts/functions/run_gam_plots.r")

## make list of all folder names in bird_model_results folder

species <- list.dirs(path = "C:/Users/hmboo/Box/02. BAD Lab/Projects/eBirdTreatment/IndividualBird_Paper_Writing_Figures/Submission_Materials/data", full.names = FALSE, recursive = FALSE)
species

#remove spoowl from species list
species <- species[species != "figures"]

### run source function for all species
for(i in 1:length(species)){
  run_gam_plots(species[i])
}



source("scripts/functions/sig_prop_plots.r")
sig_prop_plots ()
