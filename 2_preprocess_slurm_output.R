# This script will:

# 1. Move the folder that slurm created in the working directory.This is done so that the working directory doesn't get cluttered.
# 2. Preprocess the output of slurm and create a nice dataframe with each row being a step taken during the simulations, i.e. another batchSize and the result.

# The created dataframe can then be used to do whatever summary statistics you want to do on it.

# Setup global parameters #####################################################
pacman::p_load(data.table,
               tidyverse,
               rio)

saveDF <- T

folder <- 'try_1'

filename <- paste('./data/_rslurm_', 
        folder,
        '/results_0.RDS',sep = '')

saveFolder <- file.path('./analysis_output/preprocessing',
                        folder)

saveName <- paste(saveFolder,
                  '/sims_preprocessed.RData',
                  sep = '')

# If the save directory doesn't exist, create it
ifelse(!dir.exists(saveFolder), dir.create(saveFolder, recursive = T), 'Save directory already exists!')

# Import the file and concatenate #############################################
tempList <- import(filename)

sims_preprocessed <- rbindlist(tempList, idcol = 'id')

# Save the file #####################################################
if (saveDF){
        save(sims_preprocessed, file = saveName)
}