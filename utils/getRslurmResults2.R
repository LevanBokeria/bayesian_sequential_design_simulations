# This script will:

# 1. Move the folder that slurm created in the working directory.This is done so that the working directory doesn't get cluttered.
# 2. Preprocess the output of slurm and create a nice dataframe with each row being a step taken during the simulations, i.e. another batchSize and the result.

# The created dataframe can then be used to do whatever summary statistics you want to do on it.

library(data.table)

saveDF <- T

folder <- 'try_1'

filename <- paste(
        './_rslurm_', 
        folder,
        '/results_0.RDS',sep = '')

saveName <- paste(
        './_rslurm_',
        folder,
        '/sims_preprocessed.RData',
        sep = '')

tempList <- import(filename)

sims_preprocessed <- rbindlist(tempList, idcol = 'id')

if (saveDF){
        save(sims_preprocessed, file = saveName)
}