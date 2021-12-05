# This script will:

# 1. Move the folder that slurm created in the working directory.This is done so that the working directory doesn't get cluttered.
# 2. Preprocess the output of slurm and create a nice dataframe with each row being a step taken during the simulations, i.e. another batchSize and the result.

# The created dataframe can then be used to do whatever summary statistics you want to do on it.

# Setup global parameters #####################################################
pacman::p_load(data.table,
               tidyverse,
               rio)

saveDF <- TRUE

folder <- 'try_1'

filename <- paste('./data/_rslurm_', 
        folder,
        '/results_0.RDS',sep = '')

saveFolder <- file.path('./analysis_results/preprocessing',
                        folder)

saveName <- paste(saveFolder,
                  '/sims_preprocessed.RData',
                  sep = '')

# If the save directory doesn't exist, create it
ifelse(!dir.exists(saveFolder), dir.create(saveFolder, recursive = T), 'Save directory already exists!')


# First, move the slurm output ################################################
# from the working directory to the ./data dir ####

old_path <- paste('./_rslurm_',folder,sep = '')
new_path <- file.path('./data',folder)

# Create the new path if it doesn't exist
ifelse(!dir.exists(new_path), dir.create(new_path, recursive = T), '')

# Get the list of files
current_files = list.files(old_path, full.names = TRUE)

# Copy them
file.copy(from = current_files, 
          to = new_path, 
          overwrite = TRUE, 
          recursive = FALSE, 
          copy.mode = TRUE,
          copy.date = TRUE)

# Now delete the original data
unlink(old_path, recursive = TRUE)

# Import the file and concatenate #############################################
tempList <- import(filename)

sims_preprocessed <- rbindlist(tempList, idcol = 'id')

# Save the file #####################################################
if (saveDF){
        save(sims_preprocessed, file = saveName)
}