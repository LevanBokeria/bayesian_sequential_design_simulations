# This script will:

# 1. Move the folder that slurm created in the working directory.This is done so
# that the working directory doesn't get cluttered.
# 2. Preprocess the output of slurm and create a nice dataframe with each row 
# being a step taken during the simulations, i.e. another batchSize and the result.

# The created dataframe can then be used to do whatever summary statistics you
# want to do on it.

# Libraries #####################################################
pacman::p_load(data.table,
               tidyverse,
               rio)

# Flags, folder names, etc #####################################################

# Flag to save the data
saveDF <- TRUE

# This name should correspond to the name used in 1_simulation_parameters.R script
# So that the correct files are loaded and preprocessed.
folder <- 'try_2'

# The new folder where to save the preprocessed data
saveFolder <- file.path('./analysis_results',
                        folder)

# Name of the preprocessed data file.
saveName <- paste(saveFolder,
                  '/sims_preprocessed.RData',
                  sep = '')

# If the save directory doesn't exist, create it
ifelse(!dir.exists(saveFolder), 
       dir.create(saveFolder, recursive = T), 'Save directory already exists!')

# Name of the file that has the simulation output
filename <- paste('./data/',
                  folder,
                  '/results_0.RDS',
                  sep = '')

# First, move the slurm output ################################################
# from the working directory to the ./data dir
# This is done so that the working directory isn't cluttered with slurm output

old_path <- paste('./_rslurm_',folder,sep = '')
new_path <- file.path('./data',folder)

# Create the new path if it doesn't exist
ifelse(!dir.exists(new_path),
       dir.create(new_path, recursive = T),
       '')

# Get the list of files
current_files = list.files(old_path, full.names = TRUE)

# Check that results_0.RDS is part of the files! If its not, then slurm isnt'
# done with the simulations. Alert the user to wait.
if (file.path(old_path,'results_0.RDS') %in% current_files){

        print('Moving the files...')
        
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
        
} else {
        
        print('Could not find the results_0.RDS file.')
        print('This probably means that the simulations are not yet finished.')
        print('Or they have already been moved to the ./data folder')        
        
}

