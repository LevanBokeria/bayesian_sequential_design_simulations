
# Clean the environment and source the subfunctions ###########################
rm(list=ls())

# Source the functions
source('./2_move_slurm_output.R')
source('./3_preprocess_output.R')
source('./4_summary_stats_many_altNs.R')
source('./5_plot_results.R')

# Define parameters ###########################################################
saveData <- TRUE

# For the summary stats
nFrom <- 24
nTo   <- 600
nBy   <- 12

# Folder where the slurm output is
folder <- 'results_1'

# Now, call each function ######################################################

## 2_move_slurm_output --------------------------------------------------------
move_slurm_output(saveData,folder)

## 3_preprocess_output --------------------------------------------------------
sims_preprocessed <- preprocess_output(saveData,folder)

## 4_summary_stats_many_altNs -------------------------------------------------
power_table <- summary_stats(saveData,nFrom,nTo,nBy,folder)

## 5_plot_results -------------------------------------------------------------
plot_results(folder)