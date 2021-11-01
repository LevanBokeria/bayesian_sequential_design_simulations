# Description ################################################################

# This script is a wrapper to get bunch of simulation results and stats quickly 
# in a table.

# It will take the rslurm output, turn it into a nice dataframe (called df3), 
# and then analyze probabilities of supporting H1 and H0.

# Global setup ################################################################

# Clear the environment
rm(list=ls())

# Libraries
library(plyr)
library(BayesFactor)
library(assortedRFunctions)
library(tidyverse)
library(glue)
library(rio)

source('./utils/getRslurmResults.R')
source('./utils/getStats.R')

# Define global variables
nIter   <- 10000
d1      <- 0.5
d1_str  <- '05'
nLimit  <- 208
crit1   <- 6
crit2   <- 1/6
minN    <- 32
batchSize <- 16


readDF <- F # if DF was saved previously, just read it without recreating it

if (readDF){
        saveDF <- F # should df be saved? If already done, assign FALSE        
} else {
        saveDF <- T
}

saveOutData <- T # save the resulting table containing probabilities of H1 and H0?

# Call the function to construct a dataframe from slurm results ################
df <- getRslurmResults(nIter,d1_str,nLimit,crit1,saveDF,readDF)

# Call the function to get the probabilities data ##############################
outData <- getStats(df,crit1,crit2,nIter,d1_str,d1,nLimit)


# Save outData ################################################################
loadFolder <- paste('rslurm_raw_and_preprocessed/',
                    '_rslurm_d1_',d1_str,'_limpg_',
                    nLimit, '_crit1_', crit1, '_minN_', minN,
                    '_batchSize_', batchSize, sep='')

saveNameOutData   <- paste(loadFolder, '/outData.RData',sep='')

if (saveOutData){
  save(outData, file = saveNameOutData)
}


