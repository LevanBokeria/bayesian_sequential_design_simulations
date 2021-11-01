# This script will load the data frame that is a result of simulations.
# Then, instead of calculating the stats on the max N that was indicated in the
# original simulations,
# this function can calculate stats many hypothetical max Ns specified.

# Clear the environment, load libraries ######################################
rm(list=ls())

# Libraries
library(plyr)
library(assortedRFunctions)
library(tidyverse)

# Define global variables ###################################################

# Variables below specify which simulation file will be read for analysis
nIterEv <- 10000 # maybe the original simulation ran 10,000, but we want less?
d1      <- 0.5
d1_str  <- '05'
nLimit  <- 200
crit1   <- 6
crit2   <- 1/6
minN    <- 24

# What are the various maxNs we want to analyze?
nFrom <- 24 
nTo   <- 200 
nBy   <- 8
altNs <- seq(nFrom,nTo,by = nBy)

# Flags
saveOutData <- T


# Start main script ############################################################

loadFile  <- paste('rslurm_raw_and_preprocessed/',
                   '_rslurm_d1_',
                   d1_str,'_limpg_',nLimit, '_crit1_', crit1,
                   '_minN_', minN, '_batchSize_', nBy,
                   '/simulationResults_',d1_str,'_limpg_',nLimit, 
                   '_crit1_', crit1,
                   '_minN_', minN, '_batchSize_', nBy,
                   '.RData',sep='')

# load data
load(loadFile)

## Construct a function to get stats from simulations ==========================
getStats <- function(data){
    
    # What IDs do we have?
    simIDs <- unique(data$id)
    
    # How many IDs are we going to analyze?
    simIDAnalyzed <- simIDs[1:nIterEv]
    
    # Whats the effect size?
    iEffect <- data$d[1]
    
    print(paste('d=',iEffect,sep=''))
    
    # Create a data frame to report the stats
    outStats <- data.frame(d    = rep(iEffect,length(altNs)),
                           maxN = numeric(length(altNs)),
                           H0   = numeric(length(altNs)),
                           H1   = numeric(length(altNs)),
                           und  = numeric(length(altNs)))  
    
    # Add row indices
    data$rowIdx <- 1:nrow(data)
    
    # Create a data frame to store results of simulations for various max N
    bf_maxN        <- data.frame(rowIdx = 1:nIterEv)
    bf_maxN_status <- data.frame(rowIdx = 1:nIterEv)
    
    for (iN in altNs){
        iStr                 <- paste('maxN',iN,sep='_')
        bf_maxN[iStr]        <- numeric(nIterEv)
        bf_maxN_status[iStr] <- rep('undecided',nIterEv)
    }
    
    counterAltN <- 1
    
    # For each maxN, get stats for supporting H0 or H1
    for (iN in altNs){
        
        print(paste('iN=',iN,sep=''))
        
        colName <- paste('maxN',iN,sep='_')
        
        counterID <- 1
        for (iID in simIDAnalyzed){
            
            if (iID %% 500 == 0){
                print(paste('d=', iEffect,' iN=',iN,' iID=',iID,sep=''))
            }
            
            getRow <- data[data$n <= iN & 
                                 data$id == iID,]$rowIdx
            getRow <- getRow[length(getRow)]
            
            bf_maxN[colName][counterID,] <- data$bf[getRow]
            
            counterID <- counterID + 1
            
        }
        
        # Now, record the status
        logIdxH1 <- bf_maxN[colName] > crit1
        logIdxH0 <- bf_maxN[colName] < crit2
        
        bf_maxN_status[colName][logIdxH1,] <- 'H1'
        bf_maxN_status[colName][logIdxH0,] <- 'H0'
        
        # Record in outStats
        outStats$maxN[counterAltN]   <- iN
        outStats$H0[counterAltN]  <- sum(bf_maxN_status[colName] == 'H0')
        outStats$H1[counterAltN]  <- sum(bf_maxN_status[colName] == 'H1')
        outStats$und[counterAltN] <- sum(bf_maxN_status[colName] == 'undecided')
        
        # increment counter
        counterAltN <- counterAltN + 1
        
    } # for iN in altN
    
    return(list('outStats'=outStats,
                'bf_maxN'=bf_maxN,
                'bf_maxN_status'=bf_maxN_status))
    
} # function getStats

## Get the probabilities for d0 and d1 ========================================
outData_d0 <- df %>%
    subset(d == 0) %>%
    getStats()

outData_d1 <- df %>%
    subset(d == d1) %>%
    getStats()

## Save outData ================================================================

saveNameOutData <- paste('analysis_output/resultsByManyNs_d_', d1_str,
                         '_crit1_', crit1, '_',
                         altNs[1], '_to_', altNs[length(altNs)],
                         '_by_', nBy,
                         '.RData',sep='')

if (saveOutData){
  save(outData_d0, outData_d1, file = saveNameOutData)
}


