# This script will load the data frame that is a result of simulations.
# Then, instead of calculating the stats on the max N that was indicated in the original simulations,
# this function can calculate stats for a hypothetical max N specified.

# Clear the environment
rm(list=ls())

# Libraries
library(ggplot2)
library(gganimate)
library(gapminder)
library(gridExtra)
library(grid)
library(plyr)
library(BayesFactor)
library(assortedRFunctions)
library(tidyverse)

# Define global variables

nIter   <- 10000
d1      <- 0.2
d1_str  <- '02'
nLimit  <- 150
crit1   <- 10
crit2   <- 1/6

altN    <- 95

saveDF <- FALSE
saveOutData <- FALSE

# Construct a function to get stats from simulations 
loadFile  <- paste('_rslurm_d1_',
                   d1_str,'_limpg_',nLimit, '_crit1_', crit1,
                   '/simulationResults_',d1_str,'_limpg_',nLimit, '_crit1_', crit1,
                   '.RData',sep='')

# load data
load(loadFile)

# Consider an alternative max N, subset the df:
df3 <- df3[df3$n <= altN,]

# /* 
# ----------------------------- Sequential design with limit 1---------------------------
# */

tempDF <- subset(df3, d == 0.0)

tempDF$trans_bf <- NA
tempDF$trans_bf[tempDF$bf < 1] <- -1/tempDF$bf[tempDF$bf < 1] + 1
tempDF$trans_bf[tempDF$bf > 1] <- tempDF$bf[tempDF$bf > 1] - 1

tempDF_agg <- ddply(tempDF, c('id'), summarise, n = n[length(n)], bf = bf[length(bf)])
tempDF_agg$support <- 'undecided'
tempDF_agg$support[tempDF_agg$bf > crit1] <- 'H1'
tempDF_agg$support[tempDF_agg$bf < crit2] <- 'H0'

d0_undecided <- tempDF_agg[tempDF_agg$support == 'undecided',]
d0_H1 <- tempDF_agg[tempDF_agg$support == 'H1',]
d0_H0 <- tempDF_agg[tempDF_agg$support == 'H0',]

# /*
# ----------------------------- Sequential design with limit 2---------------------------
# */

tempDF <- subset(df3, d == d1)

tempDF$trans_bf <- NA
tempDF$trans_bf[tempDF$bf < 1] <- -1/tempDF$bf[tempDF$bf < 1] + 1
tempDF$trans_bf[tempDF$bf > 1] <- tempDF$bf[tempDF$bf > 1] - 1

tempDF_agg <- ddply(tempDF, c('id'), summarise, n = n[length(n)], bf = bf[length(bf)])
tempDF_agg$support <- 'undecided'
tempDF_agg$support[tempDF_agg$bf > crit1] <- 'H1'
tempDF_agg$support[tempDF_agg$bf < crit2] <- 'H0'

d1_undecided <- tempDF_agg[tempDF_agg$support == 'undecided',]
d1_H1        <- tempDF_agg[tempDF_agg$support == 'H1',]
d1_H0        <- tempDF_agg[tempDF_agg$support == 'H0',]

outData <- c(length(d0_H0$id),length(d0_H1$id),length(d0_undecided$id),
             length(d1_H0$id),length(d1_H1$id),length(d1_undecided$id))

names(outData) <- c('d0_H0','d0_H1','d0_undecided','d1_H0','d1_H1','d1_undecided')

outData <- as_tibble(as.list(outData))


# Save outData
loadFolder <- paste('_rslurm_d1_',d1_str,'_limpg_',
                    nLimit, '_crit1_', crit1, sep='')
saveNameOutData   <- paste(loadFolder, '/outData.RData',sep='')

if (saveOutData){
  save(outData, file = saveNameOutData)
}


