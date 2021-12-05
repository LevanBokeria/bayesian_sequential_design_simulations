# This script will plot the power by various max N per group 

# Clear the environment and load libraries, etc ###############################
rm(list=ls())

# Libraries
library(ggplot2)
library(plyr)
library(tidyverse)
library(rio)

# Define global variables ####################################################

nIterEv <- 10000
d1      <- 0.5
d1_str  <- '05'
nLimit  <- 240
crit1   <- 10
crit2   <- 1/10

nFrom <- 24
nTo   <- 240
nBy   <- 12

altNs   <- seq(nFrom,nTo,by = nBy)

saveFig <- F

# Load the file
loadFile  <- paste('analysis_output/',
                   'resultsByManyNs_d_',
                   d1_str,
                   '_crit1_', crit1, '_',
                   altNs[1], '_to_', altNs[length(altNs)],
                   '_by_', nBy,
                   '.RData',sep='')

load(loadFile)

# Get all the data in one dataframe
all_data <- rbind(outData_d1$outStats,outData_d0$outStats)

# Start plotting #############################################################
p1 <- 
    all_data %>%
    pivot_longer(c('H1','H0','und'),names_to = 'supports',values_to = 'n_sim') %>% 
    mutate(supports = as.factor(supports),
           d = as.factor(d),
           percent_support = n_sim/100) %>%
    ggplot(aes(x=maxN,y=percent_support,fill=supports,color=supports)) +
    geom_line() +
    geom_point() + 
    scale_x_continuous(breaks=seq(nFrom,nTo,nBy)) +
    scale_y_continuous(breaks=seq(0,100,10)) +  
    ylab('% of simulations') +
    xlab('max N per group') + 
    facet_wrap(~d, labeller = label_both) + 
    ggtitle(paste('BF10=',crit1,', BF01=',1/crit2,sep=''))

print(p1)
