# Description:

# This is the main script where you setup simulation parameters, and they get
# passed to slurm to perform fast computation.

# After slurm is done with the results, use script getRSlurmResults.R to extract
# results into a readable format.

# Clear the environment
rm(list=ls())

# Setting seed
set.seed(911225)

# Libraries
library(rslurm)
library(BayesFactor)
library(assortedRFunctions)
library(tidyverse)

# Job parameters
n_nodes       <- 1
cpus_per_node <- 16
nIter         <- 10000

# Number of participants 
nLimit    <- 72


d         <- c(0.5,1)

crit1     <- c(10)

crit2     <- c(1/10)

batchSize <- c(12)
minN      <- 24

# What type of test is it?
test_types <- c('unpaired','paired')
side_types <- c('two_tailed')

# Name for saving folder
saveFolder <- 'try_1'

# Submit the slurm job?
submitJob <- F

# Simulate locally?
simLocal <- T

# Function
helperfunction <- function(minN, d, crit1, crit2, batchSize, limit, 
                           test_type, side_type){

        bf      <- c()
        results <- data.frame()
        i       <- 1
        n       <- as.numeric(minN)        
        
        # Is this one-sided or two sided
        if (side_type == 'two_tailed'){
                null_interval <- NULL
        } else if (side_type == 'one_tailed'){
                null_interval <- C(0,Inf)
        }
        
        # Is this paired or unpaired?
        if (test_type == 'unpaired'){
                
                dataG1 <- rnorm(n, 0, 1)
                dataG2 <- rnorm(n, d, 1)

                #Calculate the initial bf
                bf <- reportBF(ttestBF(
                        dataG1,
                        dataG2,
                        nullInterval = null_interval
                )[1],4)
                
        } else if (test_type == 'paired'){
                
                dataG1 <- rnorm(n,d,1)
                
                #Calculate the initial bf
                bf <- reportBF(ttestBF(
                        dataG1,
                        nullInterval = null_interval
                )[1],4)                
                
        }
        
        # Within simulation loop
        while(bf[length(bf)] < crit1 & bf[length(bf)] > crit2 & n < limit){
                
                n         <- n + batchSize
                
                
                if (test_type == 'unpaired'){
                        
                        dataG1      <- c(dataG1, rnorm(batchSize, 0, 1))
                        dataG2      <- c(dataG2, rnorm(batchSize, d, 1))
                        
                        bf[i + 1] <- reportBF(ttestBF(
                                dataG1, 
                                dataG2,
                                nullInterval = null_interval
                                )[1],4)
                        
                } else if (test_type == 'paired'){
                        dataG1    <- c(dataG1, rnorm(batchSize, d, 1))
                        
                        
                        bf[i + 1] <- reportBF(ttestBF(
                                dataG1,
                                nullInterval = null_interval
                                )[1],4)
                }
        
                i <- i + 1
        }

        results <- as.data.frame(bf)  
        
        # Return results
        results$minN      <- minN
        results$d         <- d
        results$n         <- seq(minN,n,batchSize)
        results$crit1     <- crit1
        results$crit2     <- crit2
        results$batchSize <- batchSize
        results$limit     <- limit
        results$test_type <- test_type
        results$side_type <- side_type
        return(results)
}

# Parameters

# First, create all combinations
cart_prod <- expand.grid(minN,
                     d,
                     crit1,
                     crit2,
                     batchSize,
                     nLimit,
                     test_types,
                     side_types)
names(cart_prod) <- c('minN','d','crit1','crit2','batchSize','limit','test_type','side_type')

# Now, repeat each of these combinations nIter times
params <- cart_prod %>% 
        slice(rep(1:n(), each=nIter)) %>%
        mutate(across(c('test_type','side_type'),as.character))


# Try locally once
# results <- helperfunction(params[1,1],params[1,2],params[1,3],params[1,4],params[1,5],params[1,6],params[1,7],params[1,8])

# Try locally for every row
if (simLocal){
        results <- do.call(Map, c(f=helperfunction,params))
        saveRDS(results, file = paste(
                './_rslurm_', 
                saveFolder,
                '/results_0.RDS',sep = ''))
}

# Create job
sjob1 <- slurm_apply(helperfunction,
                     params, 
                     jobname = saveFolder,
                     nodes = n_nodes, 
                     cpus_per_node = cpus_per_node, 
                     submit = submitJob)

