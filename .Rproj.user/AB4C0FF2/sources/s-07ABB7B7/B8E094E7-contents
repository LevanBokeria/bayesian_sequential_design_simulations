# Description:

# This is the main script where you setup simulation parameters, and they get
# passed to slurm to perform fast computation.

# After slurm is done with the results, use script getRSlurmResults.R to extract
# results into a readable format.

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

# Number of participants per group, because we're running 
# between participant comparison.
nLimit        <- 200 

d0        <- 0.0
d1        <- 0.5
crit1     <- 6
crit2     <- 1/6
batchSize <- 16
minN      <- 24

# Name for saving folder
saveFolder <- paste('d1_', d1, '_limpg_', nLimit,
                    '_crit1_', crit1, '_minN_', minN,
                    '_batchSize_', batchSize, sep='')

# Submit slurm job?
submitJob = TRUE

# Function
helperfunction <- function(minN, d, crit1, crit2, batchSize, limit){
  bf        <- c()
  results   <- list()
  
  # Create minium sample and calculate BF
  i      <- 1
  n      <- as.numeric(minN)
  dataG1 <- rnorm(n, 0, 1)
  dataG2 <- rnorm(n, d, 1)
  bf     <- reportBF(ttestBF(dataG1, dataG2, paired = TRUE,
                             nullInterval = c(-Inf, 0)))
  
  # bf     <- reportBF(ttestBF(dataG1, dataG2, paired = FALSE), 4)
  
  # Within simulation loop
  while(bf[length(bf)] < crit1 & bf[length(bf)] > crit2 & n < limit){
    n         <- n + batchSize
    dataG1      <- c(dataG1, rnorm(batchSize, 0, 1))
    dataG2      <- c(dataG2, rnorm(batchSize, d, 1))
    bf[i + 1] <- reportBF(ttestBF(dataG1, dataG2, paired = FALSE), 4)
    i         <- i + 1
  }
  
  # Return results
  results$d         <- d
  results$n         <- n
  results$bf        <- bf
  results$crit1     <- crit1
  results$crit2     <- crit2
  results$batchSize <- batchSize
  results$limit     <- limit
  return(results)
}

#minN, d, crit1, crit2, batchSize, side
# Parameters
params      <- data.frame(minN      = rep(minN, nIter*2),
                          d         = c(rep(d0, nIter), rep(d1, nIter)),
                          crit1     = rep(crit1, nIter*2),
                          crit2     = rep(crit2, nIter*2),
                          batchSize = rep(batchSize, nIter*2),
                          limit     = rep(nLimit, nIter*2))


# Create job
sjob1 <- slurm_apply(helperfunction, params, jobname = saveFolder,
                     nodes = n_nodes, cpus_per_node = cpus_per_node, 
                     submit = submitJob)

