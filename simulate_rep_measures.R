# ----------------------------- General stuff ----------------------------------
# */
# Setting seed
set.seed(911225)

# Set library path
# .libPaths('/home/lb08/R/x86_64-pc-linux-gnu-library/3.6')

# Libraries
library(rslurm)
library(BayesFactor)
library(assortedRFunctions)
library(tidyverse)
library(MASS)

# Job parameters
n_nodes       <- 1
cpus_per_node <- 16
nIter         <- 10000
nLimit        <- 45 # Number of participants per group, because we're running between participant comparison.

# Setting parameters
d0          <- 0.0
d1          <- 0.8

r_scale     <- 'wide' # here r_scale=1, standard Cauchy prior

# Since we're simulating for a 2x2 ANOVA with one repeated measure, we'll generate 
# for each group data from a multivariate Gaussian distribution with a certain 
# covariance matrix specifying the correlation between repeated measures. 
# Then, taking a difference between the two will give the final two distributions
# on which to run ttestBF.
repCorr <- 0.5 # correlation between the repeated measures. 

# Name for saving folder
saveFolder <- paste('SeqDesWithtLimit_d1_', d1, 
                    '_limpg_', nLimit, '_rscale_',r_scale, '_repCorr', repCorr,
                    sep='')

# Submit slurm job?
submitJob = FALSE

# /*
# ----------------------------- Sequential design with limit ---------------------------
# */
# Function
helperfunction <- function(minN, d, crit1, crit2, batchSize, limit, r_scale, repCorr){
  bf        <- c()
  results   <- list()
  
  # Create minium sample and calculate BF
  i      <- 1
  n      <- as.numeric(minN)
  
  # The covariance matrix.
  cov_mat <- matrix(c(1,repCorr,repCorr,1),2,2) 
  
  
  # Create repeated measures.
  
  dataG1_reps <- mvrnorm(n,c(0,0),cov_mat, empirical = FALSE)
  dataG2_reps <- mvrnorm(n,c(0,d),cov_mat, empirical = FALSE)

  # Get difference between the repeated measures
  dataG1 <- dataG1_reps[,2] - dataG1_reps[,1]
  dataG2 <- dataG2_reps[,2] - dataG2_reps[,1]
  bf     <- reportBF(ttestBF(dataG1,dataG2, rscale = r_scale, paired = FALSE), 4)
  
  # Within simulation loop
  while(bf[length(bf)] < crit1 & bf[length(bf)] > crit2 & n < limit){
    n         <- n + batchSize
    dataG1      <- c(dataG1, rnorm(batchSize, 0, 1))
    dataG2      <- c(dataG2, rnorm(batchSize, d, 1))
    bf[i + 1] <- reportBF(ttestBF(dataG1,dataG2, rscale = r_scale, paired = FALSE), 4)
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
params      <- data.frame(minN      = rep(10, nIter*2),
                          d         = c(rep(d0, nIter), rep(d1, nIter)),
                          crit1     = rep(10, nIter*2),
                          crit2     = rep(1/6, nIter*2),
                          batchSize = rep(5, nIter*2),
                          limit     = rep(nLimit, nIter*2),
                          r_scale   = rep(r_scale, nIter*2),
                          repCorr   = rep(repCorr, nIter*2))

helperfunction(10,0,10,1/6,5,45,'wide',0.5)

# Create job
# sjob1 <- slurm_apply(helperfunction, params, jobname = saveFolder,
                     # nodes = n_nodes, cpus_per_node = cpus_per_node, submit = submitJob)

