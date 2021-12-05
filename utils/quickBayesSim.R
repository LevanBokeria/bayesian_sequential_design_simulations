# A script to just quickly see probabilities of supporting H1 and H0 dependent 
# on sample size, effect size, etc.

library(BayesFactor)
library(assortedRFunctions)


# Clean the environment
rm(list=ls())

# Specify parameters ###########################################################
nSim <- 10000
d1 <- 0
nMax <- 56
bf <- c()

crit1 <- 6
crit2 <- 1/6

# Start the for loop ##########################################################
for(i in 1:nSim){
    
    if (i %% 500 == 0){
        print(i)
    }
    
    bf[i] <- reportBF(ttestBF(rnorm(nMax), rnorm(nMax, d1), paired = FALSE))
}
bf10 <- mean(bf > crit1)
bf01 <- mean(bf < crit2)

save(bf,bf10,bf01,file=paste('./bayesian_sequential_design/quickSimResults/d_',d1,'_nMax_',nMax,'.RData',sep=''))
