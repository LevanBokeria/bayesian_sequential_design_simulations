# Description

# Function to calculate how many simulations supported H1, H0 or were undecided.
getStats <- function(data,crit1,crit2,nIter,d1_str,d1,nLimit){
        
        if(missing(data)){
                
                loadFile  <- paste('rslurm_raw_and_preprocessed/',
                                   '_rslurm_d1_',
                                   d1_str,'_limpg_',nLimit, '_crit1_', crit1, 
                                   '_minN_', minN,
                                   '_batchSize_', batchSize,
                                   '/simulationResults_',d1_str,'_limpg_',
                                   nLimit, '_crit1_', crit1, 
                                   '_minN_', minN, '_batchSize_', batchSize,
                                   '.RData',sep='')
                
                # load data
                data <- import(loadFile)
                
        }
        
        # Process for d = 0
        tempDF <- subset(data, d == 0.0)
        
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
        
        
        # Process for d = d1
        tempDF <- subset(data, d == d1)
        
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
        
        # Create a table to record all the data in one place
        outData <- c(length(d0_H0$id),length(d0_H1$id),length(d0_undecided$id),
                     length(d1_H0$id),length(d1_H1$id),length(d1_undecided$id))
        
        names(outData) <- c('d0_H0','d0_H1','d0_undecided','d1_H0','d1_H1','d1_undecided')
        
        outData <- as_tibble(as.list(outData))
        
        return(outData)
}
