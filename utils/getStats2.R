# Description

# Function to calculate how many simulations supported H1, H0 or were undecided.


sims_preprocessed <- import('./_rslurm_try_1/sims_preprocessed.RData')

# How many unique combinations of factors do we have? For each, we'll have to do the summary stats separately
unique_combs <- sims_preprocessed %>%
        select(d,crit1,crit2,batchSize,limit,test_type,side_type) %>%
        distinct()

n_combs <- nrow(unique_combs)
        
for (iComb in seq(1,n_combs)){
        
        current_params <- unique_combs[iComb,]
        
        
        tempDF <- sims_preprocessed %>%
                filter(d == unique_combs$d[iComb],
                       crit1 == unique_combs$crit1[iComb],
                       crit2 == unique_combs$crit2[iComb],
                       batchSize == unique_combs$batchSize[iComb],
                       limit == unique_combs$limit[iComb],
                       test_type == unique_combs$test_type[iComb],
                       side_type == unique_combs$side_type[iComb])
        
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
        
        }
        return(outData)
}
