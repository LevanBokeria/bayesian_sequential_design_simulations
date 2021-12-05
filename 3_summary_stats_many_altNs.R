# This script will load the data frame that is a result of simulations.
# Then, instead of calculating the stats on the max N that was indicated in the
# original simulations,
# this function can calculate stats many hypothetical max Ns specified.

# Clear the environment, load libraries ######################################
rm(list=ls())

# Libraries
pacman::p_load(data.table,
               tidyverse,
               rio)

# Define global variables ###################################################

# Variables below specify which simulation file will be read for analysis
# nIterEv <- 10000 # maybe the original simulation ran 10,000, but we want less?
# d1      <- 0.5
# d1_str  <- '05'
# nLimit  <- 240
# crit1   <- 10
# crit2   <- 1/10
# minN    <- 24
# 
# # What are the various maxNs we want to analyze?
nFrom <- 24
nTo   <- 396
nBy   <- 12
altNs <- seq(nFrom,nTo,by = nBy)
# 
# # Flags
# saveOutData <- T
# 
# 
# # Start main script ############################################################
# 
# loadFile  <- paste('rslurm_raw_and_preprocessed/',
#                    '_rslurm_d1_',
#                    d1_str,'_limpg_',nLimit, '_crit1_', crit1,
#                    '_minN_', minN, '_batchSize_', nBy,
#                    '/simulationResults_',d1_str,'_limpg_',nLimit, 
#                    '_crit1_', crit1,
#                    '_minN_', minN, '_batchSize_', nBy,
#                    '.RData',sep='')
# 
# # load data
# load(loadFile)


sims_preprocessed <- import('./_rslurm_try_1/sims_preprocessed.RData')

# How many unique combinations of factors do we have? For each, we'll have to do the summary stats separately
unique_combs <- sims_preprocessed %>%
        select(minN,d,crit1,crit2,batchSize,limit,test_type,side_type) %>%
        distinct()

n_combs <- nrow(unique_combs)

## Get the probabilities ========================================

outdf = list()

for (iComb in seq(1,nrow(unique_combs))){
        
        print(unique_combs[iComb,])
        
        tempDF <- sims_preprocessed %>%
                filter(d == unique_combs$d[iComb],
                       minN == unique_combs$minN[iComb],
                       crit1 == unique_combs$crit1[iComb],
                       crit2 == unique_combs$crit2[iComb],
                       batchSize == unique_combs$batchSize[iComb],
                       limit == unique_combs$limit[iComb],
                       test_type == unique_combs$test_type[iComb],
                       side_type == unique_combs$side_type[iComb])  
        
        
        for (iN in altNs){
                print(iN)
                
                outdf[[length(outdf)+1]] <- tempDF %>%
                        filter(n <= iN) %>% 
                        group_by(id) %>%
                        slice_tail() %>%
                        mutate(altMaxN = iN)
        }      
        
}


outdfbinded <- rbindlist(outdf, idcol = NULL)

outdfbinded <- outdfbinded %>% 
        mutate(bf_status = as.factor(
                case_when(
                bf >= crit1 ~ 'H1',
                bf <= crit2 ~ 'H0',
                TRUE ~ 'undecided'
        )))

# Now summarize whichever way you want
sumstats <- 
        outdfbinded %>%
        group_by(minN,
                 d,
                 crit1,
                 crit2,
                 batchSize,
                 limit,
                 test_type,
                 side_type,
                 altMaxN,
                 bf_status) %>%
        dplyr::summarise(n = n())
        # pivot_wider(id_cols = c(
        #         minN,
        #         d,
        #         crit1,
        #         crit2,
        #         batchSize,
        #         limit,
        #         test_type,
        #         side_type,
        #         altMaxN
        #         ),
        #         names_from = bf_status,
        #         values_from = n,
        #         names_prefix = 'supports_') %>% View()


## Plot results ================================================================

sumstats %>%
        ggplot(aes(x=altMaxN,
                   y=n,
                   group=bf_status,
                   color=bf_status)) +
        geom_line() +
        geom_point() +
        facet_grid(d~test_type)



## Save outData ================================================================

# saveNameOutData <- paste('analysis_output/resultsByManyNs_d_', d1_str,
#                          '_crit1_', crit1, '_',
#                          altNs[1], '_to_', altNs[length(altNs)],
#                          '_by_', nBy,
#                          '.RData',sep='')
# 
# if (saveOutData){
#         save(outData_d0, outData_d1, file = saveNameOutData)
# }


