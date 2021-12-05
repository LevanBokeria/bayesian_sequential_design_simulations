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

# Save the resulting summary statistics datafile?
saveOutData <- FALSE

# # What are the various maxNs we want to analyze?
nFrom <- 24
nTo   <- 396
nBy   <- 12
altNs <- seq(nFrom,nTo,by = nBy)

# Which preprocessed data to load?
folderName <- 'try_1'

# Load the data and get unique factor combinations ############################
sims_preprocessed <- import(file.path(
        './analysis_results/preprocessing',folderName,'sims_preprocessed.RData')
        )

# How many unique combinations of factors do we have? 
# For each, we'll have to do the summary stats separately
unique_combs <- sims_preprocessed %>%
        select(minN,d,crit1,crit2,batchSize,limit,test_type,side_type) %>%
        distinct()

n_combs <- nrow(unique_combs)

# Get the probabilities #####################################################

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
        
        # Maybe a bit faster?
        # for (iN in altNs){
        #         print(iN)
        #         
        #         outdf[[length(outdf)+1]] <- sims_preprocessed %>%
        #                 filter(d == unique_combs$d[iComb],
        #                        minN == unique_combs$minN[iComb],
        #                        crit1 == unique_combs$crit1[iComb],
        #                        crit2 == unique_combs$crit2[iComb],
        #                        batchSize == unique_combs$batchSize[iComb],
        #                        limit == unique_combs$limit[iComb],
        #                        test_type == unique_combs$test_type[iComb],
        #                        side_type == unique_combs$side_type[iComb]) %>%
        #                 filter(n <= iN) %>% 
        #                 group_by(id) %>%
        #                 slice_tail() %>%
        #                 mutate(altMaxN = iN)
        # }              
        
        
}

# Concatenate into one dataframe
outdfbinded <- rbindlist(outdf, idcol = NULL)

# Classify what the bf supported
outdfbinded <- outdfbinded %>% 
        mutate(bf_status = as.factor(
                case_when(
                bf >= crit1 ~ 'H1',
                bf <= crit2 ~ 'H0',
                TRUE ~ 'undecided'
        )))

# Summary statistics ########################################################
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
        dplyr::summarise(n_simulations = n())
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



# Save the data ###############################################################

saveNameOutData <- file.path('./analysis_results/preprocessing',
                             folderName,
                             'sumstats.RData')

if (saveOutData){
        save(sumstats, file = saveNameOutData)
}


