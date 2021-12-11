# This script will load the preprocessed dataframe from 2_preprocess_slurm_output.R
# It will then calculate the statistics on supporting H1/H0/undecided for many 
# alternative maxN stopping rules. 

# Input:
# - sims_preprocessed.RData: 

# Output:
# - power_table: 

# Libraries ######################################
rm(list=ls())

# Libraries
pacman::p_load(data.table,
               tidyverse,
               rio)

# Define the parameters and flags #############################################

# Save the resulting summary statistics datafile?
saveData <- T

# What are the various maxNs we want to analyze?
# nFrom and nBy must match what was given to the simulation script.
# nTo can be different than the maxN that was given to the original simulation job,
# but it cannot be larger than it.
nFrom <- 24
nTo   <- 456
nBy   <- 12
altNs <- seq(nFrom,nTo,by = nBy)

# Which preprocessed data to load?
# This must correspond to where the simulation job was saved.
folderName <- 'try_1'

# Load the data and get unique factor combinations ############################
sims_preprocessed <- import(file.path(
        './analysis_results',folderName,'sims_preprocessed.RData')
        )

# How many unique combinations of factors do we have? 
# For each, we'll have to do the summary stats separately
unique_combs <- sims_preprocessed %>%
        select(minN,d,crit1,crit2,batchSize,limit,test_type,side_type) %>%
        distinct()

n_combs <- nrow(unique_combs)

print(paste('There are ', 
            n_combs, 
            ' unique combinations of factors. They are:',
            sep=''))
print(unique_combs)

# Get the probabilities #####################################################
# Of supporting H1 or H0 or neither

outdf = list()

# For each combination of simulation parameters:
for (iComb in seq(1,nrow(unique_combs))){
        
        print(unique_combs[iComb,])
        
        # From the overall dataframe, select only the part that belongs to 
        # the simulation with the current combination of parameters
        tempDF <- sims_preprocessed %>%
                filter(d == unique_combs$d[iComb],
                       minN == unique_combs$minN[iComb],
                       crit1 == unique_combs$crit1[iComb],
                       crit2 == unique_combs$crit2[iComb],
                       batchSize == unique_combs$batchSize[iComb],
                       limit == unique_combs$limit[iComb],
                       test_type == unique_combs$test_type[iComb],
                       side_type == unique_combs$side_type[iComb])

        # For each alternative maxN stopping rule:
        for (iN in altNs){
                print(iN)

                outdf[[length(outdf)+1]] <- tempDF %>%
                        filter(n <= iN) %>%
                        group_by(id) %>%
                        slice_tail() %>%
                        mutate(altMaxN = iN)
        }
        
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

# How many iterations were given to the original simulation job? (nIter variable)
nIter <- sims_preprocessed %>% 
        distinct(id, .keep_all = T) %>%
        group_by(minN,d,crit1,crit2,batchSize,limit,test_type,side_type) %>% 
        mutate(iter_idx = row_number()) %>%
        ungroup() %>%
        select(iter_idx) %>% max()

# Whats the average n to run to reach a certain power?
average_n_to_run <- 
        outdfbinded %>%
        group_by(minN,
                 d,
                 crit1,
                 crit2,
                 batchSize,
                 limit,
                 test_type,
                 side_type,
                 altMaxN) %>%
        summarise(mean_n = mean(n),
                  median_n = median(n)) %>% 
        ungroup()
        
power_table <- 
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
        summarise(n_simulations = n(),
                  perc_simulations = n_simulations/nIter*100) %>%
        ungroup() %>%
        pivot_wider(id_cols = c(
                        minN,
                        d,
                        crit1,
                        crit2,
                        batchSize,
                        limit,
                        test_type,
                        side_type,
                        altMaxN
                        ),
                        names_from = bf_status,
                        values_from = c(n_simulations,perc_simulations),
                        names_prefix = 'supports_')

# If no simulation supported H0, then manually create these columns:
if (!'n_simulations_supports_H0' %in% names(power_table)){
        
        power_table$n_simulations_supports_H0 <- 0
        power_table$perc_simulations_supports_H0 <- 0
        
}

# If no simulation supported H1, then manually create these columns:
if (!'n_simulations_supports_H1' %in% names(power_table)){
        
        power_table$n_simulations_supports_H1 <- 0
        power_table$perc_simulations_supports_H1 <- 0
        
}

# If no simulation supported undecided, then manually create these columns:
if (!'n_simulations_supports_undecided' %in% names(power_table)){
        
        power_table$n_simulations_supports_undecided <- 0
        power_table$perc_simulations_supports_undecided <- 0
        
}

# Now unite these two tables
power_table <- merge(power_table,
                     average_n_to_run)

# Save the data ###############################################################

saveNameOutData <- file.path('./analysis_results',
                             folderName,
                             'power_table.RData')

if (saveData){
        save(power_table, file = saveNameOutData)
}


