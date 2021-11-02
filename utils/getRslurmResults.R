# This is a script reading the output from the rslurm.
# Organizes the output into a dataframe, where each row is a BF analysis after 
# each batch acquisition.
getRslurmResults <- function (nIter,d1_str,nLimit,crit1,saveDF,readDF){
        
        loadFolder <- paste('rslurm_raw_and_preprocessed/',
                '_rslurm_d1_',d1_str,'_limpg_',
                nLimit, '_crit1_', crit1, '_minN_', minN,
                '_batchSize_', batchSize, sep='')
        
        saveName   <- paste(loadFolder, '/simulationResults_',d1_str,'_limpg_',
                            nLimit, '_crit1_', crit1, '_minN_', minN,
                            '_batchSize_', batchSize, '.RData',sep='')
        if(readDF){
                
                df <- import(saveName)
                
        } else {
                # Load data
                paths      <- loadFolder
                tempList   <- readRDS(paste0(paths, '/results_0.RDS'), refhook = NULL)
                tempUnList <- unlist(tempList)
                
                tempUnllist_names_org <- names(tempUnList)
                tempUnllist_names     <- tempUnllist_names_org
                tempUnllist_names[tempUnllist_names_org == 'bf'] <- 'bf0' # replace bf with bf0 otherwise it's get removed.
                tempUnllist_names <- tempUnllist_names[!(tempUnllist_names == 'crit1' | tempUnllist_names == 'crit2')] # Remove crit1 and crit2
                tempUnllist_names <- gsub("[^0-9.-]", "", tempUnllist_names) # Remove all non-numerical info
                
                tempUnllist_seq   <- as.numeric(tempUnllist_names[tempUnllist_names != ''])
                tempUnllist_seq[tempUnllist_seq == 0] <- 1 # Replace zero with 1 again so that the id will repeated once
                tempUnllist_seq   <- tempUnllist_seq - c(tempUnllist_seq[2:length(tempUnllist_seq)], 1)
                tempUnllist_seq   <- tempUnllist_seq[tempUnllist_seq != -1] + 1
                
                # Get columns for df
                d         <- tempUnList[tempUnllist_names_org == 'd']
                n_raw     <- tempUnList[tempUnllist_names_org == 'n']
                crit1     <- tempUnList[tempUnllist_names_org == 'crit1']
                crit2     <- tempUnList[tempUnllist_names_org == 'crit2']
                batchSize <- tempUnList[tempUnllist_names_org == 'batchSize']
                limit     <- tempUnList[tempUnllist_names_org == 'limit']
                
                # Repeat by how many steps were taken
                id        <- rep(1:length(tempList), times = tempUnllist_seq)
                d         <- rep(d, times = tempUnllist_seq)
                crit1     <- rep(crit1, times = tempUnllist_seq)
                crit2     <- rep(crit2, times = tempUnllist_seq)
                batchSize <- rep(batchSize, times = tempUnllist_seq)
                limit     <- rep(limit, times = tempUnllist_seq)
                
                # Create sample size sequences
                # browser()
                n_sss <- c()
                for(i in 1:length(n_raw)){
                    n_sss <- c(n_sss, seq(minN, n_raw[i], batchSize[1]))
                }
                # browser()
                # print(length(n_sss))
                
                # Make DF
                df <- data.frame(id = id,
                                  d = d,
                                  n = n_sss,
                                  crit1 = crit1,
                                  crit2 = crit2,
                                  batchSize = batchSize,
                                  limit = limit, 
                                  bf = tempUnList[grep("bf", tempUnllist_names_org)])
                
                # Save dfs
                if (saveDF){
                        save(df, file = saveName)  
                }
                
        } #if loadDF
        
        return(df)
        
}


