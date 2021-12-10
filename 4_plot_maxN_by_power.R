# This script will plot the power by various max N per group 

# Clear the environment and load libraries, etc ###############################
rm(list=ls())

pacman::p_load(tidyverse,
               rio)

# Load the data ################################################################

# Load the file

# This must correspond to the variable given to the previous scripts
folderName <- 'try_2'

power_table <- import(file.path('./analysis_results',
                 folderName,
                 'power_table.RData'))

# How many unique combination of factors are here? 
# For each, make a separate plot
unique_combs <-
        power_table %>%
        select(minN,
               d,
               crit1,
               crit2,
               batchSize,
               limit,
               test_type,
               side_type) %>% 
        distinct()
        
n_combs <- nrow(unique_combs)        

print(paste('There are ', 
            n_combs, 
            ' unique combination of factors. They are:',
            sep=''))
print(unique_combs)        


# Create the plot #############################################################

# For each of the simualtions, make a separate plot.
# The user can change this part of the code to plot the data whichever way they 
# want


# Turn it into a long form version
power_table_long <- 
        power_table %>%
        select(-starts_with('n_sim')) %>%
        pivot_longer(cols = c(perc_simulations_supports_H1,
                              perc_simulations_supports_H0,
                              perc_simulations_supports_undecided),
                     names_to = 'bf_status',
                     values_to = 'perc_simulations')

# x tick marks?
x_ticks <- seq(power_table$minN[1],power_table$limit[1],power_table$batchSize[1])

for (iComb in seq(1,n_combs)){
        
        print(unique_combs[iComb,])
        
        title_string <- paste(
                'd =',unique_combs$d[iComb],
                '; minN =',unique_combs$minN[iComb],
                '; crit1 =',unique_combs$crit1[iComb],
                '; crit2 =',unique_combs$crit2[iComb],
                '\n',
                'batchSize =',unique_combs$batchSize[iComb],
                '; limit =',unique_combs$limit[iComb],
                '\n',
                'test_type =',unique_combs$test_type[iComb],
                '; side_type =',unique_combs$side_type[iComb],
                sep=''
        )
        
        fig <- power_table_long %>%
                filter(d == unique_combs$d[iComb],
                       minN == unique_combs$minN[iComb],
                       crit1 == unique_combs$crit1[iComb],
                       crit2 == unique_combs$crit2[iComb],
                       batchSize == unique_combs$batchSize[iComb],
                       limit == unique_combs$limit[iComb],
                       test_type == unique_combs$test_type[iComb],
                       side_type == unique_combs$side_type[iComb]) %>%
                ggplot(aes(x=altMaxN,
                           y=perc_simulations,
                           group=bf_status,
                           color=bf_status)) +
                geom_line() +
                geom_point() +
                scale_x_continuous(breaks=x_ticks) +
                scale_y_continuous(breaks=seq(0,100,10)) +  
                ylab('% of simulations') +
                xlab('max N per group') +                 
                ggtitle(title_string)

        print(fig)
}
