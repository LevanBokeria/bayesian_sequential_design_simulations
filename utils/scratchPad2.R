rm(list=ls())
pacman::p_load(rio,
               tidyverse)



pt <- import('./analysis_results/results_1/power_table.RData')



ag <- import_list('C:/Users/levan/Desktop/results/results/BF10_unpaired_2sided/resultsByManyNs_d_05_crit1_10_24_to_432_by_12_paired_0_sided_2.RData')


# Compare

pt <- pt %>%
        filter(d %in% c(0),
               crit1 == 10,
               crit2 == 1/10,
               test_type == 'unpaired',
               side_type == 'two_tailed') 

ag <- ag$outData_d0$outStats

mg <- merge(pt,ag,
            by.x = 'altMaxN',
            by.y = 'maxN')

mg$h0diff <- mg$n_simulations_supports_H0 - mg$H0
mg$h1diff <- mg$n_simulations_supports_H1 - mg$H1
mg$unddiff <- mg$n_simulations_supports_undecided - mg$und


fig <- mg %>%
        ggplot(aes(x=altMaxN)) +
        geom_line(aes(y=h0diff),color='red') +
        geom_line(aes(y=h1diff),color='blue') +
        geom_line(aes(y=unddiff)) +
        ylab('Levan-Andrea') +
        ggtitle('Red=H0 diff;\nBlue=H1 diff')

print(fig)