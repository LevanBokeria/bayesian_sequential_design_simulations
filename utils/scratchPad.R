# Libraries
library(BayesFactor)
library(assortedRFunctions)
library(tidyverse)
library(readxl)
library(ggpubr)
library(rstatix)
library(emmeans)

# Clear the environment
rm(list = ls())

# Load the excel file
long_form_data_all_ptp <-
    read_excel(
        'C:/Users/levan/GitHub/concept_gen_analysis/results/analysis/long_form_data_all_ptp.xlsx'
    , sheet = 1
    ) %>%
    as_tibble()


long_form_data_all_ptp <- long_form_data_all_ptp %>%
    mutate(congruency=as.factor(congruency))

long_form_data_qc_pass_ptp <- subset(long_form_data_all_ptp, data_submitted == 1 & global_pass == 1 & 
                       debrief_qc_pass == 1 & fb_int_qc_pass == 1 & 
                       phase_1_rt_qc_pass == 1 & phase_2_rt_qc_pass == 1)


# Load the results table
results_table_qc_pass_ptp_analyzed <-
    read_excel(
        'C:/Users/levan/GitHub/concept_gen_analysis/results/analysis/results_table_qc_pass_ptp_analyzed.xlsx'
        , sheet = 1
    ) %>%
    as_tibble()


results_table_qc_pass_ptp_analyzed <- results_table_qc_pass_ptp_analyzed %>%
    mutate(congruency=as.factor(congruency),arr_phase_1_1=as.factor(arr_phase_1_1))


############## Playing around with gather command ##############################
data_gathered <- results_table_qc_pass_ptp_analyzed %>%
    select(ptp,congruency,phase_1_ses_1_2_perf,phase_2_ses_1_2_perf) %>%
    gather(key='phase',value='ses_1_2_perf',
           phase_1_ses_1_2_perf,
           phase_2_ses_1_2_perf)

# position_jitterdodge(
#     jitter.width = 0.01,
#     jitter.height = 0,
#     dodge.width = 0.75,
#     seed = 123
# )

data_gathered %>%
    ggplot(aes(x=congruency,y=ses_1_2_perf,fill=phase)) +
    geom_boxplot(width=0.5,notch=TRUE, outlier.shape = " ") +
    geom_point(pch=21,
               position = position_jitterdodge(dodge.width=0.5,
                                               jitter.width=0.1)) +
    ylim(0.45,1)


################################################################################


# Simple boxplot to show arrangement effect
results_table_qc_pass_ptp_analyzed %>%
    select(ptp,congruency,arr_phase_1_1,phase_2_min_phase_1_ses_1_2_perf) %>%
    reorder_levels(arr_phase_1_1, order = c("1", "14", "3", "15")) %>%
    ggplot(aes(x=arr_phase_1_1,
               y=phase_2_min_phase_1_ses_1_2_perf,
               fill=congruency)) +
    geom_boxplot(width=0.5,notch=FALSE,outlier.shape = " ") +
    geom_point(pch=21,
               position = position_jitterdodge(dodge.width=0.5,
                                               jitter.width=0.2)) +
    ylab("P2-P1 ses 1&2 perf") + 
    xlab("Arrangements")


model <- lm(phase_2_min_phase_1_ses_1_2_perf ~ arr_phase_1_1 * congruency, 
            data = results_table_qc_pass_ptp_analyzed)
results_table_qc_pass_ptp_analyzed %>%
    group_by(arr_phase_1_1) %>%
    anova_test(phase_2_min_phase_1_ses_1_2_perf ~ congruency, error = model)

pwc <- results_table_qc_pass_ptp_analyzed %>% 
    group_by(arr_phase_1_1) %>%
    emmeans_test(phase_2_min_phase_1_ses_1_2_perf ~ congruency, 
                 p.adjust.method = "bonferroni") 
pwc


###############################################################################
# This counts number of correct, incorrect, and misses within each session, for each ptp
data_qc_pass_cumsum <- 
    data_qc_pass %>%
    select(ptp, phase, session, correct) %>%
    group_by(ptp,phase,session) %>%
    mutate(cumscore = cumsum(replace_na(correct,0)))

# This will count number of correct responses within each session, for each ptp
data_qc_pass_n_correct <- 
    data_qc_pass %>%
    select(ptp, phase, session, correct) %>%
    group_by(ptp,phase,session,correct) %>%
    mutate(cumscore = n()) %>% 
    unique() %>%
    subset(session != -1 & correct == 1)

data_qc_pass %>%
    select(ptp, phase, session, correct) %>%
    group_by(ptp,phase,session,correct) %>%
    summarize(cumscore = n()) %>%
    View()

data_qc_pass %>%
    select(ptp, phase, session, correct) %>%
    group_by(ptp,phase,session) %>%
    summarize_all(mean, na.rm = TRUE) %>%
    subset(session != -1) %>%
    View()


# Normal analysis
# Normal analysis

# Analyze learning rate

learning_rate_congruent_1 = data_all$phase_2_min_phase_1_learning_rate_exp[data_all$congruency == 1]
learning_rate_congruent_0 = data_all$phase_2_min_phase_1_learning_rate_exp[data_all$congruency == 0]

bf_learning_rate <-
    reportBF(
        ttestBF(
            learning_rate_congruent_1,
            learning_rate_congruent_0,
            paired = FALSE
        ),
        4
    )

# Analyze session differences
ses_1_2_perf_congruent_1 = data_all$phase_2_min_phase_1_ses_1_2_perf[data_all$congruency == 1]
ses_1_2_perf_congruent_0 = data_all$phase_2_min_phase_1_ses_1_2_perf[data_all$congruency == 0]

bf_ses_1_2_perf <-
    reportBF(ttestBF(
        ses_1_2_perf_congruent_1,
        ses_1_2_perf_congruent_0,
        paired = FALSE
    ),
    4)

t.test(ses_1_2_perf_congruent_0,ses_1_2_perf_congruent_1,paired=FALSE)

# Effect size

sd_c1 <- sd(ses_1_2_perf_congruent_1)
sd_c0 <- sd(ses_1_2_perf_congruent_0)

n_c1 <- length(ses_1_2_perf_congruent_1)
n_c0 <- length(ses_1_2_perf_congruent_0)

pooled_sd <- sqrt( 
    (
    (n_c1-1)*(sd_c1^2) + 
    (n_c0-1)*(sd_c0^2)
    ) / (n_c1 + n_c0 - 2)
    )

effect_size <- (mean(ses_1_2_perf_congruent_1) - mean(ses_1_2_perf_congruent_0)) /
    pooled_sd

# # Some plots

# data_all %>%
#     group_by(congruency) %>%
#     summarize(avg_ses_diff = mean(phase_2_min_phase_1_ses_1_2_perf)) %>%
#     ggplot() +
#     geom_col(mapping=aes(x=congruency, y=avg_ses_diff))



ggplot(data=data_all, mapping=aes(x=congruency, y=phase_2_min_phase_1_ses_1_2_perf)) +
    geom_violin() +
    geom_jitter(width=0.1)



############################################################################
# # Find the extra participants
# data_extra <-
#     data[data$congruency == 1 &
#              data$concept_phase_1 == 'beak_tail_space' &
#              data$arr_phase_1_1 == 14, ]
# 
# nExtra = nrow(data_extra)
# 
# bf_learning_rate <- NA
# bf_ses_1_2_perf  <- NA  
# 
# 
# for (iExtra in nExtra) {
#     # Find this one in the main data, remove it, and analyze
#     curr_ptp <- data_extra$ptp[iExtra]
#     curr_con <- data_extra$congruency[iExtra]
#     curr_concept <- data_extra$concept_phase_1[iExtra]
#     curr_arr <- data_extra$arr_phase_1_1[iExtra]
#     
#     data_all <-
#         subset(
#             data,
#             !(
#                 ptp == curr_ptp & congruency == curr_con &
#                     concept_phase_1 == curr_concept &
#                     arr_phase_1_1 == curr_arr
#             )
#         )
#     
#     
#     # Analyze learning rate
#     
#     learning_rate_congruent_1 = data_all$phase_2_min_phase_1_learning_rate_exp[data_all$congruency == 1]
#     learning_rate_congruent_0 = data_all$phase_2_min_phase_1_learning_rate_exp[data_all$congruency == 0]
#     
#     bf_learning_rate[iExtra] <-
#         reportBF(
#             ttestBF(
#                 learning_rate_congruent_1,
#                 learning_rate_congruent_0,
#                 paired = FALSE
#             ),
#             4
#         )
#     
#     # Analyze session differences
#     ses_1_2_perf_congruent_1 = data_all$phase_2_min_phase_1_ses_1_2_perf[data_all$congruency == 1]
#     ses_1_2_perf_congruent_0 = data_all$phase_2_min_phase_1_ses_1_2_perf[data_all$congruency == 0]
#     
#     bf_ses_1_2_perf[iExtra] <-
#         reportBF(ttestBF(
#             ses_1_2_perf_congruent_1,
#             ses_1_2_perf_congruent_0,
#             paired = FALSE
#         ),
#         4)
#     
# }
