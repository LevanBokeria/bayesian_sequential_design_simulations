#!/bin/bash
#
#SBATCH --array=0-0
#SBATCH --cpus-per-task=16
#SBATCH --job-name=d1_05_limpg_208_crit1_6_minN_32_batchSize_16
#SBATCH --output=slurm_%a.out
/imaging/local/software/R/3.5.3shlib/bin/Rscript --vanilla slurm_run.R
