#!/bin/bash
#
#SBATCH --array=0-0
#SBATCH --cpus-per-task=16
#SBATCH --job-name=try_1
#SBATCH --output=slurm_%a.out
C:/PROGRA~1/R/R-41~1.1/bin/x64/Rscript --vanilla slurm_run.R
