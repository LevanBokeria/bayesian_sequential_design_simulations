#!/bin/bash
#
#SBATCH --array=0-0
#SBATCH --cpus-per-task=16
#SBATCH --job-name=results_spatial_schema_32_new
#SBATCH --output=slurm_%a.out
/imaging/local/software/R/3.5.3shlib/bin/Rscript --vanilla slurm_run.R
