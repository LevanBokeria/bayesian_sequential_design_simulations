# Main sequence of scripts to run simulations:

1. specify_params_sent_to_slurm.R

This will send a job to cluster. The output will be saved in a new folder in the current working directory starting with "_rslurm...". Move that folder into the "./rslurm_raw_and_preprocessed" folder

2. getResultsWrapper.R

This script will load the specified output of the cluster, and calculate the probabilities of supporting H1 or H0 depending on effect size and maxN specified in the simulation. It saves this output in the same folder as the simulation results. Just open the file that is saved, to see the probabilities of supporting H1 and H0.

3. getResultsByManyNs.R

This script allows one to examine the probabilities of supporting H1/H0/undecided at multiple maximum N per group. It will take some cluster simulation result with, for example, 200 participants per group. That result already has all the information we need for examining probabilities of supporting H1/H0/undecided at whatever n per group we want, as long as its less than 200 and a multiple of batch-size that the simulation used. The output is saved in "./analysis_output"

4. plotPowerByN.R

This will take the output from the getResultsByManeNs.R script, and plot max N per group on the X axis, and % of simulations supporting H1/H0/undecided on the Y axis.

# Other scripts and folders

## Folders:

analysis_output:
Where results from various analysis go. For example, output of getResultsByManyNs.R

bayesianSequentialDesign-master:
Alex Quent's repository that contained the original scripts for bayesian sequential design.

power_by_n_plots: some older plots of power by various maxN per group. The new plotting script doesn't save images.

quickSimResults:
Folder that stores results from quickBayesSim.R script, where you can just set maxN per group, n-simulations, and run a for loop to see the "power" to support H1 or H0, without doing any sequential analysis.

rslurm_raw_and_preprocessed:
files that are the output of rslurm simulations ran on the CBU server. The cluster output is automatically saved in the working directory, so then you should manually copy those folders to this 'rslurm_raw_and_preprocessed' folder. This folder also contains files as a result of preprocessing those slurm simulations into a nice dataframe, using the getResultsWrapper.R script.

utils:
folder containing various helper functions.

## Scripts:

scratchPad.R:
Just space to explore and play with scripts.

simulate_rep_measures.R:
Code I was playing with when thinking that we'd need to simulated repeated measures analysis. But because we take Phase2-Phase1 scores, we didn't need this in the end.

quickBayesSim.R:
If you want to quickly see the "power" to support H1 or H0 depending on real effect size, this script will run N simulations with a set maxN participants. It doesn't do a sequential analysis, just a one-shot analysis with maxN, repeated as many times as you'd like. Its rather slow but OK.

getResultsByMaxN.R

Older script, might not be working. 

