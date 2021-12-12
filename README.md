# Main sequence of scripts to run simulations:

First, please open the .Rproj file. Having the project started makes sure the working directory is set correctly.

1_simulation_parameters.R: specify various types of simulations you'd like to run. You can either run on the cluster or locally.

2_move_slurm_output.R: slurm will dump the results in the main working directory. To avoid cluttering the space, this script will move the specified folder to the ./data folder. Subsequent scripts will look for data there. 

3_preprocess_output.R: slurm outputs the results in a list format. This script will bind it into a nice dataframe, and save the dataframe into specified subfolder of the ./analysis_results/ folder. This dataframe will have each row represent one "step" in every simulation, i.e. one addition of a batch of participants and checking of the Bayes factor.

4_summary_stats_many_altNs.R: This script takes the dataframe generated by the 3_preprocess_output.R script. It will calculate the probabilities of supporting H1/H0/undecided, depending on various maxN limits.

5_plot_results.R: this script will take the power_table.RData file produced by 4_summary_stats_many_altNs.R script. For every unique combination of factors you specified in the initial job, it will produce two plots: (1) percentage of simulations supporting H1/H0/undecided, and (2) mean and median number of participants needed to run to achieve a certain "power".

6_post_slurm_wrapper.R: This script is a wrapper to run scripts 2 through 5, so you don't have to run them separately. It cannot run the 1st script, because the 2nd script must wait until slurm is done with the simulations.


# Folders:

data:
Where the raw data output from slurm is copied to and left untouched.

analysis_output:
For preprocessing and other stats results from various analysis. 

utils:
folder containing various helper functions.

# Authors:

- Alex Quent
- Rik Henson
- Levan Bokeria
- Andrea Greve

