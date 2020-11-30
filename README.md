# Multisite ATE Estimator Simulation Study

This is the code for the simulation study discussed in Miratrix & Weiss "Estimands, Estimators, and Estimates." This code heavily uses the `blkvar` package, also on GitHub (see https://github.com/lmiratrix/blkvar) that implements all the methods in easy-to-use functions and also provides code for generating fake multisite experiments.


## Running the simulation
We ran the simulation on the Odessey cluster at Harvard.  The simulation takes several computers awhile to run, but it is easy to run smaller simulations targeting specific scenarios.

The Odyssey scripts will generate a large number of rds files, each file being the result of a fraction of the simulation runs. We then bundle these results into a single file, and then analyze that file.


## Overview of file structure

We have the following

  * `simulation_support_code` - Code for generating data and running the simulation.  Uses the blkvar package, which implements the various methods we explored.
  * `data_prep` folder - Three files that form a pipeline that takes the raw output from the Odyssey cluster and makes some aggregate datafiles.  These scripts are described below.  There is also a bundling file (`fully_prepare_simulation_results.R`) that just sources these files in order.
  * `analysis` folder - Collection of scripts that do the analysis presented in the paper.
  
  
## Instructions to get simulation results data ready

The steps are as follows:

1) Gather results from the cluster into a single file.
```
source( "prepare simulation results/bundle_odyssey_results.R" )
```

2) Clean up single file, remove bad sim runs, add in metadata and summary data.
```
source( "prepare simulation results/clean_simulation_results.R" )
```

3) Calculate the performance metrics used for the report.

```
source( "prepare simulation results/prepare_aggregate_results.R" )
```

This final file works with the following data frames:

 * scenarios (list of scenario characteristics)
 * simdata (the raw data; this can be a very big file)
 * sruns (aggregated data by method and simulation run)


## Instructions to get simulation results

Source the `calculate_paper_results.R` file.  This is the R code from an old R markdown script that was then converted to the simulation writeup tex document.


