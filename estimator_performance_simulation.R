
# Large simulation to look at stability of the different SE estimators and true
# performances of the ATE estimators.
#
# This script runs a bunch of scenarios and saves the massive amount of results
# to a file to be processed.


source( "simulation_single_trial.R" )

group = "main"

library( tibble )
library( purrr )

#SIZE_IMPACT_CORRELATE = FALSE
#VARIABLE_P = FALSE



# First try to get command line argument
args = commandArgs( trailingOnly = TRUE )
cat( "Command line arguments:\n" )
cat(args, sep = "\n")
if ( length( args ) > 0 ) {
  index = as.numeric( args[[1]] )
  scat( "Retrieved index from command line: %d\n", index )
}

if ( !exists( "index" ) ) {
  index <- as.numeric(as.character(Sys.getenv("INDEX_VAR")))
  scat( "Retrieved index from environment variable INDEX_VAR: %d\n", index )
}


if ( !exists( "index" ) || is.na( index ) ) {
  cat( "WARNING: In TEST mode for simulation script" )
  TESTING = TRUE
  index = 0 # c( 41, 42 )
  FILENAME = "results/testing_simulation_results_" 
  
  APPROX_NUM_TRIALS_PER_RUN = 20
  
  
} else {
  TESTING = FALSE
  FILENAME = "results/simulation_results_"

  #NUM_TRIALS_PER_SCENARIO = 1000
  APPROX_NUM_TRIALS_PER_RUN = 1000
}


make.file.name = function( index ) {
  if ( length( index ) > 1 ) {
    index = paste0( min(index), "_to_", max( index ) )
  }
  paste0( FILENAME, index, ".rds" )
}


##### Set up simulation factors and run simulation #####

scat( "Running simulation job %d\n", index )

ptm = proc.time()

# What simulation are we running?
#scenarios = get.scenario.by.id( index, group=group )
scenarios = make.scenario.list( group=group )

scenarios

if ( TESTING ) {
  scenarios = scenarios[1:5,]
}

NUM_TRIALS_PER_SCENARIO = round( APPROX_NUM_TRIALS_PER_RUN / nrow( scenarios ) )
scat( "Running %d trials per scenario\n", NUM_TRIALS_PER_SCENARIO )

scenarios$run = pmap( scenarios, run.scenario, R = NUM_TRIALS_PER_SCENARIO )

scat("**\n**\tTotal time elapsed:\n")
tot.time = proc.time() - ptm
print(tot.time)

sim.per.min = sim.per.min = nrow( scenarios ) * NUM_TRIALS_PER_SCENARIO / (tot.time["elapsed"] / 60)

scat( "Num trials / scenario = %d with %d scenarios = %d runs\n", NUM_TRIALS_PER_SCENARIO, nrow( scenarios ), NUM_TRIALS_PER_SCENARIO * nrow( scenarios ) )
scat("Realized simulations per minute = %.2f\n", sim.per.min )
scenarios$sim.per.min = sim.per.min

scat( "**\n**")



#### Clean up the simulation results a wee bit ####

#scenarios$ID = index

# Rearrange columns to be nice
scenarios = dplyr::select( scenarios, ID, everything() )
scenarios

# Add in (estimated) superpopulation person weighted ATE
scenarios = mutate( scenarios, 
                    ATE.super.person = map_dbl( run, ~ mean( .$ATE.finite.person ) ) )
scenarios = rename( scenarios, 
                    ATE.super.site = ATE )

# Rearrange and foreground relevant columns
scenarios = dplyr::select( scenarios, ID, J, n.bar, tau, dependence, ATE.super.site, ATE.super.person, everything() )
scenarios

filename = make.file.name(index)
saveRDS( scenarios, file=filename )
scat( "Simulations saved to '%s'\n", filename )

