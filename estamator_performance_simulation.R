
# Large simulation to look at stability of the different SE estimators and true
# performances of the ATE estimators.
#
# This script runs a bunch of scenarios and saves the massive amount of results
# to a file to be processed.


source( "simulation_single_trial.R" )


#SIZE_IMPACT_CORRELATE = FALSE
#VARIABLE_P = FALSE

index <- as.numeric(as.character(Sys.getenv("INDEX_VAR1")))

if ( !exists( "index" ) ) {
  cat( "WARNING: In TEST mode for simulation script" )
  index = 41 # c( 41, 42 )
  FILENAME = "testing_simulation_results_" 
  
  NUM_TRIALS_PER_SCENARIO = 5
  
  
  SIM_PER_MINUTE = 7 # estimated
  
} else {
  FILENAME = "simulation_results_"
  NUM_TRIALS_PER_SCENARIO = 100
}


make.file.name = function( index ) {
  if ( length( index ) > 1 ) {
    index = paste0( min(index), "_to_", max( index ) )
  }
  paste0( FILENAME, index, ".RData" )
}


##### Set up simulation factors and run simulation #####


ptm = proc.time()

# What simulation are we running?
scenarios = get.scenario.by.id( "main", index )
scenarios

scenarios$run = pmap( scenarios, run.scenario, R = NUM_TRIALS_PER_SCENARIO )

scat("**\n**\tTotal time elapsed:\n")
tot.time = proc.time() - ptm
print(tot.time)

scat("Realized simulations per minute = %.2f\n", nrow( scenarios ) * NUM_TRIALS_PER_SCENARIO / (tot.time["elapsed"] / 60) )

scat( "**\n**")



#### Clean up the simulation results a wee bit ####

scenarios$ID = index

# Rearrange columns to be nice
scenarios = dplyr::select( scenarios, ID, everything() )
scenarios

# Add in (estimated) superpopulation person weighted ATE
scenarios = mutate( scenarios, 
                    ATE.super.person = map_dbl( run, ~ mean( .$ATE.finite.person ) ) )
scenarios = rename( scenarios, 
                    ATE.super.site = ATE )

# Select relevant columns
scenarios = dplyr::select( scenarios, ID, J, n.bar, tau, dependence, ATE.super.site, ATE.super.person, everything() )
scenarios

filename = make.file.name(index)
save( scenarios, file=filename )
scat( "Simulations saved to '%s'\n", filename )

