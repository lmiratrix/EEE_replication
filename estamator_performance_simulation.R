
# Large simulation to look at stability of the different SE estimators and true
# performances of the ATE estimators.
#
# This script runs a bunch of scenarios and saves the massive amount of results
# to a file to be processed.

library( tidyverse )
library( blkvar )

#SIZE_IMPACT_CORRELATE = FALSE
#VARIABLE_P = FALSE
FILENAME = "SimulationScenariosCompleteRnd3PartE.RData" 

NUM_TRIALS_PER_SCENARIO = 100
SIM_PER_MINUTE = 7 # estimated

if ( FALSE ) {
  # for testing crashing in the simulator
  my_compare_methods =function(... ){
    if ( sample(5,1) == 1 ) {
      asdfadfga
    } else {
      compare_methods( ... )
    }
  }
}


scat = function( str, ... ) {
  cat( sprintf( str, ... ) )
}


get.estimates = function( df ) {
  safe_comp = safely( compare_methods )
  ests = safe_comp( Yobs, Z, sid, data=df )
  if ( is.null( ests[[2]] ) ) {
    ests = ests[[1]]
    ests = rename( ests, ATE.hat = tau,
                   SE.hat = SE )
    
  } else {
    cat( "\nCaught error:\n" )
    print( ests[[2]] )
    ests = data.frame( ATE.hat = c(NA),
                       SE.hat = c(NA) )    
  }
  ests
}

# Reassign treatment to do finite sample inference.
rerandomize.data = function( dat ) {
  dat = dat %>% group_by( sid ) %>%
    mutate( Z = sample( Z ) ) %>% ungroup()
  dat = mutate( dat, Yobs = ifelse( Z, Y1, Y0 ) )
  dat
}


# Generate a multisite trial, estimate ATE using all our methods, and then also
# calculate the true ATE (person and site weighted).
# @return Dataframe with the estimates along with the true baseline values.
single.MLM.trial = function( n.bar, J, tau.11.star, dependence, proptx.dependence, variable.n, variable.p,
                             ATE.superpop = 0.2, n.runs = 3 ) {
  df = gen.dat.no.cov( n.bar=n.bar, J=J,
                       tau.11.star = tau.11.star,
                       ICC = 0.20,
                       p = 0.70,
                       variable.n = TRUE,
                       variable.p = TRUE,
                       finite.model = FALSE,
                       size.impact.correlate = dependence,
                       proptx.impact.correlate = proptx.dependence )
  tau.S = attr( df, "tau.S")
  
  params = df %>% group_by( sid ) %>%
    summarise( ATE = mean( Y1 ) - mean( Y0 ),
               n = n() )
  ATE.person = with( df, mean( Y1 - Y0 ) )
  ATE.site = mean( params$ATE )
  
  ests = plyr::rdply( n.runs, {
    df = rerandomize.data( df )
    get.estimates( df ) 
  }, .id="subrun" )
  
  ests = mutate( ests, n.bar = n.bar, J=J, 
                 ATE.finite.person=ATE.person, 
                 ATE.finite.site=ATE.site ) %>% 
    dplyr::select( n.bar, J, everything() )
  ests    
}

if ( FALSE ) {
  df = gen.dat.no.cov( n.bar=200, J=30,
                       tau.11.star = 0.1^2,
                       ICC = 0.20,
                       p = 0.70,
                       variable.n = TRUE,
                       variable.p = TRUE,
                       size.impact.correlate = TRUE,
                       proptx.impact.correlate = TRUE,
                       finite.model = FALSE )
  tau.S = attr( df, "tau.S")
  tau.S    
  
  sites = df %>% group_by( sid ) %>% 
    summarise( n = n(),
               p.Z = mean( Z ),
               ATE.hat = mean( Yobs[Z==1] ) - mean( Yobs[Z==0] ) )
  
  head( sites )
  qplot( sites$n )
  summary( sites$n )
  qplot( sites$p.Z )
  summary( sites$p.Z )
  qplot( sites$n, sites$ATE.hat )
  cor( sites$n, sites$ATE.hat )

  qplot( sites$p.Z, sites$ATE.hat )
  cor( sites$p.Z, sites$ATE.hat )
  
  single.MLM.trial( 20, 10, 0.2^2 )
  
}

#single.MLM.trial( 20, 10, 0.2^2 )

# Run a simulation with R trials and return all the simulation runs as a large dataframe.
run.scenario = function( J, n.bar, tau, dependence, proptx.dependence, variable.n, variable.p, ATE, R ) {
  ptm = proc.time()
  scat( "Running J=%d\tn.bar=%d\ttau=%.2f\tATE=%.2f\tdependence=%s\tprop dep=%s\tR=%d\n", J, n.bar,tau, ATE, dependence, proptx.dependence, R)
  rps = plyr::rdply( R,  single.MLM.trial( n.bar=n.bar, J=J, tau.11.star=tau, 
                                           dependence = dependence, proptx.dependence = proptx.dependence,
                                           variable.n = variable.n, variable.p = variable.p,
                                           ATE.superpop=ATE ),
                     .id="run", .progress="text" )
  rps$subrun = paste0( rps$run, ".", rps$subrun )
  
  scat("**\n**\tTotal time elapsed:\n")
  tot.time = proc.time() - ptm
  print(tot.time)
  scat("Simulations per minute = %.2f\n", R / (tot.time["elapsed"] / 60) )

  rps
}

if ( FALSE ) {
  rr = run.scenario( J = 4, n.bar = 16, tau = 0.3^2, dependence=TRUE, proptx.dependence = TRUE, variable.n = TRUE, variable.p = TRUE,
                     ATE = 0.2, R = 10 )
  head( rr )  
  table(  table( rr$run ) )
  table(  table( rr$subrun ) )
  tt = table( rr$subrun )
  tt[ tt < 10 ]
  filter( rr, subrun %in% c( "1.2", "4.1", "4.2", "2.1" ) )
  
}


##### Set up simulation factors and run simulation #####

scenarios = expand.grid( J = c( 80, 40, 20 ),
                         n.bar = c( 8000, 4000, 2000 ), # put in totals here
                         dependence = c( 1, 0 ),
                         proptx.dependence = c( 1, 0, -1 ), 
                         variable.n = c( TRUE, FALSE ),
                         variable.p = c( TRUE, FALSE ),
                         ATE = 0.2,
                         tau = c( 0, 0.1, 0.2 )^2 )
nrow( scenarios )
#scenarios2 = expand.grid( J = c( 80, 20 ),
#                         n.bar = c( 8000, 2000 ), # put in totals here
#                         dependence = c( 1, 0 ),
#                         proptx.dependence = c( 1, 0, -1 ), 
#                         ATE = 0.2,
#                         tau = c( 0, 0.2 )^2 )
#nrow( scenarios2 )
#scenarios = bind_rows( A=scenarios2, B=scenarios, .id="file" )
#head( scenarios )
#scenarios$dup = duplicated( scenarios[-c(1) ] )

#scenarios = filter( scenarios, !dup, file=="B" ) %>%
#  dplyr::select( -file, -dup )
#head( scenarios )
#nrow( scenarios )

# drop redundant scenarios
scenarios = filter( scenarios, (variable.n == TRUE) | (dependence == 0) )
scenarios = filter( scenarios, (variable.p == TRUE) | (proptx.dependence == 0) )
nrow( scenarios )

# drop prior run scenarios
scenarios = filter( scenarios, variable.n == FALSE | variable.p == FALSE )
table( scenarios$variable.n, scenarios$variable.p )

table( scenarios$variable.n, scenarios$dependence )
table( scenarios$variable.p, scenarios$proptx.dependence )


# convert n.bar from totals to people per site
scenarios = mutate( scenarios, n.bar = round( n.bar / J ) )
scenarios = as.tibble( scenarios )
scenarios
nrow( scenarios )

# estimate time (3 sim per minute is conservative)
NUM_TRIALS_PER_SCENARIO * nrow( scenarios ) / (SIM_PER_MINUTE * 60)


ptm = proc.time()

scenarios$run = pmap( scenarios, run.scenario, R = NUM_TRIALS_PER_SCENARIO )

scat("**\n**\tTotal time elapsed:\n")
tot.time = proc.time() - ptm
print(tot.time)

scat("Realized simulations per minute = %.2f\n", nrow( scenarios ) * NUM_TRIALS_PER_SCENARIO / (tot.time["elapsed"] / 60) )

scat( "**\n**")

#library( furrr )
#plan(multicore( workers = 3 ) )
#scenarios$run = future_pmap( scenarios, run.scenario, R = NUM_TRIALS_PER_SCENARIO, .progress=TRUE )


#### Clean up the simulation results a wee bit ####

scenarios$ID = 1:nrow(scenarios)
scenarios = dplyr::select( scenarios, ID, everything() )
scenarios

# Add in (estimated) superpopulation person weighted ATE
scenarios = mutate( scenarios, 
                    ATE.super.person = map_dbl( run, ~ mean( .$ATE.finite.person ) ) )
scenarios = rename( scenarios, 
                    ATE.super.site = ATE )

scenarios = dplyr::select( scenarios, ID, J, n.bar, tau, dependence, ATE.super.site, ATE.super.person, everything() )

# Look at individual runs
sims = unnest( scenarios )
head( sims )

save( scenarios, file=FILENAME )
scat( "Simulations saved to %s\n", FILENAME )




# Ignore this old stuff
if ( FALSE ) {
  runs = run.scenario( 20, 200, R=500, tau=0.2^2, ATE=0.2 )
  head( runs )
  nrow( runs )
  
  save( runs, file="Scenario1.RData" )
  
  
  runs2 = run.scenario( 20, 200, R=100, tau=0, ATE=0.2 )
  head( runs2 )
  
  save( runs2, file="Scenario2.RData" )
  
  
  
  runs3 = run.scenario( 20, 200, R=100, tau=0.1^2, ATE=0.2 )
  head( runs3 )
  
  save( runs3, file="Scenario3.RData" )
  
}