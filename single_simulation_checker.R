#
# Utility/exploration script
#
# Runs a simulation to focus on finite sample properties in a single scenario
#

library( tidyverse )
library( blkvar )



scat = function( str, ... ) {
  cat( sprintf( str, ... ) )
}


get.estimates = function( df ) {
  safe_comp = safely( compare_methods )
  ests = safe_comp( Yobs, Z, sid, data=df, include.MLM = FALSE )
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

rerandomize.data = function( dat ) {
  dat = dat %>% group_by( sid ) %>%
    mutate( Z = sample( Z ) ) %>% ungroup()
  dat = mutate( dat, Yobs = ifelse( Z, Y1, Y0 ) )
  dat
}


# Generate a multisite trial, estimate ATE using all our methods, and then also
# calculate the true ATE (person and site weighted).
# @return Dataframe with the estimates along with the true baseline values.
single.MLM.trial = function( n.bar, J, tau.11.star, dependence,
                             ATE.superpop = 0.2, n.runs = 3 ) {
  df = gen.dat.no.cov( n.bar=n.bar, J=J,
                       tau.11.star = tau.11.star,
                       ICC = 0.20,
                       variable.n = TRUE,
                       variable.p = TRUE,
                       finite.model = FALSE,
                       size.impact.correlate = dependence )
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


sims =  single.MLM.trial( n.bar=200, J=100, tau=0.2^2, dependence=TRUE, n.runs = 100 )
head( sims )
table( sims$subrun )
sum( is.na( sims$ATE.hat ) )
     
ss = sims %>% group_by( method ) %>%
  summarise( SE = sd( ATE.hat ),
             SE.hat = mean( SE.hat ),
             ratio = SE.hat / SE )
ss





run.scenario = function( J, n.bar, tau, dependence, ATE = 0.2, R ) {
  ptm = proc.time()
  scat( "Running J=%d\tn.bar=%d\ttau=%.2f\tATE=%.2f\tdependence=%s\tR=%d\n", J, n.bar,tau, ATE, dependence, R)
  rps = plyr::rdply( R,  single.MLM.trial( n.bar=n.bar, J=J, tau.11.star=tau, dependence = dependence, ATE.superpop=ATE ),
                     .id="run", .progress="text" )
  rps$subrun = paste0( rps$run, ".", rps$subrun )
  
  scat("**\n**\tTotal time elapsed:\n")
  tot.time = proc.time() - ptm
  print(tot.time)
  scat("Simulations per minute = %.2f\n", R / (tot.time["elapsed"] / 60) )
  
  rps
}


rps = run.scenario( n.bar = 50, J = 40, tau=0.2^2, dependence = TRUE, R = 100 )

head ( rps )

SEs = rps %>% group_by( run, method ) %>% 
  summarise( nrps = n() ,
             SE = sd( ATE.hat ),
             SE.hat = mean( SE.hat ),
             ratio = SE.hat / SE )
head( SEs )

SEs2 = SEs %>% group_by( method ) %>% 
  summarise( SE = mean( SE ),
             SE.hat = )







rps = plyr::rdply( 200, {
  a = rnorm( 3000 )
b = rep( 1:1000, 3 )
df = data.frame( Y=a, sid=b )
sums = df %>% group_by( sid ) %>% 
  summarise( sd = sd( Y ),
             var = var( Y ) )
sums %>% summarise( sd = mean( sd ),
                    var = mean( var ) )

} )
head( rps )
p = gather( rps, sd, var, key="measure", value="val")
head( p )
ggplot( p, aes( val ) ) +
  facet_wrap( ~ measure ) +
  geom_histogram()
