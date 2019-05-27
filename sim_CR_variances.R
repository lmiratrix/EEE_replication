

##
## Sim study to look at CR standard errors
##

library( tidyverse )
library( blkvar )


scat = function( str, ... ) {
  cat( sprintf( str, ... ) )
}


# See https://www.jepusto.com/handmade-clubsandwich/
clubsandwich.variance = function( w, ATE.hat.b, ATE.hat ) {
  W = sum( w )
  V = (1/W^2) * sum( ( w^2 * (ATE.hat.b - ATE.hat)^2 ) / (1 - w/W) )
  
  df.inv = sum( w^2 / (W - w)^2 ) - (2/W) *sum( w^3 / (W-w)^2 ) + (1/W^2)*sum( w^2/(W-w) )^2
  
  list( var.hat=V, df = 1/df.inv )
}



my.fixed.effect.estimators = function( Yobs, Z, B, data=NULL ) {
  data = data.frame( Yobs = eval( substitute( Yobs ), data ),
                     Z = eval( substitute( Z ), data ),
                     B = eval( substitute( B ), data) )
  
  # simple linear model
  M0 = lm( Yobs ~ 0 + Z + B, data=data )
  SE.lm = summary( M0 )$coeff["Z",2]
  
  # est ATE (precision weighted)
  ATE.hat = coef(M0)[["Z"]]
  
  # Huber-White SEs
  vcov_sand = sandwich::vcovHC(M0, type = "HC1")
  SE.lm.sand <-sqrt( vcov_sand[1,1] )
  
  # Cluster robust SEs (clustering at site level)
  vcov_clust = sandwich::vcovCL( M0, data$B )
  SE.lm.clust = sqrt( vcov_clust[1,1] )
  
  # Cluster robust SEs (clustering at site level using clubSandwich)
  # aggregate!
  block.stats = data %>% group_by( B ) %>%
    summarise( ATE.hat = mean( Yobs[Z==1] ) - mean( Yobs[Z==0] ),
               n = n(),
               n1 = sum(Z == 1) )
  block.stats = mutate( block.stats,
                        prec = n * ((n-n1)/n) * (n1/n) )
  cs.var = clubsandwich.variance( block.stats$prec, block.stats$ATE.hat, ATE.hat )
  SE.lm.clust.club = sqrt( cs.var$var.hat )
  
  FEmodels = data.frame( method=c("FE", "FE-Sand", "FE-CR", "FE-Club" ),
                         ATE.hat = rep( ATE.hat, 4 ),
                         SE.hat = c( SE.lm, SE.lm.sand, SE.lm.clust, SE.lm.clust.club ),
                         stringsAsFactors = FALSE )
  
  FEmodels
}



# Generate a multisite trial, estimate ATE using all our methods, and then also
# calculate the true ATE (person and site weighted).
# @return Dataframe with the estimates along with the true baseline values.
single.MLM.trial = function( n.bar, J, tau.11.star, dependence = FALSE, proptx.dependence = FALSE, variable.n = FALSE, variable.p = FALSE,
                             ATE.superpop = 0.2, ICC = 0.20, p.tx = 0.65,
                             just.describe.data = FALSE, ... ) {
  
  df = gen.dat.no.cov( n.bar=n.bar, 
                       J=J,
                       tau.11.star = tau.11.star,
                       gamma.10 = ATE.superpop,
                       ICC = ICC,
                       p = p.tx,
                       variable.n = variable.n,
                       variable.p = variable.p,
                       finite.model = FALSE,
                       size.impact.correlate = dependence,
                       proptx.impact.correlate = proptx.dependence,
                       correlate.strength = 0.50,
                       size.ratio = 0.60 )
  
  my.fixed.effect.estimators( Yobs, Z, sid, df)
}





# Run a simulation with R trials and return all the simulation runs as a large dataframe.
run.scenario = function( J, n.bar, tau, dependence, proptx.dependence, variable.n, variable.p, ATE = 0.20, ICC = 0.20, p.tx = 0.65, R = 10, ID=NULL, ... ) {
  ptm = proc.time()
  
  scat( "Running J=%d\tn.bar=%d\ttau=%.2f\tATE=%.2f\tICC=%.2f\n\tprop tx=%.2f\tdependence=%s\tprop dep=%s\tR=%d (%d)\n", 
        J, n.bar,tau, ATE, ICC, p.tx,
        dependence, proptx.dependence, R)
  
  rps = plyr::rdply( R,  single.MLM.trial( n.bar=n.bar, J=J, tau.11.star=tau, 
                                           dependence = dependence, proptx.dependence = proptx.dependence,
                                           variable.n = variable.n, variable.p = variable.p,
                                           ATE.superpop=ATE, ICC=ICC, p.tx=p.tx, ... ),
                     .id="run", .progress="text" )
  rps$subrun = paste0( rps$run, ".", rps$subrun )
  
  scat("**\n**\tTotal time elapsed:\n")
  tot.time = proc.time() - ptm
  print(tot.time)
  sim.per.min = R / (tot.time["elapsed"] / 60)
  scat("Simulations per minute = %.2f\n", sim.per.min )
  rps$sim.per.min = sim.per.min
  rps
}


lmsim = expand.grid( J = c( 10, 20, 30, 40, 50 ), tau=c(0,0.2^2) )

lmsim$run = pmap( lmsim, run.scenario, 
                      n.bar = 100, dependence = 0, proptx.dependence = 0, variable.n = TRUE, variable.p = TRUE, 
                      include.MLM=FALSE, include.block=FALSE, R = 100 )

lmsim = unnest( lmsim )

head( lmsim )
table( lmsim$method )

# Look at club sandwich
rst = lmsim %>% group_by( method, J, tau ) %>% 
  summarise( E.ATE.hat = mean( ATE.hat ),
             SE = sd( ATE.hat ),
             E.SE.hat = mean( SE.hat ),
             med.SE.hat = median( SE.hat ),
             sd.SE.hat = sd( SE.hat ),
             rat = E.SE.hat / SE )
rst


# ATE estimates
ggplot( rst, aes( J, E.ATE.hat, col=method ) ) +
  facet_wrap( ~ tau ) +
  geom_point() + geom_line() 


# SE estimates vs. true SE
ggplot( rst, aes( J, rat, col=method ) ) +
  facet_wrap( ~ tau ) +
  geom_point() + geom_line() +
  geom_hline( yintercept= 1.0 )


# Sd of SE estimates 
ggplot( rst, aes( J, sd.SE.hat, col=method ) ) +
  facet_wrap( ~ tau ) +
  geom_point() + geom_line()

