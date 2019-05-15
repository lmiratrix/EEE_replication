

# Explore existing statistics to get our simulation parameters

source( "simulation_single_trial.R" )

library( XLConnect )
wb = loadWorkbook( "../Cross Study Stats/Xstudy_desc_stats190421.xlsx" )
dat <- readWorksheet(wb, sheet = 1, header=TRUE)
head( dat )


mosaic::favstats( dat$N )


mosaic::favstats( dat$Number_of_sites )


mosaic::favstats( dat$Number_of_sites )


wb = loadWorkbook( "../Cross Study Stats/Xstudy_Stats_And_Estimates190421.xlsx" )
dat2 <- readWorksheet(wb, sheet = 1, header=TRUE)
head( dat2 )


library( tidyverse )
dat3 <- read_csv( "../Cross Study Stats/XstudyResults190421.csv" )
head( dat3 )
nrow( dat3 )
table( dat3$model )
dat3 = filter( dat3, model=="FIRC, block" )
nrow( dat3 )

# Based on this, we set ICC=0.20
mosaic::favstats( dat3$ICC )

# Based on this, we set prop tx to 0.65
mosaic::favstats( pmax( 1 - dat3$Mean_PT_site,  dat3$Mean_PT_site ) )

# Cross site variation of 0, 0.10, and 0.20 seems reasonable
mosaic::favstats( sqrt( dat3$X_site_variation ) )

mosaic::favstats( dat3$SD_site_size / dat3$Avg_site_size )


#### Look at what our simulation studies generate #####

describe.data = function( n.bar, J, tau.11.star, dependence, proptx.dependence, variable.n, variable.p,
                             ATE.superpop = 0.2, n.runs = 3 ) {
  
  df = gen.dat.no.cov( n.bar=n.bar, J=J,
                       tau.11.star = tau.11.star,
                       ICC = 0.20,
                       p = 0.65,
                       variable.n = TRUE,
                       variable.p = TRUE,
                       finite.model = FALSE,
                       size.impact.correlate = dependence,
                       proptx.impact.correlate = proptx.dependence,
                       correlate.strength = 0.5 )
  
  tau.S = attr( df, "tau.S")
  
  df = rerandomize.data( df )
  
  params = df %>% group_by( sid ) %>%
    summarise( ATE = mean( Y1 ) - mean( Y0 ),
               n = n(),
               ATE.hat = mean( Y1[Z==1] ) - mean( Y0[Z==0] ),
               p = mean( Z == 1 ) )
  
  ATE.person = with( df, mean( Y1 - Y0 ) )
  
  
  sstat = params %>% summarise( ATE.site = mean( ATE ),
                                sd.n = sd( n ),
                                sd.p = sd( p ),
                                cor.p.Bhat = cor( ATE.hat, p ),
                                cor.n.Bhat = cor( ATE.hat, n ),
                                cor.p = cor( ATE, p ),
                                cor.n = cor( ATE, n ) )

  sstat = mutate( sstat,
                 ATE.finite.person=ATE.person )
  sstat
}


scenarios = make.scenario.list()
scenarios

scenarios$run = pmap( scenarios, describe.data )


scenarios = unnest(scenarios)
head( scenarios )

ggplot( scenarios, aes( proptx.dependence, cor.p.Bhat ) ) +
  facet_wrap( ~ tau ) +
  geom_point()


ggplot( scenarios, aes( proptx.dependence, cor.n.Bhat ) ) +
  facet_wrap( ~ tau ) +
  geom_point()

aa =scenarios %>% group_by( tau, dependence, proptx.dependence ) %>%
  summarise( mean.corr.p.hat = mean( cor.p.Bhat ),
             sd.corr.p.hat = sd( cor.p.Bhat ),
             mean.corr.n.hat = mean( cor.n.Bhat ),
             sd.corr.n.hat = sd( cor.n.Bhat ),
             mean.corr.p = mean( cor.p ),
             mean.corr.n = mean( cor.n ),
             var.site = mean( sd.n / n.bar ) )

aa

ggplot( aa, aes( proptx.dependence, mean.corr.p.hat ) ) +
  facet_grid( tau ~ dependence, labeller = label_both ) +
  geom_point() + geom_hline( yintercept= 0 )



aa = scenarios %>%  summarise(  var.site = mean( sd.n / n.bar ) )
mosaic::fav_stats( scenarios$sd.n / scenarios$n.bar )
aa
mosaic::favstats( dat3$SD_site_size / dat3$Avg_site_size )
qplot(  dat3$SD_site_size / dat3$Avg_site_size )


dplyr::select( dat3, outcome, N, Avg_site_size, SD_site_size )
qplot( N, Avg_site_size, data=filter( dat3, N < 20000 ) ) +
  geom_smooth( method="lm" )

dplyr::select( scenarios, n.bar, sd.n )
qplot( n.bar, sd.n, data=scenarios ) +
  geom_smooth( method="lm" )

