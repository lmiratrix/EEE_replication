

# Explore existing statistics to try and figure out what reasonable simulation
# parameters might be.

source( "simulation_single_trial.R" )

library( XLConnect )
wb = loadWorkbook( "../Cross Study Stats/Xstudy_desc_stats190421.xlsx" )
dat <- readWorksheet(wb, sheet = 1, header=TRUE)
head( dat )


mosaic::favstats( dat$N )
range( dat$N )

mosaic::favstats( dat$Number_of_sites )
range( dat$Number_of_sites )



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


#### Calculating some fixed parameters for simulations ####

# Based on this, we set ICC=0.20
mosaic::favstats( dat3$ICC )

# Based on this, we set prop tx to 0.65
mosaic::favstats( pmax( 1 - dat3$Mean_PT_site,  dat3$Mean_PT_site ) )

# Cross site variation of 0, 0.10, and 0.20 seems reasonable
mosaic::favstats( sqrt( dat3$X_site_variation ) )
qplot( sqrt( dat3$X_site_variation  ) )

# How much variation in site size vs. site size
mosaic::favstats( dat3$SD_site_size / dat3$Avg_site_size )




#### Look at what our simulation studies generate #####




##
## Summarize DGP for all the scenarios
##

source( "simulation_single_trial.R" )

scenarios = make.scenario.list()
scenarios

single.MLM.trial( 200, 20, 0.20^2, FALSE, FALSE, FALSE, FALSE, just.describe.data = TRUE )

scenarios$run = pmap( scenarios, single.MLM.trial, just.describe.data = TRUE )


scenarios = unnest(scenarios)
head( scenarios )
table( scenarios$proptx.dependence )


## Look at estimated correlations with prop txed in site

summary( scenarios$cor.p.Bhat )

ggplot( scenarios, aes( proptx.dependence, cor.p.Bhat ) ) +
  facet_grid(.  ~ tau, labeller = label_both) +
  geom_point()

scenarios %>% filter( !is.na( cor.p.Bhat ) ) %>%
  group_by( proptx.dependence, tau ) %>% 
  summarise( mn.cor.hat = mean( cor.p.Bhat ),
             sd.cor.hat = sd( cor.p.Bhat ),
             mn.cor = mean( cor.p ),
             sd.cor = sd( cor.p ) )



aa = scenarios %>% filter( !is.na( cor.p.Bhat ), !is.na( cor.n.Bhat ) ) %>%
  group_by( tau, dependence, proptx.dependence ) %>%
  summarise( mn.corr.p.hat = mean( cor.p.Bhat ),
             sd.corr.p.hat = sd( cor.p.Bhat ),
             mn.corr.n.hat = mean( cor.n.Bhat ),
             sd.corr.n.hat = sd( cor.n.Bhat ),
             mn.corr.p = mean( cor.p ),
             mn.corr.n = mean( cor.n ),
             site.rat = mean( sd.n^2 / n.bar^2 ) )

aa


ggplot( aa, aes( proptx.dependence, mn.corr.p.hat ) ) +
  facet_grid( tau ~ dependence, labeller = label_both ) +
  geom_point() + geom_hline( yintercept= 0 )



#### Comparing variability of site size  #####

# Our scenarios
aa = scenarios %>% summarise(  var.site = mean( sd.n / n.bar ) ) %>% filter( var.site > 0 )
aa
nzro = filter( scenarios, sd.n > 0 )
mosaic::fav_stats( nzro$sd.n / nzro$n.bar )
qplot( nzro$sd.n / nzro$n.bar )


# The truth
mosaic::favstats( dat3$SD_site_size / dat3$Avg_site_size )
qplot(  dat3$SD_site_size / dat3$Avg_site_size )

# in variance.  We are going with a ratio of 0.60 due to this.
mosaic::favstats( dat3$SD_site_size^2 / dat3$Avg_site_size^2 )


dplyr::select( dat3, outcome, N, Avg_site_size, SD_site_size )
qplot( N, Avg_site_size, data=filter( dat3, N < 20000 ) ) +
  geom_smooth( method="lm" )

sss = dplyr::select( filter( scenarios, sd.n > 0 ), n.bar, sd.n )
sss
qplot( n.bar, sd.n, data=sss ) +
  geom_smooth( method="lm" )

