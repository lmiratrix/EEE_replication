

# Explore existing statistics to try and figure out what reasonable simulation
# parameters might be.

source( "simulation_support_code.R" )
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library( XLConnect )
wb = loadWorkbook( "../Cross Study Stats/Xstudy_desc_stats190421.xlsx" )
dat <- readWorksheet(wb, sheet = 1, header=TRUE)
head( dat )
nrow( dat )
table( dat$study )




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


# The scenarios we consider
scenarios = make.scenario.list()
scenarios




#### Selecting the ICC ####

# Based on this, we set ICC=0.20
mosaic::favstats( dat3$ICC )


#### Selecting proportion treated #####

# Based on this, we set prop tx to 0.65
mosaic::favstats( pmax( 1 - dat3$Mean_PT_site,  dat3$Mean_PT_site ) )



#### Selecting degree of cross site variation #####

# Cross site variation of 0, 0.10, and 0.20 seems reasonable
mosaic::favstats( sqrt( dat3$X_site_variation ) )
qplot( sqrt( dat3$X_site_variation  ) )



#### Selecting number of sites, site sizes  #####

mosaic::favstats( dat$N )
range( dat$N )

mosaic::favstats( dat$Number_of_sites )
range( dat$Number_of_sites )

# Our range
table( scenarios$J )


mosaic::favstats( dat3$Avg_site_size )
table( scenarios$n.bar )







#### Generate data across all our simulation studies #####


# We can describe a single dataset generated under a given set of parameters
single.MLM.trial( 200, 20, 0.20^2, dependence = 1, proptx.dependence = 1, variable.n = TRUE, variable.p = TRUE, just.describe.data = TRUE )

scenarios$run = pmap( scenarios, single.MLM.trial, just.describe.data = TRUE )

scenarios = unnest(scenarios)
head( scenarios )
table( scenarios$proptx.dependence )



#### Calibrating correlations of estimated and actual impacts with prop txed in site #####

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

qplot( scenarios$cor.p.Bhat )


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



tenn = read_csv( "../block effects variation exploration/TennStarMath180831.csv" )
table( tenn$outcome )
sum( tenn$N_Cj )
sum( tenn$N_Tj )
sum( tenn$N_j )

cor( tenn$N_j, tenn$B_j )



#### Comparing variability of site size in our scenarios to emperical examples #####

# Our scenarios
aa = scenarios %>% summarise(  var.site = mean( sd.n / n.bar ) ) %>% filter( var.site > 0 )
aa
nzro = filter( scenarios, sd.n > 0 )
mosaic::fav_stats( nzro$sd.n / nzro$n.bar )
qplot( nzro$sd.n / nzro$n.bar )


# The truth
mosaic::favstats( dat3$SD_site_size / dat3$Avg_site_size )
qplot(  dat3$SD_site_size / dat3$Avg_site_size )

# How much variation in site size vs. site size
# Look at ratio in terms of variance.  We are going with a ratio of 0.60 due to this.

rbind( mosaic::favstats( dat3$SD_site_size^2 / dat3$Avg_site_size^2 ),
  mosaic::favstats( nzro$sd.n^2 / nzro$n.bar^2 ) )


dplyr::select( dat3, outcome, N, Avg_site_size, SD_site_size )
qplot( N, Avg_site_size, data=filter( dat3, N < 20000 ) ) +
  geom_smooth( method="lm" )

sss = dplyr::select( filter( scenarios, sd.n > 0 ), n.bar, sd.n )
sss
qplot( n.bar, sd.n, data=sss ) +
  geom_smooth( method="lm" )



##### Selecting degree of variation in proportion treated ######


head( scenarios )

mosaic::fav_stats( dat2$SD_PT_site )
qplot( dat2$SD_PT_site )

mosaic::fav_stats( scenarios$sd.p )
qplot( scenarios$sd.p )




#### Looking at distribution of precision weights #####

df = single.MLM.trial( 200, 50, 0.20^2, dependence = 1, proptx.dependence = 1, variable.n = TRUE, variable.p = TRUE, just.return.data = TRUE )
head( df )
wts = df %>% group_by( sid ) %>% summarise( p = mean( Z ),
                                            n = n(),
                                            wt = p * (1-p) * n,
                                            Bj = mean( Y1 - Y0 ) )
wts$wt = wts$wt / mean( wts$wt )

qplot( wts$wt )
qplot( wts$p )
max( wts$wt ) / min( wts$wt )
qplot( wt, Bj, data=wts )
qplot( p, Bj, data=wts )
qplot( n, Bj, data=wts )
cor( wts$p, wts$Bj )
cor( wts$n, wts$Bj )
cor( wts$wt, wts$Bj )
