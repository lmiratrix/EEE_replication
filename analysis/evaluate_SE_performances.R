
# Look at how the standard error estimators for each estimator perform


source( "load_simulation_results.R" )



head( sruns )
nrow( sruns )

ggplot( sruns, aes( x=method, y=inflate.f ) ) +
  facet_wrap( ~ dependence ) +
  geom_boxplot() +
  coord_flip() + 
  geom_hline( yintercept = 1, lwd=1,col="red" )


ggplot( sruns, aes( x=method, y=inflate.s ) ) +
  facet_wrap( ~ dependence ) +
  geom_boxplot() +
  coord_flip()+ 
  geom_hline( yintercept = 1, lwd=1,col="red" )


srun2 = dplyr::select( sruns, method, inflate.f, inflate.s ) %>%
  gather( inflate.f, inflate.s, key="population", value="inflation" )
ggplot( srun2, aes( x=method, y=inflation ) ) +
  facet_grid( ~ population, scales = "free" ) +
    geom_boxplot() +
  coord_flip()+ 
  geom_hline( yintercept = 1, lwd=1,col="red" ) +
  labs( y = "Ratios of average SE.hat to true SE across scenarios")


# Compare SE.hat and SE.hat.f
head( sruns )
ggplot( sruns, aes( mean.SE.hat, SE.hat.f ) ) + 
  geom_point( alpha=0.3 )
summary( sruns$mean.SE.hat - sruns$SE.hat.f )

qplot( mean.SE.hat - SE.hat.f, data=sruns )


# Look at variability of the standard error estimates
head( sruns )
srun2 = dplyr::select( sruns, method, sd.SE.s, sd.SE.f, SE.s, SE.f ) %>%
  mutate( sd.SE.s = sd.SE.s / SE.s,
          sd.SE.f = sd.SE.f / SE.f ) %>%
  gather( sd.SE.s, sd.SE.f, key="population", value="sd.SE" )
ggplot( srun2, aes( x=method, y=sd.SE ) ) +
  facet_grid( ~ population, scales = "free" ) +
  geom_boxplot() +
  coord_flip()+ 
  labs( y = "Ratio of Standard deviation of the standard errors to actual SEs (overall or within finite sample)" )



# How often are superpop SEs lower than finite pop SEs?
# Comparing FIRC to DB
head( simdata )
unique( simdata$method )
ssub = filter( simdata, method %in% c( "FIRC", "DB-FP-Persons", "FE-CR" ) ) %>% 
  dplyr::select( ID, J, n.bar, tau, dependence, subrun, method, SE.hat )
head( ssub )
ssub = spread( ssub, key=method, value=SE.hat )
head( ssub )
# Are SE estimates correlated?
ssub = ssub %>% group_by( J, n.bar, tau, dependence ) %>% 
  summarise( FIRC.less = mean( FIRC < `DB-FP-Persons` ),
             FE.CR.less = mean( `FE-CR` < `DB-FP-Persons` ) )
head( ssub )
ssub = gather( ssub, FIRC.less, FE.CR.less, key="method", value="percent" )
ggplot( ssub, aes( tau, percent, col=method, lty=dependence ) ) +
  facet_grid( J ~ n.bar ) +
  geom_point() + geom_line()


# Plot SEs across scenarios and methods
if ( FALSE ) {
  ggplot( simdata, aes( x=method, y=SE.hat ) ) +
    facet_grid( tau ~ n.bar + J, labeller = label_both ) +
    geom_boxplot() +
    coord_flip()
}



# Looking at the different SE hats for finite population estimators
head( simdata )
fsimdata = filter( simdata, population=="finite", weight=="person" )
ggplot( fsimdata, aes( x=method, y=SE.hat ) ) +
  facet_grid( tau ~ n.bar + J, labeller = label_both, scales = "free" ) +
  geom_boxplot() +
  coord_flip()
