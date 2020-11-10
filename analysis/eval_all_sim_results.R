
# Evaluate results from the SE assessment simulation
#
# Evaluate all scenarios at once and print out aggregates
#
# This file focuses on the performance of the effect estimates themselves, NOT uncertainty estimates.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source( "load_simulation_results.R" )





#### Cut down simulation results to just the unique point estimates ####

simdata = filter( simdata, method %in% selected )
sruns = filter( sruns, method %in% selected )



#### Overall plots of estimator performance vs. finite person estimand ####
# Plots to see how the point-estimates perform across scenarios

# Look at finite-person RMSEs for all the scenarios
ggplot( sruns, aes( x=method, y=RMSE.fp, col=dependence ) ) +
  facet_grid( tau ~ n.bar + J, labeller = label_both ) +
  geom_point() +
  coord_flip()



ggplot( sruns, aes( x=n.bar, y=bias.fp, col=method, lty=dependence ) ) +
  facet_grid( tau ~ J, labeller = label_both, scales = "free_x" ) + 
  geom_point() + geom_line() +
  geom_hline( yintercept = 0 )

ggplot( sruns, aes( x=method, y=bias.fp, col=dependence) ) +
  facet_grid( tau ~ n.bar + J, labeller = label_both ) +
  geom_point() +
  geom_hline( yintercept = 0 ) +
  coord_flip()




#### Examining the bias-precision tradeoff of the design based and multilevel approaches ####

head( simdata )
unique( simdata$method )


# Looking at the SEs
ssimdata = filter( simdata, weight=="site" )
ssimdata = filter( ssimdata, method != "RICC" )

ggplot( ssimdata, aes( x=method, y=SE.hat ) ) +
  geom_boxplot() +
  coord_flip()



#### Looking at SE for the site estimators ####
sruns2 = sruns
table( sruns$method )
head( sruns )
sruns2 = filter( sruns, method %in% selected, weight=="site" )
table( sruns2$method )

ggplot( sruns2, aes( x=n.bar, y=SE.s, col=method, lty=dependence ) ) +
  facet_grid( tau ~ J, labeller = label_both, scales = "free_x" ) + 
  geom_point() + geom_line()


tb = sruns %>% group_by( method ) %>% 
  summarise( mean.SE = mean( SE.s ),
             mean.sd.SE = mean( sd.SE.s ) )
bl = filter( tb, method =="FIRC" )
tb$mean.SE = tb$mean.SE / bl$mean.SE
tb$mean.sd.SE = tb$mean.sd.SE / bl$mean.sd.SE
tb

head( sruns2 )


# how often does the DB-SP-Sites give smaller SEs than 



# Select which estimands we are looking at
sruns$RMSE = sruns$RMSE.ss
sruns$bias = sruns$bias.ss
sruns$SE = sruns$SE.s




# Looking at the RMSE of the estimates

ggplot( sruns, aes( x=n.bar, y=RMSE, col=method, lty=dependence ) ) +
  facet_grid( tau ~ J, labeller = label_both, scales = "free_x" ) + 
  geom_point() + geom_line()


s2 = sruns %>% ungroup() %>% group_by( n.bar, J, tau, dependence ) %>%
  mutate( RMSE.rel = 100 * RMSE / mean( RMSE ) )
head( s2 )
s2 = s2 %>% ungroup( ) %>% group_by( tau, code, population, weight, dependence ) %>% 
  summarise( RMSE = mean( RMSE.rel ) )
s2
ggplot( s2, aes( tau, RMSE, col=code, pch=weight ) ) +
  facet_wrap( ~ dependence ) +
  geom_point(size=3) + geom_line( )


M0 = lm( log(RMSE) ~ n.bar + J + dependence * tau * code - 1, data=sruns )
summary( M0 )
pds = expand.grid( n.bar = 100, J = 40, tau = c(0,0.1,0.2), code = unique( sruns$code ), dependence = unique( sruns$dependence ) )
pds$RMSE = exp( predict( M0, newdata=pds ) )
head( pds )
#pds = pds %>% group_by( n.bar, J, tau ) %>% mutate( RMSE = RMSE / RMSE[["DB-FP-Sites"]] )
pds %>% spread( code, RMSE )
ggplot( pds, aes( tau, RMSE, col=code ) ) +
  facet_wrap( ~ dependence ) + 
  geom_point() + geom_line() 



# Looking at bias
ggplot( sruns, aes( x=n.bar, y=bias, col=method, lty=dependence ) ) +
  facet_grid( tau ~ J, labeller = label_both, scales = "free_x" ) + 
  geom_point() + geom_line() +
  geom_hline( yintercept = 0 )


# Simple average bias across all scenarios
biases = sruns %>% group_by( method, dependence, tau ) %>% 
  summarise( bias = mean( bias ) )
ggplot( biases, aes( tau, bias, col=method ) ) +
  facet_wrap( ~ dependence ) +
  geom_line() + geom_point() +
  geom_hline( yintercept = 0 )










