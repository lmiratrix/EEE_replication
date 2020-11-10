# Process simulation results.
#
# This script compares fixed effect estimate (precision-weighting) to design
# based unbiased (finite-person)

library( tidyverse )

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source( "load_simulation_results.R" )


sruns2 = sruns %>% ungroup() %>% 
  dplyr::filter( weight == "person", method %in% selected ) %>%
  dplyr::select( ID, J, n.bar, tau, dependence, method, bias.fp, RMSE.fp )
head( sruns2 )

names(sruns2) = gsub( ".fp", "", names(sruns2) )

sruns2 = mutate( sruns2, 
                 method = fct_recode( method,
                                      "fixed" = "FE",
                                      "design" = "DB-FP-Persons" ) )


head( sruns2 )

ss = sruns2 %>% group_by( tau, method, dependence ) %>%
  summarise( bias = mean( bias ) )

ggplot( ss, aes( tau, bias, col=method ) ) +
  facet_wrap( ~ dependence ) +
  geom_point() + geom_line()


# Look at percent difference
sruns2w = reshape( data.frame( sruns2 ), idvar = "ID", 
                  v.names=c("bias","RMSE" ),
                  timevar="method",
                  direction="wide" )
head( sruns2w )

sruns2w = mutate( sruns2w, 
                  benefit.RMSE = RMSE.fixed / RMSE.design )

sruns2w$J = as.factor( sruns2w$J )

ggplot( sruns2w, aes( n.bar, benefit.RMSE, col=J, group=J, pch=dependence ) ) +
  facet_wrap( ~ tau, labeller = label_both ) +
  geom_point( size = 2) +
  geom_hline( yintercept = 1 )





if ( FALSE ) {
  # When will fixed effect estimators be more precise?
  df = gen.dat.no.cov( n.bar=200, J=20,
                       tau.11.star = 0.1^2,
                       ICC = 0.20,
                       variable.n = TRUE,
                       variable.p = TRUE,
                       size.impact.correlate = TRUE,
                       finite.model = FALSE )
  
  sites = df %>% group_by( sid ) %>% 
    summarise( n = n(),
               p.Z = mean( Z ),
               Y.hat = mean( Yobs[Z==1] ) - mean( Yobs[Z==0] ) )
  
  head( sites )
  qplot( sites$n )
  qplot( sites$p.Z )
  sites = mutate( sites, precision = p.Z * (1-p.Z) )
  range( sites$precision )
  qplot( sites$precision )
  qplot( sites$n, sites$Y.hat )
  cor( sites$n, sites$Y.hat )
  
  single.MLM.trial( 20, 10, 0.2^2 )
}




