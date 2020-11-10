
##
## Scraps of exploratory code
##


library( tidyverse )
load( "results/aggregated_simulation_results.RData" )


head( sruns.core )


#####  Calculate inflation of DB-Site over FIRC true SEs    ######

unique( sruns.core$method )
ss = filter( sruns.core, method %in% c( "FIRC", "DB-Sites" ) )
head( ss )

ss = dplyr::select( ss, c( method, ID, SE.s, sd.SE.hat.s ) )

ss = mutate( ss, frac.unc = sd.SE.hat.s / SE.s )

# Table of averages across scenarios
ss %>% group_by( method ) %>% 
  summarise( mean.SE.s = mean( SE.s ),
             mean.sd.SE.hat.s = mean( sd.SE.hat.s ),
             frac.unc = mean( frac.unc ))

head( ss )
ss2 = ss %>% dplyr::select( ID, method, sd.SE.hat.s ) %>%
  spread( method, sd.SE.hat.s )
head( ss2 )
summary( 100 * ss2$`DB-Sites` / ss2$FIRC )


##### Look at ratio of true SE of site vs FIRC  ######
ss = dplyr::select( ss, c( method, ID, SE.s ) )
ss2 = spread( ss, method, SE.s )
head( ss2 )
ss = spread( ss, method, SE.s )
head( ss )
ss = mutate( ss, inflate = 100 * `DB-Sites` / FIRC )
summary( ss$inflate )


head( ss )



##### Look at scenario ATEs  #####

ss.scen = filter( sruns.core, method=="DB-Persons" )
dim( ss.scen )
head( ss.scen )

ss.scen = mutate( ss.scen, del.ATE = ATE.super.person - ATE.super.site )
qplot( ss.scen$del.ATE )

qplot( ss.scen$tau, ss.scen$del.ATE ) +
  labs( x = "Cross site impact heterogeniety", 
        y = "Differnence between Site and Person" )
