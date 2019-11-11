

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

rst = single.MLM.trial( 20, 10, 0.2^2, dependence = TRUE, proptx.dependence = TRUE, variable.n = TRUE, variable.p = TRUE )
head( rst )
table( rst$subrun )
