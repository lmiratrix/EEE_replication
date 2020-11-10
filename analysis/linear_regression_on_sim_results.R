


head( sruns.core )

sruns.core$proptx.dependence = factor( sruns.core$proptx.dependence, levels=c(0, -1, 1), labels=c("indep","neg","pos") )
sruns.core$dependence = factor( sruns.core$dependence, levels=c("indep","corr" ) )
sruns.core$tau.f = as.factor( sruns.core$tau )

M0 = lm(  log(SE.s) ~ method + as.factor(J) + as.factor(n.bar) + dependence + proptx.dependence + tau.f, data=sruns.core )

summary( M0 )
exp( coef( M0 ) ) 



M0 = lm(  log(RMSE.fp) ~ method + as.factor(J) + as.factor(n.bar) + dependence + proptx.dependence + tau.f, data=sruns.core )

summary( M0 )
exp( coef( M0 ) ) 
