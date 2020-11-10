


head( mns )


m2 = filter( mns, method %in% c("DB-Persons", "FE" ), dependence =="indep", proptx.dependence=="indep" ) %>%
    dplyr::select( method, J, n.bar, tau, RMSE.fp, RMSE.sp )

head( m2 )


ggplot( m2, aes( tau, RMSE.fp, col=method ) ) +
  facet_grid( J ~ n.bar ) +
  geom_point() + geom_line()



ggplot( m2, aes( tau, RMSE.sp, col=method ) ) +
  facet_grid( J ~ n.bar ) +
  geom_point() + geom_line()
