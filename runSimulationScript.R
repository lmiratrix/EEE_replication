
#load necessary libraries
#library(MASS)

index <- as.numeric(as.character(Sys.getenv("INDEX_VAR1")))
#INDEX_VAR2 <- as.numeric(as.character(Sys.getenv("INDEX_VAR2")))


################################
################################
################################
#Do stuff using index variables#
################################
################################
################################

source( "simulation_single_trial.R" )

#output to an RDS file (or a workspace); e.g.,
saveRDS(object = rObject, file = "fileName.rds")
#or
save.image("workspaceName.RData")
