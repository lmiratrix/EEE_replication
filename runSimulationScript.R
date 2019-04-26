#set seed
set.seed(123)

#load necessary libraries
library(MASS)

#NOTE THAT THERE ARE TWO GLOBAL VARIABLES FOR THE SBATCH SCRIPT:
INDEX_VAR1 <- as.numeric(as.character(Sys.getenv("INDEX_VAR1")))
INDEX_VAR2 <- as.numeric(as.character(Sys.getenv("INDEX_VAR2")))


################################
################################
################################
#Do stuff using index variables#
################################
################################
################################

#output to an RDS file (or a workspace); e.g.,
saveRDS(object = rObject, file = "fileName.rds")
#or
save.image("workspaceName.RData")
