### Model testing

rm(list=ls())

source("functions/DES Model.R")
# Parameters.
source("input/parsForWomen30years_ref_model.R") 

# Settings.
v0$numberOfPersons <- 1e3
v0$returnEventHistories <- TRUE 
v0$recordSizes <- TRUE
v0$returnAllPersonsQuantities <- TRUE
v0$method <- "serial"
v0$randomSeed <- 2
v0$numberOfProcesses <- detectCores()-1 ## use number cores minus 1 to allow machine to still function
v0 <- setUnspecifiedElementsOfv0(v0)

# Analysis. 
systemTimeOutput <- system.time(result <- processPersons(v0, v1other, v2))
result$meanQuantities

## adding something else