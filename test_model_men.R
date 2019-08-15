### Model testing

rm(list=ls())
setwd("Z:/My Documents/AAA/AAA_DES_model")

source("functions/DES Model.R")
source("functions/shiny_output_functions.R")

# Parameters.
source("input/parsForMen30years.R") 

# Settings.
# v0$numberOfPersons <- 1e3
# v0$showEventHistories <- FALSE  ### DEBUGGING FUNCTION - NOT TO BE USED
# v0$returnEventHistories <- TRUE ### SHOULD GENERALLY BE RETURNED UNLESS PSA IS DONE - DEFAULT TRUE, BUT SET TO FALSE IF PSA??
# v0$recordSizes <- TRUE ## RECORDS AAA DIAMETER SIZES AT SCREENING EVENT -- THIS SHOULD ALWAYS BE TRUE 
# v0$returnAllPersonsQuantities <- FALSE ## Aggregated quantities (lifeYears, qalys, ....) for each individual. DEFAULT TO FALSE
# v0$method <- "serial" ## DEFAULT TO SERIAL
# v0$randomSeed <- 2 ## DEFAULT TO 2
# v0$verbose <- TRUE ## PRINTS OUT TO SCREEN ALL OPTIONS AND DATA -- DEFAULT TO TRUE
# v0$numberOfProcesses <- detectCores()-1 ## DEFAULT TO number cores minus 1 to allow machine to still function 


# Analysis. 
baselineData <- data.frame(sex="Male", age=65, smoker=rep(c(0,1),c(87,13)))
targetGroup <- data.frame(sex="Male", age=65, smoker=1)
# AAA_DES(baselineData=baselineData, baselineDiameters, targeted = TRUE, targetGroup = targetGroup)
## baselineData - data.frame with characteristics of patient population for comparison of screening vs. not screening
## baselineDiameters -- list(targeted, notTargeted), each w
## targeted - If true then baselineDiameters and all parameters are a list with two elements (targeted and notTargeted)

dataFile <- "input/DES_Data_Input_Men30years.xlsx"

# v0$numberOfPersons <- 4000
# result <- processPersons(v0, v1other, v2)
# result$meanQuantities

result <- AAA_DES(dataFile, n = 4000, extraInputs = list(v0 = v0, v1other = v1other, v1distributions = v1distributions, v2 = v2))
result$meanQuantities
# N = 4000
# > result$meanQuantities
# lifeYears       qalys      cost discountedLifeYears discountedQalys discountedCost
# noScreening 18.24949156 13.96232845 339.04724        12.692681384     9.749719831      213.26114
# screening   18.26480492 13.97390777 386.67075        12.701752542     9.756594846      261.17318
# difference   0.01531336  0.01157933  47.62351         0.009071158     0.006875014       47.91204

## Checking PSA
psaResult <- AAA_DES(dataFile, psa = TRUE, n = 1000, nPSA = 5, extraInputs = list(v0 = v0, v1other = v1other, v1distributions = v1distributions, v2 = v2))
psaResult$psaQuantities
# lifeYears       qalys     cost discountedLifeYears discountedQalys discountedCost ICER_lifeYears
# 1 0.01595923 0.012056904 54.02139         0.009190811     0.006959797       52.62440       3384.962
# 2 0.01902521 0.014382362 45.45276         0.011046226     0.008372749       45.42861       2389.080
# 3 0.01258667 0.009497967 42.96374         0.007245606     0.005478409       43.08668       3413.431
# 4 0.01554312 0.011728462 48.43916         0.008754702     0.006619352       48.40103       3116.437
# 5 0.02327185 0.017565009 55.93527         0.013235683     0.010012087       54.72286       2403.559
# ICER_discountedLifeYears ICER_discountedQalys INMB_discountedQalys_20000 INMB_discountedQalys_30000
# 1                 5725.762             7561.197                   86.57154                   156.1695
# 2                 4112.591             5425.770                  122.02637                   205.7539
# 3                 5946.595             7864.816                   66.48150                   121.2656
# 4                 5528.575             7312.049                   83.98602                   150.1795
# 5                 4134.495             5465.680                  145.51887                   245.6397