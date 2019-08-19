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
# lifeYears       qalys      cost discountedLifeYears discountedQalys discountedCost
# noScreening 18.14576160 13.88413069 245.36191        12.637835922     9.708223819      155.63590
# screening   18.16096737 13.89562268 293.69314        12.646751449     9.714977453      203.89045
# difference   0.01520578  0.01149199  48.33123         0.008915527     0.006753634       48.25456

## Checking PSA
psaResult <- AAA_DES(dataFile, psa = TRUE, n = 1000, nPSA = 5, extraInputs = list(v0 = v0, v1other = v1other, v1distributions = v1distributions, v2 = v2))
psaResult$psaQuantities
# lifeYears       qalys     cost discountedLifeYears discountedQalys discountedCost ICER_lifeYears
# 1 0.009677407 0.007263019 47.34641         0.005050717     0.003789904       48.11619       4892.469
# 2 0.019544490 0.014755236 52.97209         0.011078663     0.008382782       51.37905       2710.334
# 3 0.020435051 0.015449109 51.19973         0.012059311     0.009139249       49.79230       2505.486
# 4 0.016339886 0.012354965 44.04155         0.009547528     0.007237160       44.69744       2695.340
# 5 0.017837088 0.013468895 55.19643         0.010308687     0.007800732       53.03410       3094.475
# ICER_discountedLifeYears ICER_discountedQalys INMB_discountedQalys_20000 INMB_discountedQalys_30000
# 1                 9526.606            12695.888                   27.68188                   65.58092
# 2                 4637.658             6129.117                  116.27659                  200.10441
# 3                 4128.951             5448.183                  132.99267                  224.38516
# 4                 4681.572             6176.103                  100.04575                  172.41735
# 5                 5144.603             6798.607                  102.98053                  180.98784