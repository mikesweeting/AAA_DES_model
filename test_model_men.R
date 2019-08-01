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
# quantity
# treatmentGroup lifeYears    qalys     cost discountedLifeYears discountedQalys discountedCost
# noScreening  18.10899 13.85543 324.5499            12.60154        9.680067       198.7866
# screening    18.11269 13.85828 360.8601            12.60589        9.683387       234.5935

## Checking PSA
psaResult <- AAA_DES(dataFile, psa = TRUE, n = 1000, nPSA = 5, extraInputs = list(v0 = v0, v1other = v1other, v1distributions = v1distributions, v2 = v2))
psaResult$psaQuantities
# , , psaIterationNumber = 1
# 
# quantity
# treatmentGroup lifeYears    qalys     cost discountedLifeYears discountedQalys discountedCost
# noScreening  17.76545 13.59409 266.4822            12.37555        9.507775       160.4007
# screening    17.77936 13.60473 263.4916            12.38542        9.515340       170.2054
# 
# , , psaIterationNumber = 2
# 
# quantity
# treatmentGroup lifeYears    qalys     cost discountedLifeYears discountedQalys discountedCost
# noScreening  18.00855 13.78001 241.0811            12.53847        9.632767       151.6118
# screening    18.11217 13.85828 373.7630            12.59885        9.678487       255.3200
# 
# , , psaIterationNumber = 3
# 
# quantity
# treatmentGroup lifeYears    qalys     cost discountedLifeYears discountedQalys discountedCost
# noScreening  17.97057 13.75310 252.8323            12.55992        9.650109       152.5875
# screening    18.00317 13.77776 342.6041            12.57969        9.665109       222.5807
# 
# , , psaIterationNumber = 4
# 
# quantity
# treatmentGroup lifeYears    qalys     cost discountedLifeYears discountedQalys discountedCost
# noScreening  17.95788 13.74312 233.1701            12.53848        9.633691       166.4996
# screening    18.04479 13.80859 311.4383            12.58549        9.669165       226.8802
# 
# , , psaIterationNumber = 5
# 
# quantity
# treatmentGroup lifeYears    qalys     cost discountedLifeYears discountedQalys discountedCost
# noScreening  18.00159 13.77507 220.9574            12.54420        9.637297       128.6028
# screening    18.03720 13.80191 324.7027            12.56262        9.651207       203.7491
