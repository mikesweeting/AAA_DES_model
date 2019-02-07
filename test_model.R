### Model testing

rm(list=ls())

source("functions/DES Model.R")
# Parameters.
source("input/parsForWomen30years_ref_model.R") 

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

result <- processPersons(v0, v1other, v2)

result$meanQuantities
##
## quantity
## treatmentGroup lifeYears    qalys      cost discountedLifeYears discountedQalys discountedCost
## noScreening  20.05352 14.89450  82.87577            13.64510        10.23625       47.92276
## screening    20.04199 14.88608 104.86951            13.63742        10.23063       72.18988

## TO DO
## 1. CURRENTLY WORKS WITH ONLY 1 AGE - DO WE EVEN WANT TO CONSIDER AN AGE RANGE?
## 1. WANT TO TARGET A POPULATION (E.G. SMOKERS) -- NEED DISTRIBUTIONS OF BASELINE COVARIATES TO SAMPLE FROM, AGE, SEX, AAA DIAMETER, 
## AND TO ALLOW ALL PARAMETERS TO DEPEND ON THESE... VERY GENERAL WISH.

## CURRENTLY TWO REALITIES ARE ALLOWED: Screening and noScreening. targetedScreening is a mixture of these. Do we need a third group?
## THEN IN PROCESS ONE PAIR - THOSE IN TARGETED SCREENING ARE ASSIGNED EITHER "SCREENING" OR "NO SCREENING" OPTION??
## THINK WE NEED A FURTHER WRAPPER FUNCTION - IF TARGETED = TRUE, WE EVALUATE PATIENT CHARACTERISTICS. 1) IF IN THE TARGET GROUP THEN 
## PROCESS ONE PAIR (SCREEN VS. NO SCREEN FOR TWIN). 2) IF NOT IN TARGET GROUP THEN PAIRS SHOULD BE IDENTICAL SO WE WANT TO COPY NO SCREENING
## ARM TO SCREENING ARM

## 1. CHANGE NAME FROM processPersons to DES_AAA
## 1b. AAA_DES could be a wrapper function for processPersons with nicer arguments (not v0, v1other and v2)??
## 2. returnEventHistories SHOULD GENERALLY BE RETURNED UNLESS PSA IS DONE - DEFAULT TRUE, BUT SET TO FALSE IF PSA??
## 3. RETURN SAME RESULTS IF SEED IS SET NO MATTER IF SERIAL OR PARALLEL -- DIFFICULT QUESTION
