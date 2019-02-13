### Model testing

rm(list=ls())

source("functions/DES Model.R")
source("functions/shiny_output_functions.R")

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

dataFile <- "input/DES_Data_Input.xlsx"

v0$numberOfPersons <- 4000
result <- processPersons(v0, v1other, v2)
result$meanQuantities
# N = 4000
# quantity
# treatmentGroup lifeYears    qalys      cost discountedLifeYears discountedQalys discountedCost
# noScreening  20.29572 15.07328  80.07796            13.80255        10.35351       41.54997
# screening    20.30187 15.07777 112.05991            13.80629        10.35625       75.19093

## N = 1000
# quantity
# treatmentGroup lifeYears    qalys     cost discountedLifeYears discountedQalys
# noScreening  20.10800 14.93558 36.35599            13.68607        10.26736
# screening    20.10654 14.93454 34.29114            13.68546        10.26693
# quantity
# treatmentGroup discountedCost
# noScreening       19.22805
# screening         31.37802

## N=10,000
## 
# quantity
# treatmentGroup lifeYears    qalys      cost discountedLifeYears discountedQalys discountedCost
# noScreening  20.43834 15.17537  90.51524            13.88279        10.41109       48.58817
# screening    20.43857 15.17555 121.74456            13.88315        10.41136       80.37521

sum(unlist(lapply(1:length(result$eventHistories),singleEvent,result,treatmentGroup="screening",event="discharged")))
result$eventHistories[which(lapply(1:length(result$eventHistories),singleEvent,result,treatmentGroup="screening",event="discharged")==1)]


v0$numberOfPersons <- 1000
psaResult <- psa(v0, v1other, v1distributions)
psaResult$psaQuantities
# , , psaIterationNumber = 1
# 
# quantity
# treatmentGroup lifeYears    qalys     cost discountedLifeYears discountedQalys discountedCost
# noScreening  20.38234 15.13479 117.9981            13.85769        10.39256        78.6714
# screening    20.37787 15.13162 185.6964            13.85568        10.39114       136.1605
# 
# , , psaIterationNumber = 2
# 
# quantity
# treatmentGroup lifeYears    qalys     cost discountedLifeYears discountedQalys discountedCost
# noScreening  20.02354 14.87621 45.93064            13.65622        10.24650       28.24466
# screening    20.01804 14.87230 51.35114            13.65316        10.24433       41.89559
# 
# , , psaIterationNumber = 3
# 
# quantity
# treatmentGroup lifeYears    qalys     cost discountedLifeYears discountedQalys discountedCost
# noScreening  20.52723 15.23731 40.80214            13.92178        10.43773       20.47535
# screening    20.53599 15.24353 70.34241            13.92557        10.44042       49.64941
# 
# , , psaIterationNumber = 4
# 
# quantity
# treatmentGroup lifeYears    qalys     cost discountedLifeYears discountedQalys discountedCost
# noScreening  20.27296 15.05948 106.4204            13.81338        10.36319       54.60000
# screening    20.27416 15.06033  55.1765            13.81381        10.36349       41.19281
# 
# , , psaIterationNumber = 5
# 
# quantity
# treatmentGroup lifeYears    qalys     cost discountedLifeYears discountedQalys discountedCost
# noScreening  20.12834 14.95018 35.65125            13.69691        10.27506       20.81372
# screening    20.12834 14.95018 50.05628            13.69691        10.27506       41.03958


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
