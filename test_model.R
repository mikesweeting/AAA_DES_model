### Model testing

rm(list=ls())
setwd("Z:/My Documents/AAA/AAA_DES_model")

source("functions/DES Model.R")
source("functions/shiny_output_functions.R")

# Parameters.
#source("input/parsForWomen30years_ref_model.R") 

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

dataFile <- "input/DES_Data_Input_Women30years_ref.xlsx"


#result <- processPersons(v0, v1other, v2)
# v0$numberOfPersons <- 4000
result <- AAA_DES(dataFile, n = 4000)
result$meanQuantities
# N = 4000
# quantity
# lifeYears        qalys      cost discountedLifeYears discountedQalys discountedCost
# noScreening 20.523800526 15.235780413  87.39073        13.925316679    10.441022438       43.85291
# screening   20.526575058 15.237762766 123.60674        13.926808177    10.442090453       77.88237
# difference   0.002774532  0.001982354  36.21601         0.001491497     0.001068015       34.02946

## N = 1000
# v0$numberOfPersons <- 1000
result <- AAA_DES(dataFile, n = 1000)
result$meanQuantities
# quantity
# lifeYears        qalys      cost discountedLifeYears discountedQalys discountedCost
# noScreening 20.786882570 15.424169289 137.40919         14.07508309      10.5486484       62.82293
# screening   20.789555991 15.426080535 172.98851         14.07653061      10.5496857       96.49565
# difference   0.002673421  0.001911246  35.57932          0.00144752       0.0010373       33.67272

## N=10,000
# v0$numberOfPersons <- 10000
result <- AAA_DES(dataFile, n = 10000)
result$meanQuantities
## 
# lifeYears        qalys      cost discountedLifeYears discountedQalys discountedCost
# noScreening 20.530564252 15.240653628  97.07479        13.927591026    10.442723963       51.18366
# screening   20.533349740 15.242645517 133.25672        13.929090443    10.443799022       85.18490
# difference   0.002785488  0.001991889  36.18193         0.001499417     0.001075058       34.00124

## cheking whether there are any discharge events (if v1other$maxNumberMonitor is not set to Inf)
sum(unlist(lapply(1:length(result$eventHistories),singleEvent,result,treatmentGroup="screening",event="discharged")))
result$eventHistories[which(lapply(1:length(result$eventHistories),singleEvent,result,treatmentGroup="screening",event="discharged")==1)]



## Checking PSA
psaResult <- AAA_DES(dataFile, psa = TRUE, n = 1000, nPSA = 5)
psaResult$psaQuantities
# > psaResult$psaQuantities
# lifeYears        qalys     cost discountedLifeYears discountedQalys discountedCost ICER_lifeYears
# 1 0.0041241399 0.0029557887 34.76256        0.0022464183    0.0016156062       31.79993       8429.045
# 2 0.0017035121 0.0012162766 31.80846        0.0009023484    0.0006457528       30.16973      18672.284
# 3 0.0013331032 0.0009517425 28.28379        0.0006979471    0.0004993879       26.87437      21216.504
# 4 0.0008241431 0.0005852049 32.42316        0.0004549908    0.0003229937       31.07872      39341.663
# 5 0.0022636503 0.0016221360 33.12514        0.0012635369    0.0009081381       31.65814      14633.507
# ICER_discountedLifeYears ICER_discountedQalys INMB_discountedQalys_20000 INMB_discountedQalys_30000
# 1                 14155.83             19682.97                  0.5121978                  16.668260
# 2                 33434.68             46720.24                -17.2546713                 -10.797143
# 3                 38504.88             53814.62                -16.8866136                 -11.892734
# 4                 68306.26             96220.81                -24.6188438                 -21.388906
# 5                 25055.17             34860.48                -13.4953732                  -4.413992



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
