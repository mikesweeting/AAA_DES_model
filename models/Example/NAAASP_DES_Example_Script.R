## Example R script to run a DES AAA-screening model
## Using updated data on AAA screening in men (11/05/2020) contained in input/NAAASP_Men_2020-05-11/

rm(list=ls())
## Require the parallel package for parallel processing of the DES model
library(parallel)

## Set the working directory to the root directory of AAA_DES_model (can only run this if sourcing the file)
this.dir <- dirname(parent.frame(2)$ofile)
setwd(paste0(this.dir,"/../../"))

source("functions/DES Model.R")

# Parameters.
## There are two ways to load in the parameters. 
## 1) Populating the Excel spreasheet template
## An example for the 2020-05-11 parameterisation of the model is in the file below
dataFile <- "input/NAAASP_Men_2020-05-11/DES_Data_Input_NAAASP_Men_30years_time_horizon_2020-05-11.xlsx"

## 2) Secondly by reading in an R script file that sets the parameters as required by the model
## parameters are stored in one of the following lists 
#  2.1.   v0 (DES operational parameters, e.g. number of patient-pairs to simulate, number of PSA iterations, whether to store all event histories,...)
#  2.2.   v1other (global fixed parameters),
#  2.3.   v2 (global uncertain parameters - those that can be sampled from in a PSA), 
#  2.4    v1distributions (distributions to sample from in a PSA to populate v2)
source("input/NAAASP_Men_2020-05-11/DES_Data_Input_NAAASP_Men_30years_time_horizon_2020-05-11.R") 

## List the parameters that we will be using
v0
v1other
v2
v1distributions

# Add some more DES operational parameters for this specific run of the model
v0$returnEventHistories <- F ## To save memory we will not return individual event histories
v0$returnAllPersonsQuantities <- F ## To save memory we will not return individual HE quantitites
v0$method <- "parallel" ## Use v0$method = "parallel" to fit the DES using parallel processing. This works best with larger n, as there is a cost to set up parallel processing initially


## Number of patients-pairs
## For each pair (clone), one is assigned to the Invitation to Screening arm, one is assigned to the No Invitation to Screening arm
n <- 1e4 ## Ideally, this should be large (~10 million) and run on a supercomputer. For this example, we run for only 10,000 patient-pairs

## We now run the model using the AAA_DES function
## This by default does selectiveSampling (argument selectiveSampling = TRUE). This means that it only samples men above the diagnosis threshold
## This is more efficient for calculating mean QALYs and Costs but does mean that eventHistories cannot be returned

## Model A. This model runs the NAAASP model for men under surveillance intervals (1 year 3.0-4.4, 3 months 4.5-5.4)
## Running the model using the dataFile input
resultA <- AAA_DES(dataFile, n = n, extraInputs = list(v0 = v0))
resultA$meanQuantities

## If we specify both dataFile and extraInputs as inputs then extraInputs overwrites anything in the dataFile
resultA <- AAA_DES(dataFile, n = n, extraInputs = list(v0 = v0, v1other = v1other, v1distributions = v1distributions, v2 = v2))
resultA$meanQuantities

## If we wish to run the model without dataFile, input dataFile = NULL
resultA <- AAA_DES(dataFile = NULL, n = n, extraInputs = list(v0 = v0, v1other = v1other, v1distributions = v1distributions, v2 = v2))
resultA$meanQuantities

## To get event histories for all invited men, but without selective sampling (resulting in more Monte Carlo error in incremental costs and effects)
v0$returnEventHistories <- TRUE
resultA.eventHistories <- AAA_DES(dataFile = NULL, n = n, extraInputs = list(v0 = v0, v1other = v1other, v1distributions = v1distributions, v2 = v2),
                                  selectiveSampling = FALSE)
resultA.eventHistories$meanQuantities
## Listing the first 10 patient-pair event histories
resultA.eventHistories$eventHistories[1:10]

## Model B. Let's change the surveillance intervals to the following (2 year 3.0-4.4, 6 months 4.5-5.4)
v0$returnEventHistories <- FALSE
v1other$aortaDiameterThresholds <- c(3, 4.5, 5.5)
v1other$monitoringIntervals <- c(2, 2, 0.5)
resultB <- AAA_DES(dataFile = NULL, n = n, extraInputs = list(v0 = v0, v1other = v1other, v1distributions = v1distributions, v2 = v2))
resultB$meanQuantities


## Doing 10 PSA runs of model B (just using 1000 patient-pairs - NOTE, this should be much larger in practice)
resultB.psa <- AAA_DES(dataFile = NULL, n = 1000, nPSA = 10, extraInputs = list(v0 = v0, v1other = v1other, v1distributions = v1distributions, v2 = v2),
                                                       psa = TRUE)
resultB.psa$psaQuantities
