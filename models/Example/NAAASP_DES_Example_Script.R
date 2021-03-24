## Example R script to run a DES AAA-screening model
## Using updated data on AAA screening in men (11/05/2020) contained in input/NAAASP_Men_2020-05-11/

rm(list=ls())
## Require the parallel package for parallel processing of the DES model
library(parallel)

## Set the working directory to the root directory of AAA_DES_model (can only run this if sourcing the file)
this.dir <- dirname(parent.frame(2)$ofile)
setwd(paste0(this.dir,"/../../"))

source("functions/DES_Model.R")

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
v0$method <- "serial" ## Use v0$method = "parallel" to fit the DES using parallel processing. This works best with larger n, as there is a cost to set up parallel processing initially


## Number of patients-pairs
## For each pair (clone), one is assigned to the Invitation to Screening arm, one is assigned to the No Invitation to Screening arm
n <- 1e4 ## Ideally, this should be large (~10 million) and run on a supercomputer. For this example, we run for only 10,000 patient-pairs

## Persons characteristics data.frame
## This should contain the distribution of ages at invitation to screening (in the column startAge) and probability weights (in the column prob)
## If prob is missing then the DES will sample with replacement from every row of the data.frame with equal probability
## If everyone is aged 65 at invitation to screening then we just need to specify the following data.frame
personData <- data.frame(startAge = 65)

## We now run the model using the AAA_DES function
## This by default does selectiveSampling (argument selectiveSampling = TRUE). This means that it only samples men above the diagnosis threshold
## This is more efficient for calculating mean QALYs and Costs but does mean that eventHistories cannot be returned

## Model A. This model runs the NAAASP model for men under surveillance intervals (1 year 3.0-4.4, 3 months 4.5-5.4)
## Running the model using the dataFile input
resultA <- AAA_DES(dataFile, n = n, extraInputs = list(v0 = v0), personData = personData)
resultA$meanQuantities

## If we specify both dataFile and extraInputs as inputs then extraInputs overwrites anything in the dataFile
resultA <- AAA_DES(dataFile, n = n, extraInputs = list(v0 = v0, v1other = v1other, v1distributions = v1distributions, v2 = v2), personData = personData)
resultA$meanQuantities

## If we wish to run the model without dataFile, input dataFile = NULL
resultA <- AAA_DES(dataFile = NULL, n = n, extraInputs = list(v0 = v0, v1other = v1other, v1distributions = v1distributions, v2 = v2), personData = personData)
resultA$meanQuantities

## To get event histories for all invited men, but without selective sampling (resulting in more Monte Carlo error in incremental costs and effects)
v0$returnEventHistories <- TRUE
resultA.eventHistories <- AAA_DES(dataFile = NULL, n = n, extraInputs = list(v0 = v0, v1other = v1other, v1distributions = v1distributions, v2 = v2),
                                  selectiveSampling = FALSE, personData = personData)
resultA.eventHistories$meanQuantities
## Listing the first 10 patient-pair event histories
resultA.eventHistories$eventHistories[1:10]
## Storing events in a data.frame
resultA.event.data.frame <- eventsData(resultA.eventHistories)
## Plotting when ruptures occurs
library(ggplot2)
eventsPlot(resultA.event.data.frame, "rupture", v1other)
## Using dplyr to summarise when ruptures occur, and last event before rupture
library(dplyr)
ruptures <- resultA.event.data.frame %>% group_by(treatmentGroup, person) %>%
  filter(any(event=="rupture")) %>%
  arrange(person, treatmentGroup, time) %>%
  mutate(lastEvent = lag(event))
## Overall number of ruptures and mean AAA size at rupture
ruptures %>%
  filter(event=="rupture") %>%
  group_by(treatmentGroup) %>%
  summarise( n = n(), meanAAASize = mean(AAASize))
## Same, but grouped by last known event
ruptures %>%
  filter(event=="rupture") %>%
  group_by(treatmentGroup, lastEvent) %>%
  summarise( n = n(), meanAAASize = mean(AAASize)) %>%
  arrange(treatmentGroup, meanAAASize)


## AAA_DES is a wrapper for the function processPersons and usually calls it twice, first to get absolute effects for the control group, then to get incremental effects (between invited and control)
## If we are only interested in the incremental effects we can run the model faster by specifying incrementalOnly = TRUE
v0$returnEventHistories <- FALSE
resultA <- AAA_DES(dataFile = NULL, n = n, extraInputs = list(v0 = v0, v1other = v1other, v1distributions = v1distributions, v2 = v2), 
                   personData = personData, incrementalOnly = TRUE)
resultA$meanQuantities

## Using personData with other patient characteristics
## personData can also be used to specify other patient characteristics, such as the baseline aorta distribution
## This can be specified in personData and will supersede inputs in v1other$baselineDiameters or in the DES_Input_Definitions.xlsx file
## Any data specified in personData will automatically be sampled jointly
## That is sampling will be done row-wise to allow the user to specify joint distributions of patient characteristics
## Cannot use selectiveSampling currently with this specification
personData <- data.frame(startAge = 65, baselineDiameter = v1other$baselineDiameters$size, prob = v1other$baselineDiameters$weight)
resultA.newSpec <- AAA_DES(dataFile, n = n, extraInputs = list(v0 = v0), personData = personData, selectiveSampling = FALSE)
resultA.newSpec$meanQuantities



## Model B. Let's change the surveillance intervals to the following:
## 2 years <3.0cm (once in surveillance)
## 2 year 3.0-4.4, 
## 6 months 4.5-5.4,
## No surveillance for contraindicated (monitoring interval = Inf)
personData <- data.frame(startAge = 65)
v0$returnEventHistories <- FALSE
v1other$aortaDiameterThresholds <- c(3, 4.5, 5.5)
v1other$monitoringIntervals <- c(2, 2, 0.5, Inf)
resultB <- AAA_DES(dataFile = NULL, n = n, extraInputs = list(v0 = v0, v1other = v1other, v1distributions = v1distributions, v2 = v2), personData=personData)
resultB$meanQuantities


## Doing 10 PSA runs of model B (just using 1000 patient-pairs - NOTE, this should be much larger in practice)
resultB.psa <- AAA_DES(dataFile = NULL, n = 1000, nPSA = 10, extraInputs = list(v0 = v0, v1other = v1other, v1distributions = v1distributions, v2 = v2),
                                                       psa = TRUE, personData = personData)
resultB.psa$psaQuantities
