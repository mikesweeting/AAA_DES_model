## SCENARIO 0: RESULTS FOR SURVEILLANCE COHORT (STATUS QUO)

## Install required packages if not already installed
list.of.packages <- c("Rcpp", "expm", "msm", "foreach", "iterators", "doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(msm)
library(foreach)
library(iterators)
library(doParallel)

rm(list=ls())

## Set the working directory to the root directory of AAA_DES_model (can only run this if sourcing the file)
this.dir <- dirname(parent.frame(2)$ofile)
setwd(paste0(this.dir,"/../../"))

# Model
source("functions/DES_Model.R")

## Input parameters
source("input/NAAASP_Men_2020-05-11/DES_Data_Input_NAAASP_Men_30years_time_horizon_2020-05-11.R") 

## Change v1other$aortaDiameterThresholds to be a list (new syntax)
v1other$aortaDiameterThresholds <- list(v1other$aortaDiameterThresholds)

## Also allow monitoring after contraindication (currently discharged)
v1other$monitoringIntervals[4] <- 0.25 ## 3 monthly monitoring for those contraindicated

## screening group only
v0$treatmentGroups <- "screening"

## List parameters
v0
v1other
v2
v1distributions

## Set other quantities
v0$returnEventHistories <- T ## return individual event histories
v0$returnAllPersonsQuantities <- F ## To save memory we will not return individual HE quantitites
v0$method <- "serial"

v0$numberOfPersons <- 1e7 



###############################################################################################################################################################
# SURVEILLANCE COHORT
###############################################################################################################################################################


## Those under surveillance. Distribution of baseline age and diameter
personData.surv <- expand.grid(startAge = 65:94, baselineDiameter = seq(3.0, 5.4, by = 0.1))
fileName <- "input/NAAASP_COVID_modelling/naaasp_men_surv_may20.csv"
temp <- read.csv(fileName, comment.char="")[,1:3]
names(temp) <- c("startAge", "baselineDiameter", "prob")
personData.surv <- merge(personData.surv, temp, by = c("startAge", "baselineDiameter"))

v2$probOfRequireReinvitation <- setType(0, "probability")
v2$probOfAttendScreen <- setType(1, "probability")
v2$probOfNonvisualization <- setType(0, "probability")
v2$costs["inviteToScreen"] <- 0
v2$costs["requireReinvitation"] <- 0
v2$costs["screen"] <- v2$costs["monitor"]


##################################################################################################################################################################

## SCENARIO 0: RESULTS FOR SURVEILLANCE COHORT (STATUS QUO)
set.seed(3210)
v0$randomSeed<-3210
scen0.surv <- processPersons(v0, v1other, v2, personData.surv)
scen0.surv$meanQuantities
TableOfCounts(scen0.surv, v1other)
scen0.tabresults<-Eventsandcosts(scen0.surv)

inv<-scen0.tabresults[1,2]
scr<-scen0.tabresults[2,2]
reinv<-scen0.tabresults[3,2]
nonatt<-scen0.tabresults[4,2]
monitor<-scen0.tabresults[6,2]
dropout<-scen0.tabresults[7,2]
oppdet<-scen0.tabresults[8,2]
consult<-scen0.tabresults[9,2]
elecevar<-scen0.tabresults[14,2]
elecopen<-scen0.tabresults[15,2]
rupt<-scen0.tabresults[16,2]
emerevar<-scen0.tabresults[17,2]
emeropen<-scen0.tabresults[18,2]
reintelecevar<-scen0.tabresults[21,2]
reintemerevar<-scen0.tabresults[21,2]
reintemeropen<-scen0.tabresults[23,2]
aaadead<-scen0.tabresults[24,2]
nonaaadead<-scen0.tabresults[25,2]
scen0ssummary<-data.frame(inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,rupt,emerevar,emeropen,reintelecevar,reintemerevar,reintemeropen,aaadead,nonaaadead)
scen0ssummary

