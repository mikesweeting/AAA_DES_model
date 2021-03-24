# SURVEILLANCE COHORT
## SCENARIO S4: sequentially combining scenarios 1-3
# S1 surveillance scans suspension 
# S2.1 varying dropout rate to 10% for 1y (S2.1) 
# S2.2 varying dropout rate to 10%, for 2y (S2.2) 
# S3 threshold increase to 7cm for 2y
# S4 threshold increase to 7cm for 2y, with 2 lockdown (op suspension) periods

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
source("functions/DES_model.R")

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


## Those under surveillance
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



####################################################################################################################################################################
## SCENARIO S4: sequentially combining scenarios 1-3
# S1 surveillance scans suspension 
# S2.1 varying dropout rate to 10% for 1y (S2.1) 
# S2.2 varying dropout rate to 10%, for 2y (S2.2) 
# S3 threshold increase to 7cm for 2y
# S4 threshold increase to 7cm for 2y, with 2 lockdown (op suspension) periods

# ADDING S2.2+S3+S4
v1other$inviteToScreenSuspensionTime <- 0
v1other$aortaDiameterThresholds <- list(c(3, 4.5, 50), c(3, 4.5, 7), c(3, 4.5, 50), c(3, 4.5, 7), c(3, 4.5, 5.5))
attr(v1other$aortaDiameterThresholds, "timeCutPoints") <- c(0.25, 0.5, 0.75, 2)
set.seed(3210)
v0$randomSeed<-3210
v2$rateOfDropoutFromMonitoring <- setType(c(0.10, 0.05720712), "rate")
attr(v2$rateOfDropoutFromMonitoring, "timeCutPoints") <- 2

# surveillance scan suspension period=0.25 yrs
v1other$monitoringIntervalsSuspensionTime <- rep(0.25, length(v1other$monitoringIntervals))
scen1.surv <- processPersons(v0, v1other, v2, personData.surv)
scen1.surv$meanQuantities
TableOfCounts(scen1.surv, v1other)
scen1.tabresults<-Eventsandcosts(scen1.surv)
inv<-scen1.tabresults[1,2]
scr<-scen1.tabresults[2,2]
reinv<-scen1.tabresults[3,2]
nonatt<-scen1.tabresults[4,2]
monitor<-scen1.tabresults[6,2]
dropout<-scen1.tabresults[7,2]
oppdet<-scen1.tabresults[8,2]
consult<-scen1.tabresults[9,2]
elecevar<-scen1.tabresults[14,2]
elecopen<-scen1.tabresults[15,2]
rupt<-scen1.tabresults[16,2]
emerevar<-scen1.tabresults[17,2]
emeropen<-scen1.tabresults[18,2]
reintelecevar<-scen1.tabresults[21,2]
reintemerevar<-scen1.tabresults[21,2]
reintemeropen<-scen1.tabresults[23,2]
aaadead<-scen1.tabresults[24,2]
nonaaadead<-scen1.tabresults[25,2]
n<-v0$numberOfPersons
dropoutrate<-0.1
dropoutperiod<-2
period<-0.25
thresh<-7
scen4dsummary<-data.frame(n,dropoutrate,dropoutperiod,period,thresh,inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,rupt,emerevar,emeropen,reintelecevar,reintemerevar,reintemeropen,aaadead,nonaaadead)


# surveillance scan suspension period=0.5 yrs
v1other$monitoringIntervalsSuspensionTime <- rep(0.5, length(v1other$monitoringIntervals))
scen1.surv <- processPersons(v0, v1other, v2, personData.surv)
scen1.surv$meanQuantities
TableOfCounts(scen1.surv, v1other)
scen1.tabresults<-Eventsandcosts(scen1.surv)
inv<-scen1.tabresults[1,2]
scr<-scen1.tabresults[2,2]
reinv<-scen1.tabresults[3,2]
nonatt<-scen1.tabresults[4,2]
monitor<-scen1.tabresults[6,2]
dropout<-scen1.tabresults[7,2]
oppdet<-scen1.tabresults[8,2]
consult<-scen1.tabresults[9,2]
elecevar<-scen1.tabresults[14,2]
elecopen<-scen1.tabresults[15,2]
rupt<-scen1.tabresults[16,2]
emerevar<-scen1.tabresults[17,2]
emeropen<-scen1.tabresults[18,2]
reintelecevar<-scen1.tabresults[21,2]
reintemerevar<-scen1.tabresults[21,2]
reintemeropen<-scen1.tabresults[23,2]
aaadead<-scen1.tabresults[24,2]
nonaaadead<-scen1.tabresults[25,2]
n<-v0$numberOfPersons
dropoutrate<-0.1
dropoutperiod<-2
period<-0.5
thresh<-7
temp<-data.frame(n,dropoutrate,dropoutperiod,period,thresh,inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,rupt,emerevar,emeropen,reintelecevar,reintemerevar,reintemeropen,aaadead,nonaaadead)
scen4dsummary<-rbind(scen4dsummary,temp)


# surveillance scan suspension period=1.0 yrs
v1other$monitoringIntervalsSuspensionTime <- rep(1, length(v1other$monitoringIntervals))
scen1.surv <- processPersons(v0, v1other, v2, personData.surv)
scen1.surv$meanQuantities
TableOfCounts(scen1.surv, v1other)
scen1.tabresults<-Eventsandcosts(scen1.surv)
inv<-scen1.tabresults[1,2]
scr<-scen1.tabresults[2,2]
reinv<-scen1.tabresults[3,2]
nonatt<-scen1.tabresults[4,2]
monitor<-scen1.tabresults[6,2]
dropout<-scen1.tabresults[7,2]
oppdet<-scen1.tabresults[8,2]
consult<-scen1.tabresults[9,2]
elecevar<-scen1.tabresults[14,2]
elecopen<-scen1.tabresults[15,2]
rupt<-scen1.tabresults[16,2]
emerevar<-scen1.tabresults[17,2]
emeropen<-scen1.tabresults[18,2]
reintelecevar<-scen1.tabresults[21,2]
reintemerevar<-scen1.tabresults[21,2]
reintemeropen<-scen1.tabresults[23,2]
aaadead<-scen1.tabresults[24,2]
nonaaadead<-scen1.tabresults[25,2]
n<-v0$numberOfPersons
dropoutrate<-0.1
dropoutperiod<-2
period<-1
thresh<-7
temp<-data.frame(n,dropoutrate,dropoutperiod,period,thresh,inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,rupt,emerevar,emeropen,reintelecevar,reintemerevar,reintemeropen,aaadead,nonaaadead)
scen4dsummary<-rbind(scen4dsummary,temp)


# surveillance scan suspension period=2.0 yrs
v1other$monitoringIntervalsSuspensionTime <- rep(2, length(v1other$monitoringIntervals))
scen1.surv <- processPersons(v0, v1other, v2, personData.surv)
scen1.surv$meanQuantities
TableOfCounts(scen1.surv, v1other)
scen1.tabresults<-Eventsandcosts(scen1.surv)
inv<-scen1.tabresults[1,2]
scr<-scen1.tabresults[2,2]
reinv<-scen1.tabresults[3,2]
nonatt<-scen1.tabresults[4,2]
monitor<-scen1.tabresults[6,2]
dropout<-scen1.tabresults[7,2]
oppdet<-scen1.tabresults[8,2]
consult<-scen1.tabresults[9,2]
elecevar<-scen1.tabresults[14,2]
elecopen<-scen1.tabresults[15,2]
rupt<-scen1.tabresults[16,2]
emerevar<-scen1.tabresults[17,2]
emeropen<-scen1.tabresults[18,2]
reintelecevar<-scen1.tabresults[21,2]
reintemerevar<-scen1.tabresults[21,2]
reintemeropen<-scen1.tabresults[23,2]
aaadead<-scen1.tabresults[24,2]
nonaaadead<-scen1.tabresults[25,2]
n<-v0$numberOfPersons
dropoutrate<-0.1
dropoutperiod<-2
period<-2
thresh<-7
temp<-data.frame(n,dropoutrate,dropoutperiod,period,thresh,inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,rupt,emerevar,emeropen,reintelecevar,reintemerevar,reintemeropen,aaadead,nonaaadead)
scen4dsummary<-rbind(scen4dsummary,temp)


# surveillance scan suspension period=3.0 yrs
v1other$monitoringIntervalsSuspensionTime <- rep(3, length(v1other$monitoringIntervals))
scen1.surv <- processPersons(v0, v1other, v2, personData.surv)
scen1.surv$meanQuantities
TableOfCounts(scen1.surv, v1other)
scen1.tabresults<-Eventsandcosts(scen1.surv)
inv<-scen1.tabresults[1,2]
scr<-scen1.tabresults[2,2]
reinv<-scen1.tabresults[3,2]
nonatt<-scen1.tabresults[4,2]
monitor<-scen1.tabresults[6,2]
dropout<-scen1.tabresults[7,2]
oppdet<-scen1.tabresults[8,2]
consult<-scen1.tabresults[9,2]
elecevar<-scen1.tabresults[14,2]
elecopen<-scen1.tabresults[15,2]
rupt<-scen1.tabresults[16,2]
emerevar<-scen1.tabresults[17,2]
emeropen<-scen1.tabresults[18,2]
reintelecevar<-scen1.tabresults[21,2]
reintemerevar<-scen1.tabresults[21,2]
reintemeropen<-scen1.tabresults[23,2]
aaadead<-scen1.tabresults[24,2]
nonaaadead<-scen1.tabresults[25,2]
n<-v0$numberOfPersons
dropoutrate<-0.1
dropoutperiod<-2
period<-3
thresh<-7
temp<-data.frame(n,dropoutrate,dropoutperiod,period,thresh,inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,rupt,emerevar,emeropen,reintelecevar,reintemerevar,reintemeropen,aaadead,nonaaadead)
scen4dsummary<-rbind(scen4dsummary,temp)


# surveillance scan suspension period=4.0 yrs
v1other$monitoringIntervalsSuspensionTime <- rep(4, length(v1other$monitoringIntervals))
scen1.surv <- processPersons(v0, v1other, v2, personData.surv)
scen1.surv$meanQuantities
TableOfCounts(scen1.surv, v1other)
scen1.tabresults<-Eventsandcosts(scen1.surv)
inv<-scen1.tabresults[1,2]
scr<-scen1.tabresults[2,2]
reinv<-scen1.tabresults[3,2]
nonatt<-scen1.tabresults[4,2]
monitor<-scen1.tabresults[6,2]
dropout<-scen1.tabresults[7,2]
oppdet<-scen1.tabresults[8,2]
consult<-scen1.tabresults[9,2]
elecevar<-scen1.tabresults[14,2]
elecopen<-scen1.tabresults[15,2]
rupt<-scen1.tabresults[16,2]
emerevar<-scen1.tabresults[17,2]
emeropen<-scen1.tabresults[18,2]
reintelecevar<-scen1.tabresults[21,2]
reintemerevar<-scen1.tabresults[21,2]
reintemeropen<-scen1.tabresults[23,2]
aaadead<-scen1.tabresults[24,2]
nonaaadead<-scen1.tabresults[25,2]
n<-v0$numberOfPersons
dropoutrate<-0.1
dropoutperiod<-2
period<-4
thresh<-7
temp<-data.frame(n,dropoutrate,dropoutperiod,period,thresh,inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,rupt,emerevar,emeropen,reintelecevar,reintemerevar,reintemeropen,aaadead,nonaaadead)
scen4dsummary<-rbind(scen4dsummary,temp)


# surveillance scan suspension period=5.0 yrs
v1other$monitoringIntervalsSuspensionTime <- rep(5, length(v1other$monitoringIntervals))
scen1.surv <- processPersons(v0, v1other, v2, personData.surv)
scen1.surv$meanQuantities
TableOfCounts(scen1.surv, v1other)
scen1.tabresults<-Eventsandcosts(scen1.surv)
inv<-scen1.tabresults[1,2]
scr<-scen1.tabresults[2,2]
reinv<-scen1.tabresults[3,2]
nonatt<-scen1.tabresults[4,2]
monitor<-scen1.tabresults[6,2]
dropout<-scen1.tabresults[7,2]
oppdet<-scen1.tabresults[8,2]
consult<-scen1.tabresults[9,2]
elecevar<-scen1.tabresults[14,2]
elecopen<-scen1.tabresults[15,2]
rupt<-scen1.tabresults[16,2]
emerevar<-scen1.tabresults[17,2]
emeropen<-scen1.tabresults[18,2]
reintelecevar<-scen1.tabresults[21,2]
reintemerevar<-scen1.tabresults[21,2]
reintemeropen<-scen1.tabresults[23,2]
aaadead<-scen1.tabresults[24,2]
nonaaadead<-scen1.tabresults[25,2]
n<-v0$numberOfPersons
dropoutrate<-0.1
dropoutperiod<-2
period<-5
thresh<-7
temp<-data.frame(n,dropoutrate,dropoutperiod,period,thresh,inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,rupt,emerevar,emeropen,reintelecevar,reintemerevar,reintemeropen,aaadead,nonaaadead)
scen4dsummary<-rbind(scen4dsummary,temp)

scen4dsummary
