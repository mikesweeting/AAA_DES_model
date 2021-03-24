# SURVEILLANCE COHORT
## SCENARIO S3: varying threshold for surgery (NB: 70mm = VS guideline results) & length of application for this variation (with scan & op suspension retained as first 3m only throughout)
## (i) All surveillance scans suspended by 6 months 
## (ii) Threshold for operation changed to 50cm for first 3 months (no elective ops), then 7cm until 6m-5y mark

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
## SCENARIO S3: varying threshold for surgery (NB: 70mm = VS guideline results) & length of application for this variation (with scan & op suspension retained as first 3m only throughout)
## (i) All surveillance scans suspended by 6 months 
## (ii) Threshold for operation changed to 50cm for first 3 months (no elective ops), then 7cm until 6m-5y mark

v1other$inviteToScreenSuspensionTime <- 0
v1other$monitoringIntervalsSuspensionTime <- rep(0.5, length(v1other$monitoringIntervals))
v1other$aortaDiameterThresholds <- list(c(3, 4.5, 50.0), c(3, 4.5, 7), c(3, 4.5, 5.5))

## 50cm first 3-months, 7cm until 6-months
set.seed(3210)
v0$randomSeed<-3210
v1other$aortaDiameterThresholds <- list(c(3, 4.5, 50), c(3, 4.5, 7), c(3, 4.5, 5.5))
attr(v1other$aortaDiameterThresholds, "timeCutPoints") <- c(0.25, 0.5)
scen1.surv <- processPersons(v0, v1other, v2, personData.surv)
scen1.surv$meanQuantities
TableOfCounts(scen1.surv, v1other)
Eventsandcosts(scen1.surv)
inv<-Eventsandcosts(scen1.surv)[1,2]
scr<-Eventsandcosts(scen1.surv)[2,2]
reinv<-Eventsandcosts(scen1.surv)[3,2]
nonatt<-Eventsandcosts(scen1.surv)[4,2]
monitor<-Eventsandcosts(scen1.surv)[6,2]
dropout<-Eventsandcosts(scen1.surv)[7,2]
oppdet<-Eventsandcosts(scen1.surv)[8,2]
consult<-Eventsandcosts(scen1.surv)[9,2]
elecevar<-Eventsandcosts(scen1.surv)[14,2]
elecopen<-Eventsandcosts(scen1.surv)[15,2]
rupt<-Eventsandcosts(scen1.surv)[16,2]
emerevar<-Eventsandcosts(scen1.surv)[17,2]
emeropen<-Eventsandcosts(scen1.surv)[18,2]
reintelecevar<-Eventsandcosts(scen1.surv)[21,2]
reintemerevar<-Eventsandcosts(scen1.surv)[21,2]
reintemeropen<-Eventsandcosts(scen1.surv)[23,2]
aaadead<-Eventsandcosts(scen1.surv)[24,2]
nonaaadead<-Eventsandcosts(scen1.surv)[25,2]
n<-v0$numberOfPersons
thresh<-7
period<-0.5
scen3summary<-data.frame(n,thresh,period,inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,rupt,emerevar,emeropen,reintelecevar,reintemerevar,reintemeropen,aaadead,nonaaadead)

## 50cm first 3-months, 7cm until 1-year
v1other$aortaDiameterThresholds <- list(c(3, 4.5, 50), c(3, 4.5, 7), c(3, 4.5, 5.5))
attr(v1other$aortaDiameterThresholds, "timeCutPoints") <- c(0.25, 1)
scen1.surv <- processPersons(v0, v1other, v2, personData.surv)
scen1.surv$meanQuantities
TableOfCounts(scen1.surv, v1other)
Eventsandcosts(scen1.surv)
inv<-Eventsandcosts(scen1.surv)[1,2]
scr<-Eventsandcosts(scen1.surv)[2,2]
reinv<-Eventsandcosts(scen1.surv)[3,2]
nonatt<-Eventsandcosts(scen1.surv)[4,2]
monitor<-Eventsandcosts(scen1.surv)[6,2]
dropout<-Eventsandcosts(scen1.surv)[7,2]
oppdet<-Eventsandcosts(scen1.surv)[8,2]
consult<-Eventsandcosts(scen1.surv)[9,2]
elecevar<-Eventsandcosts(scen1.surv)[14,2]
elecopen<-Eventsandcosts(scen1.surv)[15,2]
rupt<-Eventsandcosts(scen1.surv)[16,2]
emerevar<-Eventsandcosts(scen1.surv)[17,2]
emeropen<-Eventsandcosts(scen1.surv)[18,2]
reintelecevar<-Eventsandcosts(scen1.surv)[21,2]
reintemerevar<-Eventsandcosts(scen1.surv)[21,2]
reintemeropen<-Eventsandcosts(scen1.surv)[23,2]
aaadead<-Eventsandcosts(scen1.surv)[24,2]
nonaaadead<-Eventsandcosts(scen1.surv)[25,2]
thresh<-7
period<-1
temp<-data.frame(n,thresh,period,inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,rupt,emerevar,emeropen,reintelecevar,reintemerevar,reintemeropen,aaadead,nonaaadead)
scen3summary<-rbind(scen3summary,temp)

## 50cm first 3-months, 7cm until 2-years
v1other$aortaDiameterThresholds <- list(c(3, 4.5, 50), c(3, 4.5, 7), c(3, 4.5, 5.5))
attr(v1other$aortaDiameterThresholds, "timeCutPoints") <- c(0.25, 2)
scen1.surv <- processPersons(v0, v1other, v2, personData.surv)
scen1.surv$meanQuantities
TableOfCounts(scen1.surv, v1other)
Eventsandcosts(scen1.surv)
inv<-Eventsandcosts(scen1.surv)[1,2]
scr<-Eventsandcosts(scen1.surv)[2,2]
reinv<-Eventsandcosts(scen1.surv)[3,2]
nonatt<-Eventsandcosts(scen1.surv)[4,2]
monitor<-Eventsandcosts(scen1.surv)[6,2]
dropout<-Eventsandcosts(scen1.surv)[7,2]
oppdet<-Eventsandcosts(scen1.surv)[8,2]
consult<-Eventsandcosts(scen1.surv)[9,2]
elecevar<-Eventsandcosts(scen1.surv)[14,2]
elecopen<-Eventsandcosts(scen1.surv)[15,2]
rupt<-Eventsandcosts(scen1.surv)[16,2]
emerevar<-Eventsandcosts(scen1.surv)[17,2]
emeropen<-Eventsandcosts(scen1.surv)[18,2]
reintelecevar<-Eventsandcosts(scen1.surv)[21,2]
reintemerevar<-Eventsandcosts(scen1.surv)[21,2]
reintemeropen<-Eventsandcosts(scen1.surv)[23,2]
aaadead<-Eventsandcosts(scen1.surv)[24,2]
nonaaadead<-Eventsandcosts(scen1.surv)[25,2]
thresh<-7
period<-2
temp<-data.frame(n,thresh,period,inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,rupt,emerevar,emeropen,reintelecevar,reintemerevar,reintemeropen,aaadead,nonaaadead)
scen3summary<-rbind(scen3summary,temp)

## 50cm first 3-months, 7cm until 3-years
v1other$aortaDiameterThresholds <- list(c(3, 4.5, 50), c(3, 4.5, 7), c(3, 4.5, 5.5))
attr(v1other$aortaDiameterThresholds, "timeCutPoints") <- c(0.25, 3)
scen1.surv <- processPersons(v0, v1other, v2, personData.surv)
scen1.surv$meanQuantities
TableOfCounts(scen1.surv, v1other)
Eventsandcosts(scen1.surv)
inv<-Eventsandcosts(scen1.surv)[1,2]
scr<-Eventsandcosts(scen1.surv)[2,2]
reinv<-Eventsandcosts(scen1.surv)[3,2]
nonatt<-Eventsandcosts(scen1.surv)[4,2]
monitor<-Eventsandcosts(scen1.surv)[6,2]
dropout<-Eventsandcosts(scen1.surv)[7,2]
oppdet<-Eventsandcosts(scen1.surv)[8,2]
consult<-Eventsandcosts(scen1.surv)[9,2]
elecevar<-Eventsandcosts(scen1.surv)[14,2]
elecopen<-Eventsandcosts(scen1.surv)[15,2]
rupt<-Eventsandcosts(scen1.surv)[16,2]
emerevar<-Eventsandcosts(scen1.surv)[17,2]
emeropen<-Eventsandcosts(scen1.surv)[18,2]
reintelecevar<-Eventsandcosts(scen1.surv)[21,2]
reintemerevar<-Eventsandcosts(scen1.surv)[21,2]
reintemeropen<-Eventsandcosts(scen1.surv)[23,2]
aaadead<-Eventsandcosts(scen1.surv)[24,2]
nonaaadead<-Eventsandcosts(scen1.surv)[25,2]
thresh<-7
period<-3
temp<-data.frame(n,thresh,period,inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,rupt,emerevar,emeropen,reintelecevar,reintemerevar,reintemeropen,aaadead,nonaaadead)
scen3summary<-rbind(scen3summary,temp)

## 50cm first 3-months, 7cm until 4-years
v1other$aortaDiameterThresholds <- list(c(3, 4.5, 50), c(3, 4.5, 7), c(3, 4.5, 5.5))
attr(v1other$aortaDiameterThresholds, "timeCutPoints") <- c(0.25, 4)
scen1.surv <- processPersons(v0, v1other, v2, personData.surv)
scen1.surv$meanQuantities
TableOfCounts(scen1.surv, v1other)
Eventsandcosts(scen1.surv)
inv<-Eventsandcosts(scen1.surv)[1,2]
scr<-Eventsandcosts(scen1.surv)[2,2]
reinv<-Eventsandcosts(scen1.surv)[3,2]
nonatt<-Eventsandcosts(scen1.surv)[4,2]
monitor<-Eventsandcosts(scen1.surv)[6,2]
dropout<-Eventsandcosts(scen1.surv)[7,2]
oppdet<-Eventsandcosts(scen1.surv)[8,2]
consult<-Eventsandcosts(scen1.surv)[9,2]
elecevar<-Eventsandcosts(scen1.surv)[14,2]
elecopen<-Eventsandcosts(scen1.surv)[15,2]
rupt<-Eventsandcosts(scen1.surv)[16,2]
emerevar<-Eventsandcosts(scen1.surv)[17,2]
emeropen<-Eventsandcosts(scen1.surv)[18,2]
reintelecevar<-Eventsandcosts(scen1.surv)[21,2]
reintemerevar<-Eventsandcosts(scen1.surv)[21,2]
reintemeropen<-Eventsandcosts(scen1.surv)[23,2]
aaadead<-Eventsandcosts(scen1.surv)[24,2]
nonaaadead<-Eventsandcosts(scen1.surv)[25,2]
thresh<-7
period<-4
temp<-data.frame(n,thresh,period,inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,rupt,emerevar,emeropen,reintelecevar,reintemerevar,reintemeropen,aaadead,nonaaadead)
scen3summary<-rbind(scen3summary,temp)

## 50cm first 3-months, 7cm until 5-years
v1other$aortaDiameterThresholds <- list(c(3, 4.5, 50), c(3, 4.5, 7), c(3, 4.5, 5.5))
attr(v1other$aortaDiameterThresholds, "timeCutPoints") <- c(0.25, 5)
scen1.surv <- processPersons(v0, v1other, v2, personData.surv)
scen1.surv$meanQuantities
TableOfCounts(scen1.surv, v1other)
Eventsandcosts(scen1.surv)
inv<-Eventsandcosts(scen1.surv)[1,2]
scr<-Eventsandcosts(scen1.surv)[2,2]
reinv<-Eventsandcosts(scen1.surv)[3,2]
nonatt<-Eventsandcosts(scen1.surv)[4,2]
monitor<-Eventsandcosts(scen1.surv)[6,2]
dropout<-Eventsandcosts(scen1.surv)[7,2]
oppdet<-Eventsandcosts(scen1.surv)[8,2]
consult<-Eventsandcosts(scen1.surv)[9,2]
elecevar<-Eventsandcosts(scen1.surv)[14,2]
elecopen<-Eventsandcosts(scen1.surv)[15,2]
rupt<-Eventsandcosts(scen1.surv)[16,2]
emerevar<-Eventsandcosts(scen1.surv)[17,2]
emeropen<-Eventsandcosts(scen1.surv)[18,2]
reintelecevar<-Eventsandcosts(scen1.surv)[21,2]
reintemerevar<-Eventsandcosts(scen1.surv)[21,2]
reintemeropen<-Eventsandcosts(scen1.surv)[23,2]
aaadead<-Eventsandcosts(scen1.surv)[24,2]
nonaaadead<-Eventsandcosts(scen1.surv)[25,2]
thresh<-7
period<-5
temp<-data.frame(n,thresh,period,inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,rupt,emerevar,emeropen,reintelecevar,reintemerevar,reintemeropen,aaadead,nonaaadead)
scen3summary<-rbind(scen3summary,temp)


# adding status quo results 
v1other$inviteToScreenSuspensionTime <- 0
v1other$monitoringIntervalsSuspensionTime <- rep(0, length(v1other$monitoringIntervals))
v1other$aortaDiameterThresholds <- list(c(3, 4.5, 5.5), c(3, 4.5, 5.5), c(3, 4.5, 5.5))
attr(v1other$aortaDiameterThresholds, "timeCutPoints") <- c(0, 0)
scen1.surv <- processPersons(v0, v1other, v2, personData.surv)
scen1.surv$meanQuantities
TableOfCounts(scen1.surv, v1other)
Eventsandcosts(scen1.surv)
inv<-Eventsandcosts(scen1.surv)[1,2]
scr<-Eventsandcosts(scen1.surv)[2,2]
reinv<-Eventsandcosts(scen1.surv)[3,2]
nonatt<-Eventsandcosts(scen1.surv)[4,2]
monitor<-Eventsandcosts(scen1.surv)[6,2]
dropout<-Eventsandcosts(scen1.surv)[7,2]
oppdet<-Eventsandcosts(scen1.surv)[8,2]
consult<-Eventsandcosts(scen1.surv)[9,2]
elecevar<-Eventsandcosts(scen1.surv)[14,2]
elecopen<-Eventsandcosts(scen1.surv)[15,2]
rupt<-Eventsandcosts(scen1.surv)[16,2]
emerevar<-Eventsandcosts(scen1.surv)[17,2]
emeropen<-Eventsandcosts(scen1.surv)[18,2]
reintelecevar<-Eventsandcosts(scen1.surv)[21,2]
reintemerevar<-Eventsandcosts(scen1.surv)[21,2]
reintemeropen<-Eventsandcosts(scen1.surv)[23,2]
aaadead<-Eventsandcosts(scen1.surv)[24,2]
nonaaadead<-Eventsandcosts(scen1.surv)[25,2]
thresh<-5.5
period<-0
temp<-data.frame(n,thresh,period,inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,rupt,emerevar,emeropen,reintelecevar,reintemerevar,reintemeropen,aaadead,nonaaadead)
scen3summary<-rbind(scen3summary,temp)

scen3summary

