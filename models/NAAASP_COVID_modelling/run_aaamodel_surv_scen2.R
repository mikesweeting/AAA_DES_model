# SURVEILLANCE COHORT
## SCENARIO S2: varying dropout rate from fup
## (i) All surveillance scans suspended by 6 months initially 
## (ii) Threshold for operation changed to 50cm for first 3 months (no elective ops), then revert to 5.5cm
# (iii) varying dropout rate from 6%-15%, for 1y (S2.1)
# (iv) varying dropout rate from 6%-15%, for 2y (S2.2)

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
## SCENARIO S2: varying dropout rate from fup
## (i) All surveillance scans suspended by 6 months initially 
## (ii) Threshold for operation changed to 50cm for first 3 months (no elective ops), then revert to 5.5cm
# (iii) varying dropout rate from 6%-15%, for 1y (S2.1)
# (iv) varying dropout rate from 6%-15%, for 2y (S2.2)

v1other$inviteToScreenSuspensionTime <- 0
v1other$aortaDiameterThresholds <- list(c(3, 4.5, 50), c(3, 4.5, 5.5), c(3, 4.5, 5.5))
attr(v1other$aortaDiameterThresholds, "timeCutPoints") <- c(0.25, 0.25)
v1other$monitoringIntervalsSuspensionTime <- rep(0.5, length(v1other$monitoringIntervals))
set.seed(3210)
v0$randomSeed<-3210

# period = 1y
# dropout rate = 8%
v2$rateOfDropoutFromMonitoring <- setType(c(0.08, 0.05720712), "rate")
attr(v2$rateOfDropoutFromMonitoring, "timeCutPoints") <- 1

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
dropoutrate<-0.08
dropoutperiod<-1
scen2summary<-data.frame(n,dropoutrate,dropoutperiod,inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,rupt,emerevar,emeropen,reintelecevar,reintemerevar,reintemeropen,aaadead,nonaaadead)


# period = 1y
# dropout rate = 10%
v2$rateOfDropoutFromMonitoring <- setType(c(0.10, 0.05720712), "rate")
attr(v2$rateOfDropoutFromMonitoring, "timeCutPoints") <- 1

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
dropoutrate<-0.1
dropoutperiod<-1
temp<-data.frame(n,dropoutrate,dropoutperiod,inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,rupt,emerevar,emeropen,reintelecevar,reintemerevar,reintemeropen,aaadead,nonaaadead)
scen2summary<-rbind(scen2summary,temp)


# period = 1y
# dropout rate = 12%
v2$rateOfDropoutFromMonitoring <- setType(c(0.12, 0.05720712), "rate")
attr(v2$rateOfDropoutFromMonitoring, "timeCutPoints") <- 1

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
dropoutrate<-0.12
dropoutperiod<-1
temp<-data.frame(n,dropoutrate,dropoutperiod,inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,rupt,emerevar,emeropen,reintelecevar,reintemerevar,reintemeropen,aaadead,nonaaadead)
scen2summary<-rbind(scen2summary,temp)


# period = 1y
# dropout rate = 15%
v2$rateOfDropoutFromMonitoring <- setType(c(0.15, 0.05720712), "rate")
attr(v2$rateOfDropoutFromMonitoring, "timeCutPoints") <- 1

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
dropoutrate<-0.15
dropoutperiod<-1
temp<-data.frame(n,dropoutrate,dropoutperiod,inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,rupt,emerevar,emeropen,reintelecevar,reintemerevar,reintemeropen,aaadead,nonaaadead)
scen2summary<-rbind(scen2summary,temp)


# period = 2y
# dropout rate = 8%
v2$rateOfDropoutFromMonitoring <- setType(c(0.08, 0.05720712), "rate")
attr(v2$rateOfDropoutFromMonitoring, "timeCutPoints") <- 2

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
dropoutrate<-0.08
dropoutperiod<-2
temp<-data.frame(n,dropoutrate,dropoutperiod,inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,rupt,emerevar,emeropen,reintelecevar,reintemerevar,reintemeropen,aaadead,nonaaadead)
scen2summary<-rbind(scen2summary,temp)


# period = 2y
# dropout rate = 10%
v2$rateOfDropoutFromMonitoring <- setType(c(0.10, 0.05720712), "rate")
attr(v2$rateOfDropoutFromMonitoring, "timeCutPoints") <- 2

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
dropoutrate<-0.1
dropoutperiod<-2
temp<-data.frame(n,dropoutrate,dropoutperiod,inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,rupt,emerevar,emeropen,reintelecevar,reintemerevar,reintemeropen,aaadead,nonaaadead)
scen2summary<-rbind(scen2summary,temp)


# period = 2y
# dropout rate = 12%
v2$rateOfDropoutFromMonitoring <- setType(c(0.12, 0.05720712), "rate")
attr(v2$rateOfDropoutFromMonitoring, "timeCutPoints") <- 2

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
dropoutrate<-0.12
dropoutperiod<-2
temp<-data.frame(n,dropoutrate,dropoutperiod,inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,rupt,emerevar,emeropen,reintelecevar,reintemerevar,reintemeropen,aaadead,nonaaadead)
scen2summary<-rbind(scen2summary,temp)


# period = 2y
# dropout rate = 15%
v2$rateOfDropoutFromMonitoring <- setType(c(0.15, 0.05720712), "rate")
attr(v2$rateOfDropoutFromMonitoring, "timeCutPoints") <- 2

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
dropoutrate<-0.15
dropoutperiod<-2
temp<-data.frame(n,dropoutrate,dropoutperiod,inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,rupt,emerevar,emeropen,reintelecevar,reintemerevar,reintemeropen,aaadead,nonaaadead)
scen2summary<-rbind(scen2summary,temp)

scen2summary

