## SCENARIO 0: RESULTS FOR NEw 65YO COHORT (STATUS QUO)

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

## Persons characteristics data.frame
personData.screen <- data.frame(startAge = 65)

######################################################################################################################################################
# NEW 65-YEAR-OLD COHORT
######################################################################################################################################################

## SCENARIO 0: RESULTS FOR NEw 65YO COHORT (STATUS QUO)
set.seed(3210)
v0$randomSeed<-3210
scen0.invite <- processPersons(v0, v1other, v2, personData.screen)
scen0.invite$meanQuantities

TableOfCounts(scen0.invite, v1other)
Eventsandcosts(scen0.invite)


inv<-Eventsandcosts(scen0.invite)[1,2]
scr<-Eventsandcosts(scen0.invite)[2,2]
reinv<-Eventsandcosts(scen0.invite)[3,2]
nonatt<-Eventsandcosts(scen0.invite)[4,2]
monitor<-Eventsandcosts(scen0.invite)[6,2]
dropout<-Eventsandcosts(scen0.invite)[7,2]
oppdet<-Eventsandcosts(scen0.invite)[8,2]
consult<-Eventsandcosts(scen0.invite)[9,2]
elecevar<-Eventsandcosts(scen0.invite)[14,2]
elecopen<-Eventsandcosts(scen0.invite)[15,2]
rupt<-Eventsandcosts(scen0.invite)[16,2]
emeropen<-Eventsandcosts(scen0.invite)[18,2]
reintelecevar<-Eventsandcosts(scen0.invite)[21,2]
reintemeropen<-Eventsandcosts(scen0.invite)[23,2]
aaadead<-Eventsandcosts(scen0.invite)[24,2]
nonaaadead<-Eventsandcosts(scen0.invite)[25,2]

scen0summaryi<-data.frame(inv,scr,reinv,nonatt,monitor,dropout,oppdet,consult,elecevar,elecopen,rupt,reintelecevar,reintemeropen,aaadead,nonaaadead)
scen0summaryi

