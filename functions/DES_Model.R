################################################################################
# DES MODEL 
################################################################################
# Authors: Edmund Jones, Katya Masconi, Michael Sweeting

################################################################################
# Description
################################################################################
# This discrete event simulation model allows for the rapid assessment of different 
# screening options, and has been validated in men using  data from the randomised 
# Multicentre Aneurysm Screening Study [Glover et al. Discrete Event Simulation 
# for Decision Modeling in Health Care: Lessons from Abdominal Aortic Aneurysm 
# Screening. Medical Decision Making. 2018]. It simulates a sequence of key screening and clinical events for N 
# persons from the time of invitation, to screening up to their date of 
# death or a censoing time (the time horizon). Each person has a counterpart who 
# shares some key characteristics (age, aortic diameter at baseline, rate of 
# aortic growth, or potential time of non-AAA-related death), except that the 
# counterpart is not invited to screening. 

# The structure of the model [Appendix of Sweeting et al. Should we screen 
# women for abdominal aortic aneurysm? A cost-effectiveness analysis. The Lancet. 2018] allows people 
# to drop out of the surveillance programme or for an AAA to be incidentally 
# detected. Persons who are referred for a consultation can either be returned to 
# surveillance if their diameter, as confirmed by a CT scan, is less than the 
# intervention threshold; placed on a waiting list for elective surgery; or 
# are not offered repair because of the high surgical risk associated with 
# their comorbidities. 

# Predefined outcomes from the model are death caused by AAA, life-years, QALYs, 
# costs, and the incremental cost-effectiveness ratio (ICER). Both costs and 
# life-years can be discounted. 

# Input parameters for 
# women have been obtained from a combination of literature reviews, clinical 
# trial data, bespoke hospital datasets, and analysis of routine and registry 
# data sources. Further details included in [Sweeting et al. Should we 
# screen women for abdominal aortic aneurysm? A cost-effectiveness analysis. The Lancet. 2018]

################################################################################
# CONTENTS
################################################################################
# 0) AAA_DES - Wrapper function that runs processPersons, reading in all data from Excel Data Input Spreadsheet
## ARGUMENTS:
## dataFile -- Excel data file to be passed to AAA_DES
## psa -- Should PSA be conducted (defaults to FALSE)
## n -- Number of pairs of individuals to be run in DES (defaults to 10000)
## nPSA -- Number of PSA iterations (defaults to 100). Only used if psa = TRUE
## selectiveSampling -- Use selective sampling to estimate incremental effects? This uses processControlOnly to first get absolute numbers for control group
## incrementalOnly -- Use the with selectiveSampling = TRUE to only estimate incremental effects. This saves time by not executing processControlOnly
## extraInputs -- A list of any elements of v0, v1other, v1distributions and v2 that the user wishes to change from the R command line
## personData -- Person characteristics at baseline. Samples with replacement from the data.frame (possibly weighted sampling using prob as a column)

# 1a) processPersons 
# 1b) processPersonsAboveDiagnosisThreshold

#2) processOnePair 
#2a) Models for aorta growth and rupture. 
#2b) Generate event history
#2c) Functions for compactList, to display v1other, v2, etc. more compactly
#2d) Functions for generating time till non-AAA death

#3a) Probabilistic sensitivity analysis
#3b) Probabilistic sensitivity analysis above diagnosis threshold

#4) Calculate health-economic quantities
#5) Utilities
#6) Check arguments
#7) Output functions

################################################################################

################################################################################
# AAA_DES
AAA_DES <- function(dataFile, psa = FALSE, n = 10000, nPSA = 100, selectiveSampling = TRUE,
                    incrementalOnly = FALSE, extraInputs = list(v0 = list(), v1other = list(), v2 = list(), v1distributions = list()), 
                    personData = NULL){
  
  # Source auxiliary functions
  source("functions/Auxiliary_Functions.R", local = T)
  
  # Set unspecified elements of v0 to default values, if necessary. 
  v0 <- setUnspecifiedElementsOfv0(v0)
  
  v1distributions <- compactList() 
  v1other <- compactList()
  v2 <- compactList()

  ## Number of persons and number of PSA iterations
  v0$numberOfPersons <- n
  v0$numberOfParameterIterations <- nPSA
  
  ## Only one of v1other$startAge and personData should be specified
  if(!is.null(v1other$startAge)) warning("v1other$startAge is deprecated. Please specify age distribution in personData instead")
  if(!is.null(v1other$startAge) & !is.null(personData)) stop("Only one of v1other$startAge and personData can be specified")
  ## If v1other exists then assign is to personData$startAge 
  if(!is.null(v1other$startAge)){
    personData <- data.frame(startAge = v1other$startAge)
    v1other$startAge <- NULL
  }
  ## If prob (probability weighting) does not exist in personData then assign it the value 1
  if(is.null(personData$prob)){
    personData$prob <- 1
  }
  
  ## Input Data for the DES model
  ## Install required packages if not already installed
  list.of.packages <- c("readxl", "tibble")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  library(readxl)
  library(tibble)
  
  ## Main Data Items
  if(!is.null(dataFile)){
    DESData <- read_excel(dataFile, sheet = "Main Data Items", range="A7:AA200", 
                          col_names = T)
    DESData <- subset(DESData, !is.na(DESData$varname))
    
    ## Growth and rupture rate data
    growthData <- suppressMessages(read_excel(dataFile, sheet = "Growth and Rupture Rates", range="A19:I200", 
                                              col_names = T))
    growthData <- subset(growthData, !is.na(growthData$varname))
    
    ## AAA Size Distribution
    # use baselineDiameters in Excel file only if not specified in personData
    if(!("baselineDiameter" %in% names(personData))){
      v1other$baselineDiameters <- read_excel(dataFile, sheet = "AAA Size Distribution",  
                                              col_names = T, skip = 4)
      names(v1other$baselineDiameters) <- c("size", "weight")
      v1other$baselineDiameters <- subset(v1other$baselineDiameters, !is.na(v1other$baselineDiameters$size))
    } else {
      cat("Using baseline aorta distribution as specified in personData \n")
      v1other$baselineDiameters <- NULL
    }
    
    ## Model parameters
    modelParameters <- read_excel(dataFile, sheet = "Model Parameters", range="A6:D200", 
                                  col_names = T)
    modelParameters <- subset(modelParameters, !is.na(modelParameters$varname))
    
    ## Other cause mortality
    nonAAA <- read_excel(dataFile, sheet = "Other cause mortality", 
                         col_names = T, skip = 4)
    nonAAA <- column_to_rownames(nonAAA, var = "Age")
    v1other$nonAaaMortalityRatesFileName <- nonAAA ## allow dataset to be given instead of a csv file name. Change readMortalityRatesFromFile 
    
    
    ## For now always set these to "survivalModel"
    v1other$electiveSurgeryAaaDeathMethod <- "survivalModel"
    v1other$emergencySurgeryAaaDeathMethod <- "survivalModel"
    ## For now always set this to "onsIntegerStart"
    v1other$nonAaaDeathMethod <- "onsIntegerStart"
    ## For now always set period where re-interventions are not counted (part of initial operation period) to 30 days
    v1other$postSurgeryInitialPeriod <- 30 / 365.25
    
    ## Assign main values
    for(i in 1:dim(DESData)[1]){
      if(!is.na(DESData$Value[i])){
        if(!is.na(DESData$type[i])){
          if(DESData$type[i] == "\"function\""){
            eval(parse(text=paste0(DESData$varname[i],"<- function() {", DESData$Value[i], "}")))
          } else {
            eval(parse(text=paste0(DESData$varname[i],"<- setType(", DESData$Value[i],
                                   ", type =", DESData$type[i], ")")))
          }
        } else {
          eval(parse(text=paste0(DESData$varname[i],"<- ", DESData$Value[i])))
        }
      } else if(!is.na(DESData[i, "intercept"])) {
        pars <- unlist(DESData[i,c("intercept","age","aortaSize")])
        pars <- pars[!is.na(pars)]
        eval(parse(text=paste0(DESData$varname[i],"<- setType(pars, type =", DESData$type[i], ")")))
      } else {
        eval(parse(text=paste0(DESData$varname[i],"<- ", DESData$Value[i])))
      }
    }
    
    
    ## Assign list of costs
    v2$costs <- setType(c(
      inviteToScreen=costs.inviteToScreen, 
      requireReinvitation=costs.requireReinvitation, 
      screen=costs.screen, 
      monitor=costs.monitor,
      monitorFollowingContraindication=costs.monitorFollowingContraindication,   
      consultation=costs.consultation,
      electiveSurgeryEvar=costs.electiveSurgeryEvar,  
      electiveSurgeryOpen=costs.electiveSurgeryOpen,
      emergencySurgeryEvar=costs.emergencySurgeryEvar,
      emergencySurgeryOpen=costs.emergencySurgeryOpen,
      monitorFollowingEvarSurgery=costs.monitorFollowingEvarSurgery,
      monitorFollowingOpenSurgery=costs.monitorFollowingOpenSurgery,
      reinterventionAfterElectiveEvar=costs.reinterventionAfterElectiveEvar,
      reinterventionAfterElectiveOpen=costs.reinterventionAfterElectiveOpen,
      reinterventionAfterEmergencyEvar=costs.reinterventionAfterEmergencyEvar,
      reinterventionAfterEmergencyOpen=costs.reinterventionAfterEmergencyOpen
    ), type="costs")
    
    ## Assign QoL utilities
    # Overall QoL / utilities
    v2$qalyFactors <- setType(v2$qalyFactors, "qaly")
    
    
    ## Assign growth and rupture rate values
    for(i in 1:dim(growthData)[1]){
      if(!is.na(growthData$Value[i])){
        if(!is.na(growthData$type[i])){
          if(growthData$type[i] != "\"hyperpars for aorta model\""){
            eval(parse(text=paste0(growthData$varname[i],"<- setType(", growthData$Value[i],
                                   ", type =", growthData$type[i], ")")))
          } else {
            if(growthData$varname[i] == "v1distributions$covarianceForGrowthParameters"){
              eval(parse(text=paste0(growthData$varname[i],"<- setType( matrix( nrow = 6, data = c(", 
                                     paste(unlist(growthData[i:(i+5),4:9]), collapse=", "),
                                     ")), type =", growthData$type[i], ")")))
              s <- v1distributions$covarianceForGrowthParameters
              s[lower.tri(s)] <- t(s)[lower.tri(s)]
              v1distributions$covarianceForGrowthParameters <- s
            } else if(growthData$varname[i] == "v1distributions$covarianceForRuptureParameters"){
              eval(parse(text=paste0(growthData$varname[i],"<- setType( matrix( nrow = 2, data = c(", 
                                     paste(unlist(growthData[i:(i+1),4:5]), collapse=", "),
                                     ")), type =", growthData$type[i], ")")))
              s <- v1distributions$covarianceForRuptureParameters
              s[lower.tri(s)] <- t(s)[lower.tri(s)]
              v1distributions$covarianceForRuptureParameters <- s
            }
            
          }
        }
      }
    }
    growthParameterNames <- 
      c("beta1", "beta0", "logSigma1", "logSigma0", "atanhRho", "logSigmaW")
    ruptureParameterNames <- c("alpha", "gamma")
    v1distributions$meanForGrowthParameters <- setType(
      c(v2$beta1, v2$beta0, log(v2$sigma1), log(v2$sigma0), atanh(v2$rho), log(v2$sigmaW)),
      "hyperpars for aorta model")
    names(v1distributions$meanForGrowthParameters) <- growthParameterNames
    v1distributions$meanForRuptureParameters <- 
      setType(c(v2$alpha, v2$gamma), "hyperpars for aorta model")
    dimnames(v1distributions$covarianceForGrowthParameters) <-
      list(growthParameterNames, growthParameterNames)
    names(v1distributions$meanForRuptureParameters) <- ruptureParameterNames
    dimnames(v1distributions$covarianceForRuptureParameters) <-
      list(ruptureParameterNames, ruptureParameterNames)
    
    ## Assign model parameters
    for(i in 1:dim(modelParameters)[1]){
      if(!is.na(modelParameters$Value[i])){
        if(!is.na(modelParameters$type[i])){
          if(modelParameters$type[i] == "\"function\""){
            eval(parse(text=paste0(modelParameters$varname[i],"<- function() {", modelParameters$Value[i], "}")))
          } else {
            eval(parse(text=paste0(modelParameters$varname[i],"<- setType(", modelParameters$Value[i],
                                   ", type =", modelParameters$type[i], ")")))
          }
        } else {
          eval(parse(text=paste0(modelParameters$varname[i],"<- ", modelParameters$Value[i])))
        }
      }
    }
    
    ## Assign PSA probability distributions
    mean.d.costs <- variance.d.costs <- list()
    for(i in 1:dim(DESData)[1]){
      if(DESData$distribution.type[i]=="\"fixed value for probability\""){
        eval(parse(text=paste0(DESData$distribution.varname[i],"<- setType(", 
                               DESData$varname[i], ", type =", 
                               DESData$distribution.type[i], ")")))  
      }
      if(DESData$distribution.type[i]=="\"beta pars for probability\""){
        eval(parse(text=paste0(DESData$distribution.varname[i],
                               "<- setType(list(alpha=",
                               DESData$alpha[i], ", beta=", DESData$beta[i], 
                               "), type =", 
                               DESData$distribution.type[i], ")")))  
      }
      if(DESData$distribution.type[i]=="\"normal distribution for logit prevalence\""){
        eval(parse(text=paste0(DESData$distribution.varname[i],
                               "<- setType(list(mean=",
                               DESData$mean[i], ", variance=", DESData$sd[i]^2, 
                               "), type =", 
                               DESData$distribution.type[i], ")")))  
      }
      if(DESData$distribution.type[i]=="\"truncated normal distribution\""){
        eval(parse(text=paste0(DESData$distribution.varname[i],
                               "<- setType(list(mean=",
                               DESData$trunc.mean[i], ", variance=", DESData$trunc.sd[i]^2, 
                               "), type =", 
                               DESData$distribution.type[i], ")")))  
      }
      if(DESData$distribution.type[i] %in% "\"gamma pars for rate\""){
        eval(parse(text=paste0(DESData$distribution.varname[i],
                               "<- setType(list(shape=",
                               DESData$shape[i], ", scale=", DESData$scale[i], 
                               "), type =", 
                               DESData$distribution.type[i], ")")))  
      }
      if(DESData$distribution.type[i] %in% "\"gamma pars for multiple rates\""){
        eval(parse(text=paste0(DESData$distribution.varname[i],
                               "<- setType(list(shapes=",
                               DESData$shape[i], ", scales=", DESData$scale[i], 
                               "), type =", 
                               DESData$distribution.type[i], ")")))  
      }
      if(DESData$distribution.type[i] %in% c("\"fixed value\"", "\"fixed value for rate\"", "\"fixed value for reintervention rates\"")){
        eval(parse(text=paste0(DESData$distribution.varname[i],
                               "<- setType(",
                               DESData$varname[i],
                               ", type =", 
                               DESData$distribution.type[i], ")")))  
      }
      if(DESData$distribution.type[i]=="\"hyperpars for logistic model for probability\""){
        
        me <- unlist(DESData[i,c("mean intercept","mean age","mean aortaSize")])
        names(me)<-c("intercept","age","aortaSize")
        cov.vec <- unlist(DESData[i,c("variance intercept","covariance intercept/age","covariance intercept/aortaSize","variance age","covariance age/aortaSize","variance aortaSize")])
        names(cov.vec) <- c("V11", "V12", "V13", "V22", "V23", "V33")
        cov <- matrix(cov.vec[c(1:3,2,4:5,3,5:6)],nrow=3)
        dimnames(cov) <- list(names(me), names(me))
        if(sum(!is.na(me))==1){ ## only intercept specified
          eval(parse(text=paste0(DESData$distribution.varname[i],
                                 "<- setType(list(mean=",
                                 me[1],
                                 ", variance=",
                                 cov[1,1],
                                 "), type =", 
                                 DESData$distribution.type[i], ")")))  
        } else {
          cov <- cov[!is.na(me), !is.na(me)]
          me <- me[!is.na(me)]
          eval(parse(text=paste0(DESData$distribution.varname[i],
                                 "<- setType(list(mean= me, covariance= cov), type =", 
                                 DESData$distribution.type[i], ")")))  
        }
      }
      
      if(DESData$distribution.type[i] == "\"distribution for costs\""){
        eval(parse(text=paste0("mean.",DESData$distribution.varname[i],
                               " <-", DESData$mean[i])))  
        eval(parse(text=paste0("variance.", DESData$distribution.varname[i],
                               " <-", DESData$sd[i]^2)))  
      }
    }
    
    if(DESData$distribution.type[DESData$varname=="costs.inviteToScreen"] == "\"fixed value for costs\""){
      v1distributions$costs <- setType(v2$costs, type = "fixed value for costs")
    }
    if(DESData$distribution.type[DESData$varname=="costs.inviteToScreen"] == "\"distribution for costs\""){
      v1distributions$costs <- setType(list(mean = unlist(mean.d.costs), variance = unlist(variance.d.costs)), 
                                       type = "distribution for costs")
    }
    
    ## If prevalence is NA then set to NULL
    if(is.na(v2$prevalence)){
      v2$prevalence <- NULL
      v1distributions$prevalence <- NULL
    }
    
    # print(v0)
    # print(v1other)
    # print(v2)
    # print(v1distributions)
  }  
  
  ## Replace any inputs with user-defined inputs from AAA_DES
  for(l in 1:length(extraInputs)){
    if(length(extraInputs[[l]]) > 0){
      inputNames <- names(extraInputs[[l]])
      for(k in 1:length(inputNames)){
        if(names(extraInputs[l]) == "v0"){
          att <- attr(extraInputs[[l]][[k]], "type")
          v0[inputNames[k]] <- extraInputs[[l]][k]
          attr(v0[[inputNames[k]]], "type") <- att
        } else if(names(extraInputs[l]) == "v1other"){
          att <- attr(extraInputs[[l]][[k]], "type")
          v1other[inputNames[k]] <- extraInputs[[l]][k]
          attr(v1other[[inputNames[k]]], "type") <- att
        } else if(names(extraInputs[l]) == "v1distributions"){
          att <- attr(extraInputs[[l]][[k]], "type")
          v1distributions[inputNames[k]] <- extraInputs[[l]][k]
          attr(v1distributions[[inputNames[k]]], "type") <- att
        } else if(names(extraInputs[l]) == "v2"){
          att <- attr(extraInputs[[l]][[k]], "type")
          v2[inputNames[k]] <- extraInputs[[l]][k]
          attr(v2[[inputNames[k]]], "type") <- att
        }
      }
    }
  }

  if(psa == FALSE) {
    if(selectiveSampling == TRUE){
      if(v0$returnEventHistories == TRUE){
        stop("Cannot return event histories if selective sampling is used. 
  Either turn off selective sampling (selectiveSampling = FALSE)  
  or set v0$returnEventHistories = FALSE")
      }
      thresh <- ifelse(!is.list(v1other$aortaDiameterThresholds), 
                               v1other$aortaDiameterThresholds[1],
                               v1other$aortaDiameterThresholds[[1]][1])
      if(is.list(v1other$aortaDiameterThresholds))
         cat("Time-varying thresholds given as input, assuming prevalence threshold is v1other$aortaDiameterThresholds[[1]][1]")
      res.sampled<-processPersonsAboveDiagnosisThreshold(v0, v1other, v2, 
                                                         threshold=thresh, personData = personData)
      
      ## Run model once using point estimates
      if(incrementalOnly == FALSE){
        res<- processPersonsControlOnly(v0, v1other, v2, personData = personData)
      } else {
        res <- list()
        res$meanQuantities <- matrix(replace(res.sampled$incrementalMeanQuantities, T, NA), nrow=1, dimnames = list("noScreening",names(res.sampled$incrementalMeanQuantities) ))
      }
      
      result <- list(resultsControl = res, resultsIncremental = res.sampled)
      result$meanQuantities <- rbind(res$meanQuantities, screening = NA, difference = NA)
      result$meanQuantities["screening",] <- res$meanQuantities["noScreening",] + res.sampled$incrementalMeanQuantities
      result$meanQuantities["difference",] <- res.sampled$incrementalMeanQuantities
    } else {
      result <- processPersons(v0, v1other, v2, personData=personData)  
    }
    return(result)
  } else {
    ## Run PSA
    #result <- psa(v0, v1other, v1distributions)
    thresh <- ifelse(!is.list(v1other$aortaDiameterThresholds), 
                     v1other$aortaDiameterThresholds[1],
                     v1other$aortaDiameterThresholds[[1]][1])
    if(is.list(v1other$aortaDiameterThresholds))
      cat("Time-varying thresholds given as input, assuming prevalence threshold is v1other$aortaDiameterThresholds[[1]][1]")
    result <- psaAboveDiagnosisThreshold(v0, v1other, v1distributions, 
                               threshold = thresh, personData = personData)
    return(result)
  }
}

################################################################################
# 1a) processPersons 
# Generate a large number of individuals, analysing them 
# either in parallel or serially. The arguments to processPersons are all 
# the global variables, both fixed (v1distributions and v1other) and uncertain (v0) 
# (these arguments are specific at the beginning of model run), as well as the "user 
# parameters" that are stored in v0. 
# This returns a list that contains three elements, namely
# meanQuantities, eventHistories, and allPersonsQuantities: 
# 
# - "meanQuantities" is a matrix that contains the mean life-years and other 
#   health-economic quantities for each treatment-group. This is the main output 
#   of processPersons and will be used when this function is called by psa. 
# 
# - "eventHistories" is a list that contains one element for each person; each 
#   of those elements is in turn a list with two elements (which are event-
#   histories), one for each treatment-group. 
# 
# - "allPersonsQuantities" is a list that contains one element for each 
#   person, where each of those elements is in turn a list with two elements, 
#   one for each treatment-group. Each of those elements is a named vector that 
#   contains the quantities for that person and that treatment-group.  It is 
#   not certain whether this will be used. (Previous versions of this function 
#   stored all these quantities in a 3D array, called "personsQuantities",
#   instead of a list.) 
################################################################################
processPersons <- function(v0, v1other, v2, personData=NULL, debug = F) {
	
  # Source auxiliary functions
  source("functions/Auxiliary_Functions.R", local = T)
  
  ## Install required packages if not already installed
  list.of.packages <- c("doParallel", "msm")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
	# Set unspecified elements of v0 to default values, if necessary. 
	v0 <- setUnspecifiedElementsOfv0(v0)
	
	# Set unspecified elements of v1other to default values
	v1other <- setUnspecifiedElementsOfv1other(v1other)
	
	## Assign last monitoring interval input to v1other$monitoringIntervalFollowingContraindication and remove it from v1other$monitoringIntervals
	## check not already assigned
	if(!is.null(v1other$monitoringIntervalFollowingContraindication) | !is.null(v1other$monitoringIntervalFollowingContraindicationSuspensionTime)){
	  stop("The monitoring interval and suspension time following contraindication should now be specified as the last element of monitoringIntervals")
	}
	v1other$monitoringIntervalFollowingContraindication <- v1other$monitoringIntervals[length(v1other$monitoringIntervals)]
	v1other$monitoringIntervals <- v1other$monitoringIntervals[-length(v1other$monitoringIntervals)]
	v1other$monitoringIntervalFollowingContraindicationSuspensionTime <- v1other$monitoringIntervalsSuspensionTime[length(v1other$monitoringIntervalsSuspensionTime)]
	v1other$monitoringIntervalsSuspensionTime <- v1other$monitoringIntervalsSuspensionTime[-length(v1other$monitoringIntervalsSuspensionTime)]
	
	## Set v1other$aortaDiameterThreshold to be a list if not already
	if(!is.list(v1other$aortaDiameterThresholds)){
	  v1other$aortaDiameterThresholds <- list(v1other$aortaDiameterThresholds)
	  warning("v1other$aortaDiameterThresholds should now be given as a list, e.g. list(c(3, 4.5, 5.5))")
	}
	
	# Check the arguments.
	checkArgs(v0=v0, v1other=v1other, v2=v2, personData=personData)
	if(!is.null(v1other$qalyFactorBoundaries)){
	  warning("Assuming startAge is 65 for everyone and calculating qalyFactorAgeBoundaries for you")
	  v1other$qalyFactorAgeBoundaries <- 65 + v1other$qalyFactorBoundaries
	  v1other$qalyFactorBoundaries <- NULL
	}
	
	## Only one of v1other$startAge and personData should be specified
	if(!is.null(v1other$startAge)) warning("v1other$startAge is deprecated. Please specify age distribution in personData instead")
	if(!is.null(v1other$startAge) & !is.null(personData)) stop("Only one of v1other$startAge and personData can be specified")
	## If v1other$startAge exists then assign it to personData$startAge 
	if(!is.null(v1other$startAge)){
	  personData <- data.frame(startAge = v1other$startAge)
    v1other$startAge <- NULL
	}
	if(is.null(personData$startAge) & is.null(v1other$startAge)) {
	  stop("Must provide startAge either in personData (recommended) or in v1other$startAge")
	}
	## If prob (probability weighting) does not exist in personData then assign it the value 1
	if(is.null(personData$prob)){
	  personData$prob <- 1
	}
	## If baselineDiameter is specified in personData then prevalence cannot be specified
	if("baselineDiameter" %in% names(personData) & !is.null(v2$prevalence))
	  stop("You cannot currently specify v2$prevalence and include aortaSize in personData.
	       Either include aorta distribution in v1other$baselineDiameters or manually reweight aorta distribution in
	       personData$aortaSize to give required prevalence")
	## Set v1other$baselineDiameter to NULL if baselineDiameter is specified in personData
	if("baselineDiameter" %in% names(personData)){
	  v1other$baselineDiameters <- NULL  
	}
	
	# Display v0, v1other, and v2
	if (v0$verbose) {
		cat("Running processPersons on ", Sys.info()["nodename"], 
			" with:\n  numberOfPersons=", v0$numberOfPersons, 
			", method=", v0$method, { if(v0$method=="serial") "" else paste0(
			", numberOfProcesses=", v0$numberOfProcesses) }, "\n", sep="")
		if ("generateCensoringTime" %in% names(v0))
			cat("Censoring is being used, so life-years etc. will be calculated",
					"up to censoring times.\n")
		cat("\n########## v0 ##########\n")
		print(v0)
		cat("########## v1other ##########\n")
		print(v1other)
		cat("########## v2 ##########\n")
		print(v2)
		cat("########## personData ##############\n")
		print(personData)
		cat("########################\n\n")
	}
	
	# Create v1other$nonAaaSurvProbs or nonAaaMortalityRates. 
	## MS: 24/05/20. Changing v1other$nonAaaSurvProbs to be a data.frame with age and probabilities
	## MS: 24/05/20. This will allow individuals to have different starting ages
	if (v1other$nonAaaDeathMethod == "mass") {
		v1other$nonAaaSurvProbs <- getMassSurvivalProbabilities()
		v1other$nonAaaSurvProbs$age <- personData$startAge + v1other$nonAaaSurvProbs$followUpAtLeast
	} else if (v1other$nonAaaDeathMethod == "onsIntegerStart") { 
		v1other$nonAaaSurvProbs <- convertMortalityRatesToSurvProbs(
				min(personData$startAge), v1other$nonAaaMortalityRatesFileName)
	} else if (v1other$nonAaaDeathMethod == "onsNonintegerStart") {
		v1other$nonAaaMortalityRates <- 
				readMortalityRatesFromFile(v1other$nonAaaMortalityRatesFileName)
	} else {
		stop("v1other$nonAaaDeathMethod=", v1other$nonAaaDeathMethod, 
				" is illegal")
	}
	
	# Copy v2$sigmaW into v2$ultrasoundMeasurementErrorSD.
	# This removes the need for v2$ultrasoundMeasurementErrorSD to be defined 
	# in the "input" files
	v2$ultrasoundMeasurementErrorSD <- v2$sigmaW
	
	# Change the prevalence, if v2$prevalence exists.
	if(!("baselineDiameter" %in% names(personData))){
	  if ("prevalence" %in% names(v2)) {
	    v2$baselineDiametersWithDesiredPrevalence <- 
	      changePrevalence(baselineDiameters=v1other$baselineDiameters, 
	                       threshold=v1other$prevalenceThreshold, prevalence=v2$prevalence)
	    if (v0$verbose)
	      cat("Prevalence (in v2$baselineDiametersWithDesiredPrevalence) ",
	          "has been\n changed to ", v2$prevalence, 
	          ", using threshold=v1other$prevalenceThreshold=",
	          v1other$prevalenceThreshold, ".\n", sep="")
	  } else {
	    v2$baselineDiametersWithDesiredPrevalence <- v1other$baselineDiameters
	    if (v0$verbose) cat("v2$prevalence does not exist, so \n",
	                        " v2$baselineDiametersWithDesiredPrevalence is just a copy of",
	                        " v1other$baselineDiameters.\n", sep="")
	  }
	  v2$baselineDiametersWithDesiredPrevalence <- setType(
	    v2$baselineDiametersWithDesiredPrevalence, 
	    "baseline diameters with desired prevalence")
	  # Check the arguments.
	  if (!("weight" %in% names(v2$baselineDiametersWithDesiredPrevalence)))
	    stop("v2$baselineDiametersWithDesiredPrevalence must have a ",
	         "\"weight\" column")
	}
	
	# Set v1other$thresholdForIncidentalDetection to 
	# v1other$aortaDiameterThresholds[[1]], if the former was not set. 
	if (!("thresholdForIncidentalDetection" %in% names(v1other))) {
	  thresh <- v1other$aortaDiameterThresholds[[1]][1]
	  v1other$thresholdForIncidentalDetection <- thresh
		if (v0$verbose) cat("v1other$thresholdForIncidentalDetection was not ",
				"provided and so\n has been set to ",
				"v1other$aortaDiameterThresholds[[1]][1]=", 
				thresh, ".\n", sep="")
	}
	
	# Make a list to store the output in. 
	result <- list()
	if (v0$returnMeanQuantities)
		result$meanQuantities <- NA
	if (v0$returnEventHistories) result$eventHistories <- 
			lapply(X=1:v0$numberOfPersons, FUN=function(x) list())
	if (v0$returnAllPersonsQuantities) result$allPersonsQuantities <- 
			lapply(X=1:v0$numberOfPersons, FUN=function(x) list())

	# Create and analyze the persons. Pass all variables to processOnePair
	if (v0$method == "serial") {
    if(debug == T){
        browser()	
    }
		setAndShowRandomSeed(v0$randomSeed, verbose=v0$verbose)
		resultForEachPerson <- lapply(X=1:v0$numberOfPersons, 
				FUN=processOnePair, v0, v1other, v2, personData)
		
	} else if (v0$method == "parallel") {
	  require("parallel", quietly = T)
		cluster <- makeCluster(v0$numberOfProcesses)  
		clusterExport(cluster, getAllFunctionsAndStringsInGlobalEnv())
		setAndShowRandomSeed(randomSeed=v0$randomSeed, cluster=cluster, 
				verbose=v0$verbose)
		resultForEachPerson <- parLapply(cl=cluster, X=1:v0$numberOfPersons,
				fun=processOnePair, v0, v1other, v2, personData)
		stopCluster(cluster) 
		
	} else if (v0$method == "foreach") {
	  require("doParallel", quietly = T)
		if (!is.null(v0$randomSeed)) 
			stop("v0$randomSeed does not yet work with v0$method=\"foreach\"")
		registerDoParallel(cores=v0$numberOfProcesses)
		resultForEachPerson <- foreach(personNumber=1:v0$numberOfPersons,
				.export=getAllFunctionsAndStringsInGlobalEnv()) %dopar% {
			processOnePair(personNumber, v0, v1other, v2, personData) 
		}
		stopImplicitCluster()  
		
	} else {
		stop("v0$method=", v0$method, " is illegal")
	}
	
	# Get event-histories and person-specific quantities 
	if (v0$returnEventHistories) 
		for (i in 1:v0$numberOfPersons) 
			result$eventHistories[[i]] <- 
					resultForEachPerson[[i]]$eventHistories
	if (v0$returnAllPersonsQuantities)
		for (i in 1:v0$numberOfPersons)
			result$allPersonsQuantities[[i]] <- 
					resultForEachPerson[[i]]$personQuantities
	
	# Calculate the means, if required. 
	if (v0$returnMeanQuantities) {
		result$meanQuantities <- makeArray(treatmentGroup=v0$treatmentGroups, 
				quantity=v0$namesOfQuantities)
		for (treatmentGroup in v0$treatmentGroups) {
			totals <- rep(0, length(v0$namesOfQuantities))
			for (i in 1:v0$numberOfPersons)
				totals <- totals + resultForEachPerson[[i]]$
						personQuantities[[treatmentGroup]]
			result$meanQuantities[treatmentGroup, ] <- 
					totals / v0$numberOfPersons
		}
	}
	
	if (v0$verbose) cat("processPersons is about to return an object of size ",
			format(object.size(result), unit="auto"), ".\n", sep="")
	return(result)
}

################################################################################

################################################################################
# 1b) processPersonsAboveDiagnosisThreshold
# A function that can be used in place of processPersons.
# It will produce very accurate estimates of incremental effects and costs 
# with fewer indivduals. It assumes that there are zero incremental effects below 
# the diagnosis threshold between invited and non-invited groups. It cannot be used 
# to get total numbers of events in the population but can be used for PSA analyses
################################################################################
processPersonsAboveDiagnosisThreshold <- function(v0, v1other, v2, 
		threshold=3.0, personData) {
	
  # Source auxiliary functions
  source("functions/Auxiliary_Functions.R", local = T)
  
  # Set unspecified elements of v0 to default values, if necessary. 
  v0 <- setUnspecifiedElementsOfv0(v0)
  
  # Set unspecified elements of v1other to default values
  v1other <- setUnspecifiedElementsOfv1other(v1other)
  
  if("baselineDiameter" %in% names(personData))
    stop("Selective sampling above diagnosis threshold not currently implemented if baseline aorta diameter is
         specified in personData. Please use v1other$baselineDiameters instead.")
  
	## Weighting of baseline distribution outside of processPersons 
	# Change the prevalence, if v2$prevalence exists.
	if ("prevalence" %in% names(v2)) {
	  v1other$baselineDiameters <- 
	    changePrevalence(baselineDiameters=v1other$baselineDiameters, 
	                     threshold=v1other$prevalenceThreshold, prevalence=v2$prevalence)
	 if (v0$verbose)
	   cat("Prevalence",
	       "has been\n changed to ", v2$prevalence, 
	       ", using threshold=v1other$prevalenceThreshold=",
	       v1other$prevalenceThreshold, ".\n", sep="")
	  v2$prevalence<-NULL ## to avoid processPersons reweighting
	} else {
	  v1other$baselineDiameters <- v1other$baselineDiameters
	  if (v0$verbose) cat("v2$prevalence does not exist, so \n",
	                      " v2$baselineDiameters left as is",
	                      sep="")
	}
	
	# Make v1bd and find the true proportion who are below the threshold.
	v1bd <- v1other$baselineDiameters
	trueProportionBelowThreshold <- 
			sum(v1bd$weight[v1bd$size < threshold]) / sum(v1bd$weight)
	
	
	# Create v0over and v1over, for the run of processPersons in which all 
	# baseline diameters are greater or equal to the threshold.
	v0over <- v0
	v1otherOver <- v1other
	v1otherOver$baselineDiameters$weight[v1bd$size < threshold] <- 0
	
	# Run processPersons just once based on distribution of diameters greater or equal to the threshold.
	resultOver <- processPersons(v0over, v1otherOver, v2, personData = personData)
	# Obtain incremental effects and costs for this population, weighted by the 
	# proportion of people >= diagnosis threshold. 
	result<-list(incrementalMeanQuantities=(1-trueProportionBelowThreshold)*
	               (resultOver$meanQuantities["screening",]-resultOver$meanQuantities["noScreening",]))
	
	# Calculate incremental costs for those below the threshold
	screening.costs<-v2$costs[c("inviteToScreen","requireReinvitation","screen")]
	mean.screening.costs.in.screened.normals<- screening.costs["inviteToScreen"]+
	v2$probOfRequireReinvitation*screening.costs["requireReinvitation"]+
	  v2$probOfAttendScreen*screening.costs["screen"]
	
	result$incrementalMeanQuantities["cost"]<-result$incrementalMeanQuantities["cost"]+
	trueProportionBelowThreshold*mean.screening.costs.in.screened.normals
	# Discounted screening costs are the same as undiscounted as they all happen at time zero.
	result$incrementalMeanQuantities["discountedCost"]<-result$incrementalMeanQuantities["discountedCost"]+
	trueProportionBelowThreshold*mean.screening.costs.in.screened.normals
	
	
	if(v0$returnAllPersonsQuantities){
	   temp <- apply(sapply(X=resultOver$allPersonsQuantities, FUN=function(x) {
	     (1-trueProportionBelowThreshold)*(x$screening - x$noScreening)}),1,function(i){cumsum(i)/seq_along(i)})
	   temp[,"cost"]<-temp[,"cost"]+trueProportionBelowThreshold*mean.screening.costs.in.screened.normals
	   temp[,"discountedCost"]<-temp[,"discountedCost"]+
	   trueProportionBelowThreshold*mean.screening.costs.in.screened.normals
	   result$incrementalCumMean<-temp
	}
	
	if(v0$returnEventHistories){
	  
	  ## As above the difference in the event numbers is a the difference in numbers from the sampled over the threshold model * proportion over threshold
	  #events<-eventsandcosts(resultOver)
	  #events2<-data.frame(event=events$event,incrementalEvents=(1-trueProportionBelowThreshold)*(events[,"screening.n",drop=F]-events[,"noScreening.n"]))
	  events<-tab.events(resultOver,v0=v0,v1other=v1other)[,c(1,2,5)]
	  events[,"Difference"]<-(1-trueProportionBelowThreshold)*events[,"Difference"]
	  result$incrementalEvents<-events
	  
	}	
	
	result$trueProportionBelowThreshold<-trueProportionBelowThreshold
	
	return(result)
}

################################################################################



################################################################################
# 2c) Functions for compactList, to display v1other, v2, etc. more compactly
################################################################################
compactList <- function(...) {
	result <- list(...)
	if (length(result) == 1 && is.list(result[[1]])) 
		result <- result[[1]]  
	class(result) <- "compactList"
	result
}

print.compactList <- function(x, ...) {
	if (!is.list(x)) stop("x must be a list")
	
	# Make "spaces". 
	if ("indentSize" %in% names(list(...))) {
		indentSize <- list(...)$indent
	} else {
		indentSize <- 0
	}
	spaces <- paste0(rep(" ", indentSize), collapse="")
	
	# Deal with the case that it is an empty list. 
	if (length(x) == 0) {
		cat("[empty ", class(x), "]\n", sep="")
		return(invisible(x))
	}
	
	# Make elementNames. 
	elementNames <- names(x)
	if (is.null(elementNames))  
		elementNames <- rep("", length(x))
	for (i in 1:length(x)) 
		if (elementNames[i] == "")
			elementNames[i] <- paste0("[[", i, "]]")

	# Display the main output. 
	for (i in 1:length(x)) {
		cat(spaces, elementNames[i], " = ", sep="")
		element <- x[[i]]
		typeAttr <- attr(element, "type")
		if (is.data.frame(element)) {
			# Display the data-frame in a compact way. 
			cat("data.frame with ",  ncol(element), " columns:\n", sep="")
			numberOfElementsToShow <- 8
			for (colName in names(element)) 
				cat("  ", colName, " = ", paste(head(element[,colName], 
						numberOfElementsToShow), collapse=" "), 
						{ if(length(element[,colName]) <= 
						numberOfElementsToShow) "" else " ..." }, "\n", sep="")
		} else if (is.list(element)) {
			# If all elements have length 1 then print on one line:
			if (all(sapply(element, FUN=function(el) { length(el)==1 }))) {
				cat(class(element), ": ", sep="")
				for (i in seq_along(element))
					cat(names(element)[i], "=", element[[i]], " ", sep="")
				if (!is.null(typeAttr))
					cat("[type=", typeAttr, "] ", sep="")
				cat("\n")
			} else {
				cat(class(element), "with", length(element), "elements:\n")
				print.compactList(element, indentSize=indentSize + 5)
			}

		} else if (length(element) < 100 && 
		           any(class(element) %in% c("numeric", "logical", "character"))) {
		  if (length(element) == 0) {
		    cat(class(element), "(0)", sep="")
		  } else if (is.null(names(element))) {
		    cat(element)
		    cat(" ")
		  } else {
		    for (i in seq_along(element)) {
		      thisName <- names(element)[i]
		      cat({ if(thisName=="") "UNNAMED" else thisName }, "=", 
		          element[i], " ", sep="")
		    }
		  }
		  if (!is.null(typeAttr))
		    cat("[type=", typeAttr, "] ", sep="")
		  cat("\n")
		} else if (is.null(element)) {
			cat("NULL\n")
		} else {
			cat("\n")
			print(element)
		}
	}
	invisible(x)
}

################################################################################

################################################################################

################################################################################
# 3a) Probabilistic sensitivity analysis
# This does processPersons many times.
# The usual output of a PSA is "v0$numberOfParameterIterations" with different 
# numbers for all the quantities. This will be returned as a three-dimensional 
# array. 
# v2values, if it is supplied, must be a list of v0$numberOfParameterIterations
# lists, each of which can be used as a specific value of v2 (i.e. a specific 
# set of global uncertain parameters). 
################################################################################
psa <- function(v0, v1other, v1distributions, v2values, personData=NULL) {
	
  # Source auxiliary functions
  source("functions/Auxiliary_Functions.R", local = T)
  
  # Set elements of v0 as needed for PSA. 
	v0 <- setUnspecifiedElementsOfv0(v0)  
	v0$returnMeanQuantities <- TRUE
	v0$returnEventHistories <- FALSE
	v0$returnAllPersonsQuantities <- FALSE
	v0$showEventHistories <- FALSE
	v0$verbose <- FALSE 
	
	# Set unspecified elements of v1other to default values
	v1other <- setUnspecifiedElementsOfv1other(v1other)
	
	# Display messages, settings, parameters, etc.
	cat("Running psa on ", Sys.info()["nodename"], " with:\n  ", 
			"v0$numberOfParameterIterations=", v0$numberOfParameterIterations, 
			", v0$numberOfPersons=", v0$numberOfPersons, 
			", numberOfProcesses=", v0$numberOfProcesses, "\n", sep="")
	cat("\n########## v0 ##########\n")
	print(v0)
	cat("########## v1other ##########\n")
	print(v1other)
	cat("########## v1distributions ##########\n")
	print(v1distributions)
	cat("########## personData ##########\n")
	print(personData)
	cat("########################\n\n")
	if (v0$returnEventHistories)  
		cat("In psa, v0$returnEventHistories is TRUE, so this will take a ",
				"lot of memory.\n", sep="")
	if ("generateCensoringTime" %in% names(v0))
		cat("Censoring is being used, so life-years etc. will be calculated",
				"up to censoring times.\n")
	
	# If v2values is missing, then create it. 
	# If it is given, then check it.
		if (missing(v2values)) {  
		setAndShowRandomSeed(v0$randomSeed, verbose=TRUE)  # uses set.seed
		v2values <- replicate(n=v0$numberOfParameterIterations, 
				expr=generateV2(v1distributions, v2), simplify=FALSE) 
	} else {
		npi <- v0$numberOfParameterIterations
		if (!is.list(v2values) || !all(sapply(v2values, is.list)) ||
				length(v2values) !=	npi)
			stop("if v2values is given then it must be a list of length\n",
					"  v0$numberOfParameterIterations=", npi,
					" whose elements are all lists that are values of v2")
		cat("NB v2values has been supplied to psa, so v2 has not been ",
				"generated by psa.\n", sep="")
	}

	# Main PSA loop. 
	if (v0$method == "serial") {
		setAndShowRandomSeed(v0$randomSeed, verbose=TRUE)
		v0$randomSeed <- NULL  
		resultOfApply <- lapply(X=1:v0$numberOfParameterIterations, 
				FUN=onePsaIteration, v0, v1other, v2values, personData)
	} else if (v0$method == "parallel") {
		# Do PSA in parallel and processPersons serially. 
	  v0$method <- "serial" 
		require(parallel, quietly=TRUE)
		cluster <- makeCluster(v0$numberOfProcesses)  
		clusterExport(cluster, getAllFunctionsAndStringsInGlobalEnv())
		setAndShowRandomSeed(randomSeed=v0$randomSeed, cluster=cluster, 
				verbose=TRUE)  
		v0$randomSeed <- NULL  
		resultOfApply <- parLapply(cl=cluster, 
				X=1:v0$numberOfParameterIterations, fun=onePsaIteration, v0, 
				v1other, v2values, personData)
		stopCluster(cluster) 
	} else if (v0$method == "foreach" || v0$method == "parallelBatches") {
		stop("v0$method=", v0$method, " has not been implemented for psa")
	} else {
		stop("v0$method=", v0$method, " is illegal")
	}
	
	psaQuantities <- makeArray(
			treatmentGroup=v0$treatmentGroups, 
			quantity=v0$namesOfQuantities,
			psaIterationNumber=1:v0$numberOfParameterIterations)
	v2valuesFromResultOfApply <- 
			vector(mode="list", length=v0$numberOfParameterIterations)
	for (i in 1:v0$numberOfParameterIterations) {
		psaQuantities[,,i] <- resultOfApply[[i]]$meanQuantities
		v2valuesFromResultOfApply[[i]] <- resultOfApply[[i]]$v2
	}
	if (!identical(v2values, v2valuesFromResultOfApply))
		stop("INTERNAL ERROR: v2values as supplied to psa should be the ",
				"same as v2valuesFromResultOfApply")
	
	if (v0$returnEventHistories) {
		eventHistoryLists <- 
				vector(mode="list", length=v0$numberOfParameterIterations)
		for (i in 1:v0$numberOfParameterIterations)
			eventHistoryLists[[i]] <- resultOfApply[[i]]$eventHistories
	}
	
	# Return psaQuantities and v2values
	result <- list(psaQuantities=psaQuantities, v2values=v2values)
	if (v0$returnEventHistories) result$eventHistoryLists <- eventHistoryLists
	cat("psa is about to return an object of size ", 
			format(object.size(result), unit="auto"), ".\n", sep="")
	return(result)
}



################################################################################

################################################################################
# 3b) Probabilistic sensitivity analysis above diagnosis threshold
# As above, but adapted to work with processPersonsAboveDiagnosticThreshold
################################################################################
psaAboveDiagnosisThreshold <- function(v0, v1other, v1distributions, v2values,
                                       threshold=3.0, personData=NULL) {
	
  # Source auxiliary functions
  source("functions/Auxiliary_Functions.R", local = T)
  
  # Set elements of v0 as needed for PSA. 
	v0 <- setUnspecifiedElementsOfv0(v0)  
	v0$returnMeanQuantities <- TRUE
	v0$returnEventHistories <- FALSE
	v0$returnAllPersonsQuantities <- FALSE
	v0$showEventHistories <- FALSE
	v0$verbose <- FALSE  # prevent processPersons from being verbose
	
	# Set unspecified elements of v1other to default values
	v1other <- setUnspecifiedElementsOfv1other(v1other)
	
	
	# Display messages, settings, parameters, etc.
	cat("Running psa on ", Sys.info()["nodename"], " with:\n  ", 
			"v0$numberOfParameterIterations=", v0$numberOfParameterIterations, 
			", v0$numberOfPersons=", v0$numberOfPersons, 
			", numberOfProcesses=", v0$numberOfProcesses, "\n", sep="")
	cat("\n########## v0 ##########\n")
	print(v0)
	cat("########## v1other ##########\n")
	print(v1other)
	cat("########## v1distributions ##########\n")
	print(v1distributions)
	cat("########## personData ##########\n")
	print(personData)
	cat("########################\n\n")
	if (v0$returnEventHistories) 
		cat("In psa, v0$returnEventHistories is TRUE, so this will take a ",
				"lot of memory.\n", sep="")
	if ("generateCensoringTime" %in% names(v0))
		cat("Censoring is being used, so life-years etc. will be calculated",
				"up to censoring times.\n")

	# If v2values is missing, then create it. 
	# If it is given, then check it.
	if (missing(v2values)) {  
		setAndShowRandomSeed(v0$randomSeed, verbose=TRUE) 
		v2values <- replicate(n=v0$numberOfParameterIterations, 
				expr=generateV2(v1distributions, v2), simplify=FALSE) 
	} else {
		npi <- v0$numberOfParameterIterations
		if (!is.list(v2values) || !all(sapply(v2values, is.list)) ||
				length(v2values) !=	npi)
			stop("if v2values is given then it must be a list of length\n",
					"  v0$numberOfParameterIterations=", npi,
					" whose elements are all lists that are values of v2")
		cat("NB v2values has been supplied to psa, so v2 has not been ",
				"generated by psa.\n", sep="")
	}

	# Main PSA loop. 
	if (v0$method == "serial") {
		setAndShowRandomSeed(v0$randomSeed, verbose=TRUE)
		v0$randomSeed <- NULL 
		resultOfApply <- lapply(X=1:v0$numberOfParameterIterations, 
				FUN=onePsaIterationAboveDiagnosisThreshold, v0, v1other, v2values, threshold, personData)
	} else if (v0$method == "parallel") {
		# Do PSA in parallel and processPersons serially. 
		v0$method <- "serial" 
		require(parallel, quietly=TRUE)
		cluster <- makeCluster(v0$numberOfProcesses)
		clusterExport(cluster, getAllFunctionsAndStringsInGlobalEnv())
		clusterExport(cluster, "detectCores")
		setAndShowRandomSeed(randomSeed=v0$randomSeed, cluster=cluster, 
				verbose=TRUE) 
		v0$randomSeed <- NULL 
		resultOfApply <- parLapply(cl=cluster, 
				X=1:v0$numberOfParameterIterations, fun=onePsaIterationAboveDiagnosisThreshold, v0, 
				v1other, v2values, threshold, personData)
		stopCluster(cluster) 
	} else if (v0$method == "foreach" || v0$method == "parallelBatches") {
		stop("v0$method=", v0$method, " has not been implemented for psa")
	} else {
		stop("v0$method=", v0$method, " is illegal")
	}

	psaQuantities <- makeArray(
  		psaIterationNumber=1:v0$numberOfParameterIterations
  		,	quantity=v0$namesOfQuantities)
	v2valuesFromResultOfApply <- 
			vector(mode="list", length=v0$numberOfParameterIterations)
	for (i in 1:v0$numberOfParameterIterations) {
		psaQuantities[i,] <- resultOfApply[[i]]$incrementalMeanQuantities
		v2valuesFromResultOfApply[[i]] <- resultOfApply[[i]]$v2
	}
	if (!identical(v2values, v2valuesFromResultOfApply))
		stop("INTERNAL ERROR: v2values as supplied to psa should be the ",
				"same as v2valuesFromResultOfApply")
	
	# Calculate ICERs and INMBs
	# CI for the ICER should be calculated based on the proportion of points below the willingness 
	# to pay threshold. 
	ICER_lifeYears<-psaQuantities[,"cost"]/psaQuantities[,"lifeYears"]
	ICER_lifeYears[psaQuantities[,"lifeYears"]<0 & psaQuantities[,"cost"]>0]<-Inf
	ICER_discountedLifeYears<-psaQuantities[,"discountedCost"]/psaQuantities[,"discountedLifeYears"]
	ICER_discountedLifeYears[psaQuantities[,"discountedLifeYears"]<0 & psaQuantities[,"discountedCost"]>0]<-Inf
	ICER_discountedQalys<-psaQuantities[,"discountedCost"]/psaQuantities[,"discountedQalys"]
	ICER_discountedQalys[psaQuantities[,"discountedQalys"]<0 & psaQuantities[,"discountedCost"]>0]<-Inf
	psaQuantities<-cbind(psaQuantities,ICER_lifeYears,ICER_discountedLifeYears,ICER_discountedQalys)
	
	INMB_discountedQalys_20000<- (psaQuantities[,"discountedQalys"]*20000 - psaQuantities[,"discountedCost"])
	INMB_discountedQalys_30000<- (psaQuantities[,"discountedQalys"]*30000 - psaQuantities[,"discountedCost"])
	psaQuantities<-cbind(psaQuantities,INMB_discountedQalys_20000,INMB_discountedQalys_30000)
	                     
	# Return psaQuantities and v2values
	result <- list(psaQuantities=psaQuantities, v2values=v2values)
	cat("psa is about to return an object of size ", 
			format(object.size(result), unit="auto"), ".\n", sep="")
	return(result)
}


# Convert from a three-month transition probability to a rate.
convertThreeMonthProbToRate <- function(prob) {
	-4 * log(1 - prob)
}


setType <- function(x, typeAttr) {
	if (missing(typeAttr)) stop("typeAttr must not be missing")
	attr(x, "type") <- typeAttr
	return(x)
}


################################################################################
# 7) Output functions
# Functions for creating tables and graphs. Some are for use on objects produced by 
# processPersons, some are for use on objects produced by psa, and some are both. 
# Used in producing output from model runs and for Tables and Figures seen in:
# [Glover et al, 2018. Discrete Event Simulation for Decision Modeling in Health Care: 
# Lessons from Abdominal Aortic Aneurysm Screening] AND
# [Sweeting et al, 2018. Should we screen women for abdominal aortic aneurysm? 
# A cost-effectiveness analysis]
################################################################################

## eventsBySizeCat is a function that counts events that occur after AAA is measured in a given size range
## the person years at risk for this event, and the rate (per person-year) and risk of this event occuring before the next event (be it surveillance, dropout, death, surgery or rupture)
## where sizeLower <= measuredSize < sizeUpper
eventsBySizeCat <- function(result, event, sizeLower, sizeUpper, treatmentGroup){
  count <- pyrs <- nSizeEvents <- sumSizeEvents <- sumSizeNextEvents <- 0
  N <- length(result$eventHistories)
  for(i in 1:N) {
    sizeEvents <- result$eventHistories[[i]][[treatmentGroup]]$measuredSizes >= sizeLower & result$eventHistories[[i]][[treatmentGroup]]$measuredSizes < sizeUpper
    sizeEvents[is.na(sizeEvents)] <- FALSE
    nSizeEvents <- nSizeEvents + sum(sizeEvents)
    sumSizeEvents <- sumSizeEvents + sum(result$eventHistories[[i]][[treatmentGroup]]$measuredSizes[sizeEvents])
    nextEvents <- c(FALSE, sizeEvents[-length(sizeEvents)])
    countNew <- result$eventHistories[[i]][[treatmentGroup]]$events[nextEvents] == event
    count <- count + sum(countNew)
    if(any(countNew)){
      sumSizeNextEvents <- sumSizeNextEvents + sum(result$eventHistories[[i]][[treatmentGroup]]$trueSizes[nextEvents & result$eventHistories[[i]][[treatmentGroup]]$events == event])
    }
    pyrs <- pyrs + sum(result$eventHistories[[i]][[treatmentGroup]]$times[nextEvents] - result$eventHistories[[i]][[treatmentGroup]]$times[sizeEvents])
  }
  return(list(events = count, pyrs = pyrs, rate = count/pyrs, nSizeEvents = nSizeEvents, 
              risk = count / nSizeEvents, meanSizeEvents = sumSizeEvents / nSizeEvents,
              meanSizeNextEvents = sumSizeNextEvents / count))
}

## Function to ascertain size of AAA (true and as measured) at first consultation
## both true and measured size can be less than intervention threshold
## measured is the confirmatory measurement taken via CT so may be less than US that prompted the consultation
consultationSize <- function(result, treatmentGroup){
  N <- length(result$eventHistories)
  measured <- true <- NULL
  for(i in 1:N) {
    events <- result$eventHistories[[i]][[treatmentGroup]]$events == "consultation"
    if(any(events)){
      measured <- c(measured, min(result$eventHistories[[i]][[treatmentGroup]]$measuredSizes[result$eventHistories[[i]][[treatmentGroup]]$events == "consultation"]))
      true <- c(true, min(result$eventHistories[[i]][[treatmentGroup]]$trueSizes[result$eventHistories[[i]][[treatmentGroup]]$events == "consultation"]))
    }
  }
  return(list(measured = summary(measured), true = summary(true)))
}


# Functions for counting numbers of events, e.g. countEvents(personsInfo, "consultation", "noScreening")
# NB this counts the number of people who have the event at least once, not the total number of events. 
countEvents <- function(personsInfo, event, treatmentGroup, timeLimit=NA) {
	if (!("eventHistories" %in% names(personsInfo)))
		stop("countEvents requires personsInfo to contain eventHistories")
	if (!is.na(timeLimit) && (length(timeLimit) > 1 || !is.numeric(timeLimit)))
		stop("timeLimit must be a single number (or NA)")

	total <- 0
	for (i in 1:length(personsInfo$eventHistories)) {
		eventHistory <- personsInfo$eventHistories[[i]][[treatmentGroup]]
		if (is.na(timeLimit)) {  # original version, with no timeLimit
			if (event %in% eventHistory$events)
				total <- total + 1 
		} else {  # only count them if they had the event before timeLimit
			firstEventNumber <- match(event, eventHistory$events)
			if (!is.na(firstEventNumber) && 
					eventHistory$times[firstEventNumber] <= timeLimit)
				total <- total + 1 
		}
	}
	total 
}

# Count the number of dropouts, the number of people who were screened, found to have an aneurysm 
# (i.e. have diameter greater than 3.0cm), and subsequently dropped out 
# (not those invited to screening, found not to have an aneurysm, and subsequently 
# dropped out from monitoring, then had incidental detection, and later dropped out)
countDropouts <- function(personsInfo, v1other, timeLimit=NA) {
	if (!("eventHistories" %in% names(personsInfo)))
		stop("countDropouts requires personsInfo to contain eventHistories")
	if (!is.na(timeLimit) && (length(timeLimit) > 1 || !is.numeric(timeLimit)))
		stop("timeLimit must be a single number (or NA)")
	if (!is.list(v1other) || !("aortaDiameterThresholds" %in% names(v1other)))
		stop("v1other must be a list with an aortaDiameterThresholds element")
	if (!("measuredSizes" %in% 
			names(personsInfo$eventHistories[[1]]$screening)))
		stop("event-histories must contain measuredSizes")
	
	total <- 0
	for (i in 1:length(personsInfo$eventHistories)) {
		eventHistory <- personsInfo$eventHistories[[i]][["screening"]]
		
		# If they were screened and found to be aneurysmal
		if ("screen" %in% eventHistory$events && 
				!("nonvisualization" %in% eventHistory$events) &&
				eventHistory$measuredSizes[match("screen", 
				eventHistory$events)] >= v1other$aortaDiameterThresholds[[1]]) {

			if (is.na(timeLimit)) { 
				if ("dropout" %in% eventHistory$events)
					total <- total + 1 
			} else { 
				firstEventNumber <- match("dropout", eventHistory$events)
				if (!is.na(firstEventNumber) && 
						eventHistory$times[firstEventNumber] <= timeLimit)
					total <- total + 1 
			}		
		}
	}
	total 
}

# Functions for counting numbers of events, e.g. countEventsForSpecificRoute(personsInfo, "electiveSurgeryOpen", 
# "incidentallyDetected"). "route" must be either incidentallyDetected or screenDetected. 
countEventsForSpecificRoute <- function(personsInfo, event, route,
		treatmentGroup="screening", timeLimit=NA) {
	if (!("eventHistories" %in% names(personsInfo)))
		stop("countEventsForSpecificRoute requires personsInfo to contain ",
				"eventHistories")
	permittedEventTypes <- 
			c("electiveSurgeryOpen", "electiveSurgeryEvar", "contraindicated")
	if (!event %in% permittedEventTypes)
		stop("event must be one of ", paste(permittedEventTypes, collapse=", "))
	if (!(route %in% c("incidentallyDetected", "screenDetected")))
		stop("route must be either incidentallyDetected or screenDetected")
	if (!is.na(timeLimit) && (length(timeLimit) > 1 || !is.numeric(timeLimit)))
		stop("timeLimit must be a single number (or NA)")
	
	total <- 0
	for (i in 1:length(personsInfo$eventHistories)) {  # (seq_along is better)
		eventHistory <- personsInfo$eventHistories[[i]][[treatmentGroup]]
		events <- eventHistory$events
		# If they EVER had incidental detection, that counts as incidental
		# detection:
		followedThisRoute <- 
		(route=="incidentallyDetected" && "incidentalDetection" %in% events) || 
		(route=="screenDetected" && !("incidentalDetection" %in% events)) 
		if (is.na(timeLimit)) {
			if (followedThisRoute && event %in% events)
				total <- total + 1
		} else { 
			firstEventNumber <- match(event, events)
			if (followedThisRoute && !is.na(firstEventNumber) && 
					eventHistory$times[firstEventNumber] <= timeLimit)
				total <- total + 1 
		}
	}
	return(total)
}

# Counts all the kinds of events, used for counting events for 4 year men model
countEventsFromOurSimulation <- function(result, v1other) {
	c(
		noSc_electiveSurgeryOpen=countEvents(result, 
				"electiveSurgeryOpen", "noScreening"),
		noSc_emergencySurgeryOpen=countEvents(result, 
				"emergencySurgeryOpen", "noScreening"),
		noSc_ruptures=countEvents(result, "rupture", "noScreening"),
		noSc_contraindications=countEvents(result, 
				"contraindicated", "noScreening"),
		noSc_aaaDeaths=countEvents(result, "aaaDeath", "noScreening"),
		noSc_nonAaaDeaths=countEvents(result, "nonAaaDeath", "noScreening"),
		
		scre_electiveSurgeryOpenScreenDet=countEventsForSpecificRoute(result, 
				"electiveSurgeryOpen", "screenDetected"),
		scre_electiveSurgeryOpenIncidentallyDet=countEventsForSpecificRoute(result,
				"electiveSurgeryOpen", "incidentallyDetected"),
		scre_emergencySurgeryOpen=countEvents(result, 
				"emergencySurgeryOpen", "screening"), 
		scre_ruptures=countEvents(result, "rupture", "screening"),
		scre_contraindicationsScreenDet=countEventsForSpecificRoute(result, 
				"contraindicated", "screenDetected"),
		scre_contraindicationsIncidentallyDet=countEventsForSpecificRoute(
				result, "contraindicated", "incidentallyDetected"),
		scre_dropout=countDropouts(result, v1other),
		scre_aaaDeaths=countEvents(result, "aaaDeath", "screening"),
		scre_nonAaaDeaths=countEvents(result, "nonAaaDeath", "screening")
	)
}

# Get incremental cost or effectiveness for each PSA replication.
getIncrementalCostOrEffectiveness <- function(
		costOrEffectiveness=c("cost", "effectiveness"), arrayOfMeans, 
		discounted=TRUE, qualityAdjusted=TRUE) {
	
	costOrEffectiveness <- match.arg(costOrEffectiveness)
	
	# Checks.
	if (!is.array(arrayOfMeans))
		stop("arrayOfMeans must be an array")
	numberOfDimensions <- length(dim(arrayOfMeans))
	if (numberOfDimensions != 2 && numberOfDimensions != 3)
		stop("arrayOfMeans must have two or three dimensions")
	if (!is.logical(discounted) || !is.logical(qualityAdjusted) || 
			length(discounted) != 1 || length(qualityAdjusted) != 1)
		stop("discounted and qualityAdjusted must be single logicals")
	checkArrayOfMeansFirstDimNames(arrayOfMeans)
	
	quantityName <- workOutQuantityName(costOrEffectiveness, qualityAdjusted, 
			discounted) 
	
	# Calculate the incremental cost or effectiveness.  
	if (numberOfDimensions == 2) {
		return(arrayOfMeans[2, quantityName] - arrayOfMeans[1, quantityName])
	} else {
		return(apply(X=arrayOfMeans, MARGIN=3, FUN=function(m) {
							m[2, quantityName] - m[1, quantityName] }))
	}
}

getCostOrEffectiveness <- function(
		costOrEffectiveness=c("cost", "effectiveness"), arrayOfMeans, 
		treatmentGroup,	discounted=TRUE, qualityAdjusted=TRUE) {
	# Checks.
	if (!is.array(arrayOfMeans))
		stop("arrayOfMeans must be an array")
	numberOfDimensions <- length(dim(arrayOfMeans))
	if (numberOfDimensions != 2 && numberOfDimensions != 3)
		stop("arrayOfMeans must have two or three dimensions")
	if (!is.logical(discounted) || !is.logical(qualityAdjusted) || 
			length(discounted) != 1 || length(qualityAdjusted) != 1)
		stop("discounted and qualityAdjusted must be single logicals")
	checkArrayOfMeansFirstDimNames(arrayOfMeans)
	
	firstDimName <- names(dimnames(arrayOfMeans))[[1]] 
	treatmentGroups <- dimnames(arrayOfMeans)[[1]] # can be c("scenario1", ...)
	if (!(firstDimName %in% c("treatmentGroup", "scenario")) ||
			(firstDimName == "treatmentGroup" && 
				!identical(treatmentGroups, c("noScreening", "screening"))) ||
			(firstDimName == "scenario" && 
				!identical(treatmentGroups, c("scenario1", "scenario2"))))
		stop("dimnames(arrayOfMeans) is illegal:",
				"\nnames(dimnames(arrayOfMeans))[[1]]=", firstDimName, 
				"\ndimnames(arrayOfMeans)[[1]]=", treatmentGroups)
	if (!(treatmentGroup %in% treatmentGroups))
		stop("treatmentGroup must be in dimnames(arrayOfMeans)$[[1]];",
				"\n probably treatmentGroup should be noScreening or screening",
				"\n and dimnames(arrayOfMeans)$[[1]] should be ",
				"c(\"noScreening\", \"screening\")")
	
	quantityName <- workOutQuantityName(costOrEffectiveness, qualityAdjusted, 
			discounted) 
	
	# Get the cost or effectiveness. 
	if (numberOfDimensions == 2) {
		result <- arrayOfMeans[treatmentGroup, quantityName]
	} else {
		result <- arrayOfMeans[treatmentGroup, quantityName, ]
	}
	if (is.null(result))
		stop("result is NULL; probably some dimnames were wrong")
	return(result)
}

# A function to work out what name to use when getting elements from an array 
# such as processPersonsResult$meanQuantities or psaResult$psaQuantities. 
workOutQuantityName <- 
		function(costOrEffectiveness, qualityAdjusted, discounted) {
	# Checks.
	if (!is.logical(qualityAdjusted) || length(qualityAdjusted) != 1 ||
			!is.logical(discounted) || length(discounted) != 1)
		stop("qualityAdjusted and discounted must be single logicals")
	if (!(costOrEffectiveness %in% c("effectiveness", "cost")))
		stop("costOrEffectiveness must be effectiveness or cost")
	
	if (costOrEffectiveness == "effectiveness") {
		if (qualityAdjusted) {
			quantityName <- "qalys"
		} else {
			quantityName <- "lifeYears"
		}
	} else {
		if (qualityAdjusted) {
			stop("if costOrEffectiveness=\"cost\" then qualityAdjusted must ",
					"be FALSE")
		} else {
			quantityName <- "cost"
		}
	} 
	if (discounted) {  
		firstLetter <- substr(quantityName, 1, 1)
		rest <- substr(quantityName, 2, nchar(quantityName))
		quantityName <- paste0("discounted", toupper(firstLetter), rest)
	}
	return(quantityName)
}

# Find the ICER at which treatments are equally likely to be cost-effective, 
# and an empirical 95% CI. 
findIcerQuantiles <- function(psaQuantities, discounted=TRUE, 
		qualityAdjusted=TRUE, ciPercent=95) {
	incrementalCost <- getIncrementalCostOrEffectiveness("cost", psaQuantities, 
			discounted=discounted, qualityAdjusted=FALSE)
	incrementalEffectiveness <- getIncrementalCostOrEffectiveness(
			"effectiveness", psaQuantities, discounted=discounted, 
			qualityAdjusted=qualityAdjusted)
	
	# A function to supply to uniroot:
	probCostEffectiveMinusPr <- function(icer, pr) {
		probCostEffective <- 
				mean(incrementalCost < icer * incrementalEffectiveness)
		probCostEffective - pr
	}
	# A function to find the icer for which the probability that the treatment 
	# is cost-effective is pr. This function assumes that probability increases  
	# with icer. 
	getIcer <- function(pr, maxIcerToTry) {
		if (probCostEffectiveMinusPr(maxIcerToTry, pr) < 0) 
			return(paste0(">", maxIcerToTry))
		if (sign(probCostEffectiveMinusPr(icer=0, pr=pr)) == 
				sign(probCostEffectiveMinusPr(icer=maxIcerToTry, pr=pr))) {
			# probCostEffectiveMinusPr is too flat in this range
			return("cannotCalculate")  
		} else {
			result <- uniroot(probCostEffectiveMinusPr, pr=pr, lower=0, 
					upper=maxIcerToTry)$root
			return(round(result))
		}
	}
	# A function to find icer5050 for pr=0.5. If icer is negative, 
	# ciLower and ciUpper will later be set to "cannotCalculate".
	getIcer5050 <- function(minIcerToTry, maxIcerToTry) {
		round(uniroot(probCostEffectiveMinusPr, pr=0.5, lower=minIcerToTry, 
						upper=maxIcerToTry)$root)
	}

	# Calculate and return the "quantiles".
	maxIcerToTry <- 1e9
	icer5050 <- getIcer5050(-1e6, maxIcerToTry)
	if (icer5050 > 0) {
		return(c(icer5050=icer5050, 
				ciLower=getIcer(getLowerQuantile(ciPercent), maxIcerToTry), 
				ciUpper=getIcer(getUpperQuantile(ciPercent), maxIcerToTry)))
	} else {
		return(c(icer5050=icer5050, 
				ciLower="cannotCalculate", ciUpper="cannotCalculate"))
	}
}

# Find mean and quantiles of net benefit / net monetary benefit / NMB, based on 
# specific value(s) of willingnessToPayThreshold.
findNetBenefitMeanAndQuantiles <- function(psaQuantities, discounted=TRUE, 
		qualityAdjusted=TRUE, willingnessToPayThreshold, ciPercent=95) {
	if (missing(willingnessToPayThreshold) || 
			!is.numeric(willingnessToPayThreshold) || 
			is.na(willingnessToPayThreshold) ||  
			length(willingnessToPayThreshold) != 1)
		stop("willingnessToPayThreshold must be a single numeric")
	incrementalCost <- getIncrementalCostOrEffectiveness("cost", psaQuantities, 
			discounted=discounted, qualityAdjusted=FALSE)
	incrementalEffectiveness <- getIncrementalCostOrEffectiveness(
			"effectiveness", psaQuantities, discounted=discounted, 
			qualityAdjusted=qualityAdjusted)
	netBenefit <- incrementalEffectiveness * willingnessToPayThreshold - 
			incrementalCost
	findMeanAndQuantiles(quantities=netBenefit, ciPercent=ciPercent)
}

findMeanAndQuantiles <- function(quantities, ciPercent=95) {
	result <-  c(mean(quantities), quantile(quantities, probs=c(0.5, 
							getLowerQuantile(ciPercent), getUpperQuantile(ciPercent))))
	names(result) <- c("mean", "median", "ciLower", "ciUpper")
	return(result)	
}

#######################################

# Make a table using output from processPersons. 
# This does not show a CI for the ICERs (requires PSA). 
showTableOfLifeYears <- function(result) {
	means <- result$meanQuantities
	
	# n=noScreening, s=screening, d=discounted, u=undiscounted, 
	# l=life-years, c=cost, qa=quality-adjusted
	ndl <- means["noScreening", "discountedLifeYears"]
	ndc <- means["noScreening", "discountedCost"]
	sdl <- means["screening", "discountedLifeYears"]
	sdc <- means["screening", "discountedCost"]
	nul <- means["noScreening", "lifeYears"]
	nuc <- means["noScreening", "cost"]
	sul <- means["screening", "lifeYears"]
	suc <- means["screening", "cost"]
	ndlqa <- means["noScreening", "discountedQalys"]
	sdlqa <- means["screening", "discountedQalys"]
	
	ca <- function(...) { cat(..., "\n", sep="") }
	sp <- function(n) { paste(rep(" ", n), collapse="") }
	fo <- function(x, width, dp, leftJustified=FALSE) { 
		sprintf(paste0("%", if (leftJustified) "-", width, ".", dp, "f"), x) 
	}
	
	ca("                   noScreening  screening  difference")
	ca("Life-years")
	ca("  Undiscounted  ", fo(nul,11,4), fo(sul,12,4), fo(sul-nul,13,5))
	ca("  Discounted    ", fo(ndl,11,4), fo(sdl,12,4), fo(sdl-ndl,13,5))
	ca("  Discounted, QA", fo(ndlqa,11,4), fo(sdlqa,12,4), fo(sdlqa-ndlqa,13,5))
	ca("Costs")
	ca("  Undiscounted  ", fo(nuc,11,2), fo(suc,12,2), fo(suc-nuc,12,3))
	ca("  Discounted    ", fo(ndc,11,2), fo(sdc,12,2), fo(sdc-ndc,12,3))
	ca("ICER")
	ca("  Undiscounted  ", fo((suc - nuc)/(sul - nul), 15, 0))
	ca("  Discounted    ", fo((sdc - ndc)/(sdl - ndl), 15, 0))
	ca("  Discounted, QA", fo((sdc - ndc)/(sdlqa - ndlqa), 15, 0))
	ca("INMB, Discounted, QA")
	ca("  Lambda of £20,000  ", fo((20000*(sdlqa-ndlqa))-(sdc-ndc), 10, 2))
	ca("  Lambda of £30,000  ", fo((30000*(sdlqa-ndlqa))-(sdc-ndc), 10, 2))
	
}

# Show a table that contains various numbers of events.
TableOfCounts <- function(result, v1other) {
	rateFactor <- 1e5 
	numberOfPersonsInOurSimulation <- length(result$eventHistories)
	counts <- c(
		noSc_electiveSurgeryOpen=countEvents(result, 
				"electiveSurgeryOpen", "noScreening"),
		noSc_electiveSurgeryEvar=countEvents(result, 
				"electiveSurgeryEvar", "noScreening"),
		noSc_emergencySurgeryOpen=countEvents(result, 
				"emergencySurgeryOpen", "noScreening"),
		noSc_emergencySurgeryEvar=countEvents(result, 
				"emergencySurgeryEvar", "noScreening"),
		noSc_ruptures=countEvents(result, "rupture", "noScreening"),
		noSc_contraindications=countEvents(result, 
				"contraindicated", "noScreening"),
		noSc_aaaDeaths=countEvents(result, "aaaDeath", "noScreening"),
		noSc_nonAaaDeaths=countEvents(result, "nonAaaDeath", "noScreening"),
		
		scre_electiveSurgeryOpenScreenDet=countEventsForSpecificRoute(
				result, "electiveSurgeryOpen", "screenDetected"),
		scre_electiveSurgeryOpenIncidentallyDet=countEventsForSpecificRoute(
				result, "electiveSurgeryOpen", "incidentallyDetected"),
		scre_electiveSurgeryEvarScreenDet=countEventsForSpecificRoute(
				result, "electiveSurgeryEvar", "screenDetected"),
		scre_electiveSurgeryEvarIncidentallyDet=countEventsForSpecificRoute(
				result, "electiveSurgeryEvar", "incidentallyDetected"),

		scre_emergencySurgeryOpen=countEvents(result, 
				"emergencySurgeryOpen", "screening"), 
		scre_emergencySurgeryEvar=countEvents(result, 
				"emergencySurgeryEvar", "screening"),
		scre_ruptures=countEvents(result, "rupture", "screening"),
		scre_contraindicationsScreenDet=countEventsForSpecificRoute(result, 
				"contraindicated", "screenDetected"),
		scre_contraindicationsIncidentallyDet=countEventsForSpecificRoute(
				result, "contraindicated", "incidentallyDetected"),
		scre_dropout=countDropouts(result, v1other),
		scre_aaaDeaths=countEvents(result, "aaaDeath", "screening"),
		scre_nonAaaDeaths=countEvents(result, "nonAaaDeath", "screening"),
		
		# Reinterventions and post-surgery monitoring.
		noSc_riAfterElectiveOpen=countEvents(result, 
				"reinterventionAfterElectiveOpen", "noScreening"),
		noSc_riAfterElectiveEvar=countEvents(result, 
				"reinterventionAfterElectiveEvar", "noScreening"),
		noSc_riAfterEmergencyOpen=countEvents(result, 
				"reinterventionAfterEmergencyOpen", "noScreening"),
		noSc_riAfterEmergencyEvar=countEvents(result, 
				"reinterventionAfterEmergencyEvar", "noScreening"),
		scre_riAfterElectiveOpen=countEvents(result, 
				"reinterventionAfterElectiveOpen", "screening"),
		scre_riAfterElectiveEvar=countEvents(result, 
				"reinterventionAfterElectiveEvar", "screening"),
		scre_riAfterEmergencyOpen=countEvents(result, 
				"reinterventionAfterEmergencyOpen", "screening"),
		scre_riAfterEmergencyEvar=countEvents(result, 
				"reinterventionAfterEmergencyEvar", "screening"),
		noSc_monitorFollowingOpenSurgery=countEvents(result, 
				"monitorFollowingOpenSurgery", "noScreening"),
		noSc_monitorFollowingEvarSurgery=countEvents(result, 
				"monitorFollowingEvarSurgery", "noScreening"),
		scre_monitorFollowingOpenSurgery=countEvents(result, 
				"monitorFollowingOpenSurgery", "screening"),
		scre_monitorFollowingEvarSurgery=countEvents(result, 
				"monitorFollowingEvarSurgery", "screening"),
		noSc_monitorFollowingContraindication=countEvents(result, 
				"monitorFollowingContraindication", "noScreening"),
		scre_monitorFollowingContraindication=countEvents(result, 
				"monitorFollowingContraindication", "screening")
	)
	
	roundedRates <- round(counts / numberOfPersonsInOurSimulation * rateFactor)
	df <- data.frame(nEv=counts, numberOfEventsAsRate=
			paste0("(", roundedRates, " per ", rateFactor, " persons)"))
	print(df)
	
}

# Make a table with Full evetns and costs, using output from processPersons. 
Eventsandcosts<-function(result){
  Eventsandcosts.df<-data.frame(event=NA,screening.n=NA,screening.prev=NA,
                                screening.cost=NA,screening.mean.cost=NA,
                                noScreening.n=NA,noScreening.prev=NA,
                                noScreening.cost=NA,noScreening.mean.cost=NA)
  ordered.events<-c("inviteToScreen","screen","requireReinvitation",
                    "failToAttendScreen", "nonvisualization","monitor", 
                    "dropout","incidentalDetection","consultation",
                    "decideOnElectiveSurgery","decideOnReturnToMonitoring", 
                    "contraindicated", "monitorFollowingContraindication",
                    "electiveSurgeryEvar","electiveSurgeryOpen",
                    "rupture","emergencySurgeryEvar","emergencySurgeryOpen",
                    "monitorFollowingEvarSurgery","monitorFollowingOpenSurgery",
                    "reinterventionAfterElectiveEvar", 
                    "reinterventionAfterEmergencyEvar", "reinterventionAfterEmergencyOpen",
                    "aaaDeath","nonAaaDeath","censored")
  
  if(class(result)=="weighted processPersons"){
    n.under<-length(result$resultUnder$eventHistories)
    n.over<-length(result$resultOver$eventHistories)
    nstar.under<-result$trueProportionBelowThreshold*(n.under+n.over)
    nstar.over<-(1-result$trueProportionBelowThreshold)*(n.under+n.over)
    noScreening.events.under<-
      unlist(sapply(1:n.under,function(i){result$resultUnder$eventHistories[[i]]$noScreening$events}))
    screening.events.under<-
      unlist(sapply(1:n.under,function(i){result$resultUnder$eventHistories[[i]]$screening$events}))
    noScreening.events.over<-
      unlist(sapply(1:n.over,function(i){result$resultOver$eventHistories[[i]]$noScreening$events}))
    screening.events.over<-
      unlist(sapply(1:n.over,function(i){result$resultOver$eventHistories[[i]]$screening$events}))
    
    i<-0 
    for(event in ordered.events){
      i<-i+1
      if(event %in% c(screening.events.under,screening.events.over)){
        Eventsandcosts.df[i,"event"]<-event
        Eventsandcosts.df[i,"screening.n"]<-
          max(0,table(screening.events.under)[names(table(screening.events.under))==event])/n.under*nstar.under+
          max(0,table(screening.events.over)[names(table(screening.events.over))==event])/n.over*nstar.over  
        Eventsandcosts.df[i,"screening.prev"]<-Eventsandcosts.df[i,"screening.n"]/v0$numberOfPersons
        Eventsandcosts.df[i,"screening.cost"]<-v2$costs[event]*Eventsandcosts.df[i,"screening.n"]
        Eventsandcosts.df[i,"screening.mean.cost"]<-Eventsandcosts.df[i,"screening.cost"]/v0$numberOfPersons
      }
      if(event %in% c(noScreening.events.under,noScreening.events.over)){
        Eventsandcosts.df[i,"event"]<-event
        Eventsandcosts.df[i,"noScreening.n"]<-
          max(0,table(noScreening.events.under)[names(table(noScreening.events.under))==event])/n.under*nstar.under+
          max(0,table(noScreening.events.over)[names(table(noScreening.events.over))==event])/n.over*nstar.over  
        Eventsandcosts.df[i,"noScreening.prev"]<-Eventsandcosts.df[i,"noScreening.n"]/v0$numberOfPersons
        Eventsandcosts.df[i,"noScreening.cost"]<-v2$costs[event]*Eventsandcosts.df[i,"noScreening.n"]
        Eventsandcosts.df[i,"noScreening.mean.cost"]<-Eventsandcosts.df[i,"noScreening.cost"]/v0$numberOfPersons
      }
    }
  } else {
    noScreening.events<-
      unlist(sapply(1:v0$numberOfPersons,function(i){result$eventHistories[[i]]$noScreening$events}))
    screening.events<-
      unlist(sapply(1:v0$numberOfPersons,function(i){result$eventHistories[[i]]$screening$events}))
    i<-0
    for(event in ordered.events){
      i<-i+1
      if(event %in% screening.events){
        Eventsandcosts.df[i,"event"]<-event
        Eventsandcosts.df[i,"screening.n"]<-
          table(screening.events)[names(table(screening.events))==event]
        Eventsandcosts.df[i,"screening.prev"]<-
          Eventsandcosts.df[i,"screening.n"]/v0$numberOfPersons
        Eventsandcosts.df[i,"screening.cost"]<-
          v2$costs[event]*Eventsandcosts.df[i,"screening.n"]
        Eventsandcosts.df[i,"screening.mean.cost"]<-
          Eventsandcosts.df[i,"screening.cost"]/v0$numberOfPersons
      }
      if(event %in% noScreening.events){
        Eventsandcosts.df[i,"event"]<-event
        Eventsandcosts.df[i,"noScreening.n"]<-
          table(noScreening.events)[names(table(noScreening.events))==event]
        Eventsandcosts.df[i,"noScreening.prev"]<-
          Eventsandcosts.df[i,"noScreening.n"]/v0$numberOfPersons
        Eventsandcosts.df[i,"noScreening.cost"]<-
          v2$costs[event]*Eventsandcosts.df[i,"noScreening.n"]
        Eventsandcosts.df[i,"noScreening.mean.cost"]<-
          Eventsandcosts.df[i,"noScreening.cost"]/v0$numberOfPersons
      }
    }
  }
  ## check total mean costs
  sum(Eventsandcosts.df$screening.mean.cost,na.rm=T)
  sum(Eventsandcosts.df$noScreening.mean.cost,na.rm=T)
  return(Eventsandcosts.df)
}

# Function to show incremental effect, costs, ICER and INMB 
# after running processPersonsAboveDiagnosisThreshold
showIncrementalLifeYearsAndCosts <- function(result) {
  means <- result$incrementalMeanQuantities
  
  # d=discounted, u=undiscounted, 
  # l=life-years, c=cost, qa=quality-adjusted
  dl <- means["discountedLifeYears"]
  dc <- means["discountedCost"]
  ul <- means["lifeYears"]
  uc <- means["cost"]
  dlqa <- means["discountedQalys"]
  dlqa <- means["discountedQalys"]
  
  # ca is like cat but with sep="" and \n
  # sp creates n spaces
  # fo rounds off to dp decimal places and creates a string 
  ca <- function(...) { cat(..., "\n", sep="") }
  sp <- function(n) { paste(rep(" ", n), collapse="") }
  fo <- function(x, width, dp, leftJustified=FALSE) { 
    sprintf(paste0("%", if (leftJustified) "-", width, ".", dp, "f"), x) 
  }
  
  ca("                   Difference")
  ca("Life-years")
  ca("  Undiscounted  ", fo(ul,13,5))
  ca("  Discounted    ", fo(dl,13,5))
  ca("  Discounted, QA", fo(dlqa,13,5))
  ca("Costs")
  ca("  Undiscounted  ", fo(uc,12,3))
  ca("  Discounted    ", fo(dc,12,3))
  ca("ICER")
  ca("  Undiscounted  ", fo(uc/ul, 15, 0))
  ca("  Discounted    ", fo(dc/dl, 15, 0))
  ca("  Discounted, QA", fo(dc/dlqa, 15, 0))
  ca("INMB, Discounted, QA")
  ca("  Lambda of £20,000  ", fo((20000*dlqa)-(dc), 10, 2))
  ca("  Lambda of £30,000  ", fo((30000*dlqa)-(dc), 10, 2))
  
}

# Show various PSA summary quantities, based on psaQuantities, namely 
# incremental cost (mean, median, and CI), incremental effectiveness 
# (mean, median, and CI), and ICER ("50-50" value, i.e. point estimate 
# from PSA, and CI).
showPsaSummaryQuantities <- function(psaQuantities, discounted=TRUE,
		qualityAdjusted=TRUE, 
		willingnessToPayThresholds=c(20000, 25000, 30000), 
		digits, ciPercent=95) {
	if (missing(digits)) digits <- 5
	
	# Check the first dimname of psaQuantities. 
	firstDimName <- names(dimnames(psaQuantities))[[1]] 
	treatmentGroups <- dimnames(psaQuantities)[[1]] 
	if (!(firstDimName %in% c("treatmentGroup", "scenario")) ||
			(firstDimName == "treatmentGroup" && 
			!identical(treatmentGroups, c("noScreening", "screening"))) ||
			(firstDimName == "scenario" && 
			!identical(treatmentGroups, c("scenario1", "scenario2"))))
		stop("dimnames(psaQuantities) is illegal:",
			"\nnames(dimnames(psaQuantities))[[1]]=", firstDimName, 
			"\ndimnames(psaQuantities)[[1]]=", treatmentGroups)
	
	# Calculate the means and quantiles of the cost and effectiveness. 
	costAndEffectiveness <- data.frame(mean=numeric(), median=numeric(), 
			ciLower=numeric(), ciUpper=numeric())
	for (treatmentGroup in treatmentGroups) {
		costAndEffectiveness[paste("Cost in", treatmentGroup),] <-
				findMeanAndQuantiles(getCostOrEffectiveness("cost", 
				arrayOfMeans=psaQuantities, treatmentGroup=treatmentGroup, 
				discounted=discounted, qualityAdjusted=FALSE), 
				ciPercent=ciPercent)
		costAndEffectiveness[
				paste("Effectiveness in", treatmentGroup),] <-
				findMeanAndQuantiles(getCostOrEffectiveness("effectiveness", 
				arrayOfMeans=psaQuantities, treatmentGroup=treatmentGroup, 
				discounted=discounted, qualityAdjusted=qualityAdjusted), 
				ciPercent=ciPercent)
	}
	costAndEffectiveness <- costAndEffectiveness[c(1,3,2,4),] 
	
	# Calculate the means and quantiles of the two incremental quantities. 
	incrementalCost <- getIncrementalCostOrEffectiveness("cost", psaQuantities, 
			discounted=discounted, qualityAdjusted=FALSE)
	incrementalEffectiveness <- getIncrementalCostOrEffectiveness(
			"effectiveness", psaQuantities, discounted=discounted, 
			qualityAdjusted=qualityAdjusted)
	incrementalQuantities <- data.frame(mean=numeric(), median=numeric(), 
			ciLower=numeric(), ciUpper=numeric())
	incrementalQuantities["Incremental cost",] <- 
			findMeanAndQuantiles(incrementalCost, ciPercent=ciPercent)
	incrementalQuantities["Incremental effectiveness",] <- 
			findMeanAndQuantiles(incrementalEffectiveness, ciPercent=ciPercent)
	
	# Calculate the mean and quantiles of the net benefit, for each 
	# value of willingnessToPayThreshold.
	netBenefitQuantities <- data.frame(mean=numeric(), median=numeric(), 
			ciLower=numeric(), ciUpper=numeric())
	for (willingnessToPayThreshold in willingnessToPayThresholds) {
		thisRow <- findNetBenefitMeanAndQuantiles(psaQuantities, 
				discounted=discounted, qualityAdjusted=qualityAdjusted, 
				willingnessToPayThreshold=willingnessToPayThreshold, 
				ciPercent=ciPercent)
		rowName <- paste0("Net benefit, lambda=", willingnessToPayThreshold)
		netBenefitQuantities[rowName,] <- thisRow
	}
	
	# Calculate the point-estimate and CI for icer.
	icerQuantiles <- findIcerQuantiles(psaQuantities, discounted=discounted,
			qualityAdjusted=qualityAdjusted, ciPercent=ciPercent)
	
	roundedQuantities <- rbind(costAndEffectiveness, incrementalQuantities, 
			netBenefitQuantities)
	names(roundedQuantities) <- 
			c("       mean", "   median", " ciLower", " ciUpper")
	for (i in 1:nrow(roundedQuantities)) 
		for (j in 1:ncol(roundedQuantities))
			roundedQuantities[i,j] <- 
					roundIfNumeric(roundedQuantities[i, j], digits=digits)
	
	cat("PSA summary quantities, with discounted=", discounted, 
			" and qualityAdjusted=", qualityAdjusted, ":\n", sep="")
	print(round(roundedQuantities, digits=digits)) 
	cat("ICER:                    ")
	for (i in 1:length(icerQuantiles)) 
		cat(names(icerQuantiles)[i], "=", icerQuantiles[i], " ")
	cat("\n")
	
	invisible(list(costAndEffectiveness=costAndEffectiveness,
					incrementalQuantities=incrementalQuantities,
					netBenefitQuantities=netBenefitQuantities,
					icerQuantiles=icerQuantiles))
}

# Do showPsaSummaryQuantities for all possible combinations of the two 
# logicals/booleans "discounted" and "qualityAdjusted".
showAllPsaSummaryQuantities <- function(psaQuantities, digits, ciPercent=95) {
	cat("\n")
	if (ciPercent != 95) 
		cat("The following tables show ", ciPercent, 
				"% confidence intervals.\n\n", sep="")
	for (di in c(TRUE, FALSE))
		for (qu in c(TRUE, FALSE)) {
			showPsaSummaryQuantities(psaQuantities, discounted=di, 
					qualityAdjusted=qu, digits=digits, ciPercent=ciPercent)
			cat("\n")
		}
}

#Plot cumulative numbers of events over time
# This function extracts the counts over time of the FIRST occurence 
# of event in treatmentGroup from x=0 to x=xend by periods of length 
# xaccuracy. If event2 is specified too then the function returns counts 
# over time at which both event and 
# event2 have occurred in an individual.
makeSmallDataFrameForEventsCurve <- function(personsInfo, event, 
                                             event2=NULL, treatmentGroup,
                                             xend=30,xaccuracy=0.01) {
  
  # vector of discrete times from 0 to xend by xaccuracy
  timeVector <- seq(0,xend,by=xaccuracy)
  # counts of events and censorings between timeVector[i] and timeVector[i+1]
  eventVector <- censorVector <- numeric(length(timeVector))
  
  if(class(personsInfo)=="weighted processPersons"){
    
    n.under<-length(personsInfo$resultUnder$eventHistories)
    n.over<-length(personsInfo$resultOver$eventHistories)
    nstar.under<-personsInfo$trueProportionBelowThreshold*(n.under+n.over)
    nstar.over<-(1-personsInfo$trueProportionBelowThreshold)*(n.under+n.over)
    
    # extract event number corresponding to "event" for each individual. 
    eventNumber.under<-sapply(personsInfo$resultUnder$eventHistories,function(i)
	{match(event,i[[treatmentGroup]]$events)})
    eventNumber.over<-sapply(personsInfo$resultOver$eventHistories,function(i)
	{match(event,i[[treatmentGroup]]$events)})
    # extract event times for those who have the event. 
    eventTimes.under<-sapply(1:length(personsInfo$resultUnder$eventHistories),function(i)
	{personsInfo$resultUnder$eventHistories[[i]][[treatmentGroup]]$times[eventNumber.under[i]]})
    eventTimes.over<-sapply(1:length(personsInfo$resultOver$eventHistories),function(i)
	{personsInfo$resultOver$eventHistories[[i]][[treatmentGroup]]$times[eventNumber.over[i]]})
    # If event2!=NULL extract event number corresponding to "event2" for each individual. 
    if(!is.null(event2)){
      event2Number.under<-sapply(personsInfo$resultUnder$eventHistories,function(i)
	  {match(event2,i[[treatmentGroup]]$events)})
      event2Number.over<-sapply(personsInfo$resultOver$eventHistories,function(i)
	  {match(event2,i[[treatmentGroup]]$events)})
      # extract event times for those who have event2. 
      event2Times.under<-sapply(1:length(personsInfo$resultUnder$eventHistories),function(i)
	  {personsInfo$resultUnder$eventHistories[[i]][[treatmentGroup]]$times[event2Number.under[i]]})
      event2Times.over<-sapply(1:length(personsInfo$resultOver$eventHistories),function(i)
	  {personsInfo$resultOver$eventHistories[[i]][[treatmentGroup]]$times[event2Number.over[i]]})
    }
    # extract censoring times for each individual, where persons are censored 
    # at xend if they survive beyond this time
    censorTimes.under<-sapply(personsInfo$resultUnder$eventHistories,function(i)
	{min(max(i[[treatmentGroup]]$times),xend)})
    censorTimes.over<-sapply(personsInfo$resultOver$eventHistories,function(i)
	{min(max(i[[treatmentGroup]]$times),xend)})
    if(is.null(event2)){
      delta.under<-!is.na(eventTimes.under) & eventTimes.under<=censorTimes.under   
      delta.over<-!is.na(eventTimes.over) & eventTimes.over<=censorTimes.over   
    } else {

      delta.under<-!is.na(eventTimes.under) & !is.na(event2Times.under) & 
        eventTimes.under<=censorTimes.under & 
	  event2Times.under<=censorTimes.under
      delta.over<-!is.na(eventTimes.over) & !is.na(event2Times.over) & 
        eventTimes.over<=censorTimes.over & 
	  event2Times.over<=censorTimes.over
    }
    
    # time either censoring time if delta==0 or event time if delta==1
    if(is.null(event2)){
      time.under <- ifelse(!delta.under,censorTimes.under,eventTimes.under)
      time.over <- ifelse(!delta.over,censorTimes.over,eventTimes.over)
    } else {
      time.under <- ifelse(!delta.under,censorTimes.under,pmax(eventTimes.under,event2Times.under))
      time.over <- ifelse(!delta.over,censorTimes.over,pmax(eventTimes.over,event2Times.over))
    }

    eventVector[as.numeric(names(table(findInterval(time.under[delta.under],timeVector))))]<-
	table(findInterval(time.under[delta.under],timeVector))/n.under*nstar.under
    eventVector[as.numeric(names(table(findInterval(time.over[delta.over],timeVector))))]<-
	eventVector[as.numeric(names(table(findInterval(time.over[delta.over],timeVector))))]+
      table(findInterval(time.over[delta.over],timeVector))/n.over*nstar.over
    censorVector[as.numeric(names(table(findInterval(time.under[!delta.under],timeVector))))]<-
	table(findInterval(time.under[!delta.under],timeVector))/n.under*nstar.under
    censorVector[as.numeric(names(table(findInterval(time.over[!delta.over],timeVector))))]<-
	censorVector[as.numeric(names(table(findInterval(time.over[!delta.over],timeVector))))]+
      table(findInterval(time.over[!delta.over],timeVector))/n.over*nstar.over
    
  } else {
    # extract event number corresponding to "event" for each individual. 
    eventNumber<-sapply(personsInfo$eventHistories,function(i){match(event,i[[treatmentGroup]]$events)})

    eventTimes<-sapply(1:length(personsInfo$eventHistories),function(i)
	{personsInfo$eventHistories[[i]][[treatmentGroup]]$times[eventNumber[i]]})
    # If event2!=NULL extract event number corresponding to "event2" for each individual. 
    if(!is.null(event2)){
      event2Number<-sapply(personsInfo$eventHistories,function(i){match(event2,i[[treatmentGroup]]$events)})

      event2Times<-sapply(1:length(personsInfo$eventHistories),function(i)
	  {personsInfo$eventHistories[[i]][[treatmentGroup]]$times[event2Number[i]]})
    }
    # extract censoring times for each individual, where persons are censored 
    # at xend if they survive beyond this time
    censorTimes<-sapply(personsInfo$eventHistories,function(i){min(max(i[[treatmentGroup]]$times),xend)})
    # event indicator. TRUE = event before censorTimes, FALSE=censoring
    if(is.null(event2)){
      delta<-!is.na(eventTimes) & eventTimes<=censorTimes   
    } else {

      delta<-!is.na(eventTimes) & !is.na(event2Times) & eventTimes<=censorTimes & 
        event2Times<=censorTimes
    }
    
    # time either censoring time if delta==0 or event time if delta==1
    if(is.null(event2)){
      time <- ifelse(!delta,censorTimes,eventTimes)
    } else {
      time <- ifelse(!delta,censorTimes,pmax(eventTimes,event2Times))
    }
    

    eventVector[as.numeric(names(table(findInterval(time[delta],timeVector))))]<-
	table(findInterval(time[delta],timeVector))
    censorVector[as.numeric(names(table(findInterval(time[!delta],timeVector))))]<-
	table(findInterval(time[!delta],timeVector))
  }
  return(data.frame(time=timeVector, event=eventVector,censor=censorVector))
}


# A function for plotting cumulative number of events, including confidence intervals 
plotCumulativeEvents <- function(data, xMax, yMax, periodLength=0.25, 
                                 drawCIs=TRUE, rightAxis=FALSE, add=FALSE, main="", 
                                 numberOfPersons=NA, 
                                 ...) {

  # Check arguments. 
  if (!is.data.frame(data))
    stop("data must be a data-frame")
  dataIsTimeAndEvent <- identical(names(data), c("time", "event"))
  dataIsNumbersToPlot <- identical(names(data), c("x", "y", "ci1", "ci2"))
  if (!dataIsTimeAndEvent && !dataIsNumbersToPlot)
    stop("data columns must be either time, event or x, y, ci1, ci2")
  if (add && (!missing(xMax) || !missing(yMax)))
    stop("xMax and yMax must not be specified if add=TRUE")
  if (dataIsNumbersToPlot && rightAxis && is.na(numberOfPersons))
    stop("numberOfPersons must be given if dataIsNumbersToPlot and rightAxis")
  if (dataIsTimeAndEvent && !is.na(numberOfPersons))
    stop("numberOfPersons must not be given if dataIsTimeAndEvent")
  
  if (dataIsTimeAndEvent) {
    eventTimes <- data$time[data$event==1]
    x <- seq(0, max(data$time) + periodLength, periodLength)
    nEventsInEachPeriod <- hist(eventTimes, breaks=x, plot=FALSE)$counts
    cumNumberOfEvents <- c(0, cumsum(nEventsInEachPeriod))
    y <- cumNumberOfEvents / nrow(data)
  } else {
    x <- data$x 
    y <- data$y / numberOfPersons
  }
  
  if (drawCIs) {
    ci <- matrix(nrow=length(x), ncol=2)
    for (i in 1:length(x)) {
      if (dataIsTimeAndEvent) {
        ci[i,] <- poisson.test(cumNumberOfEvents[i], 
                               conf.level=0.95)$conf.int / nrow(data)
      } else {
        ci[i,] <- c(data$ci1[i], data$ci2[i]) / numberOfPersons
      }
    }
  } 
  if (missing(yMax)) 
    yMax <- max({ if(drawCIs) ci else y }) * 1.1
  if (missing(xMax)) xMax <- max(x)
  
  if (!add) 
    plot(NA, xlim=c(0,xMax), ylim=c(0,yMax), xaxs="i", yaxs="i",...)
  if (drawCIs)
    polygon(x=c(x,rev(x)), y=c(ci[,1],rev(ci[,2])), col="grey85", border=NA)
  abline(v=pretty(x=range(x),n=10), h=pretty(x=c(0,yMax),n=10), lty="dotted")
  lines(x, y, ...)
  
  if (rightAxis) {
    if (dataIsTimeAndEvent) numberOfPersons <- nrow(data)
    tickmarkLabels <- pretty(x=c(0, yMax * numberOfPersons), n=10)
    tickmarkPositions <- tickmarkLabels / numberOfPersons
    axis(side=4, labels=tickmarkLabels, at=tickmarkPositions,...)
  }
}

# Table of counts for men 4 year model, including MASS values
TableOfCounts_men4years <- function(result, v1other) {
  # MASS value.
  rateFactor <- 1e5  # rates are per 100,000
  numberOfPersonsInOurSimulation <- length(result$eventHistories)
  {massFourYearNumbers <- c(
    noSc_electiveSurgeryOpen=100,
    noSc_emergencySurgeryOpen=62,
    noSc_ruptures=138,
    noSc_contraindications=NA,
    noSc_aaaDeaths=113,
    noSc_nonAaaDeaths=3750,
    
    scre_electiveSurgeryOpenScreenDet=295,
    scre_electiveSurgeryOpenIncidentallyDet=31,
    scre_emergencySurgeryOpen=28,
    scre_ruptures=66,
    scre_contraindicationsScreenDet=41,
    scre_contraindicationsIncidentallyDet=NA,
    scre_dropout=290, 
    scre_aaaDeaths=65,
    scre_nonAaaDeaths=3694
  )}
  comparisons <- data.frame(matrix(nrow=length(massFourYearNumbers), ncol=0))
  rownames(comparisons) <- names(massFourYearNumbers)
  comparisons$mass <- massFourYearNumbers
  comparisons$massRate <- NA
  for (rowName in rownames(comparisons)) {
    if (grepl("^scre_", rowName)) {
      groupSize <- 33839
    } else { 
      groupSize <- 33961
    }
    comparisons[rowName, "massRate"] <- 
      comparisons[rowName, "mass"] / groupSize * rateFactor
  }
  
  # Simulation values. 
  comparisons$ourSimulation <- countEventsFromOurSimulation(result, v1other)
  
  # Simulations rates 
  comparisons$ourSimulationRate <- 
    comparisons$ourSimulation / numberOfPersonsInOurSimulation * rateFactor
  comparisons$ratioAsPercent <- 
    comparisons$ourSimulationRate / comparisons$massRate * 100
  return(comparisons)
}

## Function to create events plot data.frame for both no screened and screened group (using sampling over the threshold)
eventsData <- function(personsInfo, events = "all"){
  
  treatmentGroups <- names(personsInfo$eventHistories[[1]])
  
  n <- length(personsInfo$eventHistories)
  
  data<-data.frame(person=NULL, time=NULL, event=NULL, AAASize=NULL, treatmentGroup=NULL)
  
  ## default is to create using all possible events recorded 
  if(events=="all") {
    events <- c("inviteToScreen","screen","requireReinvitation",
                "failToAttendScreen", "nonvisualization","monitor", 
                "dropout","incidentalDetection","consultation",
                "decideOnElectiveSurgery","decideOnReturnToMonitoring", 
                "contraindicated", "monitorFollowingContraindication",
                "electiveSurgeryEvar","electiveSurgeryOpen",
                "rupture","emergencySurgeryEvar","emergencySurgeryOpen",
                "monitorFollowingEvarSurgery","monitorFollowingOpenSurgery",
                "reinterventionAfterElectiveEvar", 
                "reinterventionAfterEmergencyEvar", "reinterventionAfterEmergencyOpen",
                "aaaDeath","nonAaaDeath","censored")
  }
  
  for(t in treatmentGroups){   ## Loop over treatmentGroups
    
    b0 <- sapply(personsInfo$eventHistories,function(i){i[[t]]$b0})
    b1 <- sapply(personsInfo$eventHistories,function(i){i[[t]]$b1})
    
    for(event in events){
      # extract event number corresponding to "event" for each individual. NA means the event does not occur
      eventNumber<-sapply(personsInfo$eventHistories,function(i){match(event,i[[t]]$events)})
      ## extract event times for those who have the event. NA mean the event does not occur
      eventTimes<-sapply(1:n,function(i){personsInfo$eventHistories[[i]][[t]]$times[eventNumber[i]]})
      ## extract AAA sizes at event times
      eventAAASize<-exp(b0 + b1*eventTimes)
      
      notMissing <- !is.na(eventTimes)
      
      if(any(notMissing)){
        data<-rbind(data,data.frame(person = (1:n)[notMissing], time = eventTimes[notMissing], event=event,
                                    AAASize = eventAAASize[notMissing], treatmentGroup = t))
      }
    }
  }
  return(data=data)
}

## Function to plot a 2d scatter plot of event against AAA diameter
eventsPlot<-function(data,event,v1other){
  data<-data[data$event==event,]
  
  xlim<-c(0,max(data$time)+5)
  ylim<-c(min(data$AAASize)-1,max(data$AAASize)+1)
  diag.thresh<-ifelse(!is.list(v1other$aortaDiameterThresholds), 
         v1other$aortaDiameterThresholds[1],
         v1other$aortaDiameterThresholds[[1]][1])
  
  inter.thresh<-ifelse(!is.list(v1other$aortaDiameterThresholds), 
                       v1other$aortaDiameterThresholds[length(v1other$aortaDiameterThresholds)],
                       v1other$aortaDiameterThresholds[[1]][length(v1other$aortaDiameterThresholds[[1]])])
                       
  # ggplot(data,aes(x=time,y=AAASize))+
  #   stat_density2d(aes(alpha=..level..), geom="polygon")+facet_wrap(~treatmentGroup)+
  #   scale_alpha_continuous(limits=c(0,0.2),breaks=seq(0,0.2,by=0.025))+
  #   geom_point(colour="red",alpha=0.3)+
  #   lims(x = xlim,y = ylim)+
  #   geom_hline(yintercept=c(diag.thresh,inter.thresh),linetype=2)+
  #   xlab("Time since screening (years)")+
  #   ylab("Aorta size (cm)")+guides(alpha=guide_legend(title="Density"))
  
  ggplot(data,aes(x=time,y=AAASize))+
    stat_density2d(aes(alpha=..level..,fill=..level..),geom="polygon")+facet_wrap(~treatmentGroup)+
    scale_alpha_continuous(limits=c(0,0.2),breaks=seq(0,0.2,by=0.025))+
    geom_point(colour="red",alpha=0.3)+
    lims(x = xlim,y = ylim)+
    geom_hline(yintercept=c(diag.thresh,inter.thresh),linetype=2)+
    xlab("Time since screening (years)")+
    ylab("Aorta size (cm)")+guides(alpha=guide_legend(title="Density"))+scale_fill_viridis_c()
  
} 
