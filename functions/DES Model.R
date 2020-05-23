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
## extraInputs -- A list of any elements of v0, v1other, v1distributions and v2 that the user wishes to change from the R command line


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
                    extraInputs = list(v0 = list(), v1other = list(), v2 = list(), v1distributions = list())){
  v0 <- compactList() 
  v1distributions <- compactList() 
  v1other <- compactList()
  v2 <- compactList()

  ## Number of persons and number of PSA iterations
  v0$numberOfPersons <- n
  v0$numberOfParameterIterations <- nPSA
  
  ## Input Data for the DES model
  require(readxl)
  require(tibble)
  
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
    v1other$baselineDiameters <- read_excel(dataFile, sheet = "AAA Size Distribution",  
                                            col_names = T, skip = 4)
    names(v1other$baselineDiameters) <- c("size", "weight")
    v1other$baselineDiameters <- subset(v1other$baselineDiameters, !is.na(v1other$baselineDiameters$size))
    
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

    qalys <- createQalyFactors(
      startAge=v1other$startAge,
      qalyFactorBoundariesAsAges = qalyFactorBoundariesAsAges,
      qalyFactorsForAges = qalyFactorsForAges
    )
    v2$qalyFactors <- setType(qalys$qalyFactors, "qaly")
    v1other$qalyFactorBoundaries <- qalys$qalyFactorBoundaries
    
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
    
    ## Assign last monitoring interval input to v1other$monitoringIntervalFollowingContraindication and remove it from v1other$monitoringIntervals
    v1other$monitoringIntervalFollowingContraindication <- v1other$monitoringIntervals[length(v1other$monitoringIntervals)]
    v1other$monitoringIntervals <- v1other$monitoringIntervals[-length(v1other$monitoringIntervals)]
    
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
      
      ## Run model once using point estimates
      res<-processPersonsControlOnly(v0, v1other, v2)
      res.sampled<-processPersonsAboveDiagnosisThreshold(v0, v1other, v2, 
                                                         threshold=v1other$aortaDiameterThresholds[1])
      result <- list(resultsControl = res, resultsIncremental = res.sampled)
      result$meanQuantities <- rbind(res$meanQuantities, screening = NA, difference = NA)
      result$meanQuantities["screening",] <- res$meanQuantities["noScreening",] + res.sampled$incrementalMeanQuantities
      result$meanQuantities["difference",] <- res.sampled$incrementalMeanQuantities
    } else {
      result <- processPersons(v0, v1other, v2)  
    }
    return(result)
  } else {
    ## Run PSA
    #result <- psa(v0, v1other, v1distributions)
    result <- psaAboveDiagnosisThreshold(v0, v1other, v1distributions, 
                               threshold = v1other$aortaDiameterThresholds[1])
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
processPersons <- function(v0, v1other, v2) {
	
	
	suppressWarnings(suppressMessages(require(doParallel)))
	
	# Set unspecified elements of v0 to default values, if necessary. 
	v0 <- setUnspecifiedElementsOfv0(v0)
	
	# Set unspecified elements of v1other to default values
	v1other <- setUnspecifiedElementsOfv1other(v1other)
	
	# Check the arguments.
	checkArgs(v0=v0, v1other=v1other, v2=v2)
	
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
		cat("########################\n\n")
	}
	
	# Create v1other$nonAaaSurvProbs or nonAaaMortalityRates. 
	if (v1other$nonAaaDeathMethod == "mass") {
		v1other$nonAaaSurvProbs <- getMassSurvivalProbabilities()
	} else if (v1other$nonAaaDeathMethod == "onsIntegerStart") { 
		v1other$nonAaaSurvProbs <- convertMortalityRatesToSurvProbs(
				v1other$startAge, v1other$nonAaaMortalityRatesFileName)
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
	
	# Set v1other$thresholdForIncidentalDetection to 
	# v1other$aortaDiameterThresholds[1], if the former was not set. 
	if (!("thresholdForIncidentalDetection" %in% names(v1other))) {
		v1other$thresholdForIncidentalDetection <- 
				v1other$aortaDiameterThresholds[1]
		if (v0$verbose) cat("v1other$thresholdForIncidentalDetection was not ",
				"provided and so\n has been set to ",
				"v1other$aortaDiameterThresholds[1]=", 
				v1other$aortaDiameterThresholds[1], ".\n", sep="")
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
		setAndShowRandomSeed(v0$randomSeed, verbose=v0$verbose)
		resultForEachPerson <- lapply(X=1:v0$numberOfPersons, 
				FUN=processOnePair, v0, v1other, v2)
		
	} else if (v0$method == "parallel") {
		cluster <- makeCluster(v0$numberOfProcesses)  
		clusterExport(cluster, getAllFunctionsAndStringsInGlobalEnv())
		setAndShowRandomSeed(randomSeed=v0$randomSeed, cluster=cluster, 
				verbose=v0$verbose)
		resultForEachPerson <- parLapply(cl=cluster, X=1:v0$numberOfPersons,
				fun=processOnePair, v0, v1other, v2)
		stopCluster(cluster) 
		
	} else if (v0$method == "foreach") {
		if (!is.null(v0$randomSeed)) 
			stop("v0$randomSeed does not yet work with v0$method=\"foreach\"")
		registerDoParallel(cores=v0$numberOfProcesses)
		resultForEachPerson <- foreach(personNumber=1:v0$numberOfPersons,
				.export=getAllFunctionsAndStringsInGlobalEnv()) %dopar% {
			processOnePair(personNumber, v0, v1other, v2) 
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
		threshold=3.0) {
	
  # Set unspecified elements of v0 to default values, if necessary. 
  v0 <- setUnspecifiedElementsOfv0(v0)
  
  # Set unspecified elements of v1other to default values
  v1other <- setUnspecifiedElementsOfv1other(v1other)
  
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
	resultOver <- processPersons(v0over, v1otherOver, v2)
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
# 2) processOnePair 
# Generate and analyze one pair of twins, of which one gets the "screening" treatment 
# and one gets "noScreening". This is run in parallel by processPersons. 
# This returns a result, which contains:
#   result$personQuantities$screening
#   result$personQuantities$noScreening
#   result$eventHistories$screening
#   result$eventHistories$noScreening
# (the last two only if v0$returnEventHistories is TRUE). 
################################################################################
processOnePair <- function(personNumber, v0, v1other, v2) {

	# Check the arguments.
	if (!("namesOfQuantities" %in% names(v0)))
		stop("processOnePair needs v0$namesOfQuantities to exist;",
				"\nthis is normally done by processPersons or psa")
	if (v1other$nonAaaDeathMethod %in% c("mass", "onsIntegerStart") && 
			!("nonAaaSurvProbs" %in% names(v1other)))
		stop("when v1other$nonAaaDeathMethod is \"", v1other$nonAaaDeathMethod, "\", ",
				"processOnePair needs \nv1other$nonAaaSurvProbs ",
				"to exist; this is normally done by processPersons")
	if (v1other$nonAaaDeathMethod == "onsNonintegerStart" &&
			!("nonAaaMortalityRates" %in% names(v1other)))
		stop("when v1other$nonAaaDeathMethod is , \"onsNonintegerStart\", ",
				"processOnePair needs \nv1other$nonAaaMortalityRates ",
				"to exist; this is normally done by processPersons")
	
	# Make result, which will be returned from this function. 
	result <- list(personQuantities=
			sapply(v0$treatmentGroups, function(x) NULL)) 
	if (v0$returnEventHistories) result$eventHistories <- 
			sapply(v0$treatmentGroups, function(x) NULL)
	
	# Generate characteristics and natural events. 
	aortaGrowthParameters <- generatePersonAortaParameters(v1other, v2)
	v3 <- compactList(
		# Characteristics:
		b0=aortaGrowthParameters$b0, 
		b1=aortaGrowthParameters$b1,
		# Natural events:
		ruptureTime=aortaGrowthParameters$ruptureTime,
		nonAaaDeathTime=generateTimeTillNonAaaDeath(v0, v1other),

		# Initial diameter as measured:
		initialAortaSizeAsMeasured=
		aortaGrowthParameters$initialAortaSizeAsMeasured,
		
		# A large vector of propensities (probabilities) to be used each time an observed AAA size is required
		# This reduces MC error when comparing the two treatment groups (pairs are more closely cloned)
		aortaSize = setType(runif(300), "propensity")
	)
	
	# Generate boolean variables (v3) for v2 elements of type "probability". 
	for (elementName in sort(names(v2))) {
	  ## Making changes here so that Uniform RV is generated for each pair to represent propensity of each event occuring
	  ## This reduces MC error in event generation

	  #if (elementName %in% namesOfProbVarsForSurvivalModel) { ## Not sure why this was needed
	  v2element <- v2[[elementName]]
	  
	  if (getType(v2element) == "logistic model for probability"){
	    v3[[elementName]] <- setType(runif(1), "propensity")
	    next
	  } 
	  
	  ## Generate propensities for each rate event (e.g. rateOfAaaDeathAfterElectiveEvarSurgeryAndInitialPeriod)
	  if (getType(v2element) == "rate"){
	    if(!(elementName %in% c("rateOfIncidentalDetection", "rateOfDropoutFromMonitoring"))){
	      v3[[elementName]] <- setType(runif(1), "propensity")
	      next
	    } else {
	      ## To allow multiple events to take place in a life-time simulate multiple (e.g. 50 propensities) for events such as incidental detection and dropout
	      v3[[elementName]] <- setType(runif(50), "propensity")
	      next
	    }
	  }
	  
	  if (getType(v2element) == "reintervention rates"){
	    ## Only set propensities for reintervention rates once, as only one of the following can occur
	    ## ElectiveEvar, ElectiveOpen, EmergencyEvar, EmergencyOpen
	    if(is.null(v3[["reintervention rates"]])){
	      v3[["rateOfReintervention"]] <- setType(runif(50), "propensity")
	    }
	  }
	  
	  if (getType(v2element) == "probability")
	    v3[[elementName]] <- setType(rbernoulli(v2element), "boolean")
	}

	# Generate censoring-time.
	if ("generateCensoringTime" %in% names(v0)) {
		v3$censoringTime <- v0$generateCensoringTime()
	}
	
	# Put the person through the different treatments. 
	for (treatmentGroup in v0$treatmentGroups) {
		
		# Create full list of events. 
		eventHistory <- 
				generateEventHistory(v0, v1other, v2, v3, treatmentGroup)
		
		# If showEventHistories, then display the person's event-history.
		if (v0$showEventHistories) {
			varNames <- c("personNumber", "treatmentGroup", "v3$b0", 
					"v3$b1", "v3$ruptureTime", "v3$nonAaaDeathTime")
			for (varName in varNames) {
				var <- getAnything(varName)
				if (varName != "personNumber" && is.numeric(var)) 
					var <- sprintf("%.2f", var)
				cat(varName, "=", var, "  ", sep="")
			}
			cat("\n")
			print(eventHistory)
			cat("\n")
		}
		
		# If returnEventHistories, then store the event-history. 
		if (v0$returnEventHistories) 
			result$eventHistories[[treatmentGroup]] <- eventHistory	
		# Store individual's b0 and b1. 
      result$eventHistories[[treatmentGroup]]$b0 <- v3$b0
      result$eventHistories[[treatmentGroup]]$b1 <- v3$b1
      result$eventHistories[[treatmentGroup]]$initialAortaSizeAsMeasured <- v3$initialAortaSizeAsMeasured
		# From eventHistory, calculate the health-economic quantities.
		result$personQuantities[[treatmentGroup]] <- 
				calculateHealthEconomicQuantities(
				eventHistory, v0$namesOfQuantities, v2$costs, v1other, v2)
	}
	
	return(result)
}

################################################################################
################################################################################
# processPersonsControlOnly - generate a set of persons for CONTROL GROUP ONLY and analyze them. 

################################################################################
processPersonsControlOnly <- function(v0, v1other, v2, updateProgress=NULL) {
  
  
  cat("processPersons\n")
  cat("probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgery:\n")
  print(v2$probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgery)
  
  # Load packages, etc.
  suppressWarnings(suppressMessages(require(doParallel)))
  # suppressMessages is for the Revolution R message and suppressWarnings 
  # is for the warning that "foreach" was built under R version something.
  # doParallel loads parallel and foreach.
  
  # Set unspecified elements of v0 to default values, if necessary. 
  v0 <- setUnspecifiedElementsOfv0(v0)
  
  # Set unspecified elements of v1other to default values
  v1other <- setUnspecifiedElementsOfv1other(v1other)
  
  # Check the arguments.
  checkArgs(v0=v0, v1other=v1other, v2=v2)
  
  # Display some messages. 
  if (v0$verbose) {
    cat("Running processPersons on ", Sys.info()["nodename"], 
        " with:\n  numberOfPersons=", v0$numberOfPersons, 
        ", method=", v0$method, { if(v0$method=="serial") "" else paste0(
          ", numberOfProcesses=", v0$numberOfProcesses) }, "\n", sep="")
    if ("generateCensoringTime" %in% names(v0))
      cat("Censoring is being used, so life-years etc. will be calculated",
          "up to censoring times.\n")
    # Display all of v0, v1other, and v2 (so that the 
    # input pars appear in the same file as the output and you can easily 
    # see what analysis was done):
    cat("\n########## v0 ##########\n")
    print(v0)
    cat("########## v1other ##########\n")
    print(v1other)
    cat("########## v2 ##########\n")
    print(v2)
    cat("########################\n\n")
  }
  
  # Create v1other$nonAaaSurvProbs or nonAaaMortalityRates. 
  # (See also checkArgs.R.)
  if (v1other$nonAaaDeathMethod == "mass") {
    v1other$nonAaaSurvProbs <- getMassSurvivalProbabilities()
  } else if (v1other$nonAaaDeathMethod == "onsIntegerStart") { 
    v1other$nonAaaSurvProbs <- convertMortalityRatesToSurvProbs(
      v1other$startAge, v1other$nonAaaMortalityRatesFileName)
  } else if (v1other$nonAaaDeathMethod == "onsNonintegerStart") {
    v1other$nonAaaMortalityRates <- 
      readMortalityRatesFromFile(v1other$nonAaaMortalityRatesFileName)
  } else {
    stop("v1other$nonAaaDeathMethod=", v1other$nonAaaDeathMethod, 
         " is illegal")
  }
  
  # Copy v2$sigmaW into v2$ultrasoundMeasurementErrorSD.
  # This removes the need for v2$ultrasoundMeasurementErrorSD to be defined 
  # in the "input" files and also ensures that it has the appropriate 
  # distribution in PSA, i.e. the same value as v2$sigmaW. 
  v2$ultrasoundMeasurementErrorSD <- v2$sigmaW
  
  # Change the prevalence, if v2$prevalence exists.
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
  # NB if the user specifies v2$prevalence, then changePrevalence is done 
  # using threshold=v1other$aortaDiameterThresholds[1], not
  # v1other$thresholdForIncidentalDetection. (For the exact meaning of 
  # "threshold" see changePrevalence, though it is fairly obvious).
  # If you think that changePrevalence should be done using threshold=
  # v1other$thresholdForIncidentalDetection, or if you think that the user should 
  # be allowed or forced to specify threshold when they specify 
  # v2$prevalence, then the code above and elsewhere will need to change. 
  
  # Set v1other$thresholdForIncidentalDetection to 
  # v1other$aortaDiameterThresholds[1], if the former was not set. 
  if (!("thresholdForIncidentalDetection" %in% names(v1other))) {
    v1other$thresholdForIncidentalDetection <- 
      v1other$aortaDiameterThresholds[1]
    if (v0$verbose) cat("v1other$thresholdForIncidentalDetection was not ",
                        "provided and so\n has been set to ",
                        "v1other$aortaDiameterThresholds[1]=", 
                        v1other$aortaDiameterThresholds[1], ".\n", sep="")
  }
  
  # Make a list to store the output in. 
  result <- list()
  if (v0$returnMeanQuantities)
    result$meanQuantities <- NA
  if (v0$returnEventHistories) result$eventHistories <- 
    lapply(X=1:v0$numberOfPersons, FUN=function(x) list())
  if (v0$returnAllPersonsQuantities) result$allPersonsQuantities <- 
    lapply(X=1:v0$numberOfPersons, FUN=function(x) list())
  # The lapply lines each make a list of length numberOfPersons, in which 
  # each element is an empty list.
  
  # Create and analyze the persons. Pass all variables to processOnePair 
  # as arguments, and for parallel methods make functions available to that 
  # function by exporting them explicitly. 
  if (v0$method == "serial") {
    # Do it using lapply. 
    setAndShowRandomSeed(v0$randomSeed, verbose=v0$verbose)
    v0$treatmentGroups <- "noScreening"
    resultForEachPerson <- lapply(X=1:v0$numberOfPersons, 
                                  FUN=processOnePair, v0, v1other, v2)
    
  } else if (v0$method == "parallel") {
    # Do it using parLapply. 
    v0$treatmentGroups <- "noScreening"
    cluster <- makeCluster(v0$numberOfProcesses)  # previously: outfile=""
    clusterExport(cluster, getAllFunctionsAndStringsInGlobalEnv())
    setAndShowRandomSeed(randomSeed=v0$randomSeed, cluster=cluster, 
                         verbose=v0$verbose)
    resultForEachPerson <- parLapply(cl=cluster, X=1:v0$numberOfPersons,
                                     fun=processOnePair, v0, v1other, v2)
    stopCluster(cluster) 
    
  } else if (v0$method == "foreach") {
    v0$treatmentGroups <- "noScreening"
    if (!is.null(v0$randomSeed)) 
      stop("v0$randomSeed does not yet work with v0$method=\"foreach\"")
    # Do it using foreach and %dopar%. 
    registerDoParallel(cores=v0$numberOfProcesses)
    resultForEachPerson <- foreach(personNumber=1:v0$numberOfPersons,
                                   .export=getAllFunctionsAndStringsInGlobalEnv()) %dopar% {
                                     processOnePair(personNumber, v0, v1other, v2) 
                                   }
    stopImplicitCluster()  # (seems unreliable)	
  }  else {
    stop("v0$method=", v0$method, " is illegal")
  }
  ## REMOVED PROCESS BATCH OF PERSONS
  
  # Get event-histories and person-specific quantities from 
  # resultForEachPerson and put it in result, if required. The lines with 
  # "<-" both get the information for both treatment-groups. 
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
    for (treatmentGroup in "noScreening") {
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
# Generate and analyze a batch of persons. 
# The idea of this function is that it might make the parallel computation 
# faster. 

# processBatchOfPersonsControlOnly <- function(
#   batchNumber, v0, v1other, v2, batchSizes, updateProgress, cluster) {
#   # If we were passed a progress update function, call it
#   if (is.function(updateProgress)) {
#     text <- "\n Calculating absolute event numbers in not invited group"
#     num <-batchNumber/(length(batchSizes))
#     updateProgress(value=num,detail = text)
#   }
#   numberOfPersonsInThisBatch <- batchSizes[batchNumber]
#   parLapply(cl=cluster, X=1:numberOfPersonsInThisBatch, fun=processControlOnly, 
#             v0, v1other, v2,updateProgress=NULL)
# }

################################################################################
################################################################################
# processControlOnly
# used in Shiny app to get events for control group only
# events for intervention group will be obtained from the differences

# processControlOnly <- function(personNumber, v0, v1other, v2, updateProgress) {
#   # personNumber is not used (except if showEventHistories). It is just 
#   # needed because lapply and parLapply force you to pass the number into 
#   # the function that they call. 
#   
#   # If we were passed a progress update function, call it
#   if (is.function(updateProgress)) {
#     text <- "Calculating absolute event numbers in not invited group"
#     num <-personNumber/(2*v0$numberOfPersons)
#     updateProgress(value=num,detail = text)
#   }
#   
#   
#   # Check things. 
#   if (!("namesOfQuantities" %in% names(v0)))
#     stop("processOnePair needs v0$namesOfQuantities to exist;",
#          "\nthis is normally done by processPersons or psa")
#   if (v1other$nonAaaDeathMethod %in% c("mass", "onsIntegerStart") && 
#       !("nonAaaSurvProbs" %in% names(v1other)))
#     stop("when v1other$nonAaaDeathMethod is \"", v1other$nonAaaDeathMethod, "\", ",
#          "processOnePair needs \nv1other$nonAaaSurvProbs ",
#          "to exist; this is normally done by processPersons")
#   if (v1other$nonAaaDeathMethod == "onsNonintegerStart" &&
#       !("nonAaaMortalityRates" %in% names(v1other)))
#     stop("when v1other$nonAaaDeathMethod is , \"onsNonintegerStart\", ",
#          "processOnePair needs \nv1other$nonAaaMortalityRates ",
#          "to exist; this is normally done by processPersons")
#   
#   # Make result, which will be returned from this function. 
#   result <- list(personQuantities=
#                    sapply(v0$treatmentGroups, function(x) NULL)) 
#   if (v0$returnEventHistories) result$eventHistories <- 
#     sapply(v0$treatmentGroups, function(x) NULL)
#   
#   # Generate this person's characteristics and natural events. 
#   aortaGrowthParameters <- generatePersonAortaParameters(v1other, v2)
#   v3 <- compactList(
#     # Characteristics:
#     b0=aortaGrowthParameters$b0, 
#     b1=aortaGrowthParameters$b1,
#     # Natural events:
#     ruptureTime=aortaGrowthParameters$ruptureTime,
#     nonAaaDeathTime=generateTimeTillNonAaaDeath(v0, v1other),
#     
#     # Initial diameter as measured (this is "y0" in 
#     # generatePersonAortaParameters and is only used if they attend 
#     # screening and do not have non-visualization):
#     initialAortaSizeAsMeasured=
#       aortaGrowthParameters$initialAortaSizeAsMeasured
#   )
#   
#   
#   
#   # Generate this person's boolean variables such as requireReinvitation and 
#   # decideOnElectiveSurgery. Go through the elements of v2 and for those that 
#   # have type "probability", generate a boolean element of v3. (If it has 
#   # type "logistic model for probability" then deal with it elsewhere. 
#   ## Making changes here so that Uniform RV is generated for each pair to represent propensity of event occuring
#   ## Only need to do this for logistic regression events (as these depend on age at event so cannot generate Binomial RV at this stage)
#   ## This reduces MC error in event generation
#   #if (elementName %in% namesOfProbVarsForSurvivalModel) { ## Not sure why this was needed
#   
#   for (elementName in sort(names(v2))) {
#     v2element <- v2[[elementName]]
#     if (getType(v2element) == "logistic model for probability") {
#       v3[[elementName]] <- setType(runif(1), "propensity")
#       next
#     }
#     
#     if (getType(v2element) == "probability")
#       v3[[elementName]] <- setType(rbernoulli(v2element), "boolean")
#   }
#   
#   # Generate this person's censoring-time, if v0$generateCensoringTime exists.
#   if ("generateCensoringTime" %in% names(v0)) {
#     v3$censoringTime <- v0$generateCensoringTime()
#   }
#   
#   # Put the person through the different treatments. 
#   for (treatmentGroup in "noScreening") {
#     
#     # Create this person's full list of events. Record everything that will
#     # be necessary for calculating this person's costs, QALYs, etc. The 
#     # possible event-types can be got from the big flowchart for the 
#     # previous model. (Previously, generateEventHistory's last argument was 
#     # v4 = list(treatmentGroup=treatmentGroup), but that was pointless.) 
#     eventHistory <- 
#       generateEventHistory(v0, v1other, v2, v3, treatmentGroup)
#     
#     # If showEventHistories, then display the person's event-history.
#     # This was mostly for the early stages of development or debugging.
#     # If you want to show their age at baseline as well, change the line 
#     # "nonAaaDeathTime=...". At present, initial age is not stored. 
#     if (v0$showEventHistories) {
#       varNames <- c("personNumber", "treatmentGroup", "v3$b0", 
#                     "v3$b1", "v3$ruptureTime", "v3$nonAaaDeathTime")
#       for (varName in varNames) {
#         var <- getAnything(varName)
#         if (varName != "personNumber" && is.numeric(var)) 
#           var <- sprintf("%.2f", var)
#         cat(varName, "=", var, "  ", sep="")
#         #if (k %% 4 == 0 && k < length(varNames)) cat("\n")	
#       }
#       cat("\n")
#       print(eventHistory)
#       cat("\n")
#     }
#     
#     # If returnEventHistories, then store the event-history. 
#     if (v0$returnEventHistories) 
#       result$eventHistories[[treatmentGroup]] <- eventHistory	
#     # MS added. Store individual's b0 and b1 so that aorta size of the population can be assessed at later ages
#     result$eventHistories[[treatmentGroup]]$b0 <- v3$b0
#     result$eventHistories[[treatmentGroup]]$b1 <- v3$b1
#     result$eventHistories[[treatmentGroup]]$initialAortaSizeAsMeasured <- v3$initialAortaSizeAsMeasured
#     # From eventHistory (and nothing else that is specific to the person),
#     # calculate their life-years, QALYs, 
#     # total cost, etc., and store these in outputs. The health-economic
#     # quantities have to be calculated here, when you definitely have all 
#     # the event-times to hand (in eventHistory), because the discounting 
#     # calculations require all the specific event-times. 
#     result$personQuantities[[treatmentGroup]] <- 
#       calculateHealthEconomicQuantities(
#         eventHistory, v0$namesOfQuantities, v2$costs, v1other)
#   }
#   
#   return(result)
# }

################################################################################

################################################################################
# 2a) Models for aorta growth and rupture
# Generate person-specific aorta growth and rupture parameters. This should be 
# called from processOnePair, and the numbers returned by it should be 
# stored in v3. 
# This linear mixed model for log-diameter samples baseline diameter from a distribution, 
# which may or may not be weighted, then generates from a model with random slope. 
################################################################################
generatePersonAortaParameters <- function(v1other, v2) {  # (person not persons)
	# Check the arguments.
	if (!("baselineDiametersWithDesiredPrevalence" %in% names(v2)))
		stop("v2 must contain baselineDiametersWithDesiredPrevalence")
	if (!("weight" %in% names(v2$baselineDiametersWithDesiredPrevalence))) 
		stop("v2$baselineDiametersWithDesiredPrevalence must have a ",
				"\"weight\" column")
	
	# Generate y0.
	y0 <- sample(x=v2$baselineDiametersWithDesiredPrevalence$size, size=1, 
			prob=v2$baselineDiametersWithDesiredPrevalence$weight)
	
	# b0 is set to log(y0) for y0<3
	# b1 is generated from a conditional distribution given b0 using rnorm 
	if(y0<3){
	  b0 <- log(y0)
	  mu <- v2$beta1 + v2$rho * v2$sigma1 / v2$sigma0 * (b0 - v2$beta0) ## mean of b1 given b0
	  sigma <- sqrt((1 - v2$rho ^ 2) * v2$sigma1 ^ 2) ## SD of b1 given b0
	  b1 <- rnorm(n=1, mean=mu, sd=sigma)
	} else {
	
	  # Calculate muOfB and v. 
	  sigmaSum <- v2$sigma0 ^ 2 + v2$sigmaW ^ 2
	  muOfB <- rbind(v2$beta0, v2$beta1) +
	    rbind(v2$sigma0^2, v2$rho * v2$sigma0 * v2$sigma1) *
	    (log(y0) - v2$beta0) / sigmaSum
	  v <- matrix(c(
	    v2$sigma0^2 * v2$sigmaW^2,
	    v2$rho * v2$sigma0 * v2$sigma1 * v2$sigmaW^2,
	    v2$rho * v2$sigma0 * v2$sigma1 * v2$sigmaW^2,
	    v2$sigma0^2 * v2$sigma1^2 * (1-v2$rho^2) + v2$sigma1^2 * v2$sigmaW^2
	  ), nrow=2) / sigmaSum

	  # Generate b0 and b1. 
	  require(MASS, quietly=TRUE)
	  b <- mvrnorm(n=1, mu=muOfB, Sigma=v)
	  b0 <- b[1]
	  b1 <- b[2]
	  
	}
	
	# Generate the rupture-time. 
	ruptureTime <- myRgompertz(n=1, shape=v2$alpha * b1, 
			rate=exp(v2$gamma + v2$alpha * b0))
	
	# If v1other$zeroGrowthDiameterThreshold exists, and if the initial diameter 
	# is below this, then set b1 to zero and ruptureTime to NA.
	if ("zeroGrowthDiameterThreshold" %in% names(v1other) &&
			getExactInitialAortaSize(b0, b1) < 
			v1other$zeroGrowthDiameterThreshold) {
		b1 <- 0 
		ruptureTime <- NA
	}

	return(list(b0=b0, b1=b1, ruptureTime=ruptureTime, 
			initialAortaSizeAsMeasured=y0))
}

# Generate an aorta measurement. 
# This needs to be passed v3 (from which it uses b0 and b1), time, and method. 
# Depending on "method" it may also require the measurement-error standard 
# deviation (e.g. v2$ultrasoundMeasurementErrorSD, ctMeasurementErrorSD) and 
# v1distributions$extraDiameterForCtScan. 
getAortaMeasurement <- function(v3, time, measurementErrorSD, 
		method=c("ultrasound", "ct", "exact"), extraDiameterForCtScan, propensity=NULL) {
	method <- match.arg(method) 
	if (xor(method=="ct", !missing(extraDiameterForCtScan)))
		stop("extraDiameterForCtScan must be given iff method=\"ct\"")
	if (method=="exact" && !missing(measurementErrorSD))
		stop("measurementErrorSD must not be given if method=\"exact\"")

	if (method=="ultrasound") {
	  return(getExactAortaMeasurement(v3, time) *
				exp(qnorm(propensity, sd=measurementErrorSD)))
	} else if (method=="ct") {
	  return(getExactAortaMeasurement(v3, time) +
				qnorm(propensity, sd=measurementErrorSD) +
				extraDiameterForCtScan)
	} else { 
		return(getExactAortaMeasurement(v3, time))
	}
}

# Only to be used by getAortaMeasurement and generatePersonAortaParameters:
getExactAortaMeasurement <- function(v3, time) { 
	exp(v3$b0 + v3$b1 * time)
}

# getExactInitialAortaSize is used by generatePersonAortaParameters if 
# v1other$zeroGrowthDiameterThreshold exists. 
getExactInitialAortaSize <- function(b0, b1) {
	getExactAortaMeasurement(v3=list(b0=b0, b1=b1), time=0)
}

# Two subroutines that are used by generateIncidentalDetectionTime.
# getTimeAtGivenDiameter is the inverse of getExactAortaMeasurement:
getTimeAtGivenDiameter <- function(v3, diameter) {
	(log(diameter) - v3$b0) / v3$b1
}


# generateIncidentalDetectionTime. This assumes the linear model for log-diameter for each person. 
generateIncidentalDetectionTime <- function(currentTime, 
		thresholdForIncidentalDetection, v3, rateOfIncidentalDetection) {
	# If v1other$zeroGrowthDiameterThreshold is used, b1 might be exactly zero:
	if (v3$b1 == 0) return(list(time = NA, v3 = v3))  
	# Otherwise
	timeAtThreshold <- 
			getTimeAtGivenDiameter(v3, thresholdForIncidentalDetection)
	# There are four possibilities:
	if (timeAtThreshold <= currentTime && v3$b1 <= 0) 
		return(list(time = NA, v3 = v3))
	if (timeAtThreshold <= currentTime && v3$b1 > 0){ 
	  ## Inverse of the survival function (quantile function)
	  incidentalDetectionTime <- qexp(v3[["rateOfIncidentalDetection"]][1], rate = rateOfIncidentalDetection)
	  v3[["rateOfIncidentalDetection"]] <- v3[["rateOfIncidentalDetection"]][-1]
		return(list(time = currentTime + incidentalDetectionTime, v3 = v3))
	}
	if (timeAtThreshold > currentTime && v3$b1 <= 0) {
	  incidentalDetectionTime <- currentTime + qexp(v3[["rateOfIncidentalDetection"]][1], rate = rateOfIncidentalDetection)
	  v3[["rateOfIncidentalDetection"]] <- v3[["rateOfIncidentalDetection"]][-1]
	  if (incidentalDetectionTime < timeAtThreshold) {
			return(list(time = incidentalDetectionTime, v3 = v3))
		} else {
			return(list(time = NA, v3 = v3))
		}
	}
	if (timeAtThreshold > currentTime && v3$b1 > 0){ 
	  incidentalDetectionTime <- qexp(v3[["rateOfIncidentalDetection"]][1], rate = rateOfIncidentalDetection)
	  v3[["rateOfIncidentalDetection"]] <- v3[["rateOfIncidentalDetection"]][-1]
	  return(list(time = timeAtThreshold + incidentalDetectionTime, v3 = v3))
	}
}

# Adjust the prevalence in a baseline diameter distribution. 
changePrevalence <- function(baselineDiameters, threshold, prevalence) {
	# Check baselineDiameters. 
	if (!identical(names(baselineDiameters), c("size", "weight")))
		stop("baselineDiameters must have names \"size\" and \"weight\"")
	if (length(unique(round(diff(baselineDiameters$size), 6))) != 1)
		stop("baselineDiameters$size must be evenly spaced")
	
	# Check prevalence. 
	if (missing(prevalence) || is.na(prevalence) || !is.numeric(prevalence) || 
			length(prevalence) != 1 || prevalence < 0 || prevalence > 1)
		stop("prevalence must be a single numeric between 0 and 1")
	
	# Check threshold.
	if (missing(threshold) || is.na(threshold) || !is.numeric(threshold) || 
				length(threshold) != 1 || threshold < 0)
		stop("threshold must be a single positive numeric")
	if (!(threshold %in% baselineDiameters$size))
		stop("for now, threshold must be in baselineDiameters$size;\n", 
				"  if you want to change this, edit changePrevalence")
	
	# Calculate the new weights.
	sizeIsAboveThreshold <- baselineDiameters$size >= threshold
	originalPrevalence <-  sum(baselineDiameters$weight[sizeIsAboveThreshold])
	
	sizeTimesWeight <- baselineDiameters$size * baselineDiameters$weight
	b <- (originalPrevalence - prevalence) / 
			(originalPrevalence * sum(sizeTimesWeight) - 
				sum(sizeTimesWeight[sizeIsAboveThreshold]))
	a <- 1 - b * sum(sizeTimesWeight)
	
	# new weight = (a + b * size) * old weight, or zero if that is negative
	baselineDiameters$weight <- pmax(0, 
			(a + b * baselineDiameters$size) * baselineDiameters$weight)
	
	# Correct prevalence by multiplying weights for diameters >= threshold 
	# by a factor, as negative weights have been set to zero. 
	sumOfWeightsBelow <- 
	        sum(baselineDiameters$weight[baselineDiameters$size < threshold])
	sumOfWeightsAbove <- 
	        sum(baselineDiameters$weight[baselineDiameters$size >= threshold])
	factorToUse <- prevalence / (1 - prevalence) * 
	        sumOfWeightsBelow / sumOfWeightsAbove
	baselineDiameters$weight[baselineDiameters$size >= threshold] <- 
	        baselineDiameters$weight[baselineDiameters$size >= threshold] * 
	        factorToUse
	
	baselineDiameters$weight <- 
			baselineDiameters$weight / sum(baselineDiameters$weight)
	
	return(baselineDiameters)
}


# Untransform the vector of betas,  i.e. convert from atanhRho to rho etc.
untransformAortaGrowthPars <- function(transformedPars) {
  # Define transformedNames and make sure transformerPars is a numeric 
  # vector with the right names. (When a numeric vector has an attribute 
  # such as "type", it fails is.vector. So create
  # transformedParsWithAttrsRemoved and use is.vector on that instead.)
  transformedNames <- c("beta1", "beta0", "logSigma1", "logSigma0", 
                        "atanhRho", "logSigmaW")
  transformedParsWithAttrsRemoved <- transformedPars
  attributes(transformedParsWithAttrsRemoved) <- NULL
  if (!is.vector(transformedParsWithAttrsRemoved) || 
      !is.numeric(transformedPars) || 
      !identical(names(transformedPars), transformedNames))
    stop("transformedPars is illegal")
  result <- c(
    beta1 = transformedPars["beta1"], 
    beta0 = transformedPars["beta0"],
    sigma1 = exp(transformedPars["logSigma1"]),
    sigma0 = exp(transformedPars["logSigma0"]),
    rho = tanh(transformedPars["atanhRho"]),
    sigmaW = exp(transformedPars["logSigmaW"])
  )
  # result now has names like "sigma1.logSigma1", so cut ".logSigma1" etc.:
  names(result) <- sub("(\\w+)\\.\\w+", "\\1", names(result))
  return(result)
}

################################################################################

################################################################################
# 2b) Generate event history
# For a single person, given their treatmentGroup, ruptureTimes, 
# nonAaaDeathTime, and so on, generate their full event-history. 
################################################################################
generateEventHistory <- function(v0, v1other, v2, v3, treatmentGroup) {
	
	eventHistory <- makeEmptyEventHistory(recordSizes=v0$recordSizes)
	
	scheduledEvents <- numeric()   
	
	# Store times in scheduledEvents. 
	scheduledEvents["rupture"] <- v3$ruptureTime
	scheduledEvents["nonAaaDeath"] <- v3$nonAaaDeathTime
	if (treatmentGroup=="screening") 
		scheduledEvents["inviteToScreen"] <- v1other$waitingTimeToInvitation
	if (treatmentGroup=="noScreening"){
	  gID <- generateIncidentalDetectionTime(0, 
	                                  v1other$thresholdForIncidentalDetection, v3, 
	                                  v2$rateOfIncidentalDetection)
	  scheduledEvents["incidentalDetection"] <- gID$time
	  v3 <- gID$v3
	}
	if ("censoringTime" %in% names(v3))
	  scheduledEvents["censored"] <- v3$censoringTime
	
	## Set numberMonitor = number of times in each size group that monitoring has occurred. Include large AAA group here as well
	numberMonitor <- rep(0,length(v1other$aortaDiameterThresholds)+1)
	
	repeat {
		# Make sure that all the scheduled times are different (exclude NAs and Infinities -- MS added 24/07/2019).
	  # Allow censored == monitored - Added by MS 02/08/19
		if (!allDifferent(scheduledEvents) & scheduledEvents["censored"] != scheduledEvents["monitor"]) {
			print(scheduledEvents)
			stop("scheduledEvents times must all be different")
		}
		
		eventType <- names(scheduledEvents)[which.min(scheduledEvents)] 
		eventTime <- scheduledEvents[eventType] 
		if (is.na(eventTime)) { 
			print(scheduledEvents)
			stop("eventTime is NA") 
		}
		eventHistory <- addEvent(eventHistory, eventType, eventTime)
		scheduledEvents[eventType] <- NA 
	
		# Update scheduledEvents as appropriate based on what eventType is.
		if (eventType=="inviteToScreen") {
			if (v3$probOfRequireReinvitation) {
				eventHistory <- addEvent(eventHistory, "requireReinvitation", eventTime)
			} 
			if (v3$probOfAttendScreen) {
				scheduledEvents["screen"] <- eventTime   # ("screen" means they attend)
			} else {
				eventHistory <- addEvent(eventHistory, "failToAttendScreen", eventTime)
				gID <- generateIncidentalDetectionTime(eventTime, 
				                                       v1other$thresholdForIncidentalDetection, v3, 
				                                       v2$rateOfIncidentalDetection)
				scheduledEvents["incidentalDetection"] <- gID$time
				v3 <- gID$v3
			}
		
		} else if (eventType=="screen") {
			aortaSize <- v3$initialAortaSizeAsMeasured
			if (is.null(aortaSize) || is.na(aortaSize))
				stop("aortaSize is NA or NULL")

			if ("trueSizes" %in% names(eventHistory) && !v3$probOfNonvisualization)  
				# The true size is unknown, so set that to NA. The measured 
				# size is aortaSize, so record that.
				eventHistory <- recordSize(eventHistory, NA, aortaSize)			
	
			# If they have nonvisualization.
			if (v3$probOfNonvisualization) 
				eventHistory <- addEvent(eventHistory, "nonvisualization", 
						eventTime)
			
			# Find what interval aortaSize is in & schedule events accordingly.
			aortaSizeGroup <- findInterval(aortaSize, 
					v1other$aortaDiameterThresholds)
			## Add one to the times patient has been screened or monitored in this size group
			numberMonitor[aortaSizeGroup + 1] <- numberMonitor[aortaSizeGroup + 1] + 1
			
			if (aortaSizeGroup == 0 || v3$probOfNonvisualization) {  ## Assumes that first group is screened normal group
			  gID <- generateIncidentalDetectionTime(eventTime, 
			                                         v1other$thresholdForIncidentalDetection, v3, 
			                                         v2$rateOfIncidentalDetection)
			  scheduledEvents["incidentalDetection"] <- gID$time
			  v3 <- gID$v3
			} else if (aortaSizeGroup < length(v1other$aortaDiameterThresholds)) {
			  scheduledEvents["monitor"] <- 
						eventTime + v1other$monitoringIntervals[aortaSizeGroup+1] ## Updated MS. 07/02/19
			  gD <- generateDropoutTime(eventTime, 
                    v2$rateOfDropoutFromMonitoring, v3)
			  scheduledEvents["dropout"] <- gD$time
			  v3 <- gD$v3
			} else {
				scheduledEvents["consultation"] <- 
						eventTime + v1other$waitingTimeToConsultation
			}
			
		} else if (eventType=="monitor") {
		  # Assume non-visualization never happens in monitoring. 
			aortaSize <- getAortaMeasurement(v3, eventTime, 
					v2$ultrasoundMeasurementErrorSD, method="ultrasound", propensity=v3[["aortaSize"]][1])
			v3[["aortaSize"]] <- v3[["aortaSize"]][-1]
			if ("trueSizes" %in% names(eventHistory)) {
				trueSize <- getAortaMeasurement(v3, eventTime, method="exact")
				eventHistory <- recordSize(eventHistory, trueSize, aortaSize)
			}
			aortaSizeGroup <- findInterval(aortaSize, 
					v1other$aortaDiameterThresholds)
			## Add one to the times patient has been screened or monitored in this size group
			numberMonitor[aortaSizeGroup + 1] <- numberMonitor[aortaSizeGroup + 1] + 1
			## If numberMonitor <= maxNumberMonitor then schedule another monitor else discharge from surveillance
			## Last group has Inf number of monitors. This corresponds with large AAA group
			if (numberMonitor[aortaSizeGroup + 1] < c(v1other$maxNumberMonitor, Inf)[aortaSizeGroup + 1]){
			  if (aortaSizeGroup == 0) {  # they are below lowest threshold
			    eventHistory <- addEvent(eventHistory, 
			                             "aortaDiameterBelowThreshold", eventTime)
			    scheduledEvents["monitor"] <- 
			      eventTime + v1other$monitoringIntervals[1]
			  } else if (aortaSizeGroup < length(v1other$aortaDiameterThresholds)) {
			    scheduledEvents["monitor"] <- 
			      eventTime + v1other$monitoringIntervals[aortaSizeGroup + 1] ## updated MS 07/02/19
			  } else {
			    scheduledEvents["consultation"] <- 
			      eventTime + v1other$waitingTimeToConsultation
			    scheduledEvents["dropout"] <- NA
			  }
			} else { ## added a discharge event: MS 07/02/19
			  scheduledEvents["discharged"] <- eventTime
			  scheduledEvents["dropout"] <- NA
			}			
		} else if (eventType=="consultation") {
			# Measure the aorta diameter using CT, and if the result is less
			# than thresholdForSurgery then return to monitoring.  
		  aortaSize <- getAortaMeasurement(v3, eventTime, 
					v2$ctMeasurementErrorSD, method="ct", 
					extraDiameterForCtScan=
					v2$extraDiameterForCtScan, propensity=v3[["aortaSize"]][1])
		  v3[["aortaSize"]] <- v3[["aortaSize"]][-1]
			# Record the size.
			if ("trueSizes" %in% names(eventHistory)) {
				trueSize <- getAortaMeasurement(v3,eventTime,method="exact")
				eventHistory <- recordSize(eventHistory, trueSize,aortaSize)
			}
			aortaSizeGroup <- findInterval(aortaSize, 
					v1other$aortaDiameterThresholds)
			gBV <- getBinaryVariable("probOfContraindication", v1other, v2, v3, eventTime)
			contraindication <- gBV$result
			v3 <- gBV$v3
			
			if (aortaSizeGroup < length(v1other$aortaDiameterThresholds)) {
				eventHistory <- addEvent(eventHistory, 
						"decideOnReturnToMonitoring", eventTime)
				scheduledEvents["monitor"] <- 
						eventTime + v1other$monitoringIntervals[aortaSizeGroup + 1] ## updated MS 07/02/19
				gD <- generateDropoutTime(eventTime, 
				                          v2$rateOfDropoutFromMonitoring, v3) 
				scheduledEvents["dropout"] <- gD$time
				v3 <- gD$v3
			} else if (contraindication) {
				eventHistory <- addEvent(eventHistory, 
						"contraindicated", eventTime)
				if(!is.null(v2$rateOfNonAaaDeathAfterContraindication)){
				  scheduledEvents["nonAaaDeath"] <- eventTime + 
				    generateTimeToNonAaaDeathFromContraindication(
				      v2$rateOfNonAaaDeathAfterContraindication, v3)  
				}
				
		
				# Post-contraindication monitoring. 
				if ("monitoringIntervalFollowingContraindication" %in% 
						names(v1other))
					scheduledEvents["monitorFollowingContraindication"] <- 
							eventTime + 
							v1other$monitoringIntervalFollowingContraindication
		
			} else {  
				eventHistory <- addEvent(eventHistory, 
						"decideOnElectiveSurgery", eventTime)
				gBV <- getBinaryVariable(
				  "probOfElectiveSurgeryIsOpen", v1other, v2, v3, eventTime)
				electiveSurgeryIsOpen <- gBV$result
				v3 <- gBV$v3
				surgeryEvent <- if (electiveSurgeryIsOpen) {
					"electiveSurgeryOpen" } else { "electiveSurgeryEvar" }
				scheduledEvents[surgeryEvent] <- 
						eventTime + v1other$waitingTimeToElectiveSurgery
			} 
			
		} else if (eventType=="monitorFollowingContraindication") {
			scheduledEvents["monitorFollowingContraindication"] <- 
					eventTime + v1other$monitoringIntervalFollowingContraindication
			
		} else if (eventType=="electiveSurgeryOpen" || 
				eventType=="electiveSurgeryEvar") {
			surgeryType <- 
					{ if(eventType=="electiveSurgeryOpen") "open" else "evar" }

					if (v1other$electiveSurgeryAaaDeathMethod == "instantDeathOnly") {
				if (surgeryType=="evar")
					stop("surgeryType=\"evar\" should be impossible when ",
						"v1other$electiveSurgeryAaaDeathMethod=\"instantDeathOnly\"")
				dieFromElectiveSurgery <- { if(
						"incidentalDetection" %in% eventHistory$events)
						v3$probOfDieFromElectiveSurgeryViaIncidentalDetection
						else
						v3$probOfDieFromElectiveSurgeryViaScreeningDetection }
				if (dieFromElectiveSurgery) { 
					eventHistory <- addEvent(eventHistory, "aaaDeath",eventTime)
					break
				}

			} else if (v1other$electiveSurgeryAaaDeathMethod == "survivalModel") { 
			  gPS <- generatePostSurgeryAaaDeathTime(v1other, v2, v3, 
			                                  eventTime, surgeryType, "elective")
			  v3 <- gPS$v3
				scheduledEvents["aaaDeath"] <- eventTime + gPS$time
						
				if (is.na(scheduledEvents["aaaDeath"]))
					stop("generatePostSurgeryAaaDeathTime returned NA")
			} else {
				stop("v1other$electiveSurgeryAaaDeathMethod=", 
						v1other$electiveSurgeryAaaDeathMethod, " is illegal")
			}
			scheduledEvents["rupture"] <- NA
			
			# Schedule reinterventions.
			surgeryTime <- eventTime
			if (eventType == "electiveSurgeryOpen") {
				reinterventionRates <- v2$reinterventionRatesAfterElectiveOpen
				reinterventionTimeBoundaries <- 
						v1other$reinterventionTimeBoundariesAfterElectiveOpen
				reinterventionEventType <- "reinterventionAfterElectiveOpen"
			} else if (eventType == "electiveSurgeryEvar") {
				reinterventionRates <- v2$reinterventionRatesAfterElectiveEvar
				reinterventionTimeBoundaries <- 
						v1other$reinterventionTimeBoundariesAfterElectiveEvar
				reinterventionEventType <- "reinterventionAfterElectiveEvar"
			} else {
				stop("eventType is illegal")
			}
			gR <- generateReinterventionTime(
			  rates=reinterventionRates, 
			  timeBoundaries=reinterventionTimeBoundaries,
			  surgeryTime=surgeryTime, currentTime=eventTime, 
			  postSurgeryInitialPeriod=v1other$postSurgeryInitialPeriod, v3=v3)
			scheduledEvents[reinterventionEventType] <- gR$time
			v3 <- gR$v3
					
	
			# Post-surgery monitoring. 
			if (eventType == "electiveSurgeryOpen") {
				scheduledEvents["monitorFollowingOpenSurgery"] <- eventTime + 
						v1other$timeToMonitoringFollowingOpenSurgery
			} else {
				scheduledEvents["monitorFollowingEvarSurgery"] <- eventTime + 
						v1other$timeBetweenMonitoringFollowingEvarSurgery
			}
				
		} else if (eventType=="rupture") {
			if ("trueSizes" %in% names(eventHistory)) {
				trueSize <- getAortaMeasurement(v3, eventTime, method="exact")
				eventHistory <- recordSize(eventHistory, trueSize, NA)
			}
			if (v3$probOfEmergencySurgeryIfRupture) {
			  gBV <- getBinaryVariable(
			    "probOfEmergencySurgeryIsOpen", v1other, v2, v3, eventTime)
			  emergencySurgeryIsOpen <- gBV$result
			  v3 <- gBV$v3
				surgeryEvent <- { if(emergencySurgeryIsOpen)
						"emergencySurgeryOpen" else "emergencySurgeryEvar" }
				scheduledEvents[surgeryEvent] <- eventTime
			} else {
				eventHistory <- addEvent(eventHistory, "aaaDeath", eventTime)
				break
			}
			eventsToCancel <- c("monitor", "dropout", "incidentalDetection", 
				"electiveSurgeryOpen", "electiveSurgeryEvar", "consultation")
			scheduledEvents[eventsToCancel] <- NA
			
		} else if (eventType=="emergencySurgeryOpen" || 
				eventType=="emergencySurgeryEvar") {
			surgeryType <- 
					{ if(eventType=="emergencySurgeryOpen") "open" else "evar" }
			
			if (v1other$emergencySurgeryAaaDeathMethod == "instantDeathOnly") {
				if (surgeryType=="evar")
					stop("code for v1other$emergencySurgeryAaaDeathMethod=",
							"\"instantDeathOnly\" and surgeryType=\"evar\" ",
							"has not been written")

				if (v3$probOfDieFromEmergencySurgery) {
					eventHistory <- 
							addEvent(eventHistory, "aaaDeath", eventTime)
					break
				}				

			} else if (v1other$emergencySurgeryAaaDeathMethod == 
					"survivalModel") { 
			  gPS <- generatePostSurgeryAaaDeathTime(v1other, v2, v3, 
			                                         eventTime, surgeryType, "emergency")
				scheduledEvents["aaaDeath"] <- eventTime + gPS$time
				v3 <- gPS$v3		
				if (is.na(scheduledEvents["aaaDeath"]))
					stop("generatePostSurgeryAaaDeathTime returned NA")
				
			} else {
				stop("v1other$emergencySurgeryAaaDeathMethod=", 
						v1other$emergencySurgeryAaaDeathMethod, " is illegal")
			}

			# Schedule reinterventions.
			surgeryTime <- eventTime
			if (eventType == "emergencySurgeryOpen") {
				reinterventionRates <- v2$reinterventionRatesAfterEmergencyOpen
				reinterventionTimeBoundaries <- 
						v1other$reinterventionTimeBoundariesAfterEmergencyOpen
				reinterventionEventType <- "reinterventionAfterEmergencyOpen"
			} else if (eventType == "emergencySurgeryEvar") {
				reinterventionRates <- v2$reinterventionRatesAfterEmergencyEvar
				reinterventionTimeBoundaries <- 
						v1other$reinterventionTimeBoundariesAfterEmergencyEvar
				reinterventionEventType <- "reinterventionAfterEmergencyEvar"
			} else {
				stop("eventType is illegal")
			}
			gR <- generateReinterventionTime(
			  rates=reinterventionRates, 
			  timeBoundaries=reinterventionTimeBoundaries,
			  surgeryTime=surgeryTime, currentTime=eventTime, 
			  postSurgeryInitialPeriod=v1other$postSurgeryInitialPeriod, v3=v3)
			scheduledEvents[reinterventionEventType] <- gR$time
			v3 <- gR$v3
				

			# Post-surgery monitoring. 
			if (eventType == "emergencySurgeryOpen") {
				scheduledEvents["monitorFollowingOpenSurgery"] <- eventTime + 
						v1other$timeToMonitoringFollowingOpenSurgery
			} else {
				scheduledEvents["monitorFollowingEvarSurgery"] <- eventTime + 
						v1other$timeBetweenMonitoringFollowingEvarSurgery
			}
	
		} else if (eventType %in% c("aaaDeath", "nonAaaDeath", "censored")) {
			break
		
		} else if (eventType == "incidentalDetection") {
			if (getAortaMeasurement(v3, eventTime, method="exact") < 
					v1other$thresholdForIncidentalDetection)
				stop("incidentalDetection happened when aorta was below ",
						v1other$thresholdForIncidentalDetection, "cm")
			scheduledEvents["monitor"] <- eventTime
			gD <- generateDropoutTime(eventTime, 
			                          v2$rateOfDropoutFromMonitoring, v3)
			scheduledEvents["dropout"] <- gD$time
			v3 <- gD$v3
		
		} else if (eventType == "dropout" | eventType == "discharged") {
			scheduledEvents["monitor"] <- NA
			## Reset numberMonitor to zero so that if they get incidentally detected they start afresh
			numberMonitor <- rep(0,length(v1other$aortaDiameterThresholds)+1)
			gID <- generateIncidentalDetectionTime(eventTime, 
			                                       v1other$thresholdForIncidentalDetection, v3, 
			                                       v2$rateOfIncidentalDetection)
			scheduledEvents["incidentalDetection"] <- gID$time
			v3 <- gID$v3
		} else if (eventType %in% c(
				"reinterventionAfterElectiveOpen", 
				"reinterventionAfterElectiveEvar", 
				"reinterventionAfterEmergencyOpen", 
				"reinterventionAfterEmergencyEvar")) {
		  gR <- generateReinterventionTime(
		    rates=reinterventionRates, 
		    timeBoundaries=reinterventionTimeBoundaries,
		    surgeryTime=surgeryTime, currentTime=eventTime, 
		    postSurgeryInitialPeriod=v1other$postSurgeryInitialPeriod, v3=v3)
			scheduledEvents[eventType] <- gR$time
			v3 <- gR$v3
					
	
		} else if (eventType == "monitorFollowingOpenSurgery") {
			
		} else if (eventType == "monitorFollowingEvarSurgery") {
			# Schedule the next one.
			scheduledEvents["monitorFollowingEvarSurgery"] <- eventTime + 
					v1other$timeBetweenMonitoringFollowingEvarSurgery
					
		} else {
			stop("INTERNAL ERROR: unknown event-type ", eventType)
		}
	}
	
	# Return the event-history.
	return(eventHistory)
}

# Create getBinaryVariable.
# Calculates a probability from a beta distribution, and from a logistic model.
getBinaryVariable <- function(varName, v1other, v2, v3, eventTime) {
	
	# Check the arguments.
	if (!is.character(varName) || length(varName) != 1)
		stop("varName must be a single string")
	if (!is.list(v1other) || !is.list(v2) || !is.list(v3))
		stop("v1other, v2, and v3 must all be lists")
	if (!missing(eventTime) && (is.null(eventTime) || is.na(eventTime) || 
			!is.numeric(eventTime) || length(eventTime) != 1 || eventTime < 0))
		stop("eventTime must be a single non-negative numeric")
	
	# Get the result.
	if (varName %in% names(v3) & getType(v2[[varName]]) != "logistic model for probability") {
		if (!(getType(v3[[varName]]) %in% c("boolean", "fixed value")))
			stop("v3$", varName, " must have type boolean or fixed value")
		result <- v3[[varName]]
		
	} else if (varName %in% names(v2) && 
			getType(v2[[varName]]) == "logistic model for probability") {

		# Make beta and logOddsAdjustment. 
		beta <- v2[[varName]][names(v2[[varName]]) != "logOddsAdjustment"]
		logOddsAdjustment <- v2[[varName]]["logOddsAdjustment"]  
		
		# Calculate the covariates.
		covariates <- rep(NA, length(beta) - 1)
		names(covariates) <- names(beta)[-1]  
		for (covariateName in names(covariates)) {
		  ## NEED TO ENSURE v3 IS ASSIGNED BACK TO GENERATEEVENTHISTORY
			covariates[covariateName] <- switch(covariateName,
					age = v1other$startAge + eventTime - 80,
					aortaSize = getAortaMeasurement(v3, eventTime, 
						v2$ultrasoundMeasurementErrorSD, method="ultrasound", propensity=v3[["aortaSize"]][1]) - 6.0,
					stop("in v2$", varName, ", the name \"", covariateName, 
					"\" is illegal")
			)
			if(covariateName == "aortaSize"){
			  v3[["aortaSize"]] <- v3[["aortaSize"]][-1]
			}
			  
		}
		
		# Calculate the probability and use that to generate a boolean value 
		# from the Bernoulli distribution.
		prob <- calculateProbFromLogisticModel(beta=beta, 
				covariates=covariates, logOddsAdjustment=logOddsAdjustment)
		# result <- rbernoulli(prob)
		## Use propensity to calculate whether event occurs or not
		result <- prob > v3[[varName]]
		
	
	} else {
		cat("\n\nvarName=", varName, "\nv2:\n", sep=""); print(v2); 
		cat("\nv3:\n"); print(v3)
		stop("failed to find acceptable element ", varName, " in v2 or v3")
		
	}
	
	if (is.null(result) || is.na(result)) {
		cat("Error in getBinaryVariable: result is NULL or NA.\n")
		stop("result is ", {if (is.null(result)) "NULL" else result})
	}
	return(list(result = result, v3 = v3))
}

calculateProbFromLogisticModel <- 
		function(beta, covariates, logOddsAdjustment, verbose=FALSE) {
	# Check the arguments.
	if (length(beta) < 1) 
		stop("length(beta)=", length(beta), " but this must be at least 1")
	if (length(beta) != length(covariates) + 1)
		stop("length(beta)=", length(beta), " but length(covariates)=", 
				length(covariates))
	if (!identical(names(beta), c("intercept", names(covariates))))
		stop("names(beta)=", paste(names(beta), collapse=","), 
				" must be \"intercept\" followed by names(covariates)=",
				paste(names(covariates), collapse=","))
	
	# Check logOddsAdjustment, and set it to 0 if it is missing, NULL, or NA.
	if (missing(logOddsAdjustment) || is.null(logOddsAdjustment) ||
			is.na(logOddsAdjustment))
		logOddsAdjustment <- 0
	checkIsSingleNumeric(logOddsAdjustment)
	
	# Calculate and return the probability. 
	logOdds <- beta[1] + sum(beta[-1] * covariates) + logOddsAdjustment
	prob <- plogis(logOdds) 
	if (verbose) {
		cat("\ncalculateProbFromLogisticModel:\nbeta=\n")
		print(beta)
		cat("covariates=\n")
		print(covariates)
		cat("logOddsAdjustment=", logOddsAdjustment, "\n", sep="")		
		cat("logOdds=", logOdds, "\n", sep="")
		cat("prob=", prob, "\n", sep="")
	}
	return(prob)
}

# Post-surgery survival model, based on data from the EVAR-1 clinical trial.
# Model provides probability probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgery 
# (or same with Emergency and/or Evar) (instantaneous death); zero 
# probability of death between zero and postSurgeryInitialPeriod; and an 
# exponential distribution with parameter 
# rateOfAaaDeathAfterElectiveOpenSurgeryAndInitialPeriod (or same with 
# Emergency and/or Evar).
generatePostSurgeryAaaDeathTime <- function(v1other, v2, v3, eventTime, 
		surgeryType, surgeryAdmissionMode) {

	if (v1other$electiveSurgeryAaaDeathMethod != "survivalModel")
		stop("generatePostSurgeryAaaDeathTime must only be used if ",
				"v1other$electiveSurgeryAaaDeathMethod is \"survivalModel\"")
	
	# Check surgeryType and surgeryAdmissionMode.
	if (!(surgeryType %in% c("open", "evar")))
		stop("surgeryType must be open or evar")
	if (!(surgeryAdmissionMode %in% c("elective", "emergency")))
		stop("surgeryAdmissionMode must be elective or emergency")
	
	surgeryType <- changeFirstLetterToUpperCase(surgeryType)
	surgeryAdmissionMode <- changeFirstLetterToUpperCase(surgeryAdmissionMode) 
	probVarName <- paste0("probOfAaaDeathInInitialPeriodAfter",
			surgeryAdmissionMode, surgeryType, "Surgery")
	rateVarName <- paste0("rateOfAaaDeathAfter", 
			surgeryAdmissionMode, surgeryType, "SurgeryAndInitialPeriod")
	
	# Generate the AAA death time.
	gBV <- getBinaryVariable(probVarName, v1other, v2, v3, eventTime)
	v3 <- gBV$v3
	if (gBV$result) {
	#if (rbernoulli(v2[[probVarName]])) {
		return(list(time = 0, v3 = v3))
	} else {
	  return(list(time = v1other$postSurgeryInitialPeriod + 
	           qexp(v3[[rateVarName]], rate = v2[[rateVarName]]), v3 = v3))
	  ## Now uses propensity generated for pair of individuals         
	  #rexp(n=1, rate=v2[[rateVarName]]))
	}
	
}

# Functions for dealing with objects of S3 class "eventHistory". 
makeEmptyEventHistory <- function(recordSizes=FALSE) {
	if (!is.logical(recordSizes) || length(recordSizes) != 1)
		stop("recordSizes must be a single logical")
	if (recordSizes) {
		eventHistory <- list(events=character(), times=numeric(),
				trueSizes=numeric(), measuredSizes=numeric())
	} else {
		eventHistory <- list(events=character(), times=numeric())
	}
	class(eventHistory) <- c("eventHistory", "list")
	eventHistory
}

checkIsEventHistory <- function(obj) {
	if (!(inherits(obj, "eventHistory")))
		stop("obj must be an object of class eventHistory")
}
	
getEvent <- function(eventHistory, eventNumber) {
	checkIsEventHistory(eventHistory)
	if (!is.numeric(eventNumber) || length(eventNumber) > 1)
		stop("eventNumber must be a single numeric")
	numberOfEvents <- length(eventHistory$events)
	if (eventNumber > numberOfEvents)
		stop("eventNumber=", eventNumber, " but eventHistory only contains ", 
				numberOfEvents, " events")
	eventHistory$events[eventNumber] 
}

addEvent <- function(eventHistory, event, time, trueSize=NA, measuredSize=NA) { 
	# Checks.
	checkIsEventHistory(eventHistory)
	numberOfEvents <- length(eventHistory$events)
	if (numberOfEvents >= 1 && time < eventHistory$times[numberOfEvents]) 
		stop("the time of the new event must be >= all eventHistory times")
	# Add the new event.
	if ("trueSizes" %in% names(eventHistory)) {
		varNames <- c("event", "time", "trueSize", "measuredSize")
	} else {
		varNames <- c("event", "time")
	}
	for (varName in varNames) {
		colName <- paste0(varName, "s")
		eventHistory[[colName]] <- c(eventHistory[[colName]], get(varName))
	}
	eventHistory
}

recordSize <- function(eventHistory, trueSize, measuredSize) {
	checkIsEventHistory(eventHistory)
	if (!("trueSizes" %in% names(eventHistory)))
		stop("eventHistory does not have a trueSizes column")
	if (!is.na(trueSize) && (!is.numeric(trueSize) || length(trueSize) > 1))
		stop("trueSize must be a single numeric (or NA)")
	if (!is.na(measuredSize) && 
			(!is.numeric(measuredSize) || length(measuredSize) > 1))
		stop("measuredSize must be a single numeric (or NA)")
	
	numberOfEvents <- length(eventHistory$events)
	eventHistory$trueSizes[numberOfEvents] <- trueSize
	eventHistory$measuredSizes[numberOfEvents] <- measuredSize
	eventHistory
}
	
checkEventHistory <- function(eventHistory) {
	checkIsEventHistory(eventHistory)
	if (!is.list(eventHistory))
		stop("eventHistory must be a list")
	if (length(eventHistory) != 2 && length(eventHistory) != 4)
		stop("eventHistory must have either 2 or 4 elements")
	if (!identical(names(eventHistory), c("events", "times")) && !identical(
			names(eventHistory), 
			c("events", "times", "trueSizes", "measuredSizes")))
		stop("names(eventHistory) must be \"events\" and \"times\"", 
				" (and, optionally, \"trueSizes\" and \"measuredSizes\"")
	cat("eventHistory is AOK\n")
}
	
print.eventHistory <- function(x, ...) {
	checkIsEventHistory(x)
	numberOfEvents <- length(x$events)
	if (numberOfEvents == 0) {
		cat("[Empty event-history]\n")
	} else {
		cat("========= EVENT-HISTORY ==========")
		if ("trueSizes" %in% names(x)) cat(" (true / as measured)")
		cat("\n")
		for (i in 1:numberOfEvents) {
			writeTrueSize <- "trueSizes" %in% names(x) && !is.na(x$trueSizes[i])
			writeMeasuredSize <- 
					"measuredSizes" %in% names(x) && !is.na(x$measuredSizes[i])
			if (writeTrueSize && writeMeasuredSize) {
				sizesString <- sprintf("(%.2fcm / %.2fcm)", 
						x$trueSizes[i], x$measuredSizes[i])
			} else if (writeTrueSize) {
				sizesString <- sprintf("(%.2fcm / ---)", x$trueSizes[i])
			} else if (writeMeasuredSize) {
				sizesString <- sprintf("(--- / %.2fcm)", x$measuredSizes[i])
			} else {
				sizesString <- ""
			}
			cat(sprintf("%-27s %6.2f %s\n",
							x$events[i], x$times[i], sizesString))
		}
		cat("==================================\n")
	}
	invisible(x)
}
	
# generateReinterventionTime
generateReinterventionTime <- function(rates, 
		timeBoundaries=numeric(), surgeryTime, currentTime, 
		postSurgeryInitialPeriod, v3, verbose=FALSE) {
	
	# Check rates, timeBoundaries, surgeryTime, and currentTime.
  checkReinterventionRatesAndTimeBoundaries(rates, timeBoundaries)
	checkIsSingleNumeric(surgeryTime)
	checkIsSingleNumeric(currentTime)
	
	# If all rates are zero, return NA. No reintervention will be scheduled. 
	if (all(rates == 0)) return(list(time = NA, v3 =v3))
	
	# If currentTime is before the first possible time of reintervention, 
	# then replace it with that time. 
	if (currentTime < surgeryTime + postSurgeryInitialPeriod)
		currentTime <- surgeryTime + postSurgeryInitialPeriod
	
	# Find what period currentTime is in.
	periodNumber <- findInterval(currentTime, surgeryTime + timeBoundaries) + 1
	
	# Create timeBoundariesForGenerating.
	if (periodNumber <= length(timeBoundaries)) {
		timeBoundariesForGenerating <- surgeryTime + 
				timeBoundaries[periodNumber:length(timeBoundaries)] 
		timeBoundariesForGenerating <- timeBoundariesForGenerating[
				timeBoundariesForGenerating > currentTime]
		timeBoundariesForGenerating <- 
				c(currentTime, timeBoundariesForGenerating, Inf)
	} else {
		timeBoundariesForGenerating<- c(currentTime, Inf)
	}
	
	# Create ratesToUse and numberOfPeriodsToUse. 
	ratesToUse <- rates[periodNumber:length(rates)]
	numberOfPeriodsToUse <- length(ratesToUse)
	
	if (verbose) {
		cat("timeBoundariesForGenerating: ", timeBoundariesForGenerating, "\n")
		cat("ratesToUse: ", ratesToUse, "\n")
	}
	
	# Generate the time.
	for (i in 1:numberOfPeriodsToUse) {
		if (ratesToUse[i] == 0) {
			if (i == numberOfPeriodsToUse) {
				if (verbose) cat("zero rate in final period; returning NA\n")
				return(list(time = NA, v3 = v3)) 
			} else {
				next 
			}
		} 
		
		# Generate a time that might be usable as the next reintervention time.
	  # possibleResult <- timeBoundariesForGenerating[i] + rexp(n=1, rate=ratesToUse[i])
		possibleResult <- timeBoundariesForGenerating[i] +qexp(v3[["rateOfReintervention"]][1], rate=ratesToUse[i])
		v3[["rateOfReintervention"]] <- v3[["rateOfReintervention"]][-1]
		if (verbose) cat("i=", i, "  possibleResult (using rate=", 
					ratesToUse[i], "): ", possibleResult, " ... ", sep="")
		
		# If possibleResult is before the next time boundary, return it. 
		# Otherwise, continue to the next iteration of the for loop.
		if (possibleResult < timeBoundariesForGenerating[i + 1]) {
			if (verbose) cat("accepted!\n")
			return(list(time = possibleResult, v3 = v3))
		} else {
			if (verbose) cat("rejected\n")
		}
	}
	
	stop("INTERNAL ERROR: you should never get to here")
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
				(class(element) %in% c("numeric", "logical", "character"))) {
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
# 2d) Functions for generating time till non-AAA death
# There are three sets of functions, one for each model, plus one function, 
# generateTimeTillNonAaaDeath, that is used whichever model is used.
################################################################################
# A single function for any non-AAA death model. 
generateTimeTillNonAaaDeath <- function(v0, v1other) {
	if (v1other$nonAaaDeathMethod == "mass") {
		return(generateTimeTillNonAaaDeathFromSurvProbs(
				v1other$nonAaaSurvProbs, 0.25))
		
	} else if (v1other$nonAaaDeathMethod == "onsIntegerStart") { 
		return(generateTimeTillNonAaaDeathFromSurvProbs(
				v1other$nonAaaSurvProbs, 1))
		
	} else if (v1other$nonAaaDeathMethod == "onsNonintegerStart") {
		return(generateTimeTillNonAaaDeathWithNonIntegerStartAge(
				v1other$nonAaaMortalityRates, v0$generateAgeAtBaseline()))
		
	} else {
		stop("v1other$nonAaaDeathMethod=", v1other$nonAaaDeathMethod, 
				" is illegal")
	}
}

# Use three-monthly survival probabilities from MASS to get the survival probabilities 
# from CSV file and return them as a vector. 
getMassSurvivalProbabilities <- function(fileName=file.path("input/SWAN",
		"MASS10yrNonAAADeaths.csv")) {
	fileContents <- read.csv(fileName, comment.char="#")
	names(fileContents) <- c("cycle", "followUpAtLeast", 
			"survProbAdjustedForCensAndAaa", "probDeathInThisCycle")
	survProbs <- fileContents$survProbAdjustedForCensAndAaa
	survProbs[!is.na(survProbs)]
}

# generateTimeTillNonAaaDeathFromSurvProbs generates a non-AAA death time
# from a vector of survival probabilities. 
generateTimeTillNonAaaDeathFromSurvProbs <- 
		function(survivalProbs, periodLength) {
	# Check that the arguments are legal. 
	if (!is.vector(survivalProbs) || !is.numeric(survivalProbs) || 
			!identical(survivalProbs, sort(survivalProbs, decreasing=TRUE)) ||
			any(survivalProbs < 0) || any(survivalProbs > 1))
		stop("survivalProbs must be a decreasing vector of probabilities")
	if (!identical(periodLength, 0.25) && !identical(periodLength, 1))
		stop("periodLength should almost certainly be either 0.25 or 1")
	
	# Calculate how many periods they survive. 
	numberOfperiodsSurvived <- 
			length(survivalProbs) - findInterval(runif(1), sort(survivalProbs))
	
	# Calculate how long they survive. 
	numberOfperiodsSurvived * periodLength + runif(n=1, min=0, max=periodLength)
}

# Functions for use on ONS-style data, with start-age being different for 
# different people, and not necessarily integer.
readMortalityRatesFromFile <- function(fileName) {
	mortalityRates <- read.csv(fileName, header=FALSE, blank.lines.skip=TRUE, 
			comment.char="#")
	if (ncol(mortalityRates) < 2) 
		stop("the file must be a CSV file with age in the first column and ",
				"mortality rate in the second")
	names(mortalityRates)[1:2] <- c("age", "mortalityRate")
	if (any(mortalityRates$mortalityRate > 1))
		stop("the mortality rates must be per 1, not per 100,000")
	ageColumn <- mortalityRates$age
	if (ageColumn[1] != floor(ageColumn[1]) ||
			!identical(ageColumn, seq(min(ageColumn), max(ageColumn))))
		stop("mortalityRates$age must be of the form x x+1 x+2 ..., ",
				"where x is an integer")
	return(mortalityRates)
}

# Given a table of mortality rates (whose first two columns are age and 
# mortalityRate) and an initial age, generate time till death. 
generateTimeTillNonAaaDeathWithNonIntegerStartAge <- 
		function(mortalityRates, startAge, verbose=FALSE) {
	if (!is.data.frame(mortalityRates) || 
			!identical(names(mortalityRates)[1:2], c("age","mortalityRate")))
		stop("mortalityRates must be a data-frame whose first two columns are ",
				"age and mortalityRate")
	if (startAge < min(mortalityRates$age) || 
			startAge >= max(mortalityRates$age) + 1)
		stop("startAge is outside the range that appears in mortalityRates")
	
	# Find the first row to use.  
	integerStartAge <- floor(startAge)
	rowNumber <- match(integerStartAge, mortalityRates$age)
	
	# Deal with the first part-year, if startAge is not an integer. 
	if (startAge > integerStartAge) { 
		mortalityRate <- mortalityRates$mortalityRate[rowNumber]
		proportionOfYearLeft <- integerStartAge + 1 - startAge
		if (verbose) cat("probability of dying in first part-year:", 
				mortalityRate * proportionOfYearLeft, "\n")
		if (runif(1) < mortalityRate * proportionOfYearLeft)
			return(runif(n=1, min=0, max=proportionOfYearLeft))
		rowNumber <- rowNumber + 1
	}
	
	repeat {
		if (rowNumber > nrow(mortalityRates))
			return(max(mortalityRates$age) + 1 + runif(1) - startAge)
		if (verbose) cat("probability of dying in age-group ", 
				mortalityRates$age[rowNumber], ": ", 
				mortalityRates$mortalityRate[rowNumber], "\n", sep="")
		if (runif(1) < mortalityRates$mortalityRate[rowNumber])
			return(mortalityRates$age[rowNumber] + runif(1) - startAge)
		rowNumber <- rowNumber + 1
	}
}

# A simpler function for use with ONS-style data that assumes that 
# the same start age and that age is an integer. 
convertMortalityRatesToSurvProbs <- function(startAge, fileName) {
	## If fileName is already a data.frame then do not read.csv, otherwise use read.csv
  if(is.data.frame(fileName)){
    mortalityRates <- fileName
  } else {
    mortalityRates <- read.csv(fileName, header=FALSE, row.names=1, 
                               blank.lines.skip=TRUE, comment.char="#")
  }
  
  if (ncol(mortalityRates) != 1) 
    stop("the file must be a two-column CSV file with age in the first ",
				"column and mortality rate in the second")
	if (any(mortalityRates > 1))
		stop("the mortality rates must be per 1, not per 100,000")
	
	firstRow <- match(startAge, rownames(mortalityRates))
	if (is.na(firstRow)) 
		stop("startAge=", startAge, " was not found in ", fileName)
	survivalProbs <- 1 - mortalityRates[firstRow, 1]
	
	if (firstRow==nrow(mortalityRates)) return(survivalProbs)
	for (i in (firstRow + 1):nrow(mortalityRates)) {
		nextSurvivalProb <- survivalProbs[length(survivalProbs)] * 
				(1 - mortalityRates[i, 1])
		survivalProbs <- c(survivalProbs, nextSurvivalProb)
	}
	return(survivalProbs)
}

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
psa <- function(v0, v1other, v1distributions, v2values) {
	
	# Set elements of v0 as needed for PSA. 
	v0 <- setUnspecifiedElementsOfv0(v0)  
	v0$returnMeanQuantities <- TRUE
	v0$returnEventHistories <- FALSE
	v0$returnAllPersonsQuantities <- FALSE
	v0$showEventHistories <- FALSE
	v0$verbose <- FALSE 
	
	# Set unspecified elements of v1other to default values
	v1other <- setUnspecifiedElementsOfv1other(v1other)
	
	# Check the arguments.
	checkArgs(v0=v0, v1other=v1other, v1distributions=v1distributions)
	
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
				expr=generateV2(v1distributions), simplify=FALSE) 
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
				FUN=onePsaIteration, v0, v1other, v2values)
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
				v1other, v2values)
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

onePsaIteration <- function(psaIterationNumber, v0, v1other, 
		v2values) {
  cat(paste0("PSA iteration ", psaIterationNumber, "\n"))
  
	# Get v2, the values of the uncertain global variables, and check it.
	if (is.null(v2values)) 
	    stop("INTERNAL ERROR: v2values should be generated in psa ",
				"or passed into psa by the user")
    v2 <- v2values[[psaIterationNumber]]
    if (is.null(v2)) stop("v2values[[", psaIterationNumber, "]] is NULL")
	if (!is.list(v2)) stop("v2values[[", psaIterationNumber,"]] must be a list")
	if (!("probOfNonvisualization" %in% names(v2)))
		stop("v2 must contain probOfNonvisualization (and many other elements)")
	
	# Create and analyze the persons, and return what is needed.
	processPersonsResult <- processPersons(v0, v1other, v2)
	result <- list(meanQuantities=processPersonsResult$meanQuantities, v2=v2)
	if (v0$returnEventHistories) 
		result$eventHistories <- processPersonsResult$eventHistories
	return(result)
}

# generateV2, a function to generate specific values of v2, the global 
# uncertain parameters, from v1distributions, the global fixed parameters that 
# are used for generating elements of v2. 
generateV2 <- function(v1distributions) {
	
	v2 <- compactList()
	
	for (elementName in sort(names(v1distributions))) {
		
		# Get v1element and type. 
		v1element <- v1distributions[[elementName]]
		type <- attr(v1element, "type")
		if (is.null(type)) 
			stop("the \"type\" element of v1distributions$", elementName, 
					" is NULL")
		
		if (type == "fixed value") {
			v2element <- v1element
		} else if (type == "fixed value for qaly") {
		  v2element <- v1element
		  attr(v2element, "type") <- "qaly"
		}	else if (type == "beta pars for probability") {
			v2element <- myRbeta(n=1, 
					shape1=v1element$alpha, shape2=v1element$beta)
			attr(v2element, "type") <- "probability"
			
		} else if (type == "hyperpars for logistic model for probability") {
			require(MASS, quietly=TRUE)
			if (length(v1element$mean) == 1) {
				v2element <- rnorm(n=1, mean=v1element$mean, 
						sd=sqrt(v1element$variance))
				names(v2element) <- "intercept"
			} else {
				v2element <- mvrnorm(n=1, 
						mu=v1element$mean, Sigma=v1element$covariance)
			}
			if ("logOddsAdjustment" %in% names(v1element))
				v2element <- c(v2element, 
						logOddsAdjustment=v1element$logOddsAdjustment)
			attr(v2element, "type") <- "logistic model for probability"
			
		} else if (type == "gamma pars for rate") {
			v2element <- rgamma(n=1, 
					shape=v1element$shape, scale=v1element$scale)
			attr(v2element, "type") <- "rate"
			
		} else if (type == "gamma pars for multiple rates") {
		  v2element <- rgamma(n=length(v1element$shapes), 
		                      shape=v1element$shapes, scale=v1element$scales)
		  attr(v2element, "type") <- "reintervention rates"
			
		} else if (type == "pars for betaThenConvertThreeMonthProbToRate") {
			v2element <- convertThreeMonthProbToRate(myRbeta(n=1, 
					shape1=v1element$alpha, shape2=v1element$beta))
			attr(v2element, "type") <- "rate"
			
		} else if (type == "pars for gammaThenMultiplyByFour") {
			v2element <- rgamma(n=1, 
					shape=v1element$shape, scale=v1element$scale) * 4
			attr(v2element, "type") <- "rate"
			
		} else if (type == "hyperpars for aorta model") {
			# do nothing; see below
		
		} else if (type == "fixed value for probability") {
			v2element <- v1element
			attr(v2element, "type") <- "probability"

		} else if (type == "truncated normal distribution") {
		  v2element <- max(0, min(1, rnorm(n=1, mean=v1element$mean, 
		                     sd=sqrt(v1element$variance)))) 
		  attr(v2element, "type") <- "probability"
		  
		} else if (type == "fixed value for costs") {
			v2element <- v1element
			attr(v2element, "type") <- "costs"

		} else if (type == "distribution for costs") {
		  ## Generate by alphabetically ordered events, so results can be replicated no matter the order in which they are given
		  v2element <- 
		    vector(mode="numeric", length=length(v1distributions$costs$mean))
		  names(v2element)<-sort(names(v1distributions$costs$mean))
		  for (i in sort(names(v1distributions$costs$mean))) {
		    v2element [[i]] <- exp(rnorm(n=1, mean=v1distributions$costs$mean[[i]], 
		                            sd=sqrt(v1distributions$costs$variance[[i]])))
		  }
		  attr(v2element, "type") <- "costs"
		    
		} else if (type == "fixed value for reintervention rates") {
			v2element <- v1element
			attr(v2element, "type") <- "reintervention rates"
		
		} else if (type == "fixed value for rate") {
		  v2element <- v1element
		  attr(v2element, "type") <- "rate"
		  
		} else if(type == "normal distribution for logit prevalence") {
		  # Allow prevalence to be specified by a logistic model
		  v2element <- plogis(rnorm(n=1, mean=v1element$mean, sd=sqrt(v1element$variance)))
	    attr(v2element, "type") <- "probability"
		} else if (type == "multivariate normal distribution") {
		  require(MASS, quietly=TRUE)
		    v2element <- mvrnorm(n=1, 
		                         mu=v1element$mean, Sigma=v1element$variance)
		}	else {
			stop("INTERNAL ERROR: checkArgs should have caught this earlier.",
					"\n type=", {if (is.null(type)) "NULL" else type}, 
					" is illegal; elementName=", elementName)
		}
		
		# Check v2element is not NULL or NA and that it has a "type" attribute.
		if (type != "hyperpars for aorta model") {
			if (is.null(v2element) || any(is.na(v2element))) 
				stop("v2element is NULL or contains NAs; elementName=", 
						elementName)
			if (is.null(attr(v2element, "type")))
				stop("the \"type\" element of v2element is NULL; elementName=", 
						elementName)
		}
		
		# Set the v2 element (unless type is "hyperpars for aorta model").
		if (type != "hyperpars for aorta model")
			v2[[elementName]] <- v2element
	}
	
	# Generate the v2 elements that are needed for aortaModel.
	require(MASS, quietly=TRUE)
	growthParametersTransformed <- mvrnorm(n=1,	
			mu=v1distributions$meanForGrowthParameters, 
			Sigma=v1distributions$covarianceForGrowthParameters)
	growthParametersUntransformed <- 
			untransformAortaGrowthPars(growthParametersTransformed)
	for (parName in names(growthParametersUntransformed))
		v2[[parName]] <- growthParametersUntransformed[parName]
	ruptureParameters <- mvrnorm(n=1, 
			mu=v1distributions$meanForRuptureParameters, 
			Sigma=v1distributions$covarianceForRuptureParameters)
	v2$gamma <- ruptureParameters["gamma"]
	v2$alpha <- ruptureParameters["alpha"]
	for (elementName in c("beta0", "beta1", "sigma0", "sigma1", "rho", "sigmaW",
			"gamma", "alpha"))
		attr(v2[[elementName]], "type") <- "par for aorta model"
	
	# if ("prevalenceDistribution" %in% names(v1other))
	# 	stop("code in generateV2 for dealing with ",
	# 			"v1other$prevalenceDistribution \n has not yet been written")
	
	return(v2)
}

################################################################################

################################################################################
# 3b) Probabilistic sensitivity analysis above diagnosis threshold
# As above, but adapted to work with processPersonsAboveDiagnosticThreshold
################################################################################
psaAboveDiagnosisThreshold <- function(v0, v1other, v1distributions, v2values,threshold=3.0) {
	
	# Set elements of v0 as needed for PSA. 
	v0 <- setUnspecifiedElementsOfv0(v0)  
	v0$returnMeanQuantities <- TRUE
	v0$returnEventHistories <- FALSE
	v0$returnAllPersonsQuantities <- FALSE
	v0$showEventHistories <- FALSE
	v0$verbose <- FALSE  # prevent processPersons from being verbose
	
	# Set unspecified elements of v1other to default values
	v1other <- setUnspecifiedElementsOfv1other(v1other)
	
	# Check the arguments.
	checkArgs(v0=v0, v1other=v1other, v1distributions=v1distributions)
	
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
				expr=generateV2(v1distributions), simplify=FALSE) 
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
				FUN=onePsaIterationAboveDiagnosisThreshold, v0, v1other, v2values, threshold)
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
				v1other, v2values, threshold)
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


onePsaIterationAboveDiagnosisThreshold <- function(psaIterationNumber, v0, v1other, 
		v2values,threshold) {
  cat(paste0("PSA iteration ", psaIterationNumber, "\n"))
  
	# Get v2, the values of the uncertain global variables, and check it.
	if (is.null(v2values)) 
	    stop("INTERNAL ERROR: v2values should be generated in psa ",
				"or passed into psa by the user")
    v2 <- v2values[[psaIterationNumber]]
    if (is.null(v2)) stop("v2values[[", psaIterationNumber, "]] is NULL")
	if (!is.list(v2)) stop("v2values[[", psaIterationNumber,"]] must be a list")
	if (!("probOfNonvisualization" %in% names(v2)))
		stop("v2 must contain probOfNonvisualization (and many other elements)")
	
	processPersonsResult <- processPersonsAboveDiagnosisThreshold(v0, v1other, v2,threshold)
	result <- list(incrementalMeanQuantities=processPersonsResult$incrementalMeanQuantities, v2=v2)

	return(result)
}

################################################################################

################################################################################
# 4) Calculate health-economic quantities
################################################################################
# Create qalyFactors and qalyFactorBoundaries, which will be stored in v1other. 
# Time when you are aged between v1other, $qalyFactorBoundaries[i] and [i+1] 
# will be adjusted by a factor of v1other$qalyFactors[i+1].
createQalyFactors <- function(startAge, 
		qalyFactorBoundariesAsAges=c(25,35,45,55,65,75), 
		qalyFactorsForAges=c(0.94, 0.93, 0.91, 0.84, 0.78, 0.78, 0.75)) {
			
	# Check qalyFactorBoundariesAsAges and qalyFactorsForAges.
	if (length(qalyFactorBoundariesAsAges) + 1 != length(qalyFactorsForAges))
		stop("qalyFactorsForAges must be 1 longer than ",
				"qalyFactorBoundariesAsAges")
	if (!identical(qalyFactorBoundariesAsAges,sort(qalyFactorBoundariesAsAges)))
		stop("qalyFactorBoundariesAsAges must be increasing")
	if (!identical(qalyFactorsForAges, 
			sort(qalyFactorsForAges, decreasing=TRUE)))
		stop("qalyFactorsForAges must be decreasing")
	if (any(qalyFactorBoundariesAsAges != round(qalyFactorBoundariesAsAges)))
		stop("qalyFactorBoundariesAsAges must all be integers")
	
	# Determine qalyFactorBoundaries and qalyFactors. 
	firstBoundaryToUse <- findInterval(startAge, qalyFactorBoundariesAsAges) + 1
	if (firstBoundaryToUse == length(qalyFactorBoundariesAsAges) + 1) {
		qalyFactorBoundaries <- numeric(0)
	} else {
		qalyFactorBoundaries <- qalyFactorBoundariesAsAges[
			firstBoundaryToUse:length(qalyFactorBoundariesAsAges)] - startAge
	}
	qalyFactors <- 
			qalyFactorsForAges[firstBoundaryToUse:length(qalyFactorsForAges)]
	
	# Return qalyFactorBoundaries and qalyFactors.
	return(list(qalyFactorBoundaries=qalyFactorBoundaries, 
			qalyFactors=qalyFactors))
}

# Given an event-history, the costs, and the names of the required 
# quantities, calculate the required health-economic quantities.
calculateHealthEconomicQuantities <- function(
		eventHistory, namesOfQuantities, costs, v1other, v2) {
	
	# Check arguments. 
	if (!inherits(eventHistory, "eventHistory"))
		stop("eventHistory must be an object of class eventHistory")
	costsWithAttrsRemoved <- costs  # (vectors with attributes fail is.vector)
	attributes(costsWithAttrsRemoved) <- NULL
	if (!is.numeric(costs) || is.null(names(costs)) || 
			!is.vector(costsWithAttrsRemoved))
		stop("costs must be a numeric vector with names")
		
	result <- numeric()  
	
	# Calculate lifeYears from time zero to the last event (death or censoring)
	result["lifeYears"] <- eventHistory$times[length(eventHistory$times)]
	
	# Calculate QALYs. 
	result["qalys"] <- adjustLifeYears(result["lifeYears"], 
			qalyFactorBoundaries=v1other$qalyFactorBoundaries, 
			qalyFactors=v2$qalyFactors)
	
	# Calculate cost.
	result["cost"] <- sum(costs[eventHistory$events], na.rm=TRUE)

	# Calculate discounted life-years.
	# The first year is discounted 
	result["discountedLifeYears"] <- adjustLifeYears(result["lifeYears"], 
			discountRate=v1other$lifeYearDiscountRate)
	
	# Calculate discounted QALYs. 
	result["discountedQalys"] <- adjustLifeYears(result["lifeYears"], 
			qalyFactorBoundaries=v1other$qalyFactorBoundaries, 
			qalyFactors=v2$qalyFactors,
			discountRate=v1other$lifeYearDiscountRate)
	
	# Calculate discounted cost.
	result["discountedCost"] <- sum(costs[eventHistory$events] / 
			(1 + v1other$costDiscountRate) ^ eventHistory$times, na.rm=TRUE)	

	if (!identical(names(result), namesOfQuantities))
		stop("INTERNAL ERROR: names(result) should equal namesOfQuantities")
	return(result)
}

# Calculate quality-adjusted or discounted life-years, 
adjustLifeYears <- function(lifeYears, qalyFactorBoundaries=numeric(0), 
		qalyFactors=1, discountPeriod=1, discountRate=0) {
	numberOfDiscountPeriods <- ceiling(lifeYears / discountPeriod)
	discountTimes <- seq_len(numberOfDiscountPeriods - 1) * discountPeriod
	discountFactors <- (1 + discountRate) ^ -(1:numberOfDiscountPeriods)
		
	# Make sortedTimes. 
	sortedTimes <- mergeSortedVectors(qalyFactorBoundaries, discountTimes)
	
	# Calculate intervalLengths.
	intervalLengths <- c(sortedTimes, Inf) - c(0, sortedTimes)
	
	# Calculate numberOfIntervalsToUse. 
	numberOfIntervalsToUse <- findInterval(lifeYears, sortedTimes) + 1

	# Make factorsForEachInterval. 
	factorsForEachInterval <- 
	        matrix(NA_real_, nrow=2, ncol=numberOfIntervalsToUse)
	timesAndFactors <- list(
	        list(times=qalyFactorBoundaries, factors=qalyFactors),
	        list(times=discountTimes, factors=discountFactors))
	for (i in 1:2) { 
	    indexes <- match(timesAndFactors[[i]]$times, sortedTimes)
	    numbersOfCopies <- 
	            c(indexes, 1 + length(sortedTimes)) - c(0, indexes)
	    factorsForEachInterval[i,] <- rep(timesAndFactors[[i]]$factors, 
	            numbersOfCopies)[1:numberOfIntervalsToUse] 
	}
	
	# Make a vector of the intervals that lifeYears consists of. 
	if (numberOfIntervalsToUse == 1) {
	    intervals <- lifeYears 
	} else {
	    intervals <- c(intervalLengths[1:(numberOfIntervalsToUse-1)],
	            lifeYears - sortedTimes[numberOfIntervalsToUse-1])
	}
	
	sum(intervals * factorsForEachInterval[1,] * factorsForEachInterval[2,])
}

################################################################################

################################################################################
# 5) Utilities
################################################################################
# Make a multi-dimensional array that has properly named dimensions etc. and is full 
# of numeric NAs. 
makeArray <- function(..., initialValue) {
	if (missing(initialValue)) initialValue <- NA_real_
	args <- list(...)
	if (length(args)==1) {
		if (!is.list(args[[1]]))
			stop("if there is only one argument apart from initialValue then ", 
					"it must be a list")
		dimNamesList <- args[[1]]
	} else {
		dimNamesList <- args
	}
	array(initialValue, dim=sapply(dimNamesList, length), dimnames=dimNamesList)
}

# Generate FALSE or TRUE with probability pr. 
rbernoulli <- function(pr) {
	as.logical(rbinom(n=1, size=1, prob=pr))
}

# Find whether elements of a vector (apart from NAs) are all different or not. 
allDifferent <- function(x) {
	if (!is.vector(x)) stop("x must be a vector")
	x <- na.omit(x)
	x <- x[is.finite(x)] ## added by MS on 24/07/19 so infinite surveillance intervals can be added if required
	length(x) == length(unique(x))
}

getAnything <- function(x) {
	if (!is.character(x) || length(x) != 1) 
		stop("x must be a single character string")
	if (exists("temporaryVariableForGetAnything", envir=parent.frame()))
		stop("parent.frame() already contains a variable called ",
				"temporaryVariableForGetAnything")
	cat("### getAnything(", x, ")\n", sep="")
	
	separateLetters <- strsplit(x, NULL)[[1]]
	
	if (length(which(separateLetters == "$")) +
			length(which(separateLetters == "[")) > 1)
		stop("x is not allowed to contain more than one $ or [")
	
	if ("$" %in% separateLetters) {
		xElements <- regmatches(x, regexec(
				"^(\\w+)\\$(\\w+)$", x))[[1]][-1]
		assign("temporaryVariableForGetAnything", xElements, 
				envir=parent.frame())
		result <- with(parent.frame(), 
				getElement(get(temporaryVariableForGetAnything[1]), 
				temporaryVariableForGetAnything[2]))
		rm(temporaryVariableForGetAnything, envir=parent.frame())
		return(result)	
		
	} else if ("[" %in% separateLetters) {
		xElements <- regmatches(x, regexec(
				"^(\\w+)\\[\"(\\w+)\"\\]$", x))[[1]][-1]
		assign("temporaryVariableForGetAnything", xElements, 
				envir=parent.frame())
		result <- with(parent.frame(), 
				getElement(get(temporaryVariableForGetAnything[1]), 
				temporaryVariableForGetAnything[2]))
		rm(temporaryVariableForGetAnything, envir=parent.frame())
		return(result)	
		
	} else {
		cat("x does not contain $ or [\n")
		return(get(x, envir=parent.frame()))
	}
}

# A replacement for flexsurv::rgompertz.
myRgompertz <- function(n, shape, rate) {

	if (class(shape) != "numeric" || class(rate) != "numeric" || 
			length(shape) != 1 || length(rate) != 1 || rate <= 0)
		stop("shape and rate must be single numerics, and rate must be positive")
	if (is.na(shape) || is.na(rate))
		stop("shape and rate must not be NA or NaN")
	
	u <- runif(n)
	result <- numeric(n)
	isImmortal <- shape < 0 & u > 1 - exp(rate/shape)
	result[isImmortal] <- Inf
	result[!isImmortal] <- log1p(-shape/rate * log1p(-u[!isImmortal])) / shape
	result
}

# A replacement for rbeta.
myRbeta <- function(n, shape1, shape2) {
	if (R.version$major == "2") {
		shouldBeOne <- shape1==1 & shape2==0
		shouldBeZero <- shape1==0 & shape2==1
		others <- !shouldBeOne & !shouldBeZero
		result <- numeric(n)
		result[shouldBeOne] <- 1
		result[others] <- rbeta(sum(others), shape1[others], shape2[others])
		return(result)
	} else {
		return(rbeta(n, shape1, shape2))
	}
}

# A function to set the random seed whether the subsequent code is going to be
# serial or parallel.
setAndShowRandomSeed <- function(randomSeed, cluster, verbose=TRUE) {
	if (is.null(randomSeed) || is.na(randomSeed)) {
		message <- paste0("Random seed has not been set (because randomSeed=",
				{if (is.null(randomSeed)) "NULL" else randomSeed}, ").")
	} else {
		if (missing(cluster)) {
			set.seed(randomSeed, kind="default") 
			message <- paste0("Random seed has been set by set.seed(", 
					randomSeed, ").")
		} else {
			if (!inherits(cluster, "cluster")) 
				stop("cluster must be a valid cluster")  # for safety
			clusterSetRNGStream(cl=cluster, iseed=randomSeed)
			message <- paste0(
					"Random seed has been set by clusterSetRNGStream(iseed=", 
					randomSeed, ").")
		}
	}
	if (verbose) cat(message, "\n")
}

# Merge two sorted vectors. This assumes that x and y are sorted.
mergeSortedVectors <- function(x, y) {
	if (length(x) == 0) {
		return(y)
	} else if (length(x) == 1) {
		index <- findInterval(x, y)
		firstSection <- y[seq_len(index)]
		lastSection <- y[seq_len(length(y) - index) + index]
		return(c(firstSection, x, lastSection))
	} else {
		indexes <- findInterval(x, y) 
		firstSection <- y[seq_len(indexes[1])]
		middleSection <- unlist(lapply(X=seq_len(length(x)-1), FUN=function(i) { 
				c(x[i], y[seq_len(indexes[i+1] - indexes[i]) + indexes[i]]) }))
		lastSection <- y[seq_len(length(y) - indexes[length(indexes)]) +
				indexes[length(indexes)]]
		return(c(firstSection, middleSection, x[length(x)], lastSection))
	}
}

roundIfNumeric <- function(x, digits) {
	if (is.numeric(type.convert(as.character(x)))) {
		return(round(as.numeric(x), digits=digits))
	} else {
		return(x)
	}
}

# Given extremeValue, which is the max or min value that is going to be plotted 
# on a graph.
roundForAxisRange <- function(extremeValue, sensibleUnit, roundUp) {
	# Check the arguments.
	if (missing(extremeValue) || is.null(extremeValue) || is.na(extremeValue) ||
			!is.numeric(extremeValue) || length(extremeValue) != 1)
		stop("extremeValue must be a single numeric")
	if (missing(sensibleUnit) || is.null(sensibleUnit) || is.na(sensibleUnit) ||
			!is.numeric(sensibleUnit) || length(sensibleUnit) != 1)
		stop("sensibleUnit must be a single numeric")
	if (missing(roundUp) || is.null(roundUp) || is.na(roundUp) ||
			!is.logical(roundUp) || length(roundUp) != 1)
		stop("roundUp must be a single logical/boolean")
	
	# Round extremeValue up to the nearest sensibleUnit 
	if (roundUp) {
		return(ceiling(extremeValue / sensibleUnit) * sensibleUnit)
	} else {
		return(floor(extremeValue / sensibleUnit) * sensibleUnit)
	}
}

# Get a sensible range for plotting values. 
getSensibleRange <- function(values, sensibleUnit, includeZero) {
	# Check the arguments.
	if (missing(values) || is.null(values) || all(is.na(values)) ||
			!is.numeric(values) || length(values) == 0)
		stop("values must be a vector of numerics of length 1 or more")
	if (missing(sensibleUnit) || is.null(sensibleUnit) || is.na(sensibleUnit) ||
			!is.numeric(sensibleUnit) || length(sensibleUnit) != 1)
		stop("sensibleUnit must be a single numeric")
	if (missing(includeZero) || is.null(includeZero) || is.na(includeZero) ||
			!is.logical(includeZero) || length(includeZero) != 1)
		stop("includeZero must be a single logical/boolean")
	
	if (includeZero) {
		minValue <- min(0, 
				roundForAxisRange(min(values), sensibleUnit, roundUp=FALSE))
		maxValue <- max(0, 
				roundForAxisRange(max(values), sensibleUnit, roundUp=TRUE))
	} else {
		minValue <- roundForAxisRange(min(values), sensibleUnit, roundUp=FALSE)
		maxValue <- roundForAxisRange(max(values), sensibleUnit, roundUp=TRUE)
	}
	return(c(minValue, maxValue))
}

getFirstChar <- function(string) { substr(string, 1, 1) }

hasNames <- function(x) {
	return(!is.null(names(x)))
}

checkIsSingleNumeric <- function(x, insistOnNoName=FALSE) {
	if (!isSingleNumeric(x)) {
		cat("x must be a single numeric, but x=")
		print(x)
		stop("x must be a single numeric; ",
				"use traceback() to find the problem")
	}
	if (insistOnNoName && hasNames(x))
		stop("x must be unnamed, but names(x)=", names(x))
}

isSingleNumeric <- function(x) {
	!missing(x) && !is.null(x) && length(x) == 1 && is.numeric(x) && !is.na(x)
}

isMultipleNumeric <- function(x) {
  !missing(x) && !is.null(x) && length(x) > 1 && is.numeric(x) && !is.na(x)
}

getLowerQuantile <- function(ciPercent) {
	if (missing(ciPercent) || is.null(ciPercent) || is.na(ciPercent) || 
			!is.numeric(ciPercent) || length(ciPercent) != 1 || 
			ciPercent < 1 || ciPercent > 100)
		stop("ciPercent must be a percentage, e.g. 95 (and greater than 1)")
	(100 - ciPercent) / 200
}

getUpperQuantile <- function(ciPercent) {
	1 - getLowerQuantile(ciPercent)
}

# A function for generating a time to non-AAA death when a person is contraindicated. 
generateTimeToNonAaaDeathFromContraindication <- function(
		rateOfNonAaaDeathAfterContraindication, v3) {
  qexp(v3[["rateOfNonAaaDeathAfterContraindication"]], rate=rateOfNonAaaDeathAfterContraindication)
}

# A function for generating a dropout time.
generateDropoutTime <- function(monitoringStartTime, rateOfDropoutFromMonitoring, v3) {
	time <- monitoringStartTime + qexp(v3[["rateOfDropoutFromMonitoring"]][1], rate=rateOfDropoutFromMonitoring)
	v3[["rateOfDropoutFromMonitoring"]] <- v3[["rateOfDropoutFromMonitoring"]][-1]
	return(list(time = time, v3 =v3))
}

# Set elements of v0 to default values, if they have not already been specified in v0. 
setUnspecifiedElementsOfv0 <- function(v0) {
  # Create a list that contains the default values. 
	defaults <- list(
			treatmentGroups=c("noScreening", "screening"), ## THIS SHOULD NEVER BE CHANGED AS CURRENTLY CODE DOES NOT WORK WITH ANY OTHER OPTIONS
			namesOfQuantities=c("lifeYears", "qalys", "cost", 
					"discountedLifeYears", "discountedQalys", "discountedCost"),
			showEventHistories=FALSE,
			returnMeanQuantities=TRUE,
			returnEventHistories=TRUE,
			returnAllPersonsQuantities=FALSE,
			recordSizes=TRUE,
			method="serial",  
			verbose=TRUE,
			randomSeed=2,
			numberOfPersons=1e3,
			numberOfParameterIterations=5,
			numberOfProcesses=detectCores()-1
	)
	for (i in 1:length(defaults)) {
		varName <- names(defaults)[i]
		if (!(varName %in% names(v0)))
			v0[[varName]] <- defaults[[i]]
	}
	if (v0$method != "serial" && !("numberOfProcesses" %in% names(v0)))
		v0$numberOfProcesses <- getRecommendedNumberOfProcesses()
	return(v0)
}

# Set elements of v1other to default values, if they have not already been specified in v1other. 
setUnspecifiedElementsOfv1other <- function(v1other) {
  # Create a list that contains the default values. 
  defaults <- list(
    waitingTimeToInvitation = 0
  )
  for (i in 1:length(defaults)) {
    varName <- names(defaults)[i]
    if (!(varName %in% names(v1other)))
      v1other[[varName]] <- defaults[[i]]
  }

  return(v1other)
}

# Get the recommended number of processes. 
getRecommendedNumberOfProcesses <- function() {
  return(20)  
}

# Get all functions in the global environment. 
getAllFunctionsAndStringsInGlobalEnv <- function() {
	result <- character()
	for (objectName in ls(envir=globalenv())) {
		obj <- get(objectName)
		if (is.function(obj) || is.character(obj))
			result <- c(result, objectName)
	}
	return(result)
}

# Convert from a three-month transition probability to a rate.
convertThreeMonthProbToRate <- function(prob) {
	-4 * log(1 - prob)
}

# Save output from processPersons and psa.
saveMainAndPsaObjects <- function(processPersonsObjectsToSave, 
		psaObjectsToSave, runIdentifier, extraText) {
	# Check runIdentifier and create fileName. 
	if (missing(runIdentifier)) stop("runIdentifier must be given")
	fileName <- file.path("output", paste0(runIdentifier, "_",  
			{if (!missing(extraText)) paste0(extraText, "_")},
			format(Sys.time(), "%Y%m%d_%H%M%S"), ".RData"))

	processPersonsResult <- processPersonsObjectsToSave$result
	v0 <- processPersonsObjectsToSave$v0
	v1other <- processPersonsObjectsToSave$v1other
	v2 <- processPersonsObjectsToSave$v2
	
	savePsaObjectsToo <- 
			!missing(psaObjectsToSave) && !is.null(psaObjectsToSave) 
	if (savePsaObjectsToo) psaResult <- psaObjectsToSave$psaResult
	
	objectWhoseSizeToTest <- list(processPersonsObjectsToSave, 
			{if (savePsaObjectsToo) psaObjectsToSave else NULL})
	saveSmallerFileToo <- 
			object.size(objectWhoseSizeToTest) > 5e8 &&  
			("eventHistories" %in% names(processPersonsResult) || 
			"allPersonsQuantities" %in% names(processPersonsResult) ||
			(savePsaObjectsToo && "eventHistoryLists" %in% names(psaResult)))
	if (saveSmallerFileToo) 

		smallerFileName <- sub("\\.RData$", "_smaller\\.RData", fileName)
		
	if (!savePsaObjectsToo) {
		namesOfObjectsToSave <- c("processPersonsResult", "v0", "v1other")

	} else {
		v0_usedByProcessPersons <- processPersonsObjectsToSave$v0
		v0_usedByPsa <- psaObjectsToSave$v0
		v1other_usedByProcessPersons <- processPersonsObjectsToSave$v1other
		v1other_usedByPsa <- psaObjectsToSave$v1other
		
		if (identical(v1other_usedByProcessPersons, v1other_usedByPsa)) {
			v1other <- v1other_usedByProcessPersons
			namesOfObjectsToSave <- c("processPersonsResult", "psaResult", 
					"v0_usedByProcessPersons", "v0_usedByPsa", "v1other")
		} else {
			cat("WARNING: processPersons and psa used different v1other.\n")
			namesOfObjectsToSave <- c("processPersonsResult", "psaResult", 
					"v0_usedByProcessPersons", "v0_usedByPsa", 
					"v1other_usedByProcessPersons", "v1other_usedByPsa")
		}
		
		v1distributions <- psaObjectsToSave$v1distributions
		namesOfObjectsToSave <- c(namesOfObjectsToSave, "v1distributions")
	} 
	
	namesOfObjectsToSave <- c(namesOfObjectsToSave, "v2")
	save(list=namesOfObjectsToSave, file=fileName)
	if (saveSmallerFileToo) {
		processPersonsResult$eventHistories <- NULL
		processPersonsResult$allPersonsQuantities <- NULL
		save(list=namesOfObjectsToSave, file=smallerFileName)
	}
	
	cat("Saved processPersons ", if (savePsaObjectsToo) "and psa ", 
			"output to ", fileName, "\n", sep="")
	if (saveSmallerFileToo) {
		cat("Also saved smaller file: ", smallerFileName, "\n", sep="")
		cat("The two files are identical except that the smaller one does not",
				"\n contain large elements such as ",
				"processPersonsResult$eventHistories.\n", sep="")
	}

}

# Functions for dealing with "type" attributes. 
getType <- function(x) {
	result <- attr(x, "type")
	if (is.null(result)) stop("attr(x, \"type\") is NULL")
	return(result)
}

setType <- function(x, typeAttr) {
	if (missing(typeAttr)) stop("typeAttr must not be missing")
	attr(x, "type") <- typeAttr
	return(x)
}

#
namesOfProbVarsForSurvivalModel <- 
		c("probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgery",
		"probOfAaaDeathInInitialPeriodAfterElectiveEvarSurgery",
		"probOfAaaDeathInInitialPeriodAfterEmergencyOpenSurgery",
		"probOfAaaDeathInInitialPeriodAfterEmergencyEvarSurgery")

# Change letter case. 
changeFirstLetterToLowerCase <- function(string) {  # e.g. XxxYyy
  firstLetter <- substr(string, 1, 1)
  rest <- substr(string, 2, nchar(string))
  paste0(tolower(firstLetter), rest)             # e.g. xxxYyy
}

changeFirstLetterToUpperCase <- function(string) {  # e.g. xxxYyy
  firstLetter <- substr(string, 1, 1)
  rest <- substr(string, 2, nchar(string))
  paste0(toupper(firstLetter), rest)             # e.g. XxxYyy
}

################################################################################
# Display a number of seconds in a more readable form. 
displayTime <- function(t) {
  days <- floor(t / 86400)
  t2 <-  (t %% 86400)
  hours <- floor(t2 / 3600)
  t2 <- t2 %% 3600
  minutes <- floor(t2 / 60)
  seconds <- t2 %% 60
  
  result <- ""
  if (days > 0)
    result <- paste0(days, " days, ")
  if (days > 0 || hours > 0)
    result <- paste0(result, hours, " hours, ")
  if (days > 0 || hours > 0 || minutes > 0)
    result <- paste0(result, minutes, " minutes, ")
  if (t >= 60) {
    result <- paste0(result, sprintf("%.2f",seconds), " seconds")
    result <- paste0(result, " (", t, " seconds)")
  } else {
    result <- paste0(result, sprintf("%.2f",seconds), " seconds")
  }
  result
}

################################################################################

################################################################################
# 6) Check arguments 
# This is used by processPersons, raising errors.
################################################################################
checkArgs <- function(v0, v1other, v1distributions, v2) {
	# v0 and v1other must be given, but v1distributions and v2 can be missing.
	if (missing(v0) || missing(v1other))
		stop("v0 and v1other must not be missing")
	checkV0(v0)
	checkV0andV1other(v0, v1other)
	checkV1other(v1other)
	if (!missing(v1distributions)) checkV1distributions(v1distributions)
	if (!missing(v2)) {
		checkV2(v2)
		checkV1otherAndV2(v1other, v2)
	}
}

checkV0 <- function(v0) {
	if (!any(v0$returnMeanQuantities, v0$returnEventHistories, 
			v0$returnAllPersonsQuantities))
		stop("v0$return... variables are all FALSE; nothing will be returned\n")
	
	# Check v0$method. 
	if (!(v0$method %in% c("serial", "parallel", "foreach", "parallelBatches")))
		stop("method has to be one of serial, parallel, foreach, and ",
				"parallelBatches")
	
	# Check v0$numberOfParameterIterations.
	if ("numberOfParameterIterations" %in% names(v0) && 
			(!is.numeric(v0$numberOfParameterIterations) || 
				length(v0$numberOfParameterIterations) != 1 ||
				v0$numberOfParameterIterations < 1))
		stop("v0$numberOfParameterIterations must be a positive integer")
}

checkV0andV1other <- function(v0, v1other) {
	# Check that the necessary file-names etc. have been supplied for
	# nonAaaSurvProbs or nonAaaMortalityRates. 
	
	if (v1other$nonAaaDeathMethod == "mass") {

	} else if (v1other$nonAaaDeathMethod == "onsIntegerStart") { 
		if (any(!(c("startAge","nonAaaMortalityRatesFileName") %in% 
							names(v1other))))
			stop("v1other$nonAaaDeathMethod is onsIntegerStart, so ", 
					"v1other must contain startAge and nonAaaMortalityRatesFileName")
		
	} else if (v1other$nonAaaDeathMethod == "onsNonintegerStart") {
		if (!("nonAaaMortalityRatesFileName" %in% names(v1other)) || 
				!("generateAgeAtBaseline" %in% names(v0)))
			stop("v1other$nonAaaDeathMethod is onsNonintegerStart, so ", 
					"v1other must contain nonAaaMortalityRatesFileName and\n",
					"v0 must contain generateAgeAtBaseline")
		
	} else {
		stop("v1other$nonAaaDeathMethod=", v1other$nonAaaDeathMethod, 
				" is illegal")
	}
	
}

checkV1other <- function(v1other) {
	# Check v1other$aortaDiameterThresholds and v1other$monitoringIntervals.
	if (length(v1other$aortaDiameterThresholds) != 
			length(v1other$monitoringIntervals)) ## updated MS 07/02/19
		stop("v1other$aortaDiameterThresholds must be the same length as v1other$monitoringIntervals. 
		     The first element of v1other$monitoringIntervals should be the monitoring interval for AAA in surveillance who drop below first diameter threshold")
	if (!identical(v1other$aortaDiameterThresholds, 
			sort(v1other$aortaDiameterThresholds)))
		stop("v1other$aortaDiameterThresholds must be in increasing order")
	if (!identical(v1other$monitoringIntervals, 
			sort(v1other$monitoringIntervals, decreasing=TRUE)))
		stop("v1other$monitoringIntervals must be in decreasing order")
	
  # Check that v1other$maxNumberMonitor exists
  if(!("maxNumberMonitor" %in% names(v1other)))
    stop("v1other$maxNumberMonitor must be specified.
         This gives the maximum number of scans a patient receives for each size category, including below first diameter threshold.
         Set to Inf for unlimited numbers of scans during lifetime.")
  
  # Check length of v1other$maxNumberMonitor
  if (length(v1other$maxNumberMonitor) != 
      length(v1other$monitoringIntervals)) 
    stop("v1other$maxNumberMonitor must be the same length as v1other$monitoringIntervals. 
         The first element of v1other$maxNumberMonitor should be the number of possible scans when patient drops below first diameter threshold")
  
	# Check v1other$zeroGrowthDiameterThreshold, if it exists. 
	if ("zeroGrowthDiameterThreshold" %in% names(v1other) &&
			(!is.numeric(v1other$zeroGrowthDiameterThreshold) || 
				length(v1other$zeroGrowthDiameterThreshold) != 1 || 
				v1other$zeroGrowthDiameterThreshold < 0))
		stop("v1other$zeroGrowthDiameterThreshold must be a single ",
				"non-negative number, if it exists")
	
	# Check the reintervention time-boundaries. 
	for (varName in c("reinterventionTimeBoundariesAfterElectiveOpen",
			"reinterventionTimeBoundariesAfterElectiveEvar",
			"reinterventionTimeBoundariesAfterEmergencyOpen",
			"reinterventionTimeBoundariesAfterEmergencyEvar")) {
		v1element <- v1other[[varName]]
		if (is.null(v1element) || !is.numeric(v1element) || 
				any(is.na(v1element)) || any(v1element < 0) || 
				!identical(sort(v1element), v1element))
			stop("v1other$", varName, " must be a vector of positive numerics ",
					"in increasing order")
	}
	
	# Check v1other$postSurgeryInitialPeriod.
	if (!isSingleNumeric(v1other$postSurgeryInitialPeriod))
		stop("v1other$postSurgeryInitialPeriod must be a single numeric")
	
	# Check times of post-surgery monitoring. 
	for (varName in c("timeToMonitoringFollowingOpenSurgery",
			"timeBetweenMonitoringFollowingEvarSurgery")) {
		v1element <- v1other[[varName]]
		if (is.null(v1element)) 
			stop("v1other$", varName, " must not be NULL")
		if (is.na(v1element)) next  # it is OK for these to be NA
		if (!is.numeric(v1element) || length(v1element) != 1 || v1element < 0)
			stop("v1other$", varName, " must be a single numeric (or NA)")
	}
}

checkV1distributions <- function(v1distributions) {
	# Check v1distributions$extraDiameterForCtScan. 
	if (!("extraDiameterForCtScan" %in% names(v1distributions)))
		stop("v1distributions must contain extraDiameterForCtScan")
	
	# Check the elements of v1distributions. 
	for (v1elementName in names(v1distributions)) {
	  # Get v1element and type, check v1element is not NA.
		v1element <- v1distributions[[v1elementName]]
		if (any(is.na(v1element))) {
			cat("\nv1distributions$", v1elementName, ":\n", sep="")
			print(v1element)
			stop("v1distributions$", v1elementName, " is illegal")
		}
		type <- attr(v1element, "type")
		if (is.null(type)) 
			stop("the \"type\" element of v1distributions$", v1elementName, 
					" is NULL")
		
		# Check that v1element is legal. 
		raiseV1TypeRelatedError <- function(v1elementName) {
			cat("\nv1distributions$", v1elementName, ":\n", sep="")
			print(v1element)
			stop("v1distributions$", v1elementName, " is illegal in some way")
		}
		if (type == "fixed value" | type == "fixed value for qaly") {
			if (is.na(v1element) || length(v1element) == 0)
				raiseV1TypeRelatedError(v1elementName)
			
		} else if (type == "beta pars for probability") {
			if (!is.list(v1element) || 
					!identical(names(v1element), c("alpha", "beta")) ||
					!isSingleNumeric(v1element$alpha) || 
					!isSingleNumeric(v1element$beta))
				raiseV1TypeRelatedError(v1elementName)
			
		} else if (type == "hyperpars for logistic model for probability") {

			# Check it is a list of length 2 or 3. 
			if (!is.list(v1element) || 
					length(v1element) < 2 || length(v1element) > 3)
				raiseV1TypeRelatedError(v1elementName)
			# Check the 1st name is mean and the 2nd is variance or covariance.
			if (names(v1element)[[1]] != "mean" || 
					!(names(v1element)[[2]] %in% c("variance", "covariance")))
				raiseV1TypeRelatedError(v1elementName)
			# Check the 3rd name, if it exists, is logOddsAdjustment.
			if (length(v1element) == 3 && names(v1element)[[3]] != 
					"logOddsAdjustment")
				raiseV1TypeRelatedError(v1elementName)
			
			# Check the first two elements are numeric & the first is a vector.
			if (!is.numeric(v1element$mean) || !is.vector(v1element$mean) ||
					!is.numeric(v1element[[2]]))
				raiseV1TypeRelatedError(v1elementName)
			
			if (length(v1element$mean) == 1) {
				if (names(v1element)[[2]] != "variance" || 
						!isSingleNumeric(v1element$mean) || 
						!isSingleNumeric(v1element$variance))
					raiseV1TypeRelatedError(v1elementName)
				
			} else {
				if (names(v1element)[[2]] != "covariance" || 
						!is.matrix(v1element$covariance) || 
						!is.numeric(v1element$covariance) ||
						length(v1element$mean) != nrow(v1element$covariance) ||
						!identical(names(v1element$mean), 
								rownames(v1element$covariance)) ||
						!identical(names(v1element$mean), 
								colnames(v1element$covariance)))
					raiseV1TypeRelatedError(v1elementName)
			}
	
			# Check logOddsAdjustment, if it exists. 
			if (length(v1element) == 3 && 
					!isSingleNumeric(v1element$logOddsAdjustment))
				raiseV1TypeRelatedError(v1elementName)
	
		} else if (type == "gamma pars for rate") {
			# It must be a list with elements shape and scale (single numerics).
			if (!is.list(v1element) || 
					!identical(names(v1element), c("shape", "scale")) ||
					!isSingleNumeric(v1element$shape) || 
					!isSingleNumeric(v1element$scale))
				raiseV1TypeRelatedError(v1elementName)
		  
		} else if (type == "gamma pars for multiple rates") {
		  # It must be a list with elements shapes and scales.
		  if (!is.list(v1element) || 
		      !identical(names(v1element), c("shapes", "scales")) ||
		      !isMultipleNumeric(v1element$shapes) || 
		      !isMultipleNumeric(v1element$scales))
		    raiseV1TypeRelatedError(v1elementName)
			
		} else if (type == "pars for betaThenConvertThreeMonthProbToRate") {
			# It must be a list with elements alpha and beta (single numerics).
			if (!is.list(v1element) || 
					!identical(names(v1element), c("alpha", "beta")) ||
					!isSingleNumeric(v1element$alpha) || 
					!isSingleNumeric(v1element$beta))
				raiseV1TypeRelatedError(v1elementName)
			
		} else if (type == "pars for gammaThenMultiplyByFour") {
			# It must be a list with elements shape and scale (single numerics).
			if (!is.list(v1element) || 
					!identical(names(v1element), c("shape", "scale")) ||
					!isSingleNumeric(v1element$shape) || 
					!isSingleNumeric(v1element$scale))
				raiseV1TypeRelatedError(v1elementName)
			
		} else if (type == "hyperpars for aorta model") {
			# It must be either a vector or matrix with dimension 2 or 6.
			if (!(is.numeric(v1element) && length(v1element) %in% c(2,6)) && 
					!(is.matrix(v1element) && nrow(v1element) %in% c(2,6)))
				raiseV1TypeRelatedError(v1elementName)
			if (length(v1element) %in% c(2,6) && !hasNames(v1element))
				stop("if v1element is a vector then it must have names")
			
		} else if (type == "fixed value for probability") {
			# It must be a single numeric.
			if (!isSingleNumeric(v1element))
				raiseV1TypeRelatedError(v1elementName)
			
		} else if (type == "truncated normal distribution") {
		  # It must be a list with elements mean and variance (single numerics).
		  if (!is.list(v1element) || 
		      !identical(names(v1element), c("mean", "variance")) ||
		      !isSingleNumeric(v1element$mean) || 
		      !isSingleNumeric(v1element$variance))
		    raiseV1TypeRelatedError(v1elementName)
		  
		} else if (type == "multivariate normal distribution") {
		  # Check the 1st name is mean and the 2nd is variance or covariance.
		  if (names(v1element)[[1]] != "mean" || 
		      !(names(v1element)[[2]] %in% c("variance", "covariance")))
		    raiseV1TypeRelatedError(v1elementName)
		  
		  # It must be a list with elements mean and variance (non single numerics).
		  if (!is.list(v1element) || 
		      !identical(names(v1element), c("mean", "variance")) ||
		      isSingleNumeric(v1element$mean) || 
		      isSingleNumeric(v1element$variance))
		    raiseV1TypeRelatedError(v1elementName)
		  
		} else if (type == "fixed value for costs") {
		
		  	
		} else if (type == "distribution for costs") {
		  # It must be a list with elements mean (multiple numerics) and variance (multiple numerics).
		  if (!is.list(v1element) || 
		      !identical(names(v1element), c("mean", "variance")) 
		      )
		    raiseV1TypeRelatedError(v1elementName)
		  
		} else if (type == "fixed value for reintervention rates") {
			if (!is.numeric(v1element) || any(v1element < 0))
				raiseV1TypeRelatedError(v1elementName)

		} else if (type == "fixed value for rate") {
		  if (!is.numeric(v1element) || any(v1element < 0))
		    raiseV1TypeRelatedError(v1elementName)
		
		} else if (type == "normal distribution for logit prevalence") {
		  # It must be a list with elements mean and variance (single numerics).
		  if (!is.list(v1element) || 
		      !identical(names(v1element), c("mean", "variance")) ||
		      !isSingleNumeric(v1element$mean) || 
		      !isSingleNumeric(v1element$variance))
		    raiseV1TypeRelatedError(v1elementName)
		  
		} else {
			stop("type=", {if (is.null(type)) "NULL" else type}, 
					" is illegal; v1elementName=", v1elementName)
		}
	}
}

checkV2 <- function(v2) {
	# Check the elements of v2.
	
	for (v2elementName in names(v2)) {
		# Get v2element and type. 
		v2element <- v2[[v2elementName]]
		if (any(is.na(v2element))) {
			cat("\v2$", v2elementName, ":\n", sep="")
			print(v2element)
			stop("v2$", v2elementName, " is illegal")
		}
		type <- attr(v2element, "type")
		if (is.null(type)) 
			stop("the \"type\" element of v2$", v2elementName, " is NULL")
		
		raiseV2TypeRelatedError <- function(v2elementName) {
			cat("\nv2$", v2elementName, ":\n", sep="")
			print(v2element)
			stop("v2$", v2elementName, " is illegal in some way")
		}
		
		if (type == "fixed value") {
			if (is.na(v2element) || length(v2element) == 0)
				raiseV2TypeRelatedError(v2elementName)
			
		} else if (type == "probability") {
			if (!isSingleNumeric(v2element) || v2element < 0 || v2element > 1)
				raiseV2TypeRelatedError(v2elementName)
			
		} else if (type == "logistic model for probability") {
			if (!is.numeric(v2element) || any(is.na(v2element)) || 
					!hasNames(v2element))
				raiseV2TypeRelatedError(v2elementName)
			minimumLength <- { if (names(v2element)[length(v2element)] == 
					"logOddsAdjustment") 2 else 1 }
			if (length(v2element) < minimumLength) 
				raiseV2TypeRelatedError(v2elementName)
			# Check that logOddsAdjustment does not appear before the last name.
			if ("logOddsAdjustment" %in% names(v2element) && match(
					"logOddsAdjustment", names(v2element)) < length(v2element))
				raiseV2TypeRelatedError(v2elementName)
			
		} else if (type == "rate") {
			if (!isSingleNumeric(v2element) || v2element < 0)
				raiseV2TypeRelatedError(v2elementName)
			
		} else if (type == "par for aorta model") {
			# It must be a single numeric.
			if (!isSingleNumeric(v2element))
				raiseV2TypeRelatedError(v2elementName)
			
		} else if (type == "costs") {
			# It must be a vector of named numerics.
			if (!is.numeric(v2element) || !hasNames(v2element) || 
					length(v2element) < 9)
				raiseV2TypeRelatedError(v2elementName)
			
		} else if (type == "reintervention rates") {
			# It must be a vector of non-negative numerics. 
			if (!is.numeric(v2element) || any(v2element < 0))
				raiseV2TypeRelatedError(v2elementName)
			
		} else if (type == "qaly") {
		  # It must be a vector of non-negative numerics. 
		  if (!is.numeric(v2element) || any(v2element < 0))
		    raiseV2TypeRelatedError(v2elementName)
		  
		}	else {
			stop("type=", {if (is.null(type)) "NULL" else type}, 
					" is illegal; v2elementName=", v2elementName)
		}
	}
	
	# Check that v2 contains certain elements. 
	requiredElementsOfV2 <- c(
			"probOfRequireReinvitation", "probOfAttendScreen",
			"probOfNonvisualization", "probOfContraindication",
			"probOfEmergencySurgeryIfRupture", 
			"probOfElectiveSurgeryIsOpen",
			"probOfEmergencySurgeryIsOpen",
			"rateOfDropoutFromMonitoring", "rateOfIncidentalDetection", 
			"beta0", "beta1", "sigma0", "sigma1", "rho", "sigmaW", 
			"gamma", "alpha", 
			"costs", "ctMeasurementErrorSD")
	for (varName in requiredElementsOfV2)
		if (!(varName %in% names(v2)) || is.na(v2[[varName]]))
			stop("v2 must contain ", varName, " and it must not be NA")
	
	# Check reintervention rates. 
	for (varName in c("reinterventionRatesAfterElectiveOpen",
			"reinterventionRatesAfterElectiveEvar",
			"reinterventionRatesAfterEmergencyOpen",
			"reinterventionRatesAfterEmergencyEvar")) {
		v2element <- v2[[varName]]
		if (is.null(v2element) || !is.numeric(v2element) || 
				any(is.na(v2element) || any(v2element < 0))) 
			stop("v2element$", varName, " must be a non-negative numeric vector")
	}

}

checkV1otherAndV2 <- function(v1other, v2) {
	
	if (!("electiveSurgeryAaaDeathMethod" %in% names(v1other)))
		stop("v1other must contain an electiveSurgeryAaaDeathMethod element")
	if (!("emergencySurgeryAaaDeathMethod" %in% names(v1other)))
		stop("v1other must contain an emergencySurgeryAaaDeathMethod element")
	
	v2elementsForPostElectiveSurgeryInstantDeathOnly <- 
			c("probOfDieFromElectiveSurgeryViaScreeningDetection", 
			"probOfDieFromElectiveSurgeryViaIncidentalDetection")
	v2containsElementsForPostElectiveSurgeryInstantDeathOnly <- 
			v2elementsForPostElectiveSurgeryInstantDeathOnly %in% names(v2)
	v2elementsForPostElectiveSurgerySurvivalModel <- c( 
			"probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgery",
			"probOfAaaDeathInInitialPeriodAfterElectiveEvarSurgery",
			"rateOfAaaDeathAfterElectiveOpenSurgeryAndInitialPeriod",
			"rateOfAaaDeathAfterElectiveEvarSurgeryAndInitialPeriod")
	v2containsElementsForPostElectiveSurgerySurvivalModel <- 
			v2elementsForPostElectiveSurgerySurvivalModel %in% names(v2)
	
	if (v1other$electiveSurgeryAaaDeathMethod == "instantDeathOnly") {

		if (!all(v2containsElementsForPostElectiveSurgeryInstantDeathOnly))
			stop("v1other$electiveSurgeryAaaDeathMethod=", 
					v1other$electiveSurgeryAaaDeathMethod, 
					";\n and v2 does not contain all the required elements")
		if (any(v2containsElementsForPostElectiveSurgerySurvivalModel))
			stop("v1other$electiveSurgeryAaaDeathMethod=", 
					v1other$electiveSurgeryAaaDeathMethod, 
					";\n v2 contains elements that it should not contain")
		if (v2$probOfElectiveSurgeryIsOpen != 1)
			stop("v1other$electiveSurgeryAaaDeathMethod=", 
					v1other$electiveSurgeryAaaDeathMethod, ",\n so ",
					"v2$probOfElectiveSurgeryIsOpen",
					"\n should be 1, but it is",
					v2$probOfElectiveSurgeryIsOpen)
		
	} else if (v1other$electiveSurgeryAaaDeathMethod == "survivalModel") {

		if (!all(v2containsElementsForPostElectiveSurgerySurvivalModel))
			stop("v1other$electiveSurgeryAaaDeathMethod=", 
					v1other$electiveSurgeryAaaDeathMethod, 
					";\n and v2 does not contain all the required elements")
		if (any(v2containsElementsForPostElectiveSurgeryInstantDeathOnly))
			stop("v1other$electiveSurgeryAaaDeathMethod=", 
					v1other$electiveSurgeryAaaDeathMethod, 
					";\n v2 contains elements that it should not contain")
		# Check that the elements all have legal types.
		for (varName in v2elementsForPostElectiveSurgerySurvivalModel) { 
			type <- getType(v2[[varName]])
			if (!(type %in%
					c("probability", "logistic model for probability", "rate")))
				stop("v2$", varName, " has an illegal type: ", type)
		}
		
	} else {
		stop("v1other$electiveSurgeryAaaDeathMethod=", 
				v1other$electiveSurgeryAaaDeathMethod, " is illegal")
	}
	
	v2elementsForPostEmergencySurgeryInstantDeathOnly <- 
			"probOfDieFromEmergencySurgery"
	v2containsElementsForPostEmergencySurgeryInstantDeathOnly <- 
			v2elementsForPostEmergencySurgeryInstantDeathOnly %in% names(v2)
	v2elementsForPostEmergencySurgerySurvivalModel <- c( 
			"probOfAaaDeathInInitialPeriodAfterEmergencyOpenSurgery",
			"probOfAaaDeathInInitialPeriodAfterEmergencyEvarSurgery",
			"rateOfAaaDeathAfterEmergencyOpenSurgeryAndInitialPeriod",
			"rateOfAaaDeathAfterEmergencyEvarSurgeryAndInitialPeriod")
	v2containsElementsForPostEmergencySurgerySurvivalModel <- 
			v2elementsForPostEmergencySurgerySurvivalModel %in% names(v2)
	
	if (v1other$emergencySurgeryAaaDeathMethod == "instantDeathOnly") {

		if (!all(v2containsElementsForPostEmergencySurgeryInstantDeathOnly))
			stop("v1other$emergencySurgeryAaaDeathMethod=", 
					v1other$emergencySurgeryAaaDeathMethod, 
					";\n and v2 does not contain all the required elements")
		if (any(v2containsElementsForPostEmergencySurgerySurvivalModel))
			stop("v1other$emergencySurgeryAaaDeathMethod=", 
					v1other$emergencySurgeryAaaDeathMethod, 
					";\n v2 contains elements that it should not contain")
	  if (v2$probOfEmergencySurgeryIsOpen != 1)
	    stop("v1other$emergencySurgeryAaaDeathMethod=", 
	         v1other$emergencySurgeryAaaDeathMethod, ",\n so ",
	         "v2$probOfEmergencySurgeryIsOpen",
	         "\n should be 1, but it is",
	         v2$probOfEmergencySurgeryIsOpen)
	  
	} else if (v1other$emergencySurgeryAaaDeathMethod == "survivalModel") {

		if (!all(v2containsElementsForPostEmergencySurgerySurvivalModel))
			stop("v1other$emergencySurgeryAaaDeathMethod=", 
					v1other$emergencySurgeryAaaDeathMethod, 
					";\n and v2 does not contain all the required elements")
		if (any(v2containsElementsForPostEmergencySurgeryInstantDeathOnly))
			stop("v1other$emergencySurgeryAaaDeathMethod=", 
					v1other$emergencySurgeryAaaDeathMethod, 
					";\n v2 contains elements that it should not contain")
	  # Check that the elements all have legal types.
	  for (varName in v2elementsForPostEmergencySurgerySurvivalModel) { 
	    type <- getType(v2[[varName]])
	    if (!(type %in%
	          c("probability", "logistic model for probability", "rate")))
	      stop("v2$", varName, " has an illegal type: ", type)
	  }
	    
	  	} else {
		stop("v1other$emergencySurgeryAaaDeathMethod=", 
				v1other$emergencySurgeryAaaDeathMethod, " is illegal")
	}

}

checkArrayOfMeansFirstDimNames <- function(arrayOfMeans) {

	if (identical(dimnames(arrayOfMeans)[[1]], c("screening", "noScreening")))
		stop("dimnames(arrayOfMeans)[[1]] is the wrong way round. It ",
			"should be \n c(\"noScreening\", \"screening\").",
			" (getIncrementalCostOrEffectiveness assumes\n that the first ",
			"\"row\" is the standard treatment/scenario, e.g. noScreening, ",
			"\n and the second is the novel one, e.g. screening.)")
}

# Check rates and timeBoundaries.
checkReinterventionRatesAndTimeBoundaries <- function(rates, timeBoundaries) {

	if (is.null(timeBoundaries) || !is.numeric(timeBoundaries) || 
			any(is.na(timeBoundaries)) || any(timeBoundaries < 0) || 
			!identical(sort(timeBoundaries), timeBoundaries))
		stop("timeBoundaries=", { if (is.null(timeBoundaries)) "NULL" else 
				paste(timeBoundaries, collapse=",") },
				" must be a non-negative numeric vector in ascending order")
	
	if (is.null(rates) || !is.numeric(rates) || any(is.na(rates)) || 
			any(rates < 0))
		stop("rates=", { if (is.null(rates)) "NULL" else 
				paste(rates, collapse=",") }, 
				" must be a non-negative numeric vector")
	
	if (length(timeBoundaries) + 1 != length(rates))
		stop("length(timeBoundaries) must be 1 less than length(rates)")	
}

################################################################################

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
				eventHistory$events)] >= v1other$aortaDiameterThresholds[1]) {

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
