# Auxiliary functions for use in DES model

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
processOnePair <- function(personNumber, v0, v1other, v2, personData) {
  #cat(personNumber, "\n")
  
  # Make result, which will be returned from this function. 
  result <- list(personQuantities=
                   sapply(v0$treatmentGroups, function(x) NULL)) 
  if (v0$returnEventHistories) result$eventHistories <- 
    sapply(v0$treatmentGroups, function(x) NULL)
  
  # Generate characteristics and natural events. 
  # First sample a row from the data file
  dataItem <- personData[sample.int(dim(personData)[1], 1, prob=personData$prob),,drop=F]
  #dataItem <- personData[1,,drop=F]
  
  y0 <- dataItem$baselineDiameter
  aortaGrowthParameters <- generatePersonAortaParameters(v1other, v2, y0 = y0)
  # Patient characteristics from dataItem
  v3 <- c(as.list(dataItem), compactList(
    # Characteristics:
    b0=aortaGrowthParameters$b0, 
    b1=aortaGrowthParameters$b1,
    # Natural events:
    ruptureTime=aortaGrowthParameters$ruptureTime,
    nonAaaDeathTime=generateTimeTillNonAaaDeath(v0, v1other, dataItem$startAge),
    
    # Initial diameter as measured:
    initialAortaSizeAsMeasured=
      aortaGrowthParameters$initialAortaSizeAsMeasured,
    
    # A large vector of propensities (probabilities) to be used each time an observed AAA size is required
    # This reduces MC error when comparing the two treatment groups (pairs are more closely cloned)
    aortaSize = setType(runif(300), "propensity")
  ))
  
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
      #v3[[elementName]] <- setType(rbernoulli(v2element), "boolean")
      v3[[elementName]] <- setType(runif(1), "propensity")
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
        eventHistory, v0$namesOfQuantities, v2$costs, v1other, v2, v3)
  }
  
  return(result)
}

################################################################################
################################################################################
# processPersonsControlOnly - generate a set of persons for CONTROL GROUP ONLY and analyze them. 

################################################################################
processPersonsControlOnly <- function(v0, v1other, v2, updateProgress=NULL, personData=NULL) {
  
  
  cat("processPersons\n")
  cat("probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgery:\n")
  print(v2$probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgery)
  
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
  ## If v1other exists then assign is to personData$startAge 
  if(!is.null(v1other$startAge)){
    personData <- data.frame(startAge = v1other$startAge)
    v1other$startAge <- NULL
  }
  ## If prob (probability weighting) does not exist in personData then assign it the value 1
  if(is.null(personData$prob)){
    personData$prob <- 1
  }
  
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
    cat("########## personData ##########\n")
    print(personData)
    cat("########################\n\n")
  }
  
  # Create v1other$nonAaaSurvProbs or nonAaaMortalityRates. 
  # (See also checkArgs.R.)
  if (v1other$nonAaaDeathMethod == "mass") {
    v1other$nonAaaSurvProbs <- getMassSurvivalProbabilities()
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
  
  
  # Set v1other$thresholdForIncidentalDetection to 
  # v1other$aortaDiameterThresholds[1], if the former was not set. 
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
                                  FUN=processOnePair, v0, v1other, v2, personData)
    
  } else if (v0$method == "parallel") {
    require(parallel, quietly = T)
    
    # Do it using parLapply. 
    v0$treatmentGroups <- "noScreening"
    cluster <- makeCluster(v0$numberOfProcesses)  # previously: outfile=""
    clusterExport(cluster, getAllFunctionsAndStringsInGlobalEnv())
    setAndShowRandomSeed(randomSeed=v0$randomSeed, cluster=cluster, 
                         verbose=v0$verbose)
    resultForEachPerson <- parLapply(cl=cluster, X=1:v0$numberOfPersons,
                                     fun=processOnePair, v0, v1other, v2, personData)
    stopCluster(cluster) 
    
  } else if (v0$method == "foreach") {
    require(doParallel, quietly = T)
    
    v0$treatmentGroups <- "noScreening"
    if (!is.null(v0$randomSeed)) 
      stop("v0$randomSeed does not yet work with v0$method=\"foreach\"")
    # Do it using foreach and %dopar%. 
    registerDoParallel(cores=v0$numberOfProcesses)
    resultForEachPerson <- foreach(personNumber=1:v0$numberOfPersons,
                                   .export=getAllFunctionsAndStringsInGlobalEnv()) %dopar% {
                                     processOnePair(personNumber, v0, v1other, v2, personData) 
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
# 2a) Models for aorta growth and rupture
# Generate person-specific aorta growth and rupture parameters. This should be 
# called from processOnePair, and the numbers returned by it should be 
# stored in v3. 
# This linear mixed model for log-diameter samples baseline diameter from a distribution, 
# which may or may not be weighted, then generates from a model with random slope. 
################################################################################
generatePersonAortaParameters <- function(v1other, v2, y0 = NULL) {  # (person not persons)
  
  # Generate y0.
  if(is.null(y0))  
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
    scheduledEvents["inviteToScreen"] <- v1other$inviteToScreenSuspensionTime
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
  numberMonitor <- rep(0,length(v1other$aortaDiameterThresholds[[1]])+1)
  
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
    
    ## Define aortaDiameterThresholds once for this loop
    aDT <- v1other$aortaDiameterThresholds[[findInterval(eventTime, c(0, attr(v1other$aortaDiameterThresholds, "timeCutPoints")))]]
    
    # Update scheduledEvents as appropriate based on what eventType is.
    if (eventType=="inviteToScreen") {
      gBV <- getBinaryVariable("probOfRequireReinvitation", v1other, v2, v3, eventTime)
      requireReinvitation <- gBV$result
      v3 <- gBV$v3
      if (requireReinvitation) {
        eventHistory <- addEvent(eventHistory, "requireReinvitation", eventTime)
      } 
      gBV <- getBinaryVariable("probOfAttendScreen", v1other, v2, v3, eventTime)
      attendScreen <- gBV$result
      v3 <- gBV$v3
      if (attendScreen) {
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
      
      gBV <- getBinaryVariable("probOfNonvisualization", v1other, v2, v3, eventTime)
      nonvisualization <- gBV$result
      v3 <- gBV$v3
      if ("trueSizes" %in% names(eventHistory) && !nonvisualization)  
        # The true size is unknown, so set that to NA. The measured 
        # size is aortaSize, so record that.
        eventHistory <- recordSize(eventHistory, NA, aortaSize)			
      
      # If they have nonvisualization.
      if (nonvisualization) 
        eventHistory <- addEvent(eventHistory, "nonvisualization", 
                                 eventTime)
      
      # Find what interval aortaSize is in & schedule events accordingly.
      aortaSizeGroup <- findInterval(aortaSize,	aDT)
      ## Add one to the times patient has been screened or monitored in this size group
      numberMonitor[aortaSizeGroup + 1] <- numberMonitor[aortaSizeGroup + 1] + 1
      
      if (aortaSizeGroup == 0 || nonvisualization) {  ## Assumes that first group is screened normal group
        gID <- generateIncidentalDetectionTime(eventTime, 
                                               v1other$thresholdForIncidentalDetection, v3, 
                                               v2$rateOfIncidentalDetection)
        scheduledEvents["incidentalDetection"] <- gID$time
        v3 <- gID$v3
      } else if (aortaSizeGroup < length(v1other$aortaDiameterThresholds[[1]])) {
        scheduledEvents["monitor"] <- 
          max(v1other$monitoringIntervalsSuspensionTime[aortaSizeGroup+1], eventTime + v1other$monitoringIntervals[aortaSizeGroup+1]) 
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
      aortaSizeGroup <- findInterval(aortaSize, aDT)
      ## Add one to the times patient has been screened or monitored in this size group
      numberMonitor[aortaSizeGroup + 1] <- numberMonitor[aortaSizeGroup + 1] + 1
      ## If numberMonitor <= maxNumberMonitor then schedule another monitor else discharge from surveillance
      ## Last group has Inf number of monitors. This corresponds with large AAA group
      
      if (numberMonitor[aortaSizeGroup + 1] < c(v1other$maxNumberMonitor, Inf)[aortaSizeGroup + 1]){
        if (aortaSizeGroup == 0) {  # they are below lowest threshold
          eventHistory <- addEvent(eventHistory, 
                                   "aortaDiameterBelowThreshold", eventTime)
          scheduledEvents["monitor"] <- 
            max(v1other$monitoringIntervalsSuspensionTime[1], eventTime + v1other$monitoringIntervals[1]) 
        } else if (aortaSizeGroup < length(v1other$aortaDiameterThresholds[[1]])) {
          scheduledEvents["monitor"] <- 
            max(v1other$monitoringIntervalsSuspensionTime[aortaSizeGroup+1], eventTime + v1other$monitoringIntervals[aortaSizeGroup+1]) 
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
      aortaSizeGroup <- findInterval(aortaSize, aDT)
      gBV <- getBinaryVariable("probOfContraindication", v1other, v2, v3, eventTime)
      contraindication <- gBV$result
      v3 <- gBV$v3
      
      if (aortaSizeGroup < length(v1other$aortaDiameterThresholds[[1]])) {
        eventHistory <- addEvent(eventHistory, 
                                 "decideOnReturnToMonitoring", eventTime)
        scheduledEvents["monitor"] <- 
          max(v1other$monitoringIntervalsSuspensionTime[aortaSizeGroup+1], eventTime + v1other$monitoringIntervals[aortaSizeGroup+1]) 
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
            max(v1other$monitoringIntervalFollowingContraindicationSuspensionTime, eventTime + v1other$monitoringIntervalFollowingContraindication) 	
        
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
        max(v1other$monitoringIntervalFollowingContraindicationSuspensionTime, eventTime + v1other$monitoringIntervalFollowingContraindication) 	
      
      
    } else if (eventType=="electiveSurgeryOpen" || 
               eventType=="electiveSurgeryEvar") {
      surgeryType <- 
      { if(eventType=="electiveSurgeryOpen") "open" else "evar" }
      
      if (v1other$electiveSurgeryAaaDeathMethod == "instantDeathOnly") {
        if (surgeryType=="evar")
          stop("surgeryType=\"evar\" should be impossible when ",
               "v1other$electiveSurgeryAaaDeathMethod=\"instantDeathOnly\"")
        dieFromElectiveSurgery <- { if("incidentalDetection" %in% eventHistory$events){
          gBV <- getBinaryVariable("probOfDieFromElectiveSurgeryViaIncidentalDetection", v1other, v2, v3, eventTime)
          dieFromElectiveSurgeryViaIncidentalDetection <- gBV$result
          v3 <- gBV$v3
          dieFromElectiveSurgeryViaIncidentalDetection
        }
          else {
            gBV <- getBinaryVariable("probOfDieFromElectiveSurgeryViaScreeningDetection", v1other, v2, v3, eventTime)
            dieFromElectiveSurgeryViaScreeningDetection <- gBV$result
            v3 <- gBV$v3
            dieFromElectiveSurgeryViaScreeningDetection 
          }
        }
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
        scheduledEvents["monitorFollowingOpenSurgery"] <- 
          max(v1other$monitorFollowingOpenSurgerySuspensionTime, eventTime + v1other$timeToMonitoringFollowingOpenSurgery) 	
      } else {
        scheduledEvents["monitorFollowingEvarSurgery"] <- 
          max(v1other$monitorFollowingEvarSurgerySuspensionTime, eventTime + v1other$timeBetweenMonitoringFollowingEvarSurgery) 	
      }
      
    } else if (eventType=="rupture") {
      if ("trueSizes" %in% names(eventHistory)) {
        trueSize <- getAortaMeasurement(v3, eventTime, method="exact")
        eventHistory <- recordSize(eventHistory, trueSize, NA)
      }
      gBV <- getBinaryVariable("probOfEmergencySurgeryIfRupture", v1other, v2, v3, eventTime)
      emergencySurgeryIfRupture <- gBV$result
      v3 <- gBV$v3
      if (emergencySurgeryIfRupture) {
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
      eventsToCancel <- c("inviteToScreen", "monitor", "dropout", "incidentalDetection", 
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
        
        gBV <- getBinaryVariable("probOfDieFromEmergencySurgery", v1other, v2, v3, eventTime)
        dieFromEmergencySurgery <- gBV$result
        v3 <- gBV$v3
        if (dieFromEmergencySurgery) {
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
        scheduledEvents["monitorFollowingOpenSurgery"] <- 
          max(v1other$monitorFollowingOpenSurgerySuspensionTime, eventTime + v1other$timeToMonitoringFollowingOpenSurgery) 	
      } else {
        scheduledEvents["monitorFollowingEvarSurgery"] <- 
          max(v1other$monitorFollowingEvarSurgerySuspensionTime, eventTime + v1other$timeBetweenMonitoringFollowingEvarSurgery) 	
      }
      
    } else if (eventType %in% c("aaaDeath", "nonAaaDeath", "censored")) {
      break
      
    } else if (eventType == "incidentalDetection") {
      if (getAortaMeasurement(v3, eventTime, method="exact") < 
          v1other$thresholdForIncidentalDetection)
        stop("incidentalDetection happened when aorta was below ",
             v1other$thresholdForIncidentalDetection, "cm")
      
      scheduledEvents["monitor"] <- max(v1other$incidentalDetectionSuspensionTime, eventTime)
      gD <- generateDropoutTime(eventTime, 
                                v2$rateOfDropoutFromMonitoring, v3)
      scheduledEvents["dropout"] <- gD$time
      v3 <- gD$v3
      
    } else if (eventType == "dropout" | eventType == "discharged") {
      scheduledEvents["monitor"] <- NA
      ## Reset numberMonitor to zero so that if they get incidentally detected they start afresh
      numberMonitor <- rep(0,length(v1other$aortaDiameterThresholds[[1]])+1)
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
      scheduledEvents["monitorFollowingEvarSurgery"] <- 
        max(v1other$monitorFollowingEvarSurgerySuspensionTime, eventTime + 	v1other$timeBetweenMonitoringFollowingEvarSurgery) 	
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
    if (!(getType(v3[[varName]]) %in% c("propensity", "fixed value")))
      stop("v3$", varName, " must have type propensity or fixed value")
    if(is.null(attr(v2[[varName]], "timeCutPoints"))){
      ind <- 1
    } else {
      timeCutPoints <- c(0, attr(v2[[varName]], "timeCutPoints"))
      ind <- findInterval(eventTime, timeCutPoints)
    }
    prob <- v2[[varName]][ind]
    result <- prob > v3[[varName]]
  } else if (varName %in% names(v2) && 
             getType(v2[[varName]]) == "logistic model for probability") {
    if(is.null(attr(v2[[varName]], "timeCutPoints"))){
      # Make beta and logOddsAdjustment. 
      beta <- v2[[varName]][names(v2[[varName]]) != "logOddsAdjustment"]
      logOddsAdjustment <- v2[[varName]]["logOddsAdjustment"]  
    } else {
      # Make beta and logOddsAdjustment. 
      timeCutPoints <- c(0, attr(v2[[varName]], "timeCutPoints"))
      ind <- findInterval(eventTime, timeCutPoints)
      beta <- sapply(v2[[varName]][names(v2[[varName]]) != "logOddsAdjustment"], function(i){i[ind]})
      logOddsAdjustment <- unlist(sapply(v2[[varName]]["logOddsAdjustment"], function(i){i[ind]}))  	    
    }
    
    
    # Calculate the covariates.
    covariates <- rep(NA, length(beta) - 1)
    names(covariates) <- names(beta)[-1]  
    for (covariateName in names(covariates)) {
      ## NEED TO ENSURE v3 IS ASSIGNED BACK TO GENERATEEVENTHISTORY
      covariates[covariateName] <- switch(covariateName,
                                          age = v3$startAge + eventTime - 80,
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
  if (numberOfEvents >= 1 && time < eventHistory$times[numberOfEvents]) {
    stop("the time of the new event must be >= all eventHistory times")
  } 
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
# 2d) Functions for generating time till non-AAA death
# There are three sets of functions, one for each model, plus one function, 
# generateTimeTillNonAaaDeath, that is used whichever model is used.
################################################################################
# A single function for any non-AAA death model. 
generateTimeTillNonAaaDeath <- function(v0, v1other, startAge) {
  ## Calculate conditional survival probabilities given patient's age
  ## Probabilities are survival up to the end of that year of age
  nonAaaSurvProbs <- v1other$nonAaaSurvProbs[v1other$nonAaaSurvProbs$age >= startAge, 2] / min(v1other$nonAaaSurvProbs[v1other$nonAaaSurvProbs$age == startAge-1, 2], 1, na.rm = T)
  
  if (v1other$nonAaaDeathMethod == "mass") {
    return(generateTimeTillNonAaaDeathFromSurvProbs(
      nonAaaSurvProbs, 0.25))
    
  } else if (v1other$nonAaaDeathMethod == "onsIntegerStart") { 
    return(generateTimeTillNonAaaDeathFromSurvProbs(
      nonAaaSurvProbs, 1))
    
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
  surv <- fileContents[,c("followUpAtLeast", "survProbAdjustedForCensAndAaa")]
  surv[!is.na(surv$survProbAdjustedForCensAndAaa),]
}

# generateTimeTillNonAaaDeathFromSurvProbs generates a non-AAA death time
# from a vector of survival probabilities. 
generateTimeTillNonAaaDeathFromSurvProbs <- 
  function(survivalProbs, periodLength, startAge) {
    
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
  ages <- as.numeric(rownames(mortalityRates))
  data <- data.frame(age = ages[ages>=startAge], survivalProbs = survivalProbs)
  return(data)
}

###########################################################################################
## PSA functions

onePsaIteration <- function(psaIterationNumber, v0, v1other, 
                            v2values, personData) {
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
  processPersonsResult <- processPersons(v0, v1other, v2, personData=personData)
  result <- list(meanQuantities=processPersonsResult$meanQuantities, v2=v2)
  if (v0$returnEventHistories) 
    result$eventHistories <- processPersonsResult$eventHistories
  return(result)
}

# generateV2, a function to generate specific values of v2, the global 
# uncertain parameters, from v1distributions, the global fixed parameters that 
# are used for generating elements of v2. 
# Now does some elementary checks to make sure v1distributions is coherant with v2
generateV2 <- function(v1distributions, v2old) {
  
  v2 <- compactList()
  
  for (elementName in sort(names(v1distributions))) {
    
    # Get v1element and type. 
    v1element <- v1distributions[[elementName]]
    type <- attr(v1element, "type")
    if (is.null(type)) 
      stop("the \"type\" element of v1distributions$", elementName, 
           " is NULL")
    ## if v2old has attribute timeCutPoints then ensure v1distribution also has this and with the same length of cut-points
    if(!is.null(attr(v2old[[elementName]], "timeCutPoints"))){
      if(is.null(attr(v1distributions[[elementName]], "timeCutPoints")))
        stop(paste0("v1distributions for ", elementName, " is missing timeCutPoints"))
      if(any(attr(v1distributions[[elementName]], "timeCutPoints") != attr(v2old[[elementName]], "timeCutPoints")))
        stop(paste0("timeCutPoints for v2 and v1distributions for ", elementName, " must be identical"))
    }
    
    
    if (type == "fixed value") {
      v2element <- v1element
    } else if (type == "fixed value for qaly") {
      v2element <- v1element
      attr(v2element, "type") <- "qaly"
    }	else if (type == "beta pars for probability") {
      ## check that v2old is of type probability
      if(attr(v2old[[elementName]], "type")!="probability")
        stop(paste0("v2 and v1distributions for parmaeter ", elementName, " are incompatible"))
      if(!is.null(attr(v1element, "timeCutPoints"))){
        if(length(v1element$alpha)!=length(v1element$beta) | 
           length(v1element$alpha)!=(length(attr(v1element, "timeCutPoints"))+1))
          stop(paste0("Length of alpha and beta from v1distribution$", elementName, " not correct"))
        v2element <- myRbeta(n=length(v1element$alpha), 
                             shape1=v1element$alpha, shape2=v1element$beta)
        attr(v2element, "type") <- "probability"
        attr(v2element, "timeCutPoints") <- attr(v1element, "timeCutPoints")
      } else {
        v2element <- myRbeta(n=1, 
                             shape1=v1element$alpha, shape2=v1element$beta)
        attr(v2element, "type") <- "probability"
      }			
    } else if (type == "hyperpars for logistic model for probability") {
      ## check that v2old is of type logistic model for probability
      if(attr(v2old[[elementName]], "type")!="logistic model for probability")
        stop(paste0("v2 and v1distributions for parmaeter ", elementName, " are incompatible"))
      ## if v1element has attribute timeCutPoints 
      if(!is.null(attr(v1element, "timeCutPoints"))){
        if(length(v1element$mean$intercept)!=length(v2old[[elementName]]$intercept) | 
           length(v1element$mean$age)!=length(v2old[[elementName]]$age) |
           length(v1element$mean$aortaSize)!=length(v2old[[elementName]]$aortaSize))
          stop(paste0("Length of intercept, age and aortaSize from v1distribution$", elementName, "$mean not correct"))
        if(length(v1element$covariance) != length(v1element$mean$intercept))
          stop(paste0("Length of v1distribution$", elementName, "$covariance not correct"))
        
        require(MASS, quietly=TRUE)
        k <- length(v1element$mean$intercept)
        v2element <- as.list(mvrnorm(n=1, mu=sapply(v1element$mean, function(l) l[1]), Sigma=v1element$covariance[[1]]))
        for(kind in 2:k){
          nextv2 <- mvrnorm(n=1, mu=sapply(v1element$mean, function(l) l[kind]), Sigma=v1element$covariance[[kind]])
          for(z in 1:length(v2element)){
            v2element[[z]] <- c(v2element[[z]], nextv2[[z]])
          }
        }
        if ("logOddsAdjustment" %in% names(v1element))
          v2element$logOddsAdjustment <- v1element$logOddsAdjustment
        attr(v2element, "type") <- "logistic model for probability"
        attr(v2element, "timeCutPoints") <- attr(v1element, "timeCutPoints")
      } else {
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
      }  
    } else if (type == "gamma pars for rate") {
      # check that v2old is a rate
      if(!(attr(v2old[[elementName]], "type") %in% c("rate", "reintervention rates")))
        stop(paste0("v2 and v1distributions for parmaeter ", elementName, " are incompatible"))
      if(!is.null(attr(v1element, "timeCutPoints"))){
        if(length(v1element$shape)!=length(v1element$scale) | 
           length(v1element$shape)!=(length(attr(v1element, "timeCutPoints"))+1))
          stop(paste0("Length of shape and scale from v1distribution$", elementName, " not correct"))
        v2element <- rgamma(n=length(v1element$shape), 
                            shape=v1element$shape, scale=v1element$scale)
        attr(v2element, "type") <- "rate"
        attr(v2element, "timeCutPoints") <- attr(v1element, "timeCutPoints")
      } else {
        v2element <- rgamma(n=1, 
                            shape=v1element$shape, scale=v1element$scale)
        attr(v2element, "type") <- "rate"
      }			
    } else if (type == "gamma pars for multiple rates") {
      # check that v2old is of type reintervention rates
      if(attr(v2old[[elementName]], "type")!="reintervention rates")
        stop(paste0("v2 and v1distributions for parmaeter ", elementName, " are incompatible"))
      ## if v2old has attribute timeCutPoints 
      if(!is.null(attr(v2old[[elementName]], "timeCutPoints"))){
        stop("timeCutPoints not yet implemented for reintervention rates input")
      }		  
      v2element <- rgamma(n=length(v1element$shapes), 
                          shape=v1element$shapes, scale=v1element$scales)
      attr(v2element, "type") <- "reintervention rates"
      
    } else if (type == "pars for betaThenConvertThreeMonthProbToRate") {
      # check that v2old is a rate
      if(attr(v2old[[elementName]], "type")!="rate")
        stop(paste0("v2 and v1distributions for parmaeter ", elementName, " are incompatible"))
      ## if v2old has attribute timeCutPoints 
      if(!is.null(attr(v2old[[elementName]], "timeCutPoints"))){
        stop("timeCutPoints not yet implemented for rate input")
      }		  
      v2element <- convertThreeMonthProbToRate(myRbeta(n=1, 
                                                       shape1=v1element$alpha, shape2=v1element$beta))
      attr(v2element, "type") <- "rate"
      
    } else if (type == "pars for gammaThenMultiplyByFour") {
      # check that v2old is a rate
      if(attr(v2old[[elementName]], "type")!="rate")
        stop(paste0("v2 and v1distributions for parmaeter ", elementName, " are incompatible"))
      ## if v2old has attribute timeCutPoints 
      if(!is.null(attr(v2old[[elementName]], "timeCutPoints"))){
        stop("timeCutPoints not yet implemented for rate input")
      }		
      
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
  
  return(v2)
}

onePsaIterationAboveDiagnosisThreshold <- function(psaIterationNumber, v0, v1other, 
                                                   v2values,threshold, personData) {
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
  
  processPersonsResult <- processPersonsAboveDiagnosisThreshold(v0, v1other, v2, threshold, personData)
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
  eventHistory, namesOfQuantities, costs, v1other, v2, v3) {
  
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
  ## Get qalyFactorBoundaries and qalyFactors for this individual with their startAge
  qalyFactorBoundaries <- v1other$qalyFactorAgeBoundaries[v1other$qalyFactorAgeBoundaries > v3$startAge] - v3$startAge
  qalyFactors <- v2$qalyFactors[c(v1other$qalyFactorAgeBoundaries > v3$startAge, T)]
  
  result["qalys"] <- adjustLifeYears(result["lifeYears"], 
                                     qalyFactorBoundaries=qalyFactorBoundaries, 
                                     qalyFactors=qalyFactors)
  
  # Calculate cost.
  result["cost"] <- sum(costs[eventHistory$events], na.rm=TRUE)
  
  # Calculate discounted life-years.
  # The first year is discounted 
  result["discountedLifeYears"] <- adjustLifeYears(result["lifeYears"], 
                                                   discountRate=v1other$lifeYearDiscountRate)
  
  # Calculate discounted QALYs. 
  result["discountedQalys"] <- adjustLifeYears(result["lifeYears"], 
                                               qalyFactorBoundaries=qalyFactorBoundaries, 
                                               qalyFactors=qalyFactors,
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
  if(is.null(attr(rateOfDropoutFromMonitoring, "timeCutPoints"))){
    time <- monitoringStartTime + qexp(v3[["rateOfDropoutFromMonitoring"]][1], rate=rateOfDropoutFromMonitoring)
  } else {
    start <- as.numeric(monitoringStartTime)
    timeCutPoints <- c(0, (attr(rateOfDropoutFromMonitoring, "timeCutPoints") - start)[attr(rateOfDropoutFromMonitoring, "timeCutPoints") >=  monitoringStartTime])
    l1 <- length(timeCutPoints)
    l2 <- length(rateOfDropoutFromMonitoring)
    rates <- rateOfDropoutFromMonitoring[(l2-l1+1):l2]
    time <- monitoringStartTime + msm::qpexp(v3[["rateOfDropoutFromMonitoring"]][1], rate=rates, t=timeCutPoints)  
  }
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
    numberOfProcesses=parallel::detectCores()-1
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
    inviteToScreenSuspensionTime = 0,
    monitoringIntervalsSuspensionTime = rep(0, length(v1other$monitoringIntervals)),
    monitorFollowingOpenSurgerySuspensionTime = 0,
    monitorFollowingEvarSurgerySuspensionTime = 0,
    incidentalDetectionSuspensionTime = 0
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
checkArgs <- function(v0, v1other, v1distributions, v2, personData) {
  # v0 and v1other must be given, but v1distributions and v2  can be missing.
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
  ## Check personData if not null
  if (!is.null(personData)){
    if(class(personData)!="data.frame") stop("personData must be a data.frame")
    if(any(!(colnames(personData) %in% c("startAge", "baselineDiameter", "prob")))) stop("The columns of personData must be named one of startAge, baselineDiameter and prob")
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
    if (any(!(c("nonAaaMortalityRatesFileName") %in% 
              names(v1other))))
      stop("v1other$nonAaaDeathMethod is onsIntegerStart, so ", 
           "v1other must contain nonAaaMortalityRatesFileName")
    
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
  
  # check that v1other$qalyFactorAgeBoundaries is specified instead of v1other$qalyFactorBoundaries
  if(!is.null(v1other$qalyFactorBoundaries)){
    warning("v1other$qalyFactorBoundaries are deprecated. Please specify ages directly using v1other$qalyFactorAgeBoundaries")
  }  
  # Check v1other$aortaDiameterThresholds and v1other$monitoringIntervals.
  if(is.null(attr(v1other$aortaDiameterThresholds, "timeCutPoints"))){
    if (length(v1other$aortaDiameterThresholds[[1]]) != 
        length(v1other$monitoringIntervals)) ## updated MS 07/02/19
      stop("v1other$aortaDiameterThresholds must be the same length as v1other$monitoringIntervals. \n
           The first element of v1other$monitoringIntervals should be the monitoring interval for AAA in surveillance who drop below first diameter threshold \n
           The last element of v1other$monitoringIntervals should be the monitoring interval for contraindicated AAA")
    if (!identical(v1other$aortaDiameterThresholds[[1]], 
                   sort(v1other$aortaDiameterThresholds[[1]])))
      stop("v1other$aortaDiameterThresholds must be in increasing order")
    
  }	else {
    ## Check that timeCutPoints and aortaDiameterThresholds are the correct length
    if (length(v1other$aortaDiameterThresholds)!=(length(attr(v1other$aortaDiameterThresholds, "timeCutPoints"))+1)) 
      stop("v1other$monitoringIntervals should be a list with length one greater than timeCutPoints")
    for(i in v1other$aortaDiameterThresholds){
      if(!identical(i, sort(i))) stop("v1other$aortaDiameterThresholds must be in increasing order") 
    }
    test <- sapply(v1other$aortaDiameterThresholds, length)
    if(!all.equal( max(test) ,min(test))) stop("All elements of v1other$aortaDiameterThresholds must be the same length")
  }
  
  # Check if suspension times for monitoring exist
  # If they do, check they are of correct length
  if(!is.null(v1other$monitoringIntervalsSuspensionTime)){
    if(length(v1other$monitoringIntervalsSuspensionTime) != length(v1other$monitoringIntervals))
      stop("v1other$monitoringIntervalsSuspensionTime and v1other$monitoringIntervals must be the same length")
  }
  
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
  return(v1other)
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
      if(is.null(attr(v2element, "timeCutPoints"))){
        if (!isSingleNumeric(v2element) || v2element < 0 || v2element > 1)
          raiseV2TypeRelatedError(v2elementName)
      } else {
        if (length(v2element)!=(length(attr(v2element, "timeCutPoints"))+1) 
            || !is.numeric(v2element)
            || any(is.na(v2element))
            || any(v2element < 0) || any(v2element > 1))
          raiseV2TypeRelatedError(v2elementName)
      }			
    } else if (type == "logistic model for probability") {
      if(is.null(attr(v2element, "timeCutPoints"))){
        if (!is.numeric(v2element) || any(is.na(v2element)) || 
            !hasNames(v2element))
          raiseV2TypeRelatedError(v2elementName)
      } else {
        if (any(lapply(v2element, length)!=(length(attr(v2element, "timeCutPoints"))+1)) 
            || !is.list(v2element)
            || any(sapply(v2element, function(i){any(is.na(i))}))
            || !hasNames(v2element)
        )
          raiseV2TypeRelatedError(v2elementName)
      }	
      minimumLength <- { if (names(v2element)[length(v2element)] == 
                             "logOddsAdjustment") 2 else 1 }
      if (length(v2element) < minimumLength) 
        raiseV2TypeRelatedError(v2elementName)
      # Check that logOddsAdjustment does not appear before the last name.
      if ("logOddsAdjustment" %in% names(v2element) && match(
        "logOddsAdjustment", names(v2element)) < length(v2element))
        raiseV2TypeRelatedError(v2elementName)
      
    } else if (type == "rate") {
      if(is.null(attr(v2element, "timeCutPoints"))){
        if (!isSingleNumeric(v2element) || v2element < 0)
          raiseV2TypeRelatedError(v2elementName)
      } else {
        if (length(v2element)!=(length(attr(v2element, "timeCutPoints"))+1) 
            || !is.numeric(v2element)
            || any(is.na(v2element))
            || any(v2element < 0))
          raiseV2TypeRelatedError(v2elementName)
      }	
      
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
  for (varName in requiredElementsOfV2){
    if (!(varName %in% names(v2)) || any(is.na(v2[[varName]])))
      stop("v2 must contain ", varName, " and it must not be NA")
  }
  
  # Check reintervention rates. 
  for (varName in c("reinterventionRatesAfterElectiveOpen",
                    "reinterventionRatesAfterElectiveEvar",
                    "reinterventionRatesAfterEmergencyOpen",
                    "reinterventionRatesAfterEmergencyEvar")) {
    v2element <- v2[[varName]]
    if (is.null(v2element) || !is.numeric(v2element) || 
        any(is.na(v2element)) || any(v2element < 0)) 
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
