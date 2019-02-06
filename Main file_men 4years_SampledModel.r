################################################################################
# SWAN main file, men 4years: sampled model
################################################################################
require(parallel) 

# Basic setup. 
source("functions/DES Model.R")
runIdentifier <- paste(sample(LETTERS, size=4, replace=TRUE), collapse="")
options(width = 110L)
saveOutput <- T
doPSA <- F

# Parameters.
source("input/parsForMen4years.R") 

# Settings.
v0$numberOfPersons <- 1e7
v0$returnEventHistories <- FALSE  # Does not return large event history object
v0$recordSizes <- TRUE
v0$returnAllPersonsQuantities <- FALSE # Only set to TRUE if aim is to plot convergence plots
v0$method <- "parallel"
v0$randomSeed <- 2
v0$numberOfProcesses <- detectCores()-1 ## use number cores minus 1 to allow machine to still function
v0 <- setUnspecifiedElementsOfv0(v0)

# Analysis. 
cat("##### SWAN men 4years model: sampled model", date(), "(runIdentifier=", runIdentifier, ")",sep=" ")

systemTimeOutput <- system.time(result <- processPersonsAboveDiagnosisThreshold(v0, v1other, v2, threshold=3.0))

if (saveOutput)
	processPersonsObjectsToSave <- list(result=result, v0=v0, v1other=v1other, v2=v2)

# Show tables.
cat("\nLIFE-YEARS AND COSTS:\n\n")
showIncrementalLifeYearsAndCosts(result)

# PSA.
cat("\n#####################################################################\n")
if (doPSA) {
	cat("##### Starting PSA at ", date(), " #####\n\n", sep="")
  v0 <- compactList(
    numberOfPersons=5e5,
    numberOfParameterIterations=1000,
    method="parallel", 
    randomSeed=2,
    numberOfProcesses=detectCores()-1,
    returnEventHistories=FALSE,
    verbose=TRUE
  )
	
  systemTimeOutput <- system.time(
    psaResultNew <-psaAboveDiagnosisThreshold(v0,v1other,v1distributions,threshold=3.0))
  print(apply(psaResultNew$psaQuantities,2,quantile,c(0.025,0.05,0.5,0.95,0.975)))
  print(apply(psaResultNew$psaQuantities,2,mean))
  cat("\npsa took ", displayTime(systemTimeOutput["elapsed"]), "\n", sep="")
  if (saveOutput) psaObjectsToSave <- list(psaResult=psaResultNew, v0=v0, 
                                           v1other=v1other, v1distributions=v1distributions)
}

# Save output. 
if (saveOutput) saveMainAndPsaObjects(processPersonsObjectsToSave, 
			{if (doPSA) psaObjectsToSave}, runIdentifier=runIdentifier,  
			extraText="SWAN_men_4years_model_sampled_model")

################################################################################