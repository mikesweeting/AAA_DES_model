################################################################################
# SWAN main file, men 30years: full model
################################################################################
rm(list=ls())
require(parallel) 

## Set working directory
this.dir <- dirname(parent.frame(2)$ofile)
setwd(paste0(this.dir,"/../../"))

# Basic setup. 
source("functions/DES_Model.R")
source("functions/Auxiliary_Functions.R")
runIdentifier <- paste(sample(LETTERS, size=4, replace=TRUE), collapse="")
options(width = 110L)
saveOutput <- T
doPSA <- F

# Parameters.
source("input/SWAN/parsForMen30years.R") 

# Settings.
v0$numberOfPersons <- 1e7
v0$returnEventHistories <- TRUE 
v0$recordSizes <- TRUE
v0$returnAllPersonsQuantities <- TRUE
v0$method <- "parallel"
v0$randomSeed <- 2
v0$numberOfProcesses <- detectCores()-1 ## use number cores minus 1 to allow machine to still function
v0 <- setUnspecifiedElementsOfv0(v0)

# Analysis. 
cat("##### SWAN men 30years model: full model", date(), "(runIdentifier=", runIdentifier, ")",sep=" ")
		
personData <- data.frame(startAge = 65) 
systemTimeOutput <- system.time(result <- processPersons(v0, v1other, v2, personData = personData))

if (saveOutput)
	processPersonsObjectsToSave <- list(result=result, v0=v0, v1other=v1other, v2=v2)
	
# Show tables.
cat("\nLIFE-YEARS AND COSTS:\n\n")
showTableOfLifeYears(result)

cat("\nNUMBERS OF EVENTS (NEWER TABLE):\n\n")
TableOfCounts(result, v1other)

# Show a more detailed events and costs table.
Eventsandcosts(result)
	
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
			psaResult <- psa(v0, v1other, v1distributions, personData = personData))
	cat("\npsa took ", displayTime(systemTimeOutput["elapsed"]), "\n", sep="")
	if (saveOutput) psaObjectsToSave <- list(psaResult=psaResult, v0=v0, 
				v1other=v1other, v1distributions=v1distributions)
	
	showAllPsaSummaryQuantities(psaResult$psaQuantities)
}

# Save output. 
if (saveOutput) saveMainAndPsaObjects(processPersonsObjectsToSave, 
			{if (doPSA) psaObjectsToSave}, runIdentifier=runIdentifier,
			extraText="SWAN_men_30years_model_full_model")

################################################################################