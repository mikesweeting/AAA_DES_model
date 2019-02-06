################################################################################
# SWAN main file, men 4years: full model
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
v0$returnEventHistories <- TRUE 
v0$recordSizes <- TRUE
v0$returnAllPersonsQuantities <- TRUE
v0$method <- "parallel"
v0$randomSeed <- 2
v0$numberOfProcesses <- detectCores()-1 ## use number cores minus 1 to allow machine to still function
v0 <- setUnspecifiedElementsOfv0(v0)

# Analysis. 
cat("##### SWAN men 4years model: full model.", date(), "(runIdentifier=", runIdentifier, ")",sep=" ")
		
systemTimeOutput <- system.time(result <- processPersons(v0, v1other, v2))

if (saveOutput)
	processPersonsObjectsToSave <- list(result=result, v0=v0, v1other=v1other, v2=v2)

# Show tables.
print(result$meanQuantities)
TableOfCounts_men4years(result, v1other)

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
			psaResult <- psa(v0, v1other, v1distributions))
	cat("\npsa took ", displayTime(systemTimeOutput["elapsed"]), "\n", sep="")
	if (saveOutput) psaObjectsToSave <- list(psaResult=psaResult, v0=v0, 
				v1other=v1other, v1distributions=v1distributions)
	
	showAllPsaSummaryQuantities(psaResult$psaQuantities)
}

# Save output. 
if (saveOutput) saveMainAndPsaObjects(processPersonsObjectsToSave, 
			{if (doPSA) psaObjectsToSave}, runIdentifier=runIdentifier, 
			extraText="SWAN_men_4years_model_full_model")

################################################################################