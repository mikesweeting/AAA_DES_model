################################################################################
# Parameters for the men 30 year models
################################################################################

if (!exists("v0")) v0 <- compactList() 
if (!exists("v1distributions")) v1distributions <- compactList() 
if (!exists("v1other")) v1other <- compactList() 
if (!exists("v2")) v2 <- compactList() 

################################################################################
# MISCELLANEOUS

v1other$postSurgeryInitialPeriod <- 30 / 365.25  # = 0.08213552

# v1other$startAge <- 65

v0$generateCensoringTime <- function() { 30.000001 }

# Post-surgery monitoring. 
v1other$timeToMonitoringFollowingOpenSurgery <- 100
v1other$timeBetweenMonitoringFollowingEvarSurgery <- NA

# No growth for men with a diameter of < 2cm 
v1other$zeroGrowthDiameterThreshold <- 2.0

################################################################################
# SCREENING

# Re-invitation proportion
v2$probOfRequireReinvitation <- setType(0.1360, "probability")
v1distributions$probOfRequireReinvitation <-
		setType(list(alpha=4602, beta=29237), "beta pars for probability")

# Attendance proportion
v2$probOfAttendScreen <- setType(0.750, "probability")
v1distributions$probOfAttendScreen <-
		setType(list(alpha=93170, beta=31022), "beta pars for probability")

# Non-visualisation proportion
v2$probOfNonvisualization <- setType(0.0121, "probability")
v1distributions$probOfNonvisualization <-
		setType(list(alpha=329, beta=26818), "beta pars for probability")
		
# Prevalence proportion
fileName <- "input/SWAN/AAA max measurements.csv"
v1other$baselineDiameters <- read.csv(fileName, comment.char="#")[, c("size", "pw")]
names(v1other$baselineDiameters) <- c("size", "weight")

v1other$prevalenceThreshold<-3.0

################################################################################
# AAA GROWTH & RUPTURE

v2$beta0 <- 1.27152200
v2$beta1 <- 0.05838810
v2$sigma0 <- exp(-1.737735221)
v2$sigma1 <- exp(-3.318297793)
v2$rho <- tanh(0.455016818)
v2$sigmaW <- exp(-2.586854985)
v2$gamma <- -16.26293
v2$alpha <- 7.210208
for (elementName in c("beta0", "beta1", "sigma0", "sigma1", "rho", "sigmaW",
		"gamma", "alpha"))
	attr(v2[[elementName]], "type") <- "par for aorta model"

# psa
growthParameterNames <-
		c("beta1", "beta0", "logSigma1", "logSigma0", "atanhRho", "logSigmaW")
ruptureParameterNames <- c("alpha", "gamma")

# # Hexavariate normal distribution
v1distributions$meanForGrowthParameters <- setType(
		c(0.0583881, 1.2715220, -3.3182980, -1.7377350, 0.4550168, -2.5868550),
		"hyperpars for aorta model")

v1distributions$covarianceForGrowthParameters <- setType(
		matrix(nrow=6, data=c(
		1.99e-06, 1.71e-06, 0.0000000, 0.000e+00, 0.000e+00, 0.00e+00,
		1.71e-06, 3.01e-05, 0.0000000, 0.000e+00, 0.000e+00, 0.00e+00,
		0.00e+00, 0.00e+00, 0.0017140, -1.880e-05, 4.828e-04, -6.77e-05,
		0.00e+00, 0.00e+00, -0.0000188, 5.283e-04, 4.840e-05, -1.42e-06,
		0.00e+00, 0.00e+00, 0.0004828, 4.840e-05, 2.588e-03, 9.26e-06,
		0.00e+00, 0.00e+00, -0.0000677, -1.420e-06, 9.260e-06, 8.09e-05)),
		"hyperpars for aorta model")

# # Bivariate normal distribution
v1distributions$meanForRuptureParameters <-
		setType(c(7.210208, -16.26293),	"hyperpars for aorta model")
v1distributions$covarianceForRuptureParameters <- setType(
		matrix(nrow=2, data=c(
		1.001459, -1.650784,
		-1.650784, 2.758093)),
"hyperpars for aorta model")

names(v1distributions$meanForGrowthParameters) <- growthParameterNames
dimnames(v1distributions$covarianceForGrowthParameters) <-
		list(growthParameterNames, growthParameterNames)
names(v1distributions$meanForRuptureParameters) <- ruptureParameterNames
dimnames(v1distributions$covarianceForRuptureParameters) <-
		list(ruptureParameterNames, ruptureParameterNames)

################################################################################
# SURVEILLANCE

# Surveillance intervals
v1other$aortaDiameterThresholds <- list(c(3.0, 4.5, 5.5))
v1other$monitoringIntervals <- c(1, 1, 0.25, Inf)
v1other$maxNumberMonitor <- c(Inf, Inf, Inf)

# Dropout rate from surveillance
v2$rateOfDropoutFromMonitoring <- setType(0.01430178 * 4, "rate")  # see below
v1distributions$rateOfDropoutFromMonitoring <- setType(list(
		shape=330, scale=4*4.34e-5), "gamma pars for rate")

# # Incidental detection rate
v2$rateOfIncidentalDetection <-
		setType(convertThreeMonthProbToRate(0.0114), "rate")
v1distributions$rateOfIncidentalDetection <-
		setType(list(alpha=19.55672, beta=1695.94546), 
		"pars for betaThenConvertThreeMonthProbToRate")
		
# Delay from 5.5+cm scan to consultation		
v1other$waitingTimeToConsultation <- 71 / 365.25

# Consultation scan
v2$ctMeasurementErrorSD <- setType(0.19, "fixed value")
v1distributions$ctMeasurementErrorSD <- 
		setType(v2$ctMeasurementErrorSD, "fixed value")
v2$extraDiameterForCtScan <- setType(0.2443, "fixed value")
v1distributions$extraDiameterForCtScan <- 
		setType(v2$extraDiameterForCtScan, "fixed value") 

# Decision at consultation: proportion returned to surveillance
# No variables need to be defined here. 

# Decision at consultation: non-intervention proportion
v2$probOfContraindication <- setType(0.0977 / (0.0977 + 0.681), "probability")
v1distributions$probOfContraindication <- 
		setType(list(alpha=69, beta=481), "beta pars for probability")
		
# Decision at consultation: proportion elective surgery
# 1 minus probOfContraindication 

# Delay from consultation to elective surgery
v1other$waitingTimeToElectiveSurgery <- 59 / 365.25

################################################################################
# ELECTIVE OPERATIONS

# Proportion receiving EVAR vs. open repair
v2$probOfElectiveSurgeryIsOpen <- setType(0.2975504, "probability")  
v1distributions$probOfElectiveSurgeryIsOpen <- 
		setType(v2$probOfElectiveSurgeryIsOpen, 
		"fixed value for probability")

# Model for peri/post-operative mortality
v1other$electiveSurgeryAaaDeathMethod <- "survivalModel"
		
# EVAR 30-day operative mortality
v2$probOfAaaDeathInInitialPeriodAfterElectiveEvarSurgery <- 
		setType(0.01607717, "probability")
v1distributions$probOfAaaDeathInInitialPeriodAfterElectiveEvarSurgery <- 
		setType(list(alpha=10, beta=612), "beta pars for probability")

# Open repair 30-day operative mortality
v2$probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgery <- 
		setType(0.04109589, "probability")		
v1distributions$probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgery <- 
		setType(list(alpha=24, beta=560), "beta pars for probability")
		
# Re-intervention rate after successful EVAR
v2$reinterventionRatesAfterElectiveEvar <- setType(0, "reintervention rates")
v1other$reinterventionTimeBoundariesAfterElectiveEvar <- numeric()
v1distributions$reinterventionRatesAfterElectiveEvar <- 
		setType(v2$reinterventionRatesAfterElectiveEvar, "fixed value for reintervention rates")

# Re-intervention rate after successful open repair
v2$reinterventionRatesAfterElectiveOpen <- setType(0, "reintervention rates")
v1other$reinterventionTimeBoundariesAfterElectiveOpen <- numeric()
v1distributions$reinterventionRatesAfterElectiveOpen <- 
		setType(v2$reinterventionRatesAfterElectiveOpen, "fixed value for reintervention rates")
		
# Long-term AAA mortality rate after successful EVAR
v2$rateOfAaaDeathAfterElectiveEvarSurgeryAndInitialPeriod <- 
		setType(0.0076640849, "rate")
v1distributions$rateOfAaaDeathAfterElectiveEvarSurgeryAndInitialPeriod <- 
		setType(list(shape=34, scale=2.254143e-04), "gamma pars for rate")

# Long-term AAA mortality rate after successful open repair
v2$rateOfAaaDeathAfterElectiveOpenSurgeryAndInitialPeriod <- 
		setType(0.0006991216, "rate")
v1distributions$rateOfAaaDeathAfterElectiveOpenSurgeryAndInitialPeriod <- 
		setType(list(shape=3, scale=0.0002330405), "gamma pars for rate")

################################################################################
# EMERGENCY OPERATIONS

# % operated after rupture
v2$probOfEmergencySurgeryIfRupture <- setType(0.368, "probability")
v1distributions$probOfEmergencySurgeryIfRupture <- 
		setType(list(alpha=193, beta=331), "beta pars for probability")
		
# Proportion receiving EVAR vs. open repair
v2$probOfEmergencySurgeryIsOpen <- setType(1, "probability") 
v1distributions$probOfEmergencySurgeryIsOpen <- 
		setType(v2$probOfEmergencySurgeryIsOpen, "fixed value for probability")

# Model for peri/post-operative mortality		
# v1other$emergencySurgeryAaaDeathMethod <- "instantDeathOnly"
 v1other$emergencySurgeryAaaDeathMethod <- "survivalModel"

# EVAR 30-day operative mortality
 v2$probOfAaaDeathInInitialPeriodAfterEmergencyEvarSurgery <- setType(1, "probability") 
 v1distributions$probOfAaaDeathInInitialPeriodAfterEmergencyEvarSurgery <-
   setType(v2$probOfAaaDeathInInitialPeriodAfterEmergencyEvarSurgery, "fixed value for probability")
 
# Open repair 30-day operative mortality
v2$probOfAaaDeathInInitialPeriodAfterEmergencyOpenSurgery <- setType(0.342, "probability")
v1distributions$probOfAaaDeathInInitialPeriodAfterEmergencyOpenSurgery <-
  setType(list(alpha=66, beta=127), "beta pars for probability")
# v2$probOfDieFromEmergencySurgery <- setType(0.342, "probability")
# v1distributions$probOfDieFromEmergencySurgery <-
# 		setType(list(alpha=66, beta=127), "beta pars for probability")

# Re-intervention rate after successful EVAR
v2$reinterventionRatesAfterEmergencyEvar <- setType(0, "reintervention rates")
v1other$reinterventionTimeBoundariesAfterEmergencyEvar <- numeric()
v1distributions$reinterventionRatesAfterEmergencyEvar <- 
		setType(v2$reinterventionRatesAfterEmergencyEvar, "fixed value for reintervention rates")

# Re-intervention rate after successful open repair
v2$reinterventionRatesAfterEmergencyOpen <- setType(0, "reintervention rates")
v1other$reinterventionTimeBoundariesAfterEmergencyOpen <- numeric()
v1distributions$reinterventionRatesAfterEmergencyOpen <- 
		setType(v2$reinterventionRatesAfterEmergencyOpen, "fixed value for reintervention rates")

# Long-term rate of death after EVAR
v2$rateOfAaaDeathAfterEmergencyEvarSurgeryAndInitialPeriod <-
    setType(1e-100, "rate")
v1distributions$rateOfAaaDeathAfterEmergencyEvarSurgeryAndInitialPeriod <-
    setType(v2$rateOfAaaDeathAfterEmergencyEvarSurgeryAndInitialPeriod, type = "fixed value for rate")
# Long-term rate of death after Open
v2$rateOfAaaDeathAfterEmergencyOpenSurgeryAndInitialPeriod <-
  setType(1e-100, "rate")
v1distributions$rateOfAaaDeathAfterEmergencyOpenSurgeryAndInitialPeriod <-
  setType(v2$rateOfAaaDeathAfterEmergencyOpenSurgeryAndInitialPeriod, type = "fixed value for rate")

################################################################################
# COSTS
v2$costs <- setType(c(
		inviteToScreen=1.70, 
		requireReinvitation=1.70, 
		screen=32.20, 
		monitor=68.00,
		monitorFollowingContraindication=Inf,
		consultation=435.25,
		electiveSurgeryEvar=13345.66,
		electiveSurgeryOpen=11532.69,
		emergencySurgeryEvar=Inf, 
		emergencySurgeryOpen=19984.75,
		reinterventionAfterElectiveEvar=Inf,
		reinterventionAfterElectiveOpen=Inf,
		reinterventionAfterEmergencyEvar=Inf,
		reinterventionAfterEmergencyOpen=Inf,
				monitorFollowingOpenSurgery=Inf,
		monitorFollowingEvarSurgery=Inf
		), type="costs")

v1distributions$costs <- setType(v2$costs, "fixed value for costs")

################################################################################
# MISCELLANEOUS

# Non-AAA mortality rate
v1other$nonAaaDeathMethod <- "onsIntegerStart"
v1other$nonAaaMortalityRatesFileName <- 
	"input/SWAN/nonAaaDeathMortalityRatesForMen.csv"  

# Non-AAA mortality rate in those contraindicated
v2$rateOfNonAaaDeathAfterContraindication <- setType(41 / 166, "rate")
v1distributions$rateOfNonAaaDeathAfterContraindication <- setType(list(
		shape=41, scale=4*0.0015), "gamma pars for rate")

# Overall QoL / utilities
v1other$qalyFactorAgeBoundaries <- 75
v2$qalyFactors <- setType(c(0.78, 0.75), "qaly")
# v1other$qalyFactorBoundaries <- createQalyFactors(startAge=v1other$startAge)$qalyFactorBoundaries
# v2$qalyFactors <- setType(createQalyFactors(startAge=v1other$startAge)$qalyFactors, "qaly")
v1distributions$qalyFactors <- 
  setType(v2$qalyFactors, "fixed value for qaly")

# Discount rates
v1other$lifeYearDiscountRate <- 3.5 / 100
v1other$costDiscountRate <- 3.5 / 100

################################################################################