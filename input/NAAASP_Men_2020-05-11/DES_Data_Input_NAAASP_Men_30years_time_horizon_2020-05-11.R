################################################################################
# Parameters for the men 30 year models
# Updated 27/08/19 and 27/04/20 to include
# 1) post-operative surveillance
# 2) age-dependent (elective and emergency) operative mortality
# 3) age-dependent turn-down rates
# 4) re-intervention rates
# 5) EQ5D scores that are age-dependent

################################################################################

if (!exists("v0")) v0 <- compactList() 
if (!exists("v1distributions")) v1distributions <- compactList() 
if (!exists("v1other")) v1other <- compactList() 
if (!exists("v2")) v2 <- compactList() 

################################################################################
# MISCELLANEOUS

v1other$postSurgeryInitialPeriod <- 30 / 365.25  # = 0.08213552

# v1other$startAge <- 65 - This is now specified in personData file as fed as an argument to processPersons or AAA_DES

v0$generateCensoringTime <- function() { 30.000001 }

# Post-surgery monitoring. 
v1other$timeToMonitoringFollowingOpenSurgery <- (6 * 7)/ 365.25
v1other$timeBetweenMonitoringFollowingEvarSurgery <- 1

# No growth for men with a diameter of < 2cm 
v1other$zeroGrowthDiameterThreshold <- 2.0

################################################################################
# SCREENING

# Re-invitation proportion
# SOURCE: MASS
v2$probOfRequireReinvitation <- setType(0.1360, "probability")
v1distributions$probOfRequireReinvitation <-
		setType(list(alpha=4602, beta=29237), "beta pars for probability")

# Attendance proportion
# SOURCE: NAAASP
v2$probOfAttendScreen <- setType(0.750, "probability")
v1distributions$probOfAttendScreen <-
		setType(list(alpha=93170, beta=31022), "beta pars for probability")

# Non-visualisation proportion
# SOURCE: MASS
v2$probOfNonvisualization <- setType(0.0121, "probability")
v1distributions$probOfNonvisualization <-
		setType(list(alpha=329, beta=26818), "beta pars for probability")
		
# Prevalence proportion
fileName <- "input/NAAASP_Men_2020-05-11/AAA max measurements.csv"
v1other$baselineDiameters <- read.csv(fileName, comment.char="#")[, c("size", "pw")]
names(v1other$baselineDiameters) <- c("size", "weight")

v1other$prevalenceThreshold<-3.0

################################################################################
# AAA GROWTH & RUPTURE
## SOURCE FOR GROWTH RATES: MASS
v2$beta0 <- 1.27152200
v2$beta1 <- 0.05838810
v2$sigma0 <- exp(-1.737735221)
v2$sigma1 <- exp(-3.318297793)
v2$rho <- tanh(0.455016818)
v2$sigmaW <- exp(-2.586854985)

## SOURCE FOR RUPTURE RATES: RESCAN 11 STUDIES IN MEN
v2$gamma <- -14.574436
v2$alpha <-  5.9182415
## x <- seq(3,6,by=0.1)
## y <- exp(v2$gamma + v2$alpha*log(x))
# plot(x,y, type = "l")
# 100*exp(v2$gamma + v2$alpha*log(c(3,4,5,5.5))) ## rate per 100-person-years 
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
		setType(c(5.9182415, -14.574436),	"hyperpars for aorta model")
v1distributions$covarianceForRuptureParameters <- setType(
		matrix(nrow=2, data=c(
		0.82822549, -1.1190462,
		-1.1190462,  1.5390578)),
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
v1other$aortaDiameterThresholds <- c(3.0, 4.5, 5.5)
v1other$monitoringIntervals <- c(1, 1, 0.25, Inf)
v1other$maxNumberMonitor <- c(Inf, Inf, Inf)

# Dropout rate from surveillance
# SOURCE: MASS
v2$rateOfDropoutFromMonitoring <- setType(0.01430178 * 4, "rate")  # see below
v1distributions$rateOfDropoutFromMonitoring <- setType(list(
		shape=330, scale=4*4.34e-5), "gamma pars for rate")

# # Incidental detection rate
# SOURCE: GLOVER ET AL. CALIBRATED PARAMETER
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

# Proportion receiving Open vs. EVAR 
# SOURCE: National Vascular Registry
me <- c(intercept = -1.044769, age = -0.092481, aortaSize = 0.305962)
## prob of EVAR at age 80, AAA diameter 6.0cm = 0.74
# 1-plogis(me["intercept"])
## exp(0.092481) ## Odds ratio of receiving EVAR per year increase in age
## exp(-0.305962) ## Odds ratio of receiving EVAR per cm increase in aorta size
v2$probOfElectiveSurgeryIsOpen <-
 setType(me,
         "logistic model for probability")
co <- matrix(c(0.002483, 0.0000626, -0.00012, 0.0000626, 0.00000686, -0.00000674, -0.00012, -0.00000674, 0.000274), nrow=3)
dimnames(co) <- list(names(me), names(me))
v1distributions$probOfElectiveSurgeryIsOpen <-
             setType(list(mean=me, covariance=co), "hyperpars for logistic model for probability")

# Model for peri/post-operative mortality
v1other$electiveSurgeryAaaDeathMethod <- "survivalModel"
		
# EVAR 30-day operative mortality
# SOURCE: National Vascular Registry
me.evar.mort <- c(intercept = -4.80918, age = 0.09267, aortaSize = 0.28863)
## prob of 30-day mortality following elective EVAR at age 80, AAA diameter 6.0cm = 0.008
# plogis(me.evar.mort["intercept"])
# exp(0.09267) ## Odds ratio of receiving EVAR per year increase in age
# exp(0.28863) ## Odds ratio of receiving EVAR per cm increase in aorta size
v2$probOfAaaDeathInInitialPeriodAfterElectiveEvarSurgery <-
 setType(me.evar.mort,
		"logistic model for probability")
co.evar.mort <- matrix(c(0.129149, 0.00055, -0.00478, 0.00055, 0.000370658, -0.00026856, -0.00478, -0.00027, 0.01022), nrow=3)
dimnames(co.evar.mort) <- list(names(me.evar.mort), names(me.evar.mort))
v1distributions$probOfAaaDeathInInitialPeriodAfterElectiveEvarSurgery <-
  setType(list(mean=me.evar.mort, covariance=co.evar.mort),
          "hyperpars for logistic model for probability")

# Open repair 30-day operative mortality
# SOURCE: National Vascular Registry
me.open.mort <- c(intercept = -2.92497, age = 0.08619, aortaSize = 0.11027)
## prob of 30-day mortality following elective Open at age 80, AAA diameter 6.0cm = 0.008
# plogis(me.open.mort["intercept"])
# exp(0.08619) ## Odds ratio of receiving EVAR per year increase in age
# exp(0.11027) ## Odds ratio of receiving EVAR per cm increase in aorta size
v2$probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgery <-
  setType(me.open.mort,
          "logistic model for probability")
co.open.mort <- matrix(c(0.044745269, 7.99E-04, -1.55E-03, 7.99E-04, 1.07E-04, -6.89E-05, -1.55E-03, -6.89E-05,2.76E-03), nrow=3)
dimnames(co.open.mort) <- list(names(me.open.mort), names(me.open.mort))
v1distributions$probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgery <-
  setType(list(mean=me.open.mort, covariance=co.open.mort), 
          "hyperpars for logistic model for probability")

		
# Re-intervention rate after successful EVAR
# SOURCE: EVAR-1 TRIAL
v2$reinterventionRatesAfterElectiveEvar <-
  setType(c(13.5, 3.6) / 100, "reintervention rates")
v1other$reinterventionTimeBoundariesAfterElectiveEvar <- 120 / 365.25
v1distributions$reinterventionRatesAfterElectiveEvar <-
  setType(list(shapes=c(18, 153), scales=c(1/134, 1/4213)), "gamma pars for multiple rates")

# Re-intervention rate after successful open repair
# SOURCE: EVAR-1 TRIAL
v2$reinterventionRatesAfterElectiveOpen <- 
  setType(c(1.6, 1.3) / 100, "reintervention rates")
v1other$reinterventionTimeBoundariesAfterElectiveOpen <- 120 / 365.25
v1distributions$reinterventionRatesAfterElectiveOpen <-
 setType(list(shapes=c(2, 53), scales=c(1/122, 1/4016)), "gamma pars for multiple rates")

# Long-term AAA mortality rate after successful EVAR
# SOURCE: EVAR-1 TRIAL
v2$rateOfAaaDeathAfterElectiveEvarSurgeryAndInitialPeriod <- 
		setType(0.0076640849, "rate")
v1distributions$rateOfAaaDeathAfterElectiveEvarSurgeryAndInitialPeriod <- 
		setType(list(shape=34, scale=2.254143e-04), "gamma pars for rate")

# Long-term AAA mortality rate after successful open repair
# SOURCE: EVAR-1 TRIAL
v2$rateOfAaaDeathAfterElectiveOpenSurgeryAndInitialPeriod <- 
		setType(0.0006991216, "rate")
v1distributions$rateOfAaaDeathAfterElectiveOpenSurgeryAndInitialPeriod <- 
		setType(list(shape=3, scale=0.0002330405), "gamma pars for rate")

################################################################################
# EMERGENCY OPERATIONS

# % operated after rupture
# SOURCE: MASS
v2$probOfEmergencySurgeryIfRupture <- setType(0.368, "probability")
v1distributions$probOfEmergencySurgeryIfRupture <- 
		setType(list(alpha=193, beta=331), "beta pars for probability")
		
# Proportion receiving Open vs. EVAR repair
# SOURCE: NVR
me.emer <- c(intercept = 1.258285, age = -0.044231)
## prob of EVAR at age 80
 # 1-plogis(me.emer["intercept"])
 # exp(0.044231) ## Odds ratio of receiving EVAR per year increase in age
v2$probOfEmergencySurgeryIsOpen <- 
  setType(me.emer, 
          "logistic model for probability")
co.emer <- matrix(c(2.01E-03, 9.40E-05, 9.40E-05, 3.12E-05), nrow=2)
dimnames(co.emer) <- list(names(me.emer), names(me.emer))
v1distributions$probOfEmergencySurgeryIsOpen <-
  setType(list(mean=me.emer, covariance=co.emer), "hyperpars for logistic model for probability")


# Model for peri/post-operative mortality		
# v1other$emergencySurgeryAaaDeathMethod <- "instantDeathOnly"
 v1other$emergencySurgeryAaaDeathMethod <- "survivalModel"

# EVAR 30-day operative mortality
# SOURCE: NVR
 me.emer.evar <- c(intercept = -1.26631, age = 0.04943)
 ## prob of 30-day mortality following emergency EVAR at age 80
 #  plogis(me.emer.evar["intercept"])
 #  exp(0.04943) ## Odds ratio of receiving EVAR per year increase in age
  v2$probOfAaaDeathInInitialPeriodAfterEmergencyEvarSurgery <-
   setType(me.emer.evar,
           "logistic model for probability")
 co.emer.ever <- matrix(c(8.61E-03, 9.27E-05, 9.27E-05, 1.51E-04), nrow=2)
 dimnames(co.emer.ever) <- list(names(me.emer.evar), names(me.emer.evar))
 v1distributions$probOfAaaDeathInInitialPeriodAfterEmergencyEvarSurgery <-
   setType(list(mean=me.emer.evar, covariance=co.emer.ever),
           "hyperpars for logistic model for probability")
 
# Open repair 30-day operative mortality
# SOURCE: NVR
 me.emer.open <- c(intercept = -0.239289, age = 0.064316)
 ## prob of 30-day mortality following emergency Open at age 80
   # plogis(me.emer.open["intercept"])
   # exp(0.064316) ## Odds ratio of receiving EVAR per year increase in age
 v2$probOfAaaDeathInInitialPeriodAfterEmergencyOpenSurgery <-
   setType(me.emer.open,
           "logistic model for probability")
 co.emer.open <- matrix(c(0.001984223, 1.20E-04, 1.20E-04, 2.90E-05), nrow=2)
 dimnames(co.emer.open) <- list(names(me.emer.open), names(me.emer.open))
 v1distributions$probOfAaaDeathInInitialPeriodAfterEmergencyOpenSurgery <-
  setType(list(mean=me.emer.open, covariance=co.emer.open),
          "hyperpars for logistic model for probability")

 
# Re-intervention rate after successful Emergency EVAR
# SOURCE: IMPROVE Trial
 v2$reinterventionRatesAfterEmergencyEvar <-
   setType(10.9 / 100, "reintervention rates")
 v1other$reinterventionTimeBoundariesAfterEmergencyEvar <- numeric()
 v1distributions$reinterventionRatesAfterEmergencyEvar <-
   setType(list(shape=29, scale=1/267), "gamma pars for rate")

# Re-intervention rate after successful open repair
# SOURCE: IMPROVE Trial
 v2$reinterventionRatesAfterEmergencyOpen <- setType(6.1 / 100, "reintervention rates")
 v1other$reinterventionTimeBoundariesAfterEmergencyOpen <- numeric()
 v1distributions$reinterventionRatesAfterEmergencyOpen <-
   setType(list(shape=25, scale=1/410), "gamma pars for rate")

# Long-term rate of death after emergency EVAR
# SOURCE: IMPROVE Trial
 v2$rateOfAaaDeathAfterEmergencyEvarSurgeryAndInitialPeriod <-
   setType(0.985/100, "rate")  
 v1distributions$rateOfAaaDeathAfterEmergencyEvarSurgeryAndInitialPeriod <-
   setType(list(shape=4, scale=1/406), "gamma pars for rate")
 
# Long-term rate of death after emergency Open
# SOURCE: IMPROVE Trial
 v2$rateOfAaaDeathAfterEmergencyOpenSurgeryAndInitialPeriod <-
   setType(1.437 / 100, "rate")
 v1distributions$rateOfAaaDeathAfterEmergencyOpenSurgeryAndInitialPeriod <-
   setType(list(shape=9, scale=1/626), "gamma pars for rate")

################################################################################
# COSTS 
 v2$costs <- setType(c(
   inviteToScreen=1.91,  # SOURCE: NAAASP inflated to 2018/19 prices (Curtis and Burns inflation indices)
   requireReinvitation=1.91, # SOURCE: NAAASP inflated to 2018/19 prices (Curtis and Burns inflation indices)
   screen=36.18, # SOURCE: NAAASP inflated to 2018/19 prices (Curtis and Burns inflation indices)
   monitor=76.40, # SOURCE: NAAASP inflated to 2018/19 prices (Curtis and Burns inflation indices)
   monitorFollowingContraindication=76.40, # SOURCE: NAAASP inflated to 2018/19 prices (Curtis and Burns inflation indices)
   consultation=315.37, # SOURCE: NHS Reference Costs 17/18 and 18/19
   electiveSurgeryEvar=14001.40, # SOURCE: EVAR-1 & HES LOS (Curtis and Burns inflation)
   electiveSurgeryOpen=13184.50, # SOURCE: EVAR-1 & HES LOS (Curtis and Burns inflation)
   emergencySurgeryEvar=19941.99, # SOURCE: IMPROVE & HES LOS (Curtis and Burns inflation)
   emergencySurgeryOpen=20232.53, # SOURCE: IMPROVE & HES LOS (Curtis and Burns inflation)
   reinterventionAfterElectiveEvar=9175.81, # SOURCE: EVAR-1 (Curtis and Burns inflation)
   reinterventionAfterElectiveOpen=12249.27, # SOURCE: EVAR-1 (Curtis and Burns inflation)
   reinterventionAfterEmergencyEvar=9175.81, # SOURCE: EVAR-1 (Curtis and Burns inflation)
   reinterventionAfterEmergencyOpen=12249.27, # SOURCE: EVAR-1 (Curtis and Burns inflation)
   monitorFollowingEvarSurgery=267.43, # SOURCE: Clinical opinion / NHS Reference Costs
   monitorFollowingOpenSurgery=188.84 # SOURCE: Clinical opinion / NHS Reference Costs
 ), type="costs")


## PSA to include 95% interval that is -20%, +25%
 v1distributions$costs <- 
 setType(list(mean=log(v2$costs), variance=rep((0.114)^2,length(v2$costs))), "distribution for costs")
 names(v1distributions$costs$variance) <- names(v1distributions$costs$mean)


################################################################################
# MISCELLANEOUS

# Non-AAA mortality rate
v1other$nonAaaDeathMethod <- "onsIntegerStart"
v1other$nonAaaMortalityRatesFileName <- 
	"input/NAAASP_Men_2020-05-11/nonAaaDeathMortalityRatesForMen_2020-05-11.csv"  


# Overall QoL / utilities
#		createQalyFactors(startAge=v1other$startAge)))
# SOURCE: Love-Koh 2015 (https://reader.elsevier.com/reader/sd/pii/S1098301515018471?token=AB11057F469D57EBE990778CCDC883E9D35D3CFD83AD143352F3AD497E822198F957DBCE2EA86AE6DC16F2F6CE350409)
qol.coef <- c(0.0340613, 0.0475121, 0.0746856, 0.0857069, -0.0021861, -0.00000798, -0.0209698, 0.9346683)
names(qol.coef) <- c("dep2", "dep3", "dep4", "dep5", "age", "agesq", "female", "_cons")
## Cholesky decomposition of vcov matrix from Love-Koh. 
qol.chol <- as.matrix(read.csv("input/NAAASP_Men_2020-05-11/QOL_Cholesky.csv", header=F))
rownames(qol.chol) <- colnames(qol.chol) <- names(qol.coef)
## Assign these to v1other inputs
## Using deprivation quintile 3 (median group)
v1other$qalyFactorAgeBoundaries <- 65+1:35 ## years after age 65 that calculate new EQ5D
qol.ages <- 65:100
X <- cbind(0, 1, 0, 0, 65:100, (65:100)^2, 0, 1) ## Design matrix
v2$qalyFactors <- setType(c(X %*% t(t(qol.coef))), "qaly")
#v1other$qalyFactors <- qol.coef["_cons"] + qol.coef["dep3"] + qol.ages*qol.coef["age"] + qol.ages^2*qol.coef["agesq"]
qalyFactors.vcov <- X %*% qol.chol %*% t(qol.chol) %*% t(X)
v1distributions$qalyFactors <- setType(list(mean = v2$qalyFactors, variance = qalyFactors.vcov), "multivariate normal distribution")


# Discount rates
v1other$lifeYearDiscountRate <- 3.5 / 100
v1other$costDiscountRate <- 3.5 / 100

################################################################################