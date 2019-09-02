################################################################################
# Parameters for the men 30 year models
# Updated 27/08/19 to include
# 1) post-operative surveillance
# 2) age-dependent (elective and emergency) operative mortality
# 3) re-intervention rates

################################################################################

if (!exists("v0")) v0 <- compactList() 
if (!exists("v1distributions")) v1distributions <- compactList() 
if (!exists("v1other")) v1other <- compactList() 
if (!exists("v2")) v2 <- compactList() 

################################################################################
# MISCELLANEOUS

v1other$postSurgeryInitialPeriod <- 30 / 365.25  # = 0.08213552

v1other$startAge <- 65

v0$generateCensoringTime <- function() { 30.000001 }

# Post-surgery monitoring. 
v1other$timeToMonitoringFollowingOpenSurgery <- (6 * 7)/ 365.25
v1other$timeBetweenMonitoringFollowingEvarSurgery <- 1

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
fileName <- "input/AAA max measurements.csv"
v1other$baselineDiameters <- read.csv(fileName, comment.char="#")[, c("size", "pw")]
names(v1other$baselineDiameters) <- c("size", "weight")

v1other$prevalenceThreshold<-3.0

################################################################################
# AAA GROWTH & RUPTURE
## USING GAMMA AND ALPHA ESTIMATES FROM JOINT LONGITUDINAL & RUPTURE MODEL WITH SEX AS A COVARIATE
v2$beta0 <- 1.27152200
v2$beta1 <- 0.05838810
v2$sigma0 <- exp(-1.737735221)
v2$sigma1 <- exp(-3.318297793)
v2$rho <- tanh(0.455016818)
v2$sigmaW <- exp(-2.586854985)
v2$gamma <- -13.843
v2$alpha <- 5.446
## x <- seq(3,6,by=0.1)
## y <- exp(v2$gamma + v2$alpha*log(x))
# plot(x,y, type = "l")
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
v1other$aortaDiameterThresholds <- c(3.0, 4.5, 5.5)
v1other$monitoringIntervals <- c(1, 1, 0.25)
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
## MS CHANGED TO ALLOW TURN-DOWN RATE TO INCREASE WITH AGE
## LOG-ODDS RATIO PER YR INCREASE IS 0.1838371 TAKEN FROM WHIITAKER (DUDLEY) STUDY IN WOMEN
## ASSUMING AT MEAN AGE OF CONSULTATION (AROUND 75) THE PROBABILITY IS 18.6% AS FOUND IN SYSTEMATIC REVIEW
## NO PSA FOR THIS CURRENTLY
#v2$probOfContraindication <- setType(0.0977 / (0.0977 + 0.681), "probability")
# v1distributions$probOfContraindication <- 
# 		setType(list(alpha=69, beta=481), "beta pars for probability")
# Intercept refers to age 80 so need intercept = qlogis(0.186)+5*0.1838371 = -0.5570
v2$probOfContraindication <- setType(c(intercept = -0.5570, age = 0.1838371), "logistic model for probability")
#x <- 70:90
#plot(x, plogis(-0.557+0.1838371*(x-80)),type="l",ylim=c(0,1))
		
# Decision at consultation: proportion elective surgery
# 1 minus probOfContraindication 

# Delay from consultation to elective surgery
v1other$waitingTimeToElectiveSurgery <- 59 / 365.25

################################################################################
# ELECTIVE OPERATIONS

# Proportion receiving EVAR vs. open repair
me <- c(intercept = -1.044769, age = -0.092481, aortaSize = 0.305962)
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
me.evar.mort <- c(intercept = -4.80918, age = 0.09267, aortaSize = 0.28863)
v2$probOfAaaDeathInInitialPeriodAfterElectiveEvarSurgery <-
 setType(me.evar.mort,
		"logistic model for probability")
co.evar.mort <- matrix(c(0.129149, 0.00055, -0.00478, 0.00055, 0.000370658, -0.00026856, -0.00478, -0.00027, 0.01022), nrow=3)
dimnames(co.evar.mort) <- list(names(me.evar.mort), names(me.evar.mort))
v1distributions$probOfAaaDeathInInitialPeriodAfterElectiveEvarSurgery <-
  setType(list(mean=me.evar.mort, covariance=co.evar.mort),
          "hyperpars for logistic model for probability")

# Open repair 30-day operative mortality
me.open.mort <- c(intercept = -2.92497, age = 0.08619, aortaSize = 0.11027)
v2$probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgery <-
  setType(me.open.mort,
          "logistic model for probability")
co.open.mort <- matrix(c(0.044745269, 7.99E-04, -1.55E-03, 7.99E-04, 1.07E-04, -6.89E-05, -1.55E-03, -6.89E-05,2.76E-03), nrow=3)
dimnames(co.open.mort) <- list(names(me.open.mort), names(me.open.mort))
v1distributions$probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgery <-
  setType(list(mean=me.open.mort, covariance=co.open.mort), 
          "hyperpars for logistic model for probability")

		
# Re-intervention rate after successful EVAR
## TAKEN FROM EVAR-1 TRIAL
v2$reinterventionRatesAfterElectiveEvar <-
  setType(c(13.5, 3.6) / 100, "reintervention rates")
v1other$reinterventionTimeBoundariesAfterElectiveEvar <- 120 / 365.25
v1distributions$reinterventionRatesAfterElectiveEvar <-
  setType(list(shapes=c(18, 153), scales=c(1/134, 1/4213)), "gamma pars for multiple rates")

# Re-intervention rate after successful open repair
v2$reinterventionRatesAfterElectiveOpen <- 
  setType(c(2, 53) / 100, "reintervention rates")
v1other$reinterventionTimeBoundariesAfterElectiveOpen <- 120 / 365.25
v1distributions$reinterventionRatesAfterElectiveOpen <-
 setType(list(shapes=c(2, 53), scales=c(1/122, 1/4016)), "gamma pars for multiple rates")

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
me.emer <- c(intercept = 1.258285, age = -0.044231)
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
 me.emer.evar <- c(intercept = -1.26631, age = 0.04943)
 v2$probOfAaaDeathInInitialPeriodAfterEmergencyEvarSurgery <-
   setType(me.emer.evar,
           "logistic model for probability")
 co.emer.ever <- matrix(c(8.61E-03, 9.27E-05, 9.27E-05, 1.51E-04), nrow=2)
 dimnames(co.emer.ever) <- list(names(me.emer.evar), names(me.emer.evar))
 v1distributions$probOfAaaDeathInInitialPeriodAfterEmergencyEvarSurgery <-
   setType(list(mean=me.emer.evar, covariance=co.emer.ever),
           "hyperpars for logistic model for probability")
 
# Open repair 30-day operative mortality
 me.emer.open <- c(intercept = -0.239289, age = 0.064316)
 v2$probOfAaaDeathInInitialPeriodAfterEmergencyOpenSurgery <-
   setType(me.emer.open,
           "logistic model for probability")
 co.emer.open <- matrix(c(0.001984223, 1.20E-04, 1.20E-04, 2.90E-05), nrow=2)
 dimnames(co.emer.open) <- list(names(me.emer.open), names(me.emer.open))
 v1distributions$probOfAaaDeathInInitialPeriodAfterEmergencyOpenSurgery <-
  setType(list(mean=me.emer.open, covariance=co.emer.open),
          "hyperpars for logistic model for probability")

 
# Re-intervention rate after successful EVAR
 v2$reinterventionRatesAfterEmergencyEvar <-
   setType(10.9 / 100, "reintervention rates")
 v1other$reinterventionTimeBoundariesAfterEmergencyEvar <- numeric()
 v1distributions$reinterventionRatesAfterEmergencyEvar <-
   setType(list(shape=29, scale=1/267), "gamma pars for rate")

# Re-intervention rate after successful open repair
 v2$reinterventionRatesAfterEmergencyOpen <- setType(6.1 / 100, "reintervention rates")
 v1other$reinterventionTimeBoundariesAfterEmergencyOpen <- numeric()
 v1distributions$reinterventionRatesAfterEmergencyOpen <-
   setType(list(shape=25, scale=1/410), "gamma pars for rate")

# Long-term rate of death after EVAR
 v2$rateOfAaaDeathAfterEmergencyEvarSurgeryAndInitialPeriod <-
   setType(0.985/100, "rate")  
 v1distributions$rateOfAaaDeathAfterEmergencyEvarSurgeryAndInitialPeriod <-
   setType(list(shape=4, scale=1/406), "gamma pars for rate")
 
# Long-term rate of death after Open
 v2$rateOfAaaDeathAfterEmergencyOpenSurgeryAndInitialPeriod <-
   setType(1.437 / 100, "rate")
 v1distributions$rateOfAaaDeathAfterEmergencyOpenSurgeryAndInitialPeriod <-
   setType(list(shape=9, scale=1/626), "gamma pars for rate")

################################################################################
# COSTS (SAME AS SWAN MODEL FOR WOMEN)
 v2$costs <- setType(c(
   inviteToScreen=1.80,
   requireReinvitation=1.80,
   screen=34.11,
   monitor=72.03,
   monitorFollowingContraindication=72.03,
   consultation=328.64,
   electiveSurgeryEvar=13844,
   electiveSurgeryOpen=13060,
   emergencySurgeryEvar=16154,
   emergencySurgeryOpen=17613,
   reinterventionAfterElectiveEvar=7546,
   reinterventionAfterElectiveOpen=8986,
   reinterventionAfterEmergencyEvar=7546,
   reinterventionAfterEmergencyOpen=8986,
   monitorFollowingEvarSurgery=258.16,
   monitorFollowingOpenSurgery=196.79
 ), type="costs")

v1distributions$costs <- setType(v2$costs, "fixed value for costs")

################################################################################
# MISCELLANEOUS

# Non-AAA mortality rate
v1other$nonAaaDeathMethod <- "onsIntegerStart"
v1other$nonAaaMortalityRatesFileName <- 
	"input/nonAaaDeathMortalityRatesForMen.csv"  

# Non-AAA mortality rate in those contraindicated
## REMOVED THIS DUE TO ISSUE WITH BIASING INCREMENTAL EFFECTS (CONTRAINDICATION DEATH RATE ONLY APPLIED ONCE INDIVIDUAL IS DIAGNOSED)
# v2$rateOfNonAaaDeathAfterContraindication <- setType(41 / 166, "rate")
# v1distributions$rateOfNonAaaDeathAfterContraindication <- setType(list(
# 		shape=41, scale=4*0.0015), "gamma pars for rate")

# Overall QoL / utilities
v1other <- compactList(append(v1other,
		createQalyFactors(startAge=v1other$startAge)))

# Discount rates
v1other$lifeYearDiscountRate <- 3.5 / 100
v1other$costDiscountRate <- 3.5 / 100

################################################################################