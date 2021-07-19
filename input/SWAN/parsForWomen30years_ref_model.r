################################################################################
# Parameters for the women 30 year reference models
################################################################################

v0 <- compactList()
v1distributions <- compactList()
v1other <- compactList()
v2 <- compactList()

################################################################################
# MISCELLANEOUS

v1other$postSurgeryInitialPeriod <- 30 / 365.25

#v1other$startAge <- 65

v0$generateCensoringTime <- function() { 30.000001 }

# Post-surgery monitoring. 
v1other$timeToMonitoringFollowingOpenSurgery <- (6 * 7)/ 365.25
v1other$timeBetweenMonitoringFollowingEvarSurgery <- 1

# No growth for women with a diameter of < 2cm 
v1other$zeroGrowthDiameterThreshold <- 2.0

################################################################################
# SCREENING

# Re-invitation proportion
v2$probOfRequireReinvitation <- setType(142127/594376, "probability")
v1distributions$probOfRequireReinvitation <- setType(v2$probOfRequireReinvitation,
         "fixed value for probability")

# Attendance proportion
v2$probOfAttendScreen <- setType(218/300, "probability")
v1distributions$probOfAttendScreen <-
 setType(list(alpha=218, beta=300-218), "beta pars for probability")

# Non-visualisation proportion
v2$probOfNonvisualization <- setType(1652/470531, "probability")
v1distributions$probOfNonvisualization <- setType(v2$probOfNonvisualization,
                                       "fixed value for probability")

# Prevalence proportion
fileName <- "input/SWAN/AAA max measurements.csv"
v1other$baselineDiameters <- read.csv(fileName, comment.char="#")[, c("size", "pw")]
names(v1other$baselineDiameters) <- c("size", "weight")
v2$prevalence <- setType(0.0042756, "probability")
v1distributions$prevalence <-
 setType(list(mean=-5.45054, variance=(0.32321)^2), "normal distribution for logit prevalence")

v1other$prevalenceThreshold<-3.0

################################################################################
# AAA GROWTH & RUPTURE

v2$beta0 <- 1.334426035
v2$beta1 <- 0.05239619
v2$sigma0 <- exp(-1.986425701)
v2$sigma1 <- exp(-3.281826664)
v2$rho <- tanh(0.409567743)
v2$sigmaW <- exp(-2.960876192)
v2$gamma <- -12.39926
v2$alpha <- 5.468415

for (elementName in c("beta0", "beta1", "sigma0", "sigma1", "rho", "sigmaW",
                     "gamma", "alpha"))
 attr(v2[[elementName]], "type") <- "par for aorta model"

# psa 
growthParameterNames <-
 c("beta1", "beta0", "logSigma1", "logSigma0", "atanhRho", "logSigmaW")
ruptureParameterNames <- c("alpha", "gamma")

# Hexavariate normal distribution
v1distributions$meanForGrowthParameters <- setType(
  c(0.05239619, 1.334426035, -3.281826664, -1.986425701, 0.409567743, -2.960876192),
  "hyperpars for aorta model")

v1distributions$covarianceForGrowthParameters <- setType(
  matrix(nrow=6, data=c(
    0.000014724, 0.000006551, 0.000028327, 0.000186028, -0.000124773, -0.000087445,
    0.000006551, 0.000567509, -0.000751974, -0.001363946, -0.000417865, -0.001799988,
    0.000028327, -0.000751974, 0.009516028, 0.005153026, -0.0000471, 0.00240076,
    0.000186028, -0.001363946, 0.005153026, 0.011569443, 0.000842861, 0.005566187,
    -0.000124773, -0.000417865, -0.0000471, 0.000842861, 0.011419265, 0.005260149,
    -0.000087445, -0.001799988, 0.00240076, 0.005566187, 0.005260149, 0.013688456)),
    "hyperpars for aorta model")

# Bivariate normal distribution
v1distributions$meanForRuptureParameters <-
  setType(c(5.468415, -12.39926), "hyperpars for aorta model")
v1distributions$covarianceForRuptureParameters <- setType(
  matrix(nrow=2, data=c(
    1.589157, -2.217831,
    -2.217831, 3.140564)),
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
## Here once in the screening programme, no discharge occurs (maxNumberMonitor == Inf for all sizes)
v1other$aortaDiameterThresholds <- list(c(3.0, 4.5, 5.5))
v1other$monitoringIntervals <- c(1, 1, 0.25, 0.5) ## updated so first interval relates to those normal (e.g. below first threshold), and last interval relates to those contraindicated
v1other$maxNumberMonitor <- c(Inf, Inf, Inf) ## maximum number of monitorings in each size group before discharge from surveillance
# Sub-AAA set-up whereby individuals who initially measure 2.5-3.0 come back for a scan 5 years later. Only 2 scans 2.5-3.0 are allowed before discharge
#v1other$aortaDiameterThresholds <- c(2.5, 3.0, 4.5, 5.5)
#v1other$monitoringIntervals <- c(Inf, 5, 1, 0.25) ## MS. updated so first interval relates to those normal (e.g. below first threshold)
#v1other$maxNumberMonitor <- c(1, 2, Inf, Inf) ## maximum number of monitorings in each size group before discharge

# Dropout rate from surveillance
v2$rateOfDropoutFromMonitoring <- setType(1072/19650, "rate")
v1distributions$rateOfDropoutFromMonitoring <-
 setType(list(shape=1072, scale=1/19650), "gamma pars for rate")

# Incidental detection rate
v2$rateOfIncidentalDetection <- setType(40/1364.25, "rate")
v1distributions$rateOfIncidentalDetection <-
 setType(list(shape=40, scale=1/(321*4.25)), "gamma pars for rate")

# Delay from 5.5+cm scan to consultation
v1other$waitingTimeToConsultation <- 10.6 / 365.25

# Consultation scan
v2$extraDiameterForCtScan <- setType(0.244, "fixed value")
v2$ctMeasurementErrorSD <- setType(0.19, "fixed value")
v1distributions$extraDiameterForCtScan <-
 setType(v2$extraDiameterForCtScan, "fixed value")
v1distributions$ctMeasurementErrorSD <-
 setType(v2$ctMeasurementErrorSD, "fixed value")

# Decision at consultation: proportion returned to surveillance
# No variables need to be defined here. 

# Decision at consultation: non-intervention proportion
v2$probOfContraindication <- setType(c(intercept = qlogis(0.34226)), "logistic model for probability")
v1distributions$probOfContraindication <-
 setType(list(mean=-0.65324, variance=(0.13502)^2), "hyperpars for logistic model for probability")

# Decision at consultation: proportion elective surgery
# 1 minus probOfContraindication 

# Delay from consultation to elective surgery
v1other$waitingTimeToElectiveSurgery <- 70.8 / 365.25

################################################################################
# ELECTIVE OPERATIONS

# Proportion receiving EVAR vs. open repair
v2$ probOfElectiveSurgeryIsOpen <-
 setType(c(intercept = -0.702391, age = -0.095305, aortaSize = 0.303022),
         "logistic model for probability")
me <- c(intercept = -0.702391, age = -0.095305, aortaSize = 0.303022)
names(me) <- c("intercept", "age", "aortaSize")
co <- matrix(c(0.003111046, 0.0002046573, -0.0008173591, 0.0002046573, 0.00005598497, -0.000067048, -0.0008173591, -0.000067048, 0.0028017033), nrow=3)
dimnames(co) <- list(names(me), names(me))
v1distributions$probOfElectiveSurgeryIsOpen <-
             setType(list(mean=me, covariance=co), "hyperpars for logistic model for probability")

# Model for peri/post-operative mortality
v1other$electiveSurgeryAaaDeathMethod <- "survivalModel"

# EVAR 30-day operative mortality
v2$probOfAaaDeathInInitialPeriodAfterElectiveEvarSurgery <-
 setType(c(intercept = -3.909914, age = 0.002297, aortaSize = -0.027854,
		logOddsAdjustment =log((37/(1642-37))/(27/(1642-27)))),
         "logistic model for probability")

# PSA
me.evar.mort <- c(intercept = -3.909914, age = 0.002297, aortaSize = -0.027854)
names(me.evar.mort) <- c("intercept", "age", "aortaSize")
co.evar.mort <- matrix(c(0.0471354, 0.001398098, -0.008779374, 0.001398098, 0.000998351, -0.000983538, -0.008779374, -0.000983538, 0.0662445), nrow=3)
dimnames(co.evar.mort) <- list(names(me.evar.mort), names(me.evar.mort))
v1distributions$probOfAaaDeathInInitialPeriodAfterElectiveEvarSurgery <-
  setType(list(mean=me.evar.mort, covariance=co.evar.mort, logOddsAdjustment=log((37/(1642-37))/(27/(1642-27)))),
          "hyperpars for logistic model for probability")

# Open repair 30-day operative mortality
  v2$probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgery <-
    setType(c(intercept = -2.33601, age = 0.06416, aortaSize = 0.07745,
          logOddsAdjustment=log((75/(1066-75))/(64/(1066-64)))),
            "logistic model for probability")
# PSA
me.open.mort <- c(intercept = -2.33601, age = 0.06416, aortaSize = 0.07745)
names(me.open.mort) <- c("intercept", "age", "aortaSize")
co.open.mort <- matrix(c(0.02722675, 0.00172895, -0.00852093, 0.00172895, 0.000487737, -0.00047439, -0.00852093, -0.00047439, 0.016345717), nrow=3)
dimnames(co.open.mort) <- list(names(me.open.mort), names(me.open.mort))
v1distributions$probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgery <-
  setType(list(mean=me.open.mort, covariance=co.open.mort, logOddsAdjustment=log((75/(1066-75))/(64/(1066-64)))),
          "hyperpars for logistic model for probability")

# Re-intervention rate after successful EVAR
v2$reinterventionRatesAfterElectiveEvar <-
 setType(c(20.3, 6.4) / 100, "reintervention rates")
v1other$reinterventionTimeBoundariesAfterElectiveEvar <- 120 / 365.25
v1distributions$reinterventionRatesAfterElectiveEvar <-
 setType(list(shapes=c(3, 27), scales=c(1/15, 1/421)), "gamma pars for multiple rates")

# Re-intervention rate after successful open repair
v2$reinterventionRatesAfterElectiveOpen <- setType(0, "reintervention rates")
v1other$reinterventionTimeBoundariesAfterElectiveOpen <- numeric()
v1distributions$reinterventionRatesAfterElectiveOpen <-
 setType(v2$reinterventionRatesAfterElectiveOpen, "fixed value for reintervention rates")

# Long-term AAA mortality rate after successful EVAR
v2$rateOfAaaDeathAfterElectiveEvarSurgeryAndInitialPeriod <-
 setType(1.799 / 100, "rate")
v1distributions$rateOfAaaDeathAfterElectiveEvarSurgeryAndInitialPeriod <-
 setType(list(shape=8, scale=1/444.7), "gamma pars for rate")

# Long-term AAA mortality rate after successful open repair
v2$rateOfAaaDeathAfterElectiveOpenSurgeryAndInitialPeriod <-
 setType(0.499 / 100, "rate")
v1distributions$rateOfAaaDeathAfterElectiveOpenSurgeryAndInitialPeriod <-
 setType(list(shape=2, scale=1/400.8), "gamma pars for rate")

################################################################################
# EMERGENCY OPERATIONS

# % operated after rupture
v2$probOfEmergencySurgeryIfRupture <- setType(0.25, "probability")
v1distributions$probOfEmergencySurgeryIfRupture <-
 setType(list(mean=0.25, variance=(0.05)^2), "truncated normal distribution")

# Proportion receiving EVAR vs. open repair
v2$probOfEmergencySurgeryIsOpen <-
 setType(c(intercept = 1.547574, age = -0.040946),
         "logistic model for probability")
# PSA
me.emer <- c(intercept = 1.547574, age = -0.040946)
names(me.emer) <- c("intercept", "age")
co.emer <- matrix(c(0.009274124, 0.000107859, 0.000107859, 0.0001915343), nrow=2)
dimnames(co.emer) <- list(names(me.emer), names(me.emer))
v1distributions$probOfEmergencySurgeryIsOpen <-
 setType(list(mean=me.emer, covariance=co.emer), "hyperpars for logistic model for probability")

# Model for peri/post-operative mortality
v1other$emergencySurgeryAaaDeathMethod <- "survivalModel"

# EVAR 30-day operative mortality
v2$probOfAaaDeathInInitialPeriodAfterEmergencyEvarSurgery <-
 setType(c(intercept = -1.14998, age = 0.06071,
	logOddsAdjustment=log((48/(244-48))/(31/(244-31)))),
         "logistic model for probability")
# PSA
me.emer.evar <- c(intercept = -1.14998, age = 0.06071)
names(me.emer.evar) <- c("intercept", "age")
co.emer.ever <- matrix(c(0.04464549, -0.00136216, -0.00136216, 0.000737284), nrow=2)
dimnames(co.emer.ever) <- list(names(me.emer.evar), names(me.emer.evar))
v1distributions$probOfAaaDeathInInitialPeriodAfterEmergencyEvarSurgery <-
 setType(list(mean=me.emer.evar, covariance=co.emer.ever, logOddsAdjustment=log((48/(244-48))/(31/(244-31)))),
         "hyperpars for logistic model for probability")

# Open repair 30-day operative mortality
v2$probOfAaaDeathInInitialPeriodAfterEmergencyOpenSurgery <-
 setType(c(intercept = -0.34268, age = 0.03293,
	logOddsAdjustment=log((319/(845-319))/(284/(845-284)))),
         "logistic model for probability")
# PSA
me.emer.open <- c(intercept = -0.34268, age = 0.03293)
names(me.emer.open) <- c("intercept", "age")
co.emer.open <- matrix(c(0.006999579, 0.000271148, 0.000271148, 0.000138722), nrow=2)
dimnames(co.emer.open) <- list(names(me.emer.open), names(me.emer.open))
v1distributions$probOfAaaDeathInInitialPeriodAfterEmergencyOpenSurgery <-
 setType(list(mean=me.emer.open, covariance=co.emer.open, logOddsAdjustment=log((319/(845-319))/(284/(845-284)))),
         "hyperpars for logistic model for probability")

# Re-intervention rate after successful EVAR
v2$reinterventionRatesAfterEmergencyEvar <-
 setType(15.8 / 100, "reintervention rates")
v1other$reinterventionTimeBoundariesAfterEmergencyEvar <- numeric()
v1distributions$reinterventionRatesAfterEmergencyEvar <-
 setType(list(shape=9, scale=1/57), "gamma pars for rate")

# Re-intervention rate after successful open repair
v2$reinterventionRatesAfterEmergencyOpen <- setType(2.3 / 100, "reintervention rates")
v1other$reinterventionTimeBoundariesAfterEmergencyOpen <- numeric()
v1distributions$reinterventionRatesAfterEmergencyOpen <-
 setType(list(shape=2, scale=1/85), "gamma pars for rate")

# Long-term AAA mortality rate after successful EVAR
v2$rateOfAaaDeathAfterEmergencyEvarSurgeryAndInitialPeriod <-
 setType(1e-100, "rate")  # the document says zero; search for myRexp
v1distributions$rateOfAaaDeathAfterEmergencyEvarSurgeryAndInitialPeriod <-
 setType(v2$rateOfAaaDeathAfterEmergencyEvarSurgeryAndInitialPeriod, "fixed value for rate")

# Long-term AAA mortality rate after successful open repair
v2$rateOfAaaDeathAfterEmergencyOpenSurgeryAndInitialPeriod <-
 setType(1.613 / 100, "rate")
v1distributions$rateOfAaaDeathAfterEmergencyOpenSurgeryAndInitialPeriod <-
 setType(list(shape=2, scale=1/124), "gamma pars for rate")

################################################################################
# COSTS

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

v1distributions$costs <-
 setType(list(mean=log(v2$costs), 
              variance=rep((0.114)^2,length(v2$costs))), "distribution for costs")
names(v1distributions$costs$variance) <- names(v1distributions$costs$mean)


################################################################################
# MISCELLANEOUS

# Non-AAA mortality rate
v1other$nonAaaDeathMethod <- "onsIntegerStart"
v1other$nonAaaMortalityRatesFileName <-
 "input/SWAN/nonAaaDeathMortalityRatesForWomen.csv"

# Overall QoL / utilities
v1other$qalyFactorAgeBoundaries <- 75
v2$qalyFactors <- setType(c(0.78, 0.71), "qaly")
# qalys <- createQalyFactors(
#   startAge=v1other$startAge,
#   qalyFactorBoundariesAsAges=c(65, 75),
#   qalyFactorsForAges=c(0.81, 0.78, 0.71)
# )
# v1other$qalyFactorBoundaries <- qalys$qalyFactorBoundaries
# v2$qalyFactors <- setType(qalys$qalyFactors, "qaly")
v1distributions$qalyFactors <- 
  setType(v2$qalyFactors, "fixed value for qaly")


# Discount rates
v1other$lifeYearDiscountRate <- 3.5 / 100
v1other$costDiscountRate <- 3.5 / 100

################################################################################

