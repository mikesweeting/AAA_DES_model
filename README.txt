This directory contains R scripts to run the discrete event simulation models for AAA screening. 

Please ensure that the R working directory is first set to this directory location, using setwd()

Specifically, this directory contains the following files:

-- Main file_men 4years_FullModel.R   Runs a Full DES model for a time horizon of 4-years based on input parameters for men matching the MASS study (This is used to validate the DES model)
-- Main file_men 4years_SampledModel.R   Runs a Sampled DES model (only men with AAA diameter >3.0cm) for a time horizon of 4-years based on input parameters for men matching the MASS study (This is used to validate the DES model). Provides more accurate incremental costs and effects.
-- Main file_men 30years_FullModel.R   Runs a Full DES model for a time horizon of 30-years based on contemporary input parameters for men (see Glover et al. Medical Decision Making, 2018)
-- Main file_men 30years_SampledModel.R   Runs a Sampled DES model (only men with AAA diameter >3.0cm) for a time horizon of 30-years based on contemporary input parameters for men (see Glover et al. Medical Decision Making, 2018). Provides more accurate incremental costs and effects.
-- Main file_women 30years_RefModel_FullModel.R   Runs a Full DES model for a time horizon of 30-years based on input parameters for women using the current NAAASP screening protocol (see Sweeting et al. The Lancet, 2018)
-- Main file_women 30years_RefModel_SampledModel.R   Runs a Sampled DES model (only women with AAA diameter >3.0cm) for a time horizon of 30-years based on input parameters for women using the current NAAASP screening protocol (see Sweeting et al. The Lancet, 2018). Provides more accurate incremental costs and effects.
-- Main file_women 30years_BestCaseModel_FullModel.R   Runs a Full DES model for a time horizon of 30-years based on input parameters for women using an alternative screening protocol [2.5cm diagnosis, 5.0cm referral, screen age 70] (see Sweeting et al. The Lancet, 2018)
-- Main file_women 30years_BestCaseModel_SampledModel.R   Runs a Sampled DES model (only women with AAA diameter >3.0cm) for a time horizon of 30-years based on input parameters for women using an alternative screening protocol [2.5cm diagnosis, 5.0cm referral, screen age 70] (see Sweeting et al. The Lancet, 2018). Provides more accurate incremental costs and effects.


The following directories are included:

/functions   -- Contains the DES model code
/input 	     -- Contains the input parameters and .csv files needed to run the DES models
/output      -- Directory where Rdata output files are saved

