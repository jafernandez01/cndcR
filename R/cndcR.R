#' cndcR.
#'
#' @name cndcR
#' @docType package
NULL

#' Case study on maize CNDC dataset.
#'
#' The data were extracted from the global synthesis database developed by Ciampitti et al. (2022b) for a global NNI
#' initiative across species. From the cited database, we utilized a subset for maize crop species, including 1150 pairs
#' of observations for W and N% published in eleven studies. The variables of interest for the present study are:
#'
#' \itemize{
#'   \item Id. Number of publication.
#'   \item Paper. Publication of reference.
#'   \item Species. Crop species name.
#'   \item Year. Year of publication.
#'   \item Country.
#'   \item Sites. Location of the study.
#'   \item e. Experimental design of the study.
#'   \item Repetitions. Number of replicates
#'   \item Nrates. N fertilization supply treatment in kg ha-1
#'   \item W_kg_ha. Shoot biomass W in kg ha-1
#'   \item Nupt_kg_ha. Shoot N uptake in kg ha-1
#'   \item Na. Shoot N concentration in %
#'   \item Samp. Sampling time
#'   \item Site_year. Id for the GxExM combination
#'   \item Na_SE. standard deviation for shoot N%
#'   \item W_SE. standard deviation for shoot W biomass
#' }
#'
#' @docType data
#' @keywords datasets
#' @name Data
#' @usage data(Data)
#' @format A data frame with 1150 rows and 39 variables
NULL

#' Review on CNDC in literature dataset.
#'
#' Articles publishing CNDCs in agriculture were screened with a systematic review search using Web of Science and
#' Google Scholar databases (last search on 03/14/2022, 392 articles retrieved in total). The following keywords
#' were used: critical AND nitrogen AND dilution AND curve AND crop. This review covered 111 CNDCs for multiple
#' genotype ✕ environment ✕ management (G ✕ E ✕ M) combinations of 20 crop species (Table S1). These were reported
#' in 46 publications from 1984 to 2022.
#'
#' \itemize{
#'   \item Paper.	Publication reference
#'   \item Year.	Year of publication
#'   \item Crop_type.	Crop name
#'   \item Species.	Scientific name of species
#'   \item studyType.	Type of CNDC developed in the study
#'   \item a.	a coefficient of CNDC
#'   \item b.	b coefficient of CNDC
#'   \item nExp_calibration.	Number of experiments used in calibrations of the CNDC
#'   \item nExp_validation.	Number of experiments used in validation of the CNDC
#'   \item nRates.	Number of N rates used in dataset for fitting CNDC
#'   \item nSamp.	Number of sampling times in dataset for fitting CNDC
#'   \item Model.	Statistical procedure
#'   \item hierarchicalMod.	Specifies if a hierarchical model structure was used
#'   \item minW.	Minimum value of biomass used in dataset
#'   \item maxW.	Maximum value of biomass used in dataset
#'   \item Method.	Method for fitting the CNDC in the study
#'   \item preProcess.	Specifies if a criteria was used to define lower biomass values in the CNDC dataset
#'   \item NLabMethod.	Methodology for N analyses in laboratory
#' }
#'
#' @docType data
#' @keywords datasets
#' @name biblioCNDC
#' @usage data(biblioCNDC)
#' @format A data frame with 111 rows and 18 variables
NULL

