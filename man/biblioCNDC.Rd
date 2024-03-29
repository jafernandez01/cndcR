% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cndcR.R
\docType{data}
\name{biblioCNDC}
\alias{biblioCNDC}
\title{Review on CNDC in literature dataset.}
\format{
A data frame with 111 rows and 18 variables
}
\usage{
data(biblioCNDC)
}
\description{
Articles publishing CNDCs in agriculture were screened with a systematic review search using Web of Science and
Google Scholar databases (last search on 03/14/2022, 392 articles retrieved in total). The following keywords
were used: critical AND nitrogen AND dilution AND curve AND crop. This review covered 111 CNDCs for multiple
genotype ✕ environment ✕ management (G ✕ E ✕ M) combinations of 20 crop species (Table S1). These were reported
in 46 publications from 1984 to 2022.
}
\details{
\itemize{
  \item Paper.	Publication reference
  \item Year.	Year of publication
  \item Crop_type.	Crop name
  \item Species.	Scientific name of species
  \item studyType.	Type of CNDC developed in the study
  \item a.	a coefficient of CNDC
  \item b.	b coefficient of CNDC
  \item nExp_calibration.	Number of experiments used in calibrations of the CNDC
  \item nExp_validation.	Number of experiments used in validation of the CNDC
  \item nRates.	Number of N rates used in dataset for fitting CNDC
  \item nSamp.	Number of sampling times in dataset for fitting CNDC
  \item Model.	Statistical procedure
  \item hierarchicalMod.	Specifies if a hierarchical model structure was used
  \item minW.	Minimum value of biomass used in dataset
  \item maxW.	Maximum value of biomass used in dataset
  \item Method.	Method for fitting the CNDC in the study
  \item preProcess.	Specifies if a criteria was used to define lower biomass values in the CNDC dataset
  \item NLabMethod.	Methodology for N analyses in laboratory
}
}
\keyword{datasets}
