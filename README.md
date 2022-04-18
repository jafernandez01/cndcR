# cndcR

[![R-CMD-check](https://github.com/jafernandez01/cndcR/actions/workflows/check-standard.yml/badge.svg?branch=main)](https://github.com/jafernandez01/cndcR/actions/workflows/check-standard.yml)
[![License: CC BY-SA 4.0](https://img.shields.io/badge/License-CC_BY--SA_4.0-red.svg)](https://github.com/jafernandez01/cndcR/blob/main/LICENSE.md)
[![DOI](https://zenodo.org/badge/482033970.svg)](https://zenodo.org/badge/latestdoi/482033970)

cndcR is an R data package of the manuscript "Minimum dataset characteristics for the determination of critical nitrogen dilution curves using field experiments". The package contains the source data and R codes used for sensitivity analyses to fit critical N dilution curves in crop species using Bayesian models.

## Installation

You can install the development version from Github, with the `remotes` package:

```r
remotes::install_github("jafernandez01/cndcR")
```

The package contains:

* R source code to retrieve figures in the paper.
* Data files required to produce all results and figures presented.

## Paper: *Minimum dataset characteristics for the determination of critical nitrogen dilution curves using field experiments*

by [Javier A. Fernandez](http://jafernandez.netlify.app)<sup>1*</sup>, Emmanuela van Versendaal<sup>1</sup>, Josefina Lacasa<sup>1</sup>, David Makowski<sup>2</sup>, Gilles Lemaire<sup>3</sup>, and Ignacio A. Ciampitti <sup>1*</sup>

<sup>1</sup> Department of Agronomy, Kansas State University, Manhattan, KS 66506

<sup>2</sup> University Paris-Saclay, INRAE, AgroParisTech, UMR MIA 518, 75231 Paris, France

<sup>3</sup> Honorary Director of Research, INRAE, 86600 Lusignan, France

<br> 

**Submitted to European Journal of Agronomy**


## Abstract
The determination of critical nitrogen (N) dilution curves (CNDCs) has been the subject of intensive study for the last decades due to its relevance to diagnosing crop nitrogen (N) status. However, to date, minimum steps, data requirements and robust science-based guidelines to estimate CNDCs based on experimental data have not been formalized in the literature to ensure the reliability of an established nutrient dilution curve. In this study, we conducted a systematic review of the literature on CNDCs, described the main characteristics of the datasets used to establish CNDCs in the past, and finally identified a set of criteria specifying the minimum characteristics that a dataset must satisfy to be used to establish an accurate CNDC. Published CNDC studies showed large heterogeneity in the number of experiments (from 1 to 35), fertilizer N rates (from 2 to 7), and sampling times (from 2 to 16) used to fit the CNDCs. Given that, we quantified the sensitivity of the CNDC parameters to five dataset characteristics (number of experiments, N rates, and sampling times, attainment of Wmax plateau, and precision of the studies) using a Bayesian statistical model on a case-study estimation for maize. We found that the number of experiments is the main factor affecting the uncertainty of the fitted CNDC. Bootstrapping analyses showed that accurate CNDCs can be fitted with experiments having at least three N rates. Increasing the number of sampling times was more effective than using more than three N rates for reducing the uncertainty in the estimation of the parameters for the CNDCs. We also showed that more reliable CNDCs can be obtained when weighting each data by its precision. We concluded our analysis with formal recommendations of the minimum steps and data requirements to fit CNDC. The criteria established here should serve as a guide for the establishment of reliable dilution curves for N and other nutrients in the agricultural and biological sciences.


## License
This data package is distributed under the [CC-BY-SA 4.0 License](https://creativecommons.org/licenses/by-sa/4.0/), see [LICENSE](https://github.com/jafernandez01/cndcR/blob/main/LICENSE.md) for more information.
