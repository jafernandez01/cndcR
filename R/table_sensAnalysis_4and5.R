#' @name table1
#' @title
#' Retrieve table 1 from Fernandez et al. (2022)
#' @details
#' The four models were compared based on model parameters:
#' 1.  Posterior distributions for a (A1) and b (A2) are averaged to obtain posterior medians and
#' credibility intervals from all the samples.
#' 2.  Then, they are used to estimate NNI values across an independent validation set of
#' observations and compared against the reference curve by Plenet and Lemaire (2000).
#' 3.  Error metrics for the agreement between NNI estimated and reference are calculated using the
#' metrica:: library.
#' 4.  Deviance information criterion (DIC) is retrieved for each model.

#' @description
#' This function returns values for table 1 in Fernandez et al.(2022): Evaluation of four critical
#' nitrogen dilution curves (CNDC) models fitted with the Bayesian approach using four types of
#' datasets: (All data or only Wmax achieved data) ✕ (unweighted or variance-weighted data model).
multi_metrics <- yardstick::metric_set(yardstick::rsq,yardstick::rmse, yardstick::ccc)

table1 <- function() {

# Obtain medians of a and b parameters -------------------------------
  parSens_4and5 <- eval(parse(text = "cndcR:::fdataSens_4and5"))  %>%
  dplyr::group_by(.data$Method, .data$Parameter)  %>%
  dplyr::summarise(Value = stats::median(.data$Value))

# Testing CNDCs on a validation data_base ---------------------------
  metSens_4and5 <- cndcR::validationSet  %>%
  tidyr::expand_grid(Method = c("Unweighted-Wmax", "Unweighted-Wall",
                                "Weighted-Wmax", "Weighted-Wall", "ref"))  %>%
  # computing NNI from reference and estimated curve
  dplyr::mutate(
    Nc = dplyr::case_when(
      Method == "ref" ~ 3.40 * (W^(-0.37)),
      Method == "Unweighted-Wmax" ~ as.numeric(parSens_4and5[parSens_4and5$Method == "Unweighted-Wmax" & parSens_4and5$Parameter == "A1", 3]) * (W^(-as.numeric(parSens_4and5[parSens_4and5$Method == "Unweighted-Wmax" & parSens_4and5$Parameter == "A2", 3]))),
      Method == "Unweighted-Wall" ~ as.numeric(parSens_4and5[parSens_4and5$Method == "Unweighted-Wall" & parSens_4and5$Parameter == "A1", 3]) * (W^(-as.numeric(parSens_4and5[parSens_4and5$Method == "Unweighted-Wall" & parSens_4and5$Parameter == "A2", 3]))),
      Method == "Weighted-Wmax" ~ as.numeric(parSens_4and5[parSens_4and5$Method == "Weighted-Wmax" & parSens_4and5$Parameter == "A1", 3]) * (W^(-as.numeric(parSens_4and5[parSens_4and5$Method == "Weighted-Wmax" & parSens_4and5$Parameter == "A2", 3]))),
      Method == "Weighted-Wall" ~ as.numeric(parSens_4and5[parSens_4and5$Method == "Weighted-Wall" & parSens_4and5$Parameter == "A1", 3]) * (W^(-as.numeric(parSens_4and5[parSens_4and5$Method == "Weighted-Wall" & parSens_4and5$Parameter == "A2", 3]))),
      TRUE ~ NA_real_),
    NNI = .data$`N(%)` / .data$Nc)  %>%
  dplyr::filter(.data$NNI < 1.8)  %>%  # removing unrealistic outliers of NNI
  tidyr::pivot_wider(names_from = .data$Method, values_from = .data$NNI,
                       id_cols = c(.data$Id, .data$Paper, .data$W, .data$`N(%)`))  %>%
  tidyr::pivot_longer(names_to = "Method", values_to = "NNI", cols = c(5:8)) %>%
  # calculate agreement between estimated and reference values for NNI
  dplyr::group_by(.data$Method) %>%
  cndcR::multi_metrics(ref,NNI) %>%
  dplyr::mutate_if(is.double, .funs = ~ round(digits = 3, x = .))

#' @examples
#' \dontrun{
#' Compare DIC of models ---------------------------------------------
#' dicSens_4and5 <- data_frame(Method = c("Unweighted-Wmax", "Unweighted-Wall", "Weighted-Wmax",
#' "Weighted-Wall"))  %>%
#'   mutate(DIC = case_when(
#'     Method == "Unweighted-Wmax" ~ dic_4[[1]][[1]]  %>%  sum(),
#'     Method == "Unweighted-Wall" ~ dic_4[[2]][[1]]  %>%  sum(),
#'     Method == "Weighted-Wmax" ~ dic_5[[1]][[1]]  %>%  sum(),
#'     Method == "Weighted-Wall" ~ dic_5[[2]][[1]]  %>%  sum(),
#'     TRUE ~ NA_real_
#'   ))  %>%
#'   mutate_if(is.double, .funs = ~ round(digits = 0, x = .))
#' }

  return(list(parSens_4and5, metSens_4and5))
}
