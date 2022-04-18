#' @name sensAnalysis_4and5
#' @title
#' Fourth and fifth sensitivity analysis
#' @details
#' See 'Examples' for the source code to obtain the given results. Briefly, the Bayesian
#' hierarchical model explained by Makowski et al. (2020) was fitted to four combinations of
#' datasets: using all data or only Wmax plateau achieved AND variance-weighted or unweighted data.
#' @description
#' This function returns results from the 4th and 5th sensitivity analysis in Fernandez et al.
#' (2022): Using all data or only Wmax plateau achieved AND variance-weighted or unweighted data.
#' Output is a tibble with posterior expectations and credibility intervals of a (A1) and b (A2)
#' parameters of the CNDC.
#'
#' @note Parallel computation is recommended if running the example code.
#'
#' @examples
#' \dontrun{
#' # Filter data by Wmax plateau achieved (or not):
#' # 1.  For each sampling time within a study, relative biomass (RelW) was first calculated as the
#' # quotient between biomass and the maximum biomass achieved across all N supply treatments.
#' # This procedure was performed to standardize the levels of biomass across sampling dates.
#'
#' # 2.  Then, both linear and linear-plateau models were fitted to the biomass data across N rates
#' # with an MCMC algorithm using the mcp:: library. Weakly-informative priors were defined for
#' # both models. The algorithm was run with three chains of 15,000 iterations each (5,000
#' # discarded as a burn-in period).
#'
#' # 3.  Last, models were compared by calculating the widely application information criteria
#' # (WAIC). A sampling time by study combination was defined as "selected" or "excluded" if
#' # the lowest WAIC was observed for a linear-plateau or linear response of RelW with
#' # increasing N supply.
#'
#' # 4.  Predictions are extracted for both models (linear and linear-plateau) in case visual
#' # assessment is performed.
#'
#' model_linp <- list(
#'   RelW ~ 1 + Nrates,
#'   1 ~ 0
#' )
#' model_null <- list(RelW ~ 1 + Nrates)
#'
#' prior_linp <- list(
#'   int_1 = "dnorm(0.5, 1) T(, 1)",
#'   Nrates_1 = "dnorm(0, 5)",
#'   cp_1 = "dunif(MINX, MAXX)"
#' )
#' prior_null <- list(
#'   int_1 = "dnorm(0.5, 1) T(, 1)",
#'   Nrates_1 = "dnorm(0, 5)"
#' )
#'
#'     dataSens_4 <- Data |>
#'       dplyr::group_by(Site_year, Samp) |>
#'       dplyr::mutate(
#'         Wmax = max(W, na.rm = T),
#'         RelW = W / Wmax
#'       ) |>
#'       dplyr::group_by(Site_year, Samp) |>
#'       tidyr::nest() |>
#'
#'       # loop for linear and linear-plateau models --------------------
#'     dplyr::mutate(
#'       m1 = data |> purrr::map(purrr::possibly(~ mcp::mcp(
#'         model = model_null, data = .x, iter = 10000, adapt = 5000,
#'         chains = 3, prior = prior_null, cores = 3
#'       ), otherwise = NA, quiet = TRUE)),
#'       m2 = data |> purrr::map(purrr::possibly(~ mcp::mcp(
#'         model = model_linp, data = .x, iter = 10000, adapt = 5000,
#'         chains = 3, prior = prior_linp, cores = 3
#'       ), otherwise = NA, quiet = TRUE))
#'     ) |>
#'
#'       # loop for obtaining waic for both models --------------------
#'     dplyr::mutate(
#'       WAICm1 = m1 |> purrr::map(purrr::possibly(~ mcp::waic(.x)$waic, otherwise = NA,
#'       quiet = TRUE)),
#'       WAICm2 = m2 |> purrr::map(purrr::possibly(~ mcp::waic(.x)$waic, otherwise = NA,
#'       quiet = TRUE))
#'     ) |>
#'
#'       # loop for selecting model --------------------
#'     tidyr::unnest(WAICm1, WAICm2) |>
#'       dplyr::mutate(
#'         m_sel = dplyr::case_when(
#'           is.na(WAICm1) ~ m2,
#'           is.na(WAICm2) ~ m1,
#'           WAICm1 <= WAICm2 ~ m1,
#'           TRUE ~ m2
#'         ),
#'         Wmax_test = dplyr::case_when(
#'           is.na(WAICm1) & is.na(WAICm2) ~ "excluded",
#'           is.na(WAICm1) ~ "selected",
#'           is.na(WAICm2) ~ "excluded",
#'           WAICm1 <= WAICm2 ~ "excluded",
#'           TRUE ~ "selected"
#'         )
#'       ) |>
#'
#'       # loop for extracting predictions for visual assessment --------------------
#'     dplyr::mutate(ndat = data |> purrr::map(~ tidyr::expand_grid(Nrates = full_seq(.x$Nrates,
#'     10, tol = 5)))) |>
#'       dplyr::mutate(ndat = purrr::map2(.x = ndat, .y = m_sel,
#'       purrr::possibly(~ predict(object = .y, newdata = .x, probs = c(0.5)),
#'                                                          otherwise = NA, quiet = TRUE
#'       )))
#'
#'
#' # Filter data with variance information and combined 4th and 5th types of data:
#' dataSens_5 <- Data |> filter_at(.vars = vars(Na_SE, W_SE), .vars_predicate= all_vars(!is.na(.)))
#'
#' dataSens_4and5 <- list(
#'   dataSens_4 |>
#'     dplyr::select(Wmax_test) |>
#'     right_join(dataSens_5) |>
#'     filter(Wmax_test == "selected"),
#'   dataSens_5
#' )
#'
#' # Parameters and JAGS settings are defined for the MCMC (Markov Chain Monte Carlo) procedure to
#' # model the power function of CNDC across sampling dates. Weakly-informative priors were defined
#' # following Makowski et al. (2020) and Ciampitti et al. (2021). The algorithm was run with three
#' # chains of 200,000 iterations each (100,000 discarded as tuning and burn-in periods). The
#' # statistical model was fitted using the rjags:: library. Last, MCMC samples are extracted in a
#' # vector for all samples and deviance information criterion (DIC) is obtained.
#'
#'     # Specify parameters and JAGS settings
#'     parameters <- c("A1", "A2", "Bmax", "S", "Nc")
#'
#'     # JAGS settings
#'     adaptSteps <- 50000 #' number of steps to "tune" the samplers
#'     burnInSteps <- 50000 #' number of steps to "burn-in" the samplers
#'     nChains <- 3 #' number of chains to run
#'     thinSteps <- 10 #' number of steps to "thin" (keep every 10 steps)
#'     nIter <- 100000 #' steps per chain
#'
#'     mcmcChain_4 <- NULL
#'     dic_4 <- NULL
#'     for (d in seq(1, 2, 1)) {
#'       Date <- as.numeric(as.factor(paste(dataSens_4and5[[d]]$Site_year, "_",
#'       dataSens_4and5[[d]]$Samp, sep = "")))
#'
#'       dataList <- list(
#'         "W" = dataSens_4and5[[d]]$W,
#'         "N" = dataSens_4and5[[d]]$Na,
#'         "Date" = Date,
#'         "Q" = length(Date),
#'         "K" = length(unique(Date))
#'       )
#'
#'       m <- textConnection("
#' model {
#'
#' 	for (i in 1:Q)
#' 	{
#' 		W[i]~dnorm(mu[i], tau_b)
#' 		N[i]~dnorm(Nc[Date[i]], tau_n)
#' 		mu[i]<-min(Bmax[Date[i]], Bmax[Date[i]]+S[Date[i]]*(N[i]-Nc[Date[i]]))
#' 	}
#'
#' 	for (j in 1:K)
#' 	{
#' 		Nc[j]=A1*Bmax[j]^(-A2)
#' 		Bmax[j]~dnorm(Mu_Bmax,Prec_Bmax)T(0,)
#' 		S[j]~dnorm(Mu_S,Prec_S)T(0,)
#' 			}
#'
#' 			#'Weakly informative
#' 			Mu_Bmax~dnorm(6,0.1)
#' 			Mu_S~dnorm(0,0.1)
#' 			A1~dunif(2,6)
#' 			A2~dunif(0,0.7)
#'
#' 			Prec_Bmax~dgamma(0.001,0.001)
#' 			Prec_S~dgamma(0.001,0.001)
#' 			tau_b~dgamma(0.001,0.001)
#' 			tau_n~dgamma(0.001,0.001)
#'
#' }
#' ")
#'       set.seed(500)
#'       jagsModel_4 <- rjags::jags.model(m,
#'                                 data = dataList,
#'                                 n.chains = nChains,
#'                                 n.adapt = adaptSteps
#'       )
#'       close(m)
#'
#'       if (burnInSteps > 0) {
#'         update(jagsModel_4, n.iter = burnInSteps)
#'       }
#'       codaSamples_4 <- rjags::coda.samples(jagsModel_4,
#'                                     variable.names = parameters,
#'                                     n.iter = nIter,
#'                                     thin = thinSteps
#'       )
#'       mcmcChain_4[[d]] <- as.matrix(codaSamples_4)
#'       dic_4[[d]] <- rjags::dic.samples(jagsModel_4, 10000)
#'     }
#'
#'
#'
#'     # Specify parameters and JAGS settings
#'     parameters <- c("A1", "A2", "Bmax", "S", "Nc")
#'
#'     # JAGS settings
#'     adaptSteps <- 50000 #' number of steps to "tune" the samplers
#'     burnInSteps <- 50000 #' number of steps to "burn-in" the samplers
#'     nChains <- 3 #' number of chains to run
#'     thinSteps <- 10 #' number of steps to "thin" (keep every 10 steps)
#'     nIter <- 100000 #' steps per chain
#'
#'     mcmcChain_5 <- NULL
#'     dic_5 <- NULL
#'     for (d in seq(1, 2, 1)) {
#'       Date <- as.numeric(as.factor(paste(dataSens_4and5[[d]]$Site_year, "_",
#'       dataSens_4and5[[d]]$Samp, sep = "")))
#'
#'       dataList <- list(
#'         "W" = dataSens_4and5[[d]]$W,
#'         "N" = dataSens_4and5[[d]]$Na,
#'         "Date" = Date,
#'         "Q" = length(Date),
#'         "K" = length(unique(Date)),
#'         "Pw" = 1 / ((dataSens_4and5[[d]]$W_SE)^2),
#'         "Pn" = 1 / ((dataSens_4and5[[d]]$Na_SE)^2)
#'       )
#'
#'       m <- textConnection("
#' model {
#'
#' 	for (i in 1:Q)
#' 	{
#'     #'#'#'#'The next two lines were modified
#'     W[i]~dnorm(Wtrue[i], Pw[i])
#'     N[i]~dnorm(Ntrue[i], Pn[i])
#'
#' 		Wtrue[i]~dnorm(mu[i], tau_b)
#' 		Ntrue[i]~dnorm(Nc[Date[i]], tau_n)
#'
#' 		mu[i]<-min(Bmax[Date[i]], Bmax[Date[i]]+S[Date[i]]*(N[i]-Nc[Date[i]]))
#' 	}
#'
#' 	for (j in 1:K)
#' 	{
#' 		Nc[j]=A1*Bmax[j]^(-A2)
#' 		Bmax[j]~dnorm(Mu_Bmax,Prec_Bmax)T(0,)
#' 		S[j]~dnorm(Mu_S,Prec_S)T(0,)
#' 			}
#'
#' 			#'Weakly informative
#' 			Mu_Bmax~dnorm(6,0.1)
#' 			Mu_S~dnorm(0,0.1)
#' 			A1~dunif(2,6)
#' 			A2~dunif(0,0.7)
#'
#' 			Prec_Bmax~dgamma(0.001,0.001)
#' 			Prec_S~dgamma(0.001,0.001)
#' 			tau_b~dgamma(0.001,0.001)
#' 			tau_n~dgamma(0.001,0.001)
#'
#' }
#' ")
#'       set.seed(500)
#'       jagsModel_5 <- rjags::jags.model(m,
#'                                 data = dataList,
#'                                 n.chains = nChains,
#'                                 n.adapt = adaptSteps
#'       )
#'       close(m)
#'
#'       if (burnInSteps > 0) {
#'         update(jagsModel_5, n.iter = burnInSteps)
#'       }
#'       codaSamples_5 <- rjags::coda.samples(jagsModel_5,
#'                                     variable.names = parameters,
#'                                     n.iter = nIter,
#'                                     thin = thinSteps
#'       )
#'       mcmcChain_5[[d]] <- as.matrix(codaSamples_5)
#'       dic_5[[d]] <- rjags::dic.samples(jagsModel_5, 10000)
#'     }
#'
#' # Posterior probability distributions of *a* and *b* parameters are extracted for the four
#' combinations of models (two for each sensitivity test, 4th and 5th).
#'
#' fdataSens_4and5 <- bind_rows(
#'   #' unweighted models
#'   purrr::map2_dfr(
#'     mcmcChain_4, seq(1, 2, 1),
#'     ~ .x |>
#'       as.data.frame() |>
#'       dplyr::mutate(Imp = .y) |>
#'       tidyr::pivot_longer(-Imp, names_to = "Parameter", values_to = "Value")
#'   ) |>
#'     dplyr::mutate(
#'       Method = dplyr::if_else(Imp == 1, "Unweighted-Wmax", "Unweighted-Wall"),
#'       Samp = stringr::str_replace(Parameter, ".*\\[(\\d{1,2})\\]$", "\\1"),
#'       Parameter = stringr::str_remove(Parameter, "\\[\\d{1,2}\\]$")
#'     ) |>
#'     dplyr::filter(Parameter %in% c("A1", "A2")),
#'   #' weighted models
#'   purrr::map2_dfr(
#'     mcmcChain_5, seq(1, 2, 1),
#'     ~ .x |>
#'       as.data.frame() |>
#'       dplyr::mutate(Imp = .y) |>
#'       tidyr::pivot_longer(-Imp, names_to = "Parameter", values_to = "Value")
#'   ) |>
#'     dplyr::mutate(
#'       Method = dplyr::if_else(Imp == 1, "Weighted-Wmax", "Weighted-Wall"),
#'       Samp = stringr::str_replace(Parameter, ".*\\[(\\d{1,2})\\]$", "\\1"),
#'       Parameter = stringr::str_remove(Parameter, "\\[\\d{1,2}\\]$")
#'     ) |>
#'     dplyr::filter(Parameter %in% c("A1", "A2"))
#' )
#' }
NULL

sensAnalysis_4and5 <- function() {
  return(eval(parse(text = "cndcR:::fdataSens_4and5")) %>%
           dplyr::group_by(.data$Parameter, .data$Method) |>
           dplyr::summarise(lowCI = stats::quantile(.data$Value, 0.025),
                            uppCI = stats::quantile(.data$Value, 0.975),
                            mean = mean(.data$Value))
           )
}

