
#' # 1st sensitivity analysis: *number of experiments*
#' Bootstrap procedure in which *n* (where *n* = 1, 2, 3, 5, 8, 13, 21, and 34) number of studies of our database are sampled 100 times with replacement to create 100 new datasets.
#'
#' @function
#' \dontrun{
#'
#' data("Data")
#'
#' set.seed(500)
#' dataSens_1 <- NULL
#' for (i in c(1, 2, 3, 5, 8, 13, 21, 34)) {
#'   dataSens_1[[i]] <- replicate(100, filter(Data, Site_year %in% sample(unique(Data$Site_year), i, replace = T)), simplify = F)
#' }
#'
#' dataSens_1 <- dataSens_1 |> discard(is.null)
#'
#'
#'
#'     set.seed(500)
#'     bootSamp_1 <- NULL
#'     bootSamp_1 <- foreach::foreach(i = seq(1, 8, 1), .verbose = T) %do% {
#'
#'       # Specify parameters and JAGS settings
#'       parameters <- c("A1", "A2", "Bmax", "S", "Nc")
#'
#'       # JAGS settings
#'       adaptSteps <- 5000 # number of steps to "tune" the samplers
#'       burnInSteps <- 5000 # number of steps to "burn-in" the samplers
#'       nChains <- 3 # number of chains to run
#'       thinSteps <- 10 # number of steps to "thin" (keep every 10 steps)
#'       nIter <- 10000 # steps per chain
#'
#'       mcmcChain_1 <- NULL
#'       for (d in seq(1, 100, 1)) {
#'         Date <- as.numeric(as.factor(paste(dataSens_1[[i]][[d]]$Site_year, "_", dataSens_1[[i]][[d]]$Samp, sep = "")))
#'
#'         dataList <- list(
#'           "W" = dataSens_1[[i]][[d]]$W,
#'           "N" = dataSens_1[[i]][[d]]$Na,
#'           "Date" = Date,
#'           "Q" = length(Date),
#'           "K" = length(unique(Date))
#'         )
#'
#'         m <- textConnection("
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
#' 			#Weakly informative
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
#'         set.seed(500)
#'         jagsModel_1 <- rjags::jags.model(m,
#'                                          data = dataList,
#'                                          n.chains = nChains,
#'                                          n.adapt = adaptSteps
#'         )
#'         close(m)
#'
#'         if (burnInSteps > 0) {
#'           update(jagsModel_1, n.iter = burnInSteps)
#'         }
#'         codaSamples_1 <- rjags::coda.samples(jagsModel_1,
#'                                              variable.names = parameters,
#'                                              n.iter = nIter,
#'                                              thin = thinSteps
#'         )
#'         mcmcChain_1[[d]] <- as.matrix(codaSamples_1)
#'       }
#'
#'
#'     bootSamp_1[[i]] <- mcmcChain_1
#'     }
#'
#'
#'
#' # Posterior probability distributions of *a* and *b* parameters are extracted for each bootstrapped sample (i.e., extracting a total of 100 probability distributions).
#' # These distributions are then being averaged to obtain posterior medians and credibility intervals from all the samples.
#' # Parallel computation is being used for this part of the code.
#'
#' # The effect of the sample size (i.e., number of experimental trials) on the posterior medians was analyzed via the relative bias and uncertainty over the full model.
#' # Figures **2b-c** are obtained using ggplot:: library.
#'
#' # Obtain final data_frame with posteriors for a and b
#' fdataSens_1 <- foreach::foreach(i = seq(1, 8, 1)) %dopar% { # for all the different no.studies sizes tested
#'
#'   purrr::map2_dfr(
#'     bootSamp_1[[i]], seq(1, 100, 1), # map the extraction of parameters within the bootstrap samples
#'     ~ .x |>
#'       as.data.frame() |>
#'       dplyr::mutate(Imp = .y) |>
#'       tidyr::pivot_longer(-Imp, names_to = "Parameter", values_to = "Value")
#'   ) |>
#'     dplyr::mutate(
#'       Method = paste("Boot", i, sep = ""),
#'       Samp = stringr::str_replace(Parameter, ".*\\[(\\d{1,2})\\]$", "\\1"),
#'       Parameter = stringr::str_remove(Parameter, "\\[\\d{1,2}\\]$")
#'     ) |>
#'     dplyr::filter(Parameter %in% c("A1", "A2"))
#' }
#'
#' fdataSens_1 <- bind_rows(fdataSens_1) |> # extract summary statistics of the posteriors for a and b (A1 and A2)
#'   dplyr::group_by(Parameter, Method) |>
#'   dplyr::summarise(lowCI = quantile(Value, 0.025), uppCI = quantile(Value, 0.975), mean = mean(Value))
#'
#'
#' # save the data
#' save(fdataSens_1, file = here('data', 'fdataSens_1.RData'))
#' }
NULL

data("fdataSens_1")
fdataSens_1
