
## 2nd sensitivity analysis: *number of N rates*
# Bootstrap procedure in which studies with *r* (where *r* = 2, 3, 4, 5, and 6) number of N rates are sampled 100 times with replacement to create 100 new datasets.

set.seed(500)
dataSens_2 <- NULL
for (i in seq(1, 5, 1)) {
  dataSens_2[[i]] <- replicate(100, Data |>
                                 group_by(Site_year) |>
                                 dplyr::filter(length(unique(Nrates)) > 5) |>
                                 nest() |>
                                 mutate(sample = purrr::map(data, ~ dplyr::filter(.x, Nrates <= 30 | Nrates %in% sample(unique(dplyr::filter(.x, Nrates > 30)$Nrates), i, replace = F)))) |>
                                 unnest(sample), simplify = F)
}

#The Bayesian hierarchical model explained by Makowski et al. (2020) was fitted to each bootstrapped sample.
#1.  The first section is not required but only allows to run the code-chunk in an RStudio job instead of the
# console (job:: library) and using parallel computation (doParallel:: and foreach:: libraries).

#2.  Then, parameters and JAGS settings are defined for the MCMC (Markov Chain Monte Carlo) procedure to model
# the power function of CNDC across sampling dates. Weakly-informative priors were defined following Makowski et al. (2020)
# and Ciampitti et al. (2021). To facilitate computation of the sensitivity analyses, the algorithm was run with three chains
# of 20,000 iterations each (10,000 discarded as tuning and burn-in periods). The statistical model was fitted using the rjags:: library.

#3.  Last, MCMC samples are extracted in a vector for all samples.

job::job({
    cl <- parallel::makeCluster(parallel::detectCores()[1] - 1) # the no. of cores - 1, to avoid overloading
    doParallel::registerDoParallel(cl)

    set.seed(500)
    bootSamp_2 <- NULL
    bootSamp_2 <- foreach::foreach(i = seq(1, 5, 1), .verbose = T) %dopar% {

      # Specify parameters and JAGS settings
      parameters <- c("A1", "A2", "Bmax", "S", "Nc")

      # JAGS settings
      adaptSteps <- 5000 # number of steps to "tune" the samplers
      burnInSteps <- 5000 # number of steps to "burn-in" the samplers
      nChains <- 3 # number of chains to run
      thinSteps <- 10 # number of steps to "thin" (keep every 10 steps)
      nIter <- 10000 # steps per chain

      mcmcChain_2 <- NULL
      for (d in seq(1, 100, 1)) {
        Date <- as.numeric(as.factor(paste(dataSens_2[[i]][[d]]$Site_year, "_", dataSens_2[[i]][[d]]$Samp, sep = "")))

        dataList <- list(
          "W" = dataSens_2[[i]][[d]]$W,
          "N" = dataSens_2[[i]][[d]]$Na,
          "Date" = Date,
          "Q" = length(Date),
          "K" = length(unique(Date))
        )

        m <- textConnection("
model {

	for (i in 1:Q)
	{
		W[i]~dnorm(mu[i], tau_b)
		N[i]~dnorm(Nc[Date[i]], tau_n)
		mu[i]<-min(Bmax[Date[i]], Bmax[Date[i]]+S[Date[i]]*(N[i]-Nc[Date[i]]))
	}

	for (j in 1:K)
	{
		Nc[j]=A1*Bmax[j]^(-A2)
		Bmax[j]~dnorm(Mu_Bmax,Prec_Bmax)T(0,)
		S[j]~dnorm(Mu_S,Prec_S)T(0,)
			}

			#Weakly informative
			Mu_Bmax~dnorm(6,0.1)
			Mu_S~dnorm(0,0.1)
			A1~dunif(2,6)
			A2~dunif(0,0.7)

			Prec_Bmax~dgamma(0.001,0.001)
			Prec_S~dgamma(0.001,0.001)
			tau_b~dgamma(0.001,0.001)
			tau_n~dgamma(0.001,0.001)

}
")
        set.seed(500)
        jagsModel_2 <- rjags::jags.model(m,
                                         data = dataList,
                                         n.chains = nChains,
                                         n.adapt = adaptSteps
        )
        close(m)

        if (burnInSteps > 0) {
          update(jagsModel_2, n.iter = burnInSteps)
        }
        codaSamples_2 <- rjags::coda.samples(jagsModel_2,
                                             variable.names = parameters,
                                             n.iter = nIter,
                                             thin = thinSteps
        )
        mcmcChain_2[[d]] <- as.matrix(codaSamples_2)
      }


    bootSamp_2[[i]] <- mcmcChain_2
    }

    parallel::stopCluster(cl)
  }, import = "auto", title = "mcmc_no.rates" )


# Posterior probability distributions of *a* and *b* parameters are extracted for each bootstrapped sample
# (i.e., extracting a total of 100 probability distributions). These distributions are then being averaged
# to obtain posterior medians and credibility intervals from all the samples. Parallel computation is being
# used for this part of the code.

# The effect of the sample size (i.e., number of N rates) on the posterior medians was analyzed via the
# relative bias and uncertainty over the full model.

# Figures **3c-d** are obtained using ggplot:: library.

cl <- parallel::makeCluster(parallel::detectCores()[1] - 1) # the no. of cores - 1, to avoid overloading
doParallel::registerDoParallel(cl)

# Obtain final data_frame with posteriors for a and b
fdataSens_2 <- foreach::foreach(i = seq(1, 5, 1)) %dopar% { # for all the different no.rates tested

  purrr::map2_dfr(
    bootSamp_2[[i]], seq(1, 100, 1), # map the extraction of parameters within the bootstrap samples
    ~ .x |>
      as.data.frame() |>
      dplyr::mutate(Imp = .y) |>
      tidyr::pivot_longer(-Imp, names_to = "Parameter", values_to = "Value")
  ) |>
    dplyr::mutate(
      Method = paste("Boot", i, sep = ""),
      Samp = stringr::str_replace(Parameter, ".*\\[(\\d{1,2})\\]$", "\\1"),
      Parameter = stringr::str_remove(Parameter, "\\[\\d{1,2}\\]$")
    ) |>
    dplyr::filter(Parameter %in% c("A1", "A2"))
}

fdataSens_2 <- bind_rows(fdataSens_2) |> # extract summary statistics of the posteriors for a and b (A1 and A2)
  dplyr::group_by(Parameter, Method) |>
  dplyr::summarise(lowCI = quantile(Value, 0.025), uppCI = quantile(Value, 0.975), mean = mean(Value))

parallel::stopCluster(cl)
