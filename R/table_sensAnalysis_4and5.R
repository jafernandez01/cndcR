
# The four models were compared based on model parameters:

#  1.  Posterior distributions for *a* and *b* are averaged to obtain posterior medians and credibility intervals from all the samples.

# 2.  Then, they are used to estimate NNI values across an independent validation set of observations and compared against the reference curve by Plenet and Lemaire (2000).

# 3.  Error metrics for the agreement between NNI estimated and reference are calculated using the metrica:: library.

# 4.  Deviance information criterion (DIC) is retrieved for each model.

data("fdataSens_4and5")

# Obtain medians of a and b parameters -------------------------------
parSens_4and5 <- fdataSens_4and5  %>%
  group_by(Method, Parameter)  %>%
  summarise(Value = median(Value))

# Testing CNDCs on a validation data_base ---------------------------
metSens_4and5 <- readxl::read_xlsx("data-raw/maize_validationset.xlsx", sheet = 1)  %>%
  mutate(W = W_kg_ha * .001)  %>%
  filter(W > 1)  %>%
  expand_grid(Method = c("Unweighted-Wmax", "Unweighted-Wall", "Weighted-Wmax", "Weighted-Wall", "ref"))  %>%
  # computing NNI from reference and estimated curve
  mutate(
    Nc = case_when(
      Method == "ref" ~ 3.40 * (W^(-0.37)),
      Method == "Unweighted-Wmax" ~ as.numeric(parSens_4and5[parSens_4and5$Method == "Unweighted-Wmax" & parSens_4and5$Parameter == "A1", 3]) * (W^(-as.numeric(parSens_4and5[parSens_4and5$Method == "Unweighted-Wmax" & parSens_4and5$Parameter == "A2", 3]))),
      Method == "Unweighted-Wall" ~ as.numeric(parSens_4and5[parSens_4and5$Method == "Unweighted-Wall" & parSens_4and5$Parameter == "A1", 3]) * (W^(-as.numeric(parSens_4and5[parSens_4and5$Method == "Unweighted-Wall" & parSens_4and5$Parameter == "A2", 3]))),
      Method == "Weighted-Wmax" ~ as.numeric(parSens_4and5[parSens_4and5$Method == "Weighted-Wmax" & parSens_4and5$Parameter == "A1", 3]) * (W^(-as.numeric(parSens_4and5[parSens_4and5$Method == "Weighted-Wmax" & parSens_4and5$Parameter == "A2", 3]))),
      Method == "Weighted-Wall" ~ as.numeric(parSens_4and5[parSens_4and5$Method == "Weighted-Wall" & parSens_4and5$Parameter == "A1", 3]) * (W^(-as.numeric(parSens_4and5[parSens_4and5$Method == "Weighted-Wall" & parSens_4and5$Parameter == "A2", 3]))),
      TRUE ~ NA_real_
    ),
    NNI = `N(%)` / Nc
  )  %>%
  filter(NNI < 1.8)  %>%  # removing unrealistic outliers of NNI
  pivot_wider(names_from = Method, values_from = NNI, id_cols = c(Id, Paper, W, `N(%)`))  %>%
  pivot_longer(names_to = "Method", values_to = "NNI", cols = c(5:8))  %>%
  # calculate agreement between estimated and reference values for NNI
  group_by(Method)  %>%
  summarise(
    R2 = metrica::R2(ref, NNI), # r-squared
    RMSE = metrica::RMSE(ref, NNI), # root mean square error
    CCC = metrica::CCC(ref, NNI) # concordance correlation coefficient
  )  %>%
  mutate_if(is.double, .funs = ~ round(digits = 3, x = .))

# Compare DIC of models ---------------------------------------------
# dicSens_4and5 <- data_frame(Method = c("Unweighted-Wmax", "Unweighted-Wall", "Weighted-Wmax", "Weighted-Wall"))  %>%
#   mutate(DIC = case_when(
#     Method == "Unweighted-Wmax" ~ dic_4[[1]][[1]]  %>%  sum(),
#     Method == "Unweighted-Wall" ~ dic_4[[2]][[1]]  %>%  sum(),
#     Method == "Weighted-Wmax" ~ dic_5[[1]][[1]]  %>%  sum(),
#     Method == "Weighted-Wall" ~ dic_5[[2]][[1]]  %>%  sum(),
#     TRUE ~ NA_real_
#   ))  %>%
#   mutate_if(is.double, .funs = ~ round(digits = 0, x = .))
