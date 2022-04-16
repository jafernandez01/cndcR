#' @name fig2b_c
#' @title
#' Plot figure 2b and 2c from Fernandez et al. (2022)
#' @description
#' This function returns figure 2b and 2c in Fernandez et al.(2022): Sensitivity analyses for the number of
#' experiments used to estimate a and b parameters of the CNDC in a case-study for maize, presenting bias
#' and uncertainty (precision).

fig2b_c <- function() {

fdataSens_1 <- cndcR:::fdataSens_1

# Pre-process for plotting results ----------------------
## extract parameters for the full-model (boot 8) for comparisons
a1 <- as.numeric(fdataSens_1[fdataSens_1$Method == "Boot8" & fdataSens_1$Parameter == "A1", 5])
a2 <- as.numeric(fdataSens_1[fdataSens_1$Method == "Boot8" & fdataSens_1$Parameter == "A2", 5])

ci1 <- as.numeric(fdataSens_1[fdataSens_1$Method == "Boot8" & fdataSens_1$Parameter == "A1", 4]) -
  as.numeric(fdataSens_1[fdataSens_1$Method == "Boot8" & fdataSens_1$Parameter == "A1", 3])
ci2 <- as.numeric(fdataSens_1[fdataSens_1$Method == "Boot8" & fdataSens_1$Parameter == "A2", 4]) -
  as.numeric(fdataSens_1[fdataSens_1$Method == "Boot8" & fdataSens_1$Parameter == "A2", 3])

figSens_1 <- fdataSens_1  %>%  # wrangling variable before plotting and calculating bias and variance measures
  dplyr::mutate(
    n = dplyr::case_when(
      Method == "Boot1" ~ 1,
      Method == "Boot2" ~ 2,
      Method == "Boot3" ~ 3,
      Method == "Boot4" ~ 5,
      Method == "Boot5" ~ 8,
      Method == "Boot6" ~ 13,
      Method == "Boot7" ~ 21,
      Method == "Boot8" ~ 34,
      TRUE ~ NA_real_
    ),
    CI = dplyr::case_when( # calculate relative uncertainty
      Parameter == "A1" ~ 100 * abs((uppCI - lowCI) - ci1) / (ci1),
      TRUE ~ 100 * abs((uppCI - lowCI) - ci2) / (ci2)
    ),
    Bias = dplyr::case_when( # calculate relative bias
      Parameter == "A1" ~ 100 * abs(mean - a1) / a1,
      TRUE ~ 100 * abs(mean - a2) / a2
    )
  )  %>%
  pivot_longer(cols = c(CI, Bias), names_to = "var", values_to = "value")  %>%
  # Figures 2b-c for sensitivity analysis ----------------------
ggplot2::ggplot(aes(x = n, y = value, fill = Parameter, shape = Parameter)) +
  ggplot2::geom_line(aes(color = Parameter)) +
  ggplot2::geom_point(size = 4) +
  ggplot2::facet_wrap(~var, scales = "free", labeller = ggplot2::as_labeller(c(Bias = "Bias", CI = "Uncertainty (precision)"))) +
  ggplot2::scale_color_manual(
    values = c("black", "darkgrey"),
    labels = c(
      expression(paste(italic("a"), " coefficient")),
      expression(paste(italic("b"), " coefficient"))
    )
  ) +
  ggplot2::scale_shape_manual(
    values = c(21, 22),
    labels = c(
      expression(paste(italic("a"), " coefficient")),
      expression(paste(italic("b"), " coefficient"))
    )
  ) +
  ggplot2::scale_fill_manual(
    values = c("black", "white"),
    labels = c(
      expression(paste(italic("a"), " coefficient")),
      expression(paste(italic("b"), " coefficient"))
    )
  ) +
  ggplot2::scale_x_continuous(breaks = seq(0, 35, 5)) +
  ggplot2::xlab("Number of experiments") +
  ggplot2::ylab("Relative to the full-dataset CNDC (%)") +
  ggplot2::theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#e8e9eb"),
    panel.background = element_rect(fill = "#f5f5f5"),
    panel.border = element_rect(colour = "black", fill = NA),
    strip.background = element_rect(fill = NA),
    strip.text = element_text(face = 4, size = 12, hjust = 0),
    text = element_text(size = 14),
    legend.title = element_blank(),
    legend.position = c(0.2, 0.8),
    legend.background = element_blank(),
    axis.ticks.length = unit(-0.15, "cm"),
    axis.text.x = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
    axis.text.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), size = 13)
  )

return(figSens_1)
}
