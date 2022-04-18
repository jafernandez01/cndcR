#' @name fig3c_d
#' @title
#' Plot figure 3c and 3d from Fernandez et al. (2022)
#' @description
#' This function returns figure 3c and 3d in Fernandez et al.(2022): Sensitivity analyses for the
#' number of N rates used to estimate a and b parameters of the CNDC in a case-study for maize,
#' presenting bias and uncertainty (precision).

fig3c_d <- function() {

fdataSens_2 <- eval(parse(text = "cndcR:::fdataSens_2"))

# Pre-process for plotting results ----------------------
## extract parameters for the full-model (boot 5) for comparisons
a1 <- as.numeric(fdataSens_2[fdataSens_2$Method == "Boot5" & fdataSens_2$Parameter == "A1", 5])
a2 <- as.numeric(fdataSens_2[fdataSens_2$Method == "Boot5" & fdataSens_2$Parameter == "A2", 5])

ci1 <- as.numeric(fdataSens_2[fdataSens_2$Method == "Boot5" & fdataSens_2$Parameter == "A1", 4]) -
  as.numeric(fdataSens_2[fdataSens_2$Method == "Boot5" & fdataSens_2$Parameter == "A1", 3])
ci2 <- as.numeric(fdataSens_2[fdataSens_2$Method == "Boot5" & fdataSens_2$Parameter == "A2", 4]) -
  as.numeric(fdataSens_2[fdataSens_2$Method == "Boot5" & fdataSens_2$Parameter == "A2", 3])

figSens_2 <- fdataSens_2  %>%
  dplyr::mutate(
    n = dplyr::case_when(
      Method == "Boot1" ~ 2,
      Method == "Boot2" ~ 3,
      Method == "Boot3" ~ 4,
      Method == "Boot4" ~ 5,
      Method == "Boot5" ~ 6,
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
  pivot_longer(cols = c(.data$CI, .data$Bias), names_to = "var", values_to = "value")  %>%
  # Figures 3c-d for sensitivity analysis ----------------------
ggplot2::ggplot(ggplot2::aes(x = n, y = .data$value, fill = .data$Parameter,
                             shape = .data$Parameter)) +
  ggplot2::geom_line(ggplot2::aes(color = .data$Parameter)) +
  ggplot2::geom_point(size = 4) +
  ggplot2::facet_wrap(~var, scales = "free",
                      labeller = ggplot2::as_labeller(c(Bias = "Bias",
                                                        CI = "Uncertainty (precision)"))) +
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
  ggplot2::scale_x_continuous(breaks = seq(1, 6, 1)) +
  ggplot2::xlab("Number of N rates") +
  ggplot2::ylab("Relative to the full-dataset CNDC (%)") +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(color = "#e8e9eb"),
    panel.background = ggplot2::element_rect(fill = "#f5f5f5"),
    panel.border = ggplot2::element_rect(colour = "black", fill = NA),
    strip.background = ggplot2::element_rect(fill = NA),
    strip.text = ggplot2::element_text(face = 4, size = 12, hjust = 0),
    text = ggplot2::element_text(size = 14),
    legend.title = ggplot2::element_blank(),
    legend.position = c(0.2, 0.8),
    legend.background = ggplot2::element_blank(),
    axis.ticks.length = ggplot2::unit(-0.15, "cm"),
    axis.text.x = ggplot2::element_text(margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
    axis.text.y = ggplot2::element_text(margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
                                        size = 13)
  )

return(figSens_2)
}
