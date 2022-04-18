#' @name fig5b_c
#' @title
#' Plot figure 5b and 5c from Fernandez et al. (2022)
#' @description
#' This function returns figure 5b and 5c in Fernandez et al.(2022): SHistograms and density plots of the
#' posterior distributions for a (A1) and b (A2) parameters of the critical nitrogen dilution curve (CNDC)
#' of maize using four types of datasets: (i) All data or (ii) only Wmax plateau achieved data, and
#' (iii) unweighted or (iv) variance-weighted data.

fig5b_c <- function() {

# Figures 5b-c for sensitivity analysis ----------------------
  figSens_4and5 <- eval(parse(text = "cndcR:::fdataSens_4and5"))  %>%
  ggplot2::ggplot(ggplot2::aes(x = .data$Value, fill = .data$Method, color = .data$Method)) +
  ggplot2::geom_histogram(ggplot2::aes(y = .data$..density..), alpha = 0.5, position = "identity") +
  ggplot2::geom_density(alpha = .2) +
  ggplot2::facet_wrap(~Samp,
             scales = "free", nrow = 2,
             labeller = ggplot2::as_labeller(c(
               A1 = "a coefficient",
               A2 = "b coefficient"
             ))
  ) +
  ggplot2::xlab("Value") +
  ggplot2::ylab("Density") +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(color = "#e8e9eb"),
    panel.background = ggplot2::element_rect(fill = "#f5f5f5"),
    panel.border = ggplot2::element_rect(colour = "black", fill = NA),
    strip.background = ggplot2::element_rect(fill = NA),
    strip.text = ggplot2::element_text(face = 2, size = 15, hjust = 0.5),
    text = ggplot2::element_text(size = 14),
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 7),
    legend.position = "top",
    legend.background = ggplot2::element_blank(),
    axis.ticks.length = ggplot2::unit(-0.15, "cm"),
    axis.text.x = ggplot2::element_text(margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
    axis.text.y = ggplot2::element_text(margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "cm"), size = 13)
  )

  return(figSens_4and5)
}

