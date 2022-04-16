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
  figSens_4and5 <- cndcR:::fdataSens_4and5  %>%
  ggplot(aes(x = Value, fill = Method, color = Method)) +
  geom_histogram(aes(y = ..density..), alpha = 0.5, position = "identity") +
  geom_density(alpha = .2) +
  facet_wrap(~Samp,
             scales = "free", nrow = 2,
             labeller = as_labeller(c(
               A1 = "a coefficient",
               A2 = "b coefficient"
             ))
  ) +
  xlab("Value") +
  ylab("Density") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#e8e9eb"),
    panel.background = element_rect(fill = "#f5f5f5"),
    panel.border = element_rect(colour = "black", fill = NA),
    strip.background = element_rect(fill = NA),
    strip.text = element_text(face = 2, size = 15, hjust = 0.5),
    text = element_text(size = 14),
    legend.title = element_blank(),
    legend.text = element_text(size = 7),
    legend.position = "top",
    legend.background = element_blank(),
    axis.ticks.length = unit(-0.15, "cm"),
    axis.text.x = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
    axis.text.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), size = 13)
  )

  return(figSens_4and5)
}

