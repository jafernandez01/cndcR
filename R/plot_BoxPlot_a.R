#' @name fig1c
#' @title
#' Plot figure 1c from Fernandez et al. (2022)
#' @description
#' This function returns figure 1c in Fernandez et al.(2022): Boxplots and density plots for the
#' distribution of the a (A1) parameter of the CNDC for C3 (red color) and C4 (blue color) species
#' in studies from literature.

fig1c <- function() {

  figBoxPlot_a <- cndcR::biblioCNDC  %>%
  dplyr::mutate(speciesType = dplyr::case_when(Species == "Zea mays L." ~ "C4 type",
                                 Species == "Sorghum bicolor L." ~ "C4 type",
                                 Species == "Saccharum officinarum L." ~ "C4 type",
                                 Species == "Miscanthus x giganteus" ~ "C4 type",
                                 Species == "Generic" ~ "C4 type",
                                 TRUE ~ "C3 type"))  %>%
  dplyr::mutate(speciesType = dplyr::case_when(`Crop type` == "C3 species" ~ "C3 type",
                                               TRUE ~ as.character(speciesType)))  %>%

  ggplot2::ggplot(ggplot2::aes(x = .data$speciesType, y = .data$a, fill = .data$speciesType,
                     color = .data$speciesType)) +
  ggdist::stat_halfeye(adjust = .7, width = .6, .width = 0,
                       justification = -.2, point_colour = NA) +
  gghalves::geom_half_point(side = "l", range_scale = .4, alpha = .7, color = "black") +
  ggplot2::geom_boxplot(width = .15, outlier.shape = NA, alpha = .5) +

  ggsci::scale_fill_npg() +
  ggsci::scale_color_npg() +

  ggplot2::ylab("Value") +
  ggplot2::xlab(NULL) +
  ggplot2::ggtitle(expression(paste(italic("a")," coefficient"))) +

  ggplot2::coord_flip() +

  ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(color = "#e8e9eb"),
        panel.background = ggplot2::element_rect(fill = "#f5f5f5"),
        panel.border = ggplot2::element_rect(colour = "black", fill = NA),

        text = ggplot2::element_text(size = 14),
        legend.title = ggplot2::element_blank(),
        legend.position = c(0.8,0.8),
        legend.background = ggplot2::element_blank(),

        axis.ticks.length.y = ggplot2::unit(0, "cm"),
        axis.ticks.length.x = ggplot2::unit(-0.15, "cm"),
        axis.text.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(margin=ggplot2::unit(c(0.5,0.5,0.5,0.5), "cm")))

return(figBoxPlot_a)
}
