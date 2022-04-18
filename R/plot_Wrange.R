#' @name fig4a
#' @title
#' Plot figure 4a from Fernandez et al. (2022)
#' @description
#' This function returns figure 4a in Fernandez et al.(2022): Regression: Range of biomass of the
#' CNDC across crop species.

fig4a <- function() {

  figW_range <- cndcR::biblioCNDC  %>%
  dplyr::filter(!c(.data$Species == "Generic"))  %>%
  dplyr::group_by(.data$Species)  %>%
  dplyr::summarise(minW = min(.data$minW, na.rm = T), maxW = max(.data$maxW, na.rm = T))  %>%
  dplyr::mutate_if(is.numeric, list(~na_if(., Inf)))  %>%
  dplyr::mutate_if(is.numeric, list(~na_if(., -Inf)))  %>%
  dplyr::mutate(range = .data$minW-.data$maxW,
                Species = forcats::fct_reorder(.data$Species, .data$range, .desc = F) )  %>%
  dplyr::filter(!c(is.na(.data$minW) & is.na(.data$maxW)))  %>%

  ggplot2::ggplot() +
  ggplot2::geom_segment(ggplot2::aes(y = .data$minW, yend = .data$maxW,
                                     x = .data$Species, xend = .data$Species), color = "#8d8f8e") +
  ggplot2::geom_point(ggplot2::aes(y = .data$minW, x = .data$Species), fill = "#E64B35FF",
                      shape = 21, size = 2) +
  ggplot2::geom_point(ggplot2::aes(y = .data$maxW, x = .data$Species), fill = "#49695c",
                      shape = 19) +

  ggplot2::ylab(expression("Biomass (Mg ha"^"-1"~")")) + ggplot2::xlab(NULL) +

  ggplot2::scale_y_continuous(limits = c(0,35), breaks = seq(0,30,5)) +
  ggplot2::coord_flip() +

  ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(color = "#e8e9eb"),
        panel.background = ggplot2::element_rect(fill = "#f5f5f5"),
        panel.border = ggplot2::element_rect(colour = "black", fill = NA),

        text = ggplot2::element_text(size = 14),
        legend.title = ggplot2::element_blank(),
        legend.position = c(0.8,0.8),
        legend.background = ggplot2::element_blank(),

        axis.ticks.length = unit(-0.15, "cm"),
        axis.text.y = ggplot2::element_text(margin=ggplot2::unit(c(0.5,0.2,0.5,0.5), "cm"),
                                            size = 8, face = 3),
        axis.text.x = ggplot2::element_text(margin=ggplot2::unit(c(0.3,0.5,0.2,0.5), "cm")))

  return(figW_range)
}
