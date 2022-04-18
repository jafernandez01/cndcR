#' @name fig2a
#' @title
#' Plot figure 2a from Fernandez et al. (2022)
#' @description
#' This function returns figure 2a in Fernandez et al.(2022): Frequency distribution of number of
#' experiments used for fitting CNDC in literature.

fig2a <- function() {

  figFreq_exps <- cndcR::biblioCNDC %>%
  dplyr::mutate(nExp_calibration = dplyr::if_else(is.na(.data$nExp_calibration), 0,
                                                  as.numeric(.data$nExp_calibration)),
         nExp_validation = dplyr::if_else(is.na(.data$nExp_validation), 0,
                                          as.numeric(.data$nExp_validation)) ) %>%
  dplyr::mutate(nStudies = .data$nExp_calibration + .data$nExp_validation) %>%
  ggplot2::ggplot(ggplot2::aes(x = .data$nStudies )) +
  ggplot2::geom_histogram(binwidth = 3, boundary = 0, fill = "#3d3c3c", color = "#e8e9eb") +

  ggplot2::stat_bin(binwidth=3, geom="text", colour="black", size=3.5, fontface = 3,
            ggplot2::aes(label=.data$..count.., y=1 + (.data$..count..)), boundary = 0) +

  ggplot2::ylab("Number of CNDCs") +
  ggplot2::ggtitle("Number of experiments") + ggplot2::xlab(NULL) +

  ggplot2::scale_x_continuous(breaks = seq(0,40,3)) +

  ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(color = "#e8e9eb"),
        panel.background = ggplot2::element_rect(fill = "#f5f5f5"),
        panel.border = ggplot2::element_rect(colour = "black", fill = NA),

        text = ggplot2::element_text(size = 14),
        legend.title = ggplot2::element_blank(),
        legend.position = c(0.8,0.8),
        legend.background = ggplot2::element_blank(),

        axis.ticks.length=ggplot2::unit(-0.15, "cm"),
        axis.text.x = ggplot2::element_text(margin=ggplot2::unit(c(0.5,0.5,0.5,0.5), "cm")),
        axis.text.y = ggplot2::element_text(margin=ggplot2::unit(c(0.5,0.5,0.5,0.5), "cm"),
                                            size = 13))

return(figFreq_exps)
}
