#' @name fig4b
#' @title
#' Plot figure 4b from Fernandez et al. (2022)
#' @description
#' This function returns figure 4b in Fernandez et al.(2022): Frequency distribution of reported low W criteria for fitting CNDC
#' in literature.

fig4b <- function() {

  figFreq_lowWcriteria <- cndcR::biblioCNDC %>%
  dplyr::mutate(preProcess = if_else(.data$preProcess == "NA", NA_character_,
                                     as.character(.data$preProcess) ),
         preProcess = if_else(is.na(.data$preProcess), "NO", as.character(.data$preProcess) ))  %>%

  tidyr::drop_na(.data$preProcess) %>%
    ggplot2::ggplot(ggplot2::aes(x = forcats::fct_infreq(.data$preProcess) ) ) +
    ggplot2::geom_bar(color = "#3d3c3c") +
    ggplot2::geom_text(stat='count', ggplot2::aes(label=.data$..count..), vjust=-0.3,
                       fontface = 3) +

    ggplot2::ylab("Number of CNDCs") +
    ggplot2::xlab(NULL) +
    ggplot2::ggtitle("Pre-processing step for low biomass observations") +

    ggplot2::scale_y_continuous(limits = c(0,100)) +

    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(color = "#e8e9eb"),
        panel.background = ggplot2::element_rect(fill = "#f5f5f5"),
        panel.border = ggplot2::element_rect(colour = "black", fill = NA),

        text = ggplot2::element_text(size = 13),
        legend.title = ggplot2::element_blank(),
        legend.position = c(0.8,0.8),
        legend.background = ggplot2::element_blank(),

        axis.ticks.length=ggplot2::unit(-0.15, "cm"),
        axis.text.x = ggplot2::element_text(margin=ggplot2::unit(c(0.5,0.5,0.5,0.5), "cm")),
        axis.text.y = ggplot2::element_text(margin=ggplot2::unit(c(0.5,0.5,0.5,0.5), "cm"),
                                            size = 13))

  return(figFreq_lowWcriteria)
}
