#' @name fig1b
#' @title
#' Plot figure 1b from Fernandez et al. (2022)
#' @description
#' This function returns figure 1b in Fernandez et al.(2022): Frequency distribution of N determination method used for fitting CNDC
#' in literature.

fig1b <- function() {

  figFreq_Ndeterm <- cndcR::biblioCNDC %>%
  tidyr::drop_na(.data$NLabMethod) %>%
  ggplot2::ggplot(aes(x = forcats::fct_infreq(.data$NLabMethod) ) ) +
  ggplot2::geom_bar(color = "#3d3c3c") +
  ggplot2::geom_text(stat='count', ggplot2::aes(label=.data$..count..), vjust=-0.3, fontface = 3) +

  ggplot2::ylab("Number of CNDCs") +
  ggplot2::ggtitle("N determination method") +
  ggplot2::xlab(NULL) +

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
        axis.text.y = ggplot2::element_text(margin=ggplot2::unit(c(0.5,0.5,0.5,0.5), "cm"), size = 13))

  return(figFreq_Ndeterm)
}
