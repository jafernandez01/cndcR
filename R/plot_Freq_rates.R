#' @name fig3a
#' @title
#' Plot figure 3a from Fernandez et al. (2022)
#' @description
#' This function returns figure 3a in Fernandez et al.(2022): Frequency distribution of number of N-rates used for fitting CNDC
#' in literature.

fig3a <- function() {

  figFreq_rates <- cndcR::biblioCNDC %>%
  dplyr::mutate(min = as.numeric(sub("to.*","",.data$nRates)),
         max = as.numeric(sub(".*to","",.data$nRates)),
         range = max - min) %>%
  dplyr::mutate(typeStudy = dplyr::case_when(!is.na(range) & range == 0 ~ "Single N-design",
                                             is.na(range) ~ NA_character_,
                                             TRUE ~ "Multiple N-designs"  )) %>%
  tidyr::drop_na(.data$typeStudy) %>%
  dplyr::mutate(nRates = dplyr::if_else(.data$typeStudy == "Single N-design",
                                        min, min + round((range/2),0 ) )) %>%
  ggplot2::ggplot(ggplot2::aes(x = .data$nRates )) +
  ggplot2::geom_bar(ggplot2::aes(fill = .data$typeStudy),color = "#3d3c3c") +
  ggplot2::geom_text(stat='count', ggplot2::aes(label=.data$..count..), vjust=-0.3, fontface = 3) +

  ggplot2::ylab("Number of CNDCs") +
  ggplot2::ggtitle("Number of N rates") + ggplot2::xlab(NULL) +

  ggplot2::scale_fill_manual(values = c("#3d3c3c","#c4c2c2")) +
  ggplot2::scale_x_continuous(breaks = seq(2,7,1)) +
  ggplot2::scale_y_continuous(limits = c(0,45)) +

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

  return(figFreq_rates)
}
