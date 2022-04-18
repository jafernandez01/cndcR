#' @name fig1a
#' @title
#' Plot figure 1a from Fernandez et al. (2022)
#' @description
#' This function returns figure 1a in Fernandez et al.(2022): Frequency distribution of statistical
#' method used for fitting CNDC in literature.

fig1a <- function() {

  figFreq_method <- cndcR::biblioCNDC %>%
  dplyr::mutate(Model = dplyr::if_else(.data$Model == "NA", NA_character_,
                                       as.character(.data$Model))) %>%
  tidyr::drop_na(.data$Model) %>%
  dplyr::mutate(Method = dplyr::case_when(Method == "Justes et al." ~ "Justes et al. (1994)",
                            Method == "Herrmann and Taube" ~ "Herrmann and Taube (2004)",
                            Method == "Greenwood et al." ~ "Greenwood et al. (1990)",
                            Method == "Greenwood et al., 1990" ~ "Greenwood et al. (1990)",
                            Method == "Lemaire and Salette" ~ "Lemaire and Salette (1984)",
                            Method == "Makowski et al." ~ "Makowski et al. (2019)",
                            TRUE ~ "Other")) %>%
  tidyr::drop_na(.data$Model) %>%
  ggplot2::ggplot(ggplot2::aes(fill = .data$Model, x = forcats::fct_infreq(.data$Method) ) ) +
  ggplot2::geom_bar(color = "#3d3c3c") +
  ggplot2::geom_text(stat='count', ggplot2::aes(label=.data$..count..), vjust=-0.3, fontface = 3) +

  ggplot2::ylab("Number of CNDCs") +
  ggplot2::ggtitle("Statistical method") + ggplot2::xlab(NULL) +

  ggplot2::scale_fill_manual(values = c("#c4c2c2", "#3d3c3c")) +

  ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(color = "#e8e9eb"),
        panel.background = ggplot2::element_rect(fill = "#f5f5f5"),
        panel.border = ggplot2::element_rect(colour = "black", fill = NA),

        text = ggplot2::element_text(size = 14),
        legend.title = ggplot2::element_blank(),
        legend.position = c(0.8,0.8),
        legend.background = ggplot2::element_blank(),

        axis.ticks.length=unit(-0.15, "cm"),
        axis.text.x = ggplot2::element_text(margin=ggplot2::unit(c(0.5,0.5,0.5,0.5), "cm"),
                                            size = 10, hjust = .8, angle = 15, vjust = 1),
        axis.text.y = ggplot2::element_text(margin=ggplot2::unit(c(0.5,0.5,0.5,0.5), "cm"),
                                            size = 13))

return(figFreq_method)
}
