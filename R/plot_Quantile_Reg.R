#' @name fig4c
#' @rdname fig4c
#' @title
#' Plot figure 4c from Fernandez et al. (2022)
#' @description
#' This function returns figure 4c in Fernandez et al.(2022): Regression: minW vs. b coefficient of
#' the CNDC.
#' @return figQuantile_Reg: ggplot object figure
#' @export
fig4c <- function() {

  biblioCNDC <- cndcR::biblioCNDC %>% dplyr::rename(A2 = b)

  #m_a1 = lm(A2 ~ minW, data = biblioCNDC)
  m_a2 <- quantreg::nlrq(A2~SSlinp(minW, a ,b,xs) , data = biblioCNDC, tau = 0.05 )

  ndat_a2 <- expand.grid(minW = seq(min(biblioCNDC$minW, na.rm = T),
                                    max(biblioCNDC$minW, na.rm = T), 0.1))
  ndat_a2$preds <- predict(m_a2, newdata = ndat_a2)

  figQuantile_Reg <- biblioCNDC %>%
  ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = minW, y = A2), shape = 19, size = 1.7, fill = "#3d3c3c",
                        alpha = .8) +
    ggplot2:: geom_line(data = ndat_a2, ggplot2::aes(x = minW, y = preds), color = "#E64B35FF",
                        linetype = "solid", size = .5) +

    ggplot2::xlab(expression("Minimum value of biomass (Mg ha"^"-1"~")")) +
    ggplot2::ylab(expression(paste(italic("b"), " coefficient"))) +

    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(color = "#e8e9eb"),
        panel.background = ggplot2::element_rect(fill = "#f5f5f5"),
        panel.border = ggplot2::element_rect(colour = "black", fill = NA),

        text = ggplot2::element_text(size = 14),
        legend.title = ggplot2::element_blank(),
        legend.position = c(0.8,0.8),
        legend.background = ggplot2::element_blank(),

        axis.ticks.length=unit(-0.15, "cm"),
        axis.text.x = ggplot2::element_text(margin=ggplot2::unit(c(0.3,0.5,0.2,0.5), "cm")),
        axis.text.y = ggplot2::element_text(margin=ggplot2::unit(c(0.5,0.3,0.5,0.2), "cm"),
                                            size = 13))

  return(figQuantile_Reg)
}
