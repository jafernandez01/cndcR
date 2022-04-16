#' @name fig1b
#' @title
#' Plot figure 1b from Fernandez et al. (2022)
#' @description
#' This function returns figure 1b in Fernandez et al.(2022): Frequency distribution of N determination method used for fitting CNDC
#' in literature.

fig1b <- function() {

  figFreq_Ndeterm <- cndcR::biblioCNDC %>%
  drop_na(NLabMethod) %>%
  ggplot(aes(x = forcats::fct_infreq(NLabMethod) ) ) +
  geom_bar(color = "#3d3c3c") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.3, fontface = 3) +

  ylab("Number of CNDCs") +
  ggtitle("N determination method") + xlab(NULL) +

  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#e8e9eb"),
        panel.background = element_rect(fill = "#f5f5f5"),
        panel.border = element_rect(colour = "black", fill = NA),

        text = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = c(0.8,0.8),
        legend.background = element_blank(),

        axis.ticks.length=unit(-0.15, "cm"),
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"), size = 13))

  return(figFreq_Ndeterm)
}
