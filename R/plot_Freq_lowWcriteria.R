#' @name fig4b
#' @title
#' Plot figure 4b from Fernandez et al. (2022)
#' @description
#' This function returns figure 4b in Fernandez et al.(2022): Frequency distribution of reported low W criteria for fitting CNDC
#' in literature.

fig4b <- function() {

  data("biblioCNDC")
  figFreq_lowWcriteria <- biblioCNDC %>%
  dplyr::mutate(preProcess = if_else(preProcess == "NA", NA_character_, as.character(preProcess) ),
         preProcess = if_else(is.na(preProcess), "NO", as.character(preProcess) )) |>

  drop_na(preProcess) %>%
  ggplot(aes(x = forcats::fct_infreq(preProcess) ) ) +
  geom_bar(color = "#3d3c3c") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.3, fontface = 3) +

  ylab("Number of CNDCs") +
  xlab(NULL) + ggtitle("Pre-processing step for low biomass observations") +

  scale_y_continuous(limits = c(0,100)) +

  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#e8e9eb"),
        panel.background = element_rect(fill = "#f5f5f5"),
        panel.border = element_rect(colour = "black", fill = NA),

        text = element_text(size = 13),
        legend.title = element_blank(),
        legend.position = c(0.8,0.8),
        legend.background = element_blank(),

        axis.ticks.length=unit(-0.15, "cm"),
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"), size = 13))

  return(figFreq_lowWcriteria)
}
