#' @name fig1d
#' @title
#' Plot figure 1d from Fernandez et al. (2022)
#' @description
#' This function returns figure 1d in Fernandez et al.(2022): Boxplots and density plots for the distribution
#' of the b (A2) parameter of the CNDC for C3 (red color) and C4 (blue color) species in studies from literature.

fig1d <- function() {

figBoxPlot_b <- cndcR::biblioCNDC  %>%
  mutate(speciesType = case_when(Species == "Zea mays L." ~ "C4 type",
                                 Species == "Sorghum bicolor L." ~ "C4 type",
                                 Species == "Saccharum officinarum L." ~ "C4 type",
                                 Species == "Miscanthus x giganteus" ~ "C4 type",
                                 Species == "Generic" ~ "C4 type",
                                 TRUE ~ "C3 type"))  %>%
  mutate(speciesType = case_when(`Crop type` == "C3 species" ~ "C3 type", TRUE ~ as.character(speciesType)))  %>%

  ggplot(aes(x = speciesType, y = A2, fill = speciesType, color = speciesType)) +
  ggdist::stat_halfeye(adjust = .7, width = .6, .width = 0, justification = -.2, point_colour = NA) +
  gghalves::geom_half_point(side = "l", range_scale = .4, alpha = .7, color = "black") +
  geom_boxplot(width = .15, outlier.shape = NA, alpha = .5) +

  ggsci::scale_fill_npg() +
  ggsci::scale_color_npg() +

  ylab("Value") + xlab(NULL) +
  ggtitle(expression(paste(italic("b")," coefficient"))) +

  coord_flip() +

  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#e8e9eb"),
        panel.background = element_rect(fill = "#f5f5f5"),
        panel.border = element_rect(colour = "black", fill = NA),

        text = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = "none",
        legend.background = element_blank(),

        axis.ticks.length.y = unit(0, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")))

return(figBoxPlot_b)
}
