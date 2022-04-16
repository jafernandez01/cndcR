#' @name fig1a
#' @title
#' Plot figure 1a from Fernandez et al. (2022)
#' @description
#' This function returns figure 1a in Fernandez et al.(2022): Frequency distribution of statistical method used for fitting CNDC
#' in literature.

fig1a <- function() {

  figFreq_method <- cndcR::biblioCNDC %>%
  mutate(Model = if_else(Model == "NA", NA_character_, as.character(Model))) %>% drop_na(Model) %>%
  mutate(Method = case_when(Method == "Justes et al." ~ "Justes et al. (1994)",
                            Method == "Herrmann and Taube" ~ "Herrmann and Taube (2004)",
                            Method == "Greenwood et al." ~ "Greenwood et al. (1990)",
                            Method == "Greenwood et al., 1990" ~ "Greenwood et al. (1990)",
                            Method == "Lemaire and Salette" ~ "Lemaire and Salette (1984)",
                            Method == "Makowski et al." ~ "Makowski et al. (2019)",
                            TRUE ~ "Other")) %>% drop_na(Model) %>%
  ggplot(aes(fill = Model, x = forcats::fct_infreq(Method) ) ) +
  geom_bar(color = "#3d3c3c") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.3, fontface = 3) +

  ylab("Number of CNDCs") +
  ggtitle("Statistical method") + xlab(NULL) +

  scale_fill_manual(values = c("#c4c2c2", "#3d3c3c")) +

  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#e8e9eb"),
        panel.background = element_rect(fill = "#f5f5f5"),
        panel.border = element_rect(colour = "black", fill = NA),

        text = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = c(0.8,0.8),
        legend.background = element_blank(),

        axis.ticks.length=unit(-0.15, "cm"),
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"), size = 10, hjust = .8, angle = 15, vjust = 1),
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"), size = 13))

return(figFreq_method)
}
