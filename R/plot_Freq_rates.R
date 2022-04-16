#' @name fig3a
#' @title
#' Plot figure 3a from Fernandez et al. (2022)
#' @description
#' This function returns figure 3a in Fernandez et al.(2022): Frequency distribution of number of N-rates used for fitting CNDC
#' in literature.

fig3a <- function() {

  figFreq_rates <- cndcR::biblioCNDC %>%
  mutate(min = as.numeric(sub("to.*","",nRates)),
         max = as.numeric(sub(".*to","",nRates)),
         range = max - min) %>%
  mutate(typeStudy = case_when(!is.na(range) & range == 0 ~ "Single N-design", is.na(range) ~ NA_character_,  TRUE ~ "Multiple N-designs"  )) %>%
  drop_na(typeStudy) %>%
  mutate(nRates = if_else(typeStudy == "Single N-design", min, min + round((range/2),0 ) )) %>%
  ggplot(aes(x = nRates )) +
  geom_bar(aes(fill = typeStudy),color = "#3d3c3c") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.3, fontface = 3) +

  ylab("Number of CNDCs") +
  ggtitle("Number of N rates") + xlab(NULL) +

  scale_fill_manual(values = c("#3d3c3c","#c4c2c2")) +
  scale_x_continuous(breaks = seq(2,7,1)) +
  scale_y_continuous(limits = c(0,45)) +

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

  return(figFreq_rates)
}
