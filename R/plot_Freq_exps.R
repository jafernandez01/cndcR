
## No. studies
figFreq_exps <- biblioCNDC %>%
  mutate(nExp_calibration = if_else(is.na(nExp_calibration), 0, as.numeric(nExp_calibration)),
         nExp_validation = if_else(is.na(nExp_validation), 0, as.numeric(nExp_validation)) ) %>%
  mutate(nStudies = nExp_calibration + nExp_validation) %>%
  ggplot(aes(x = nStudies )) +
  geom_histogram(binwidth = 3, boundary = 0, fill = "#3d3c3c", color = "#e8e9eb") +

  stat_bin(binwidth=3, geom="text", colour="black", size=3.5, fontface = 3,
           aes(label=..count.., y=1 + (..count..)), boundary = 0) +

  ylab("Number of CNDCs") +
  ggtitle("Number of experiments") + xlab(NULL) +

  scale_x_continuous(breaks = seq(0,40,3)) +

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
