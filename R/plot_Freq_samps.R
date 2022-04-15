
## No. samplings
figFreq_samps <- biblioCNDC %>%
  mutate(min = as.numeric(sub("to.*","",nSamp)),
         max = as.numeric(sub(".*to","",nSamp)),
         range = max - min) %>%
  mutate(typeStudy = case_when(!is.na(range) & range == 0 ~ "Single sampling-design", is.na(range) ~ NA_character_,  TRUE ~ "Multiple sampling-designs"  )) %>%
  drop_na(typeStudy) %>%
  mutate(nSamp = if_else(typeStudy == "Single sampling-design", min, min + round((range/2),0 ) )) %>%
  ggplot(aes(x = nSamp)) +
  geom_histogram(aes(fill = typeStudy),binwidth = 2, boundary = 0, color = "black") +

  stat_bin(binwidth=2, geom="text", colour="black", size=3.5, fontface = 3,
           aes(label=..count.., y=(..count..)+2 ), boundary = 0) +

  ylab("Number of CNDCs") +
  ggtitle("Number of sampling times") + xlab(NULL) +

  scale_fill_manual(values = c("#3d3c3c","#c4c2c2")) +
  scale_x_continuous(breaks = seq(0,20,2)) +
  scale_y_continuous(limits = c(0,50)) +

  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#e8e9eb"),
        panel.background = element_rect(fill = "#f5f5f5"),
        panel.border = element_rect(colour = "black", fill = NA),

        text = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = c(0.7,0.8),
        legend.background = element_blank(),

        axis.ticks.length=unit(-0.15, "cm"),
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"), size = 13))
