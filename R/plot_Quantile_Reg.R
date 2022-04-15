
# Regression: minW vs. b coefficient

#m_a1 = lm(A2 ~ minW, data = biblioCNDC)
m_a2 = quantreg::nlrq(A2 ~ nlraa::SSlinp(minW, a ,b,xs), data = biblioCNDC, tau = 0.05 )
ndat_a2 = expand.grid(minW = seq(min(biblioCNDC$minW, na.rm = T), max(biblioCNDC$minW, na.rm = T), 0.1))
ndat_a2$preds = quantreg::predict(m_a2, newdata = ndat_a2)

figQuantile_Reg <- biblioCNDC %>%
  ggplot() +
  geom_point(aes(x = minW, y = A2), shape = 19, size = 1.7, fill = "#3d3c3c", alpha = .8) +
  geom_line(data = ndat_a2, aes(x = minW, y = preds), color = "#E64B35FF", linetype = "solid", size = .5) +

  xlab(expression("Minimum value of biomass (Mg ha"^"-1"~")")) + ylab(expression(paste(italic("b"), " coefficient"))) +

  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#e8e9eb"),
        panel.background = element_rect(fill = "#f5f5f5"),
        panel.border = element_rect(colour = "black", fill = NA),

        text = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = c(0.8,0.8),
        legend.background = element_blank(),

        axis.ticks.length=unit(-0.15, "cm"),
        axis.text.x = element_text(margin=unit(c(0.3,0.5,0.2,0.5), "cm")),
        axis.text.y = element_text(margin=unit(c(0.5,0.3,0.5,0.2), "cm"), size = 13))
