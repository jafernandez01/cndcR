
## Min and max biomass
data("biblioCNDC")

figW_range <- biblioCNDC |>
  filter(!c(Species == "Generic")) |>
  group_by(Species) |>
  summarise(minW = min(minW, na.rm = T), maxW = max(maxW, na.rm = T)) |>
  mutate_if(is.numeric, list(~na_if(., Inf))) |>
  mutate_if(is.numeric, list(~na_if(., -Inf))) |>
  mutate(range = minW-maxW, Species = forcats::fct_reorder(Species, range, .desc = F) ) |>
  filter(!c(is.na(minW) & is.na(maxW))) |>

  ggplot() +
  geom_segment(aes(y = minW, yend = maxW, x = Species, xend = Species), color = "#8d8f8e") +
  geom_point(aes(y = minW, x = Species), fill = "#E64B35FF", shape = 21, size = 2) +
  geom_point(aes(y = maxW, x = Species), fill = "#49695c", shape = 19) +

  ylab(expression("Biomass (Mg ha"^"-1"~")")) + xlab(NULL) +
  #ggtitle("Range of biomass for the CNDC") +

  scale_y_continuous(limits = c(0,35), breaks = seq(0,30,5)) +
  coord_flip() +

  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#e8e9eb"),
        panel.background = element_rect(fill = "#f5f5f5"),
        panel.border = element_rect(colour = "black", fill = NA),

        text = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = c(0.8,0.8),
        legend.background = element_blank(),

        axis.ticks.length = unit(-0.15, "cm"),
        axis.text.y = element_text(margin=unit(c(0.5,0.2,0.5,0.5), "cm"), size = 8, face = 3),
        axis.text.x = element_text(margin=unit(c(0.3,0.5,0.2,0.5), "cm")))

