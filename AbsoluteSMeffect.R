
#count studies that included soil moisture values
metadat %>%
  filter(!is.na(manipSM_per)) %>%
  count(Study_number) -> thing

ggplot(data = metadat,
  aes(manipSM_per, yi)) +
  geom_hline(yintercept = 0) +
  geom_point(aes(colour = Ecosystem_type),
             width = .15, size = 4) +
  geom_smooth(method = lm, formula = y ~ x) +
  ylab("ln(RR)") +
  facet_grid(.~Manipulation,
             labeller = as_labeller(Manip_labs)) +
  xlab("Manipulation %Soil Moisture") +
  geom_smooth(method = lm, formula = y ~ x) +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))) +
  scale_color_manual(name = "Ecosystem",
                     values = Eco_colors) +
  theme(legend.position = "bottom")
