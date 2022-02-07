
ggplot(metadat, aes(SM_mean, yi)) +
  geom_point(aes(color = Ecosystem_type)) + 
    facet_grid(Manipulation~.) +
  geom_smooth(method = "lm") +
  scale_color_viridis_d()
  

metadat %>%
  select("Study_number", "SM_mean", "Ecosystem_type",
         "Duration", "sg_ocs", "yi",
         "vi") %>%
  na.omit() -> metadat_SM

units <- bquote('%Change ('*mu~ 'mol ' ~C~ m^-2~s^-1*')')

ggplot(
  data = metadat_ipreds,
  aes(Duration, 100*(10^pred-1))
) +
  geom_hline(yintercept = 0) +
  geom_jitter(aes(fill = Ecosystem_type,
                  colour = Ecosystem_type), 
              na.rm = TRUE) +
  geom_smooth(method = lm, formula = y ~ x, aes(fill = Ecosystem_type, color = Ecosystem_type)) +
  ylab("Percent Change") +
  scale_x_continuous(
    breaks = seq(0, max(metadat$Duration), 1),
    labels = seq(1, max(metadat$Duration) + 1, 1),
    name = "Study Duration (years)"
  ) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  guides(fill = EcoTitle, colour = EcoTitle)
  

