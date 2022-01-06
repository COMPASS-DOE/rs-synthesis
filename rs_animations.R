

WD <- ggplot(data = metadat,
       aes(Duration, yi)) +
  geom_hline(yintercept = 0)+ geom_point() +
  geom_point(aes(fill = Ecosystem_type, colour = Ecosystem_type), na.rm = TRUE, size = 5) +
  #geom_smooth(data = metadat[metadat$Ecosystem_type != "Shrubland",],
  #            method = lm, aes(fill = Ecosystem_type, color = Ecosystem_type)) +
  facet_grid(Manipulation~., labeller = as_labeller(Manip_labs)) +
  ggtitle("Water-Limitation and Study Duration") +
  scale_x_continuous(breaks=seq(0,max(metadat$Duration), 1),
                     labels = seq(1, max(metadat$Duration) +1, 1),
                     name = "Study Durationyears)") +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  guides(fill= EcoTitle, colour = EcoTitle) +
  theme(legend.position = "bottom")

newWD <- WD + transition_states(Ecosystem_type,
                                transition_length = 5,
                                state_length = 10) + ggtitle("{closest_state}")+
  enter_grow() + exit_fade()

animate(newWD, nframes = 100)

othernewWD <- ggplot(data = metadat,
             aes(Duration, yi)) +
  geom_hline(yintercept = 0)+ geom_point() +
  #geom_point(aes(fill = Ecosystem_type, colour = Ecosystem_type), na.rm = TRUE, size = 5) +
  geom_smooth(data = metadat,
              method = lm, aes(fill = Ecosystem_type,
                               color = Ecosystem_type)) +
  facet_grid(Manipulation~., labeller = as_labeller(Manip_labs)) +
  ggtitle("Water-Limitation and Study Duration") +
  scale_x_continuous(breaks=seq(0,max(metadat$Duration), 1),
                     labels = seq(1, max(metadat$Duration) +1, 1),
                     name = "Study Duration") +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  guides(fill= EcoTitle, colour = EcoTitle) +
  theme(legend.position = "bottom") +
    transition_reveal(metadat$Ecosystem_type, along = Duration) + 
  ease_aes("linear")


animate(othernewWD, nframes = 100)

