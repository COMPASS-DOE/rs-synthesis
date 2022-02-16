
#a bit of scratch code which does not function separately of rs-synthesis.Rmd
#Exploring the relationship between effect size and soil moisture

ggplot(metadat, aes(SM_mean, yi)) +
  geom_point(aes(color = Ecosystem_type)) + 
    facet_grid(Manipulation~.) +
  geom_smooth(method = "lm") +
  scale_color_viridis_d()
  
