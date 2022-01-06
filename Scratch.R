
#rm(list=ls())

# #Studies that last @least four years
# #no forest studies longer than 3 years
# metadat %>%
#   group_by(Study_number) %>%
#   filter(length(unique(Study_midyear)) > 2 ) -> md4year

library(gganimate)
library(transformr)

tidyMD <- metadat[complete.cases(metadat$yi),] 
SD <- 
  ggplot(tidyMD, aes(yi, color = Manipulation)) + 
  geom_density() + transition_reveal(along = yi)#+ facet_grid(Variable~., scales = "free") +
  #ggtitle("Distribution of Data")
animateSD <- animate(SD)




plyr::count(metadat[metadat$Manipulation == "Irrigation",]$Ecosystem_type)
Ecoi_ss <- c("36","5","7","83","13")

Foresti <- forest(Coef_irrigation[1:5,]$estimate,
                  ci.lb = Coef_irrigation[1:5,]$ci.lb,
                  ci.ub = Coef_irrigation[1:5,]$ci.ub,
                  annotate = TRUE,
                  xlab = "ln(Response Ratio)",
                  slab = c("Forest", "Shrubland",
                           "Savanna", "Grassland", "Desert"),
                  cex = 1.25,
                  cex.lab = 2,
                  cex.main = 3,
                  digits = 2,
                  lwd = 3
                  )

text(-.14, rev(seq(5:1)), Ecoi_ss, cex = 1.5) # Code to write sample size of sub-groups on graph
op <- par(cex=2, font=2) # Set up font for rest of graph (just the headers of the graph remain), to make bold headings, set font=2
text(-.5, 6.2, "Ecosystem") # For this code, enter x-position of text, then y-position. You may have to experiment a bit.
text(-.14, 6.2, "Sample Size")
text(0.85, 6.2, "ln(RR) [95% CI]")
text(0,7, "Increased Precipitation Studies", cex = 1.2)

plyr::count(metadat[metadat$Manipulation == "Drought",]$Ecosystem_type)
Ecod_ss <- c("2","38","27","1","55","7")

Forestd <- forest(Coef_drought[1:6,]$estimate,
                  ci.lb = Coef_drought[1:6,]$ci.lb,
                  ci.ub = Coef_drought[1:6,]$ci.ub,
                  annotate = TRUE,
                  xlab = "ln(Response Ratio)",
                  slab = c("Wetland", "Forest", "Shrubland",
                           "Savanna", "Grassland", "Wetland"),
                  cex = 1.25,
                  cex.lab = 2,
                  cex.main = 3,
                  digits = 2,
                  lwd = 3
)

text(-1.2, rev(seq(6:1)), Ecod_ss, cex = 1.5) # Code to write sample size of sub-groups on graph
op <- par(cex=2, font=2) # Set up font for rest of graph (just the headers of the graph remain), to make bold headings, set font=2
text(-2.25, 7.2, "Ecosystem") # For this code, enter x-position of text, then y-position. You may have to experiment a bit.
text(-1.2, 7.2, "Sample Size")
text(1.3, 7.2, "ln(RR) [95% CI]")
text(0,8, "Decreased Precipitation Studies", cex = 1.2)


library(ggpmisc)

ggplot(metadat,
       aes(Duration, yi)) +
  geom_hline(yintercept = 0)+
  geom_point(na.rm = TRUE) + geom_smooth(method = lm) +
  facet_grid(Manipulation~., labeller = as_labeller(Manip_labs)) +
  ggtitle("Effect of Study Duration") +
  scale_x_continuous(breaks=seq(0,max(metadat$Duration), 1),
                     labels = seq(1, max(metadat$Duration) +1, 1),
                     name = "Study Duration (years)") +
  stat_poly_eq()


  ggplot(metadat,
         aes(Duration, yi, color=Ecosystem_type, fill = Ecosystem_type)) + 
    scale_fill_viridis_d() +
    geom_hline(yintercept = 0)+
    geom_point(na.rm = TRUE) + 
    geom_smooth(data = metadat[metadat$Manipulation == "Irrigation" &
                                 metadat$Ecosystem_type == "Forest" |
                                 metadat$Manipulation == "Irrigation" &
                                 metadat$Ecosystem_type == "Grassland",],
                               method = lm, aes(fill = Ecosystem_type)) +
    theme(legend.position = "bottom") +
    facet_grid(Manipulation~., labeller = as_labeller(Manip_labs)) +
    ggtitle("Effect of Study Duration By Ecosystem") +
    scale_x_continuous(breaks=seq(0,max(metadat$Duration), 1),
                       labels = seq(1, max(metadat$Duration) +1, 1),
                       name = "Study Duration (years)") +
    scale_fill_viridis_d() +
    guides(fill= EcoTitle, colour = EcoTitle)
  
###For dealing with suspicious rows
##come back to this, study #13079

cooksd <- cooks.distance(res_drought)
influential <- names(cooksd)[(cooksd > 50)]

res_ALTdrought <- rma.mv(yi, vi, random = ~ 1|Study_number,
                      mods = ~Ecosystem_type + Percent_control + Duration -1,
                      method = "ML",
                      data = metadat[metadat$Study_number != "13079",],
                      subset = Manipulation == "Drought")
summary(res_ALTdrought)
profile(res_ALTdrought)
forest.rma(res_ALTdrought)
funnel(res_ALTdrought)
plot(cooks.distance(res_ALTdrought), type = "o")
 