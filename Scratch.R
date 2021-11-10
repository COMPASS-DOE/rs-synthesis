

#conduct meta-analysis
#this is the best fitting model of irrigiation studies - not sure if the "/" notation is correct
#does it makes sense to have Ecosystem_type as a random effect???? Maybe???



res_irrigation <- rma(yi, vi, random = ~ 1|Study_number/Ecosystem_type,
              mods = ~Duration*Percent_control*Latitude,
              method = "ML",
           data = metadat[metadat$Manipulation == "Irrigation",],
           test = "knha")
summary.rma(res_irrigation)
profile(res_irrigation)


res_drought <- rma.mv(yi, vi, random = ~ 1|Study_number/Ecosystem_type,
                         mods = ~Duration * Percent_control * Latitude,
                         method = "ML",
                         data = metadat_short[metadat_short$Manipulation == "Drought",])
summary.rma(res_drought)
profile(res_drought)

library(MuMIn)
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("MathiasHarrer/dmetar")

multimodel.inference(TE = "yi", seTE = "vi",
                     data = metadat[metadat$Manipulation == "Drought",],
                     predictors = c("Ecosystem_type", "Duration", "Percent_control", "Soil_drainage"),
                     interaction = TRUE)

#would be nice to drop desert and wetland, as those are under represented
#this didn't actually reduce the AIC very much
metadat %>%
  filter(Ecosystem_type != "Savanna" &
         Ecosystem_type != "Wetland") %>%
  droplevels() -> metadat_short




#conduct meta-analysis
res2 <- rma(yi, vi, 
           mods= ~Ecosystem_type*Duration*Percent_control - 1,
           data = metadat[metadat$Manipulation == "Drought",])
res2




#Studies that last @least four years
#no forest studies longer than 3 years
metadat %>%
  group_by(Study_number) %>%
  filter(length(unique(Study_midyear)) > 2 ) -> md4year


ggplot(metadat_short,
       aes(Duration, yi, color=Ecosystem_type)) + 
  geom_hline(yintercept = 0)+
  geom_point(na.rm = TRUE) + geom_smooth(method = lm, aes(fill = Ecosystem_type)) +
  facet_grid(Manipulation~.) + theme(legend.position = "bottom")

ggplot(metadat[metadat$Ecosystem_type == "Grassland" | metadat$Ecosystem_type == "Forest",],
       aes(Duration, yi, color=Manipulation)) + 
  geom_point(na.rm = TRUE) + geom_smooth(method = lm) +
  facet_grid(.~Ecosystem_type, scales="free") #+ theme(legend.position = "bottom")

ggplot(metadat,
       aes(yi, Latitude, color=Manipulation)) + 
  geom_point(na.rm = TRUE) + geom_smooth(method = lm)


  


