
#this code is a companion to rs-synthesis.Rmd,
#but can be run independently
#it produces several different meta-regressions
#use single modifiers and combinations
#the aim is to demonstrate
#that measurement interval (how often soil respiration is measured)
#is confounded with study duration
#and ecosystem in the +P studies

library(dplyr)
library(metafor)
library(ggplot2)

theme_set(theme_minimal() + theme(text = element_text(size = 16)))

#read in metadat (written as csv on 19.08.2022)
metadat <- read.csv("rs/metadat.csv")

#sort levels of ecosystem_type
metadat$Ecosystem_type <- factor(metadat$Ecosystem_type,
                                 levels = c(
                                   "Wetland", "Forest", "Shrubland",
                                   "Savanna", "Grassland", "Desert"))
#for graphing later
Manip_labs <- c("Drought" = "-P",
                "Irrigation" = "+P")
Eco_colors <- c("#01665e","#5ab4ac","#c7eae5","#f6e8c3","#d8b365","#8c510a")

#convert measurement interval to factor
metadat$mi_bins <- ""
metadat %>%
  mutate(mi_bins =
           if_else(Meas_interval <= 1, "frequent",
                   if_else(Meas_interval >= 30, 
                           "infrequent", "intermediate"))) -> metadat
metadat[is.na(metadat$mi_bins),]$mi_bins <- "unknown"

#sort levels of measurement interval bins
metadat$mi_bins <- factor(metadat$mi_bins,
                                 levels = c(
                                   "unknown", "frequent",
                                   "intermediate", "infrequent"))

#subset increased precipitation studies, all possible mods
metadat_i <- metadat %>%
  filter(Manipulation == "Irrigation") %>%
  select("Study_number", "Ecosystem_type",
         "Percent_control", "Meas_interval",
         "sg_ocs", "sg_clay", "Duration", 
         "mi_bins", "yi", "vi")

#subset decreased precipitation studies, all possible mods
metadat_d <- metadat %>%
  filter(Manipulation == "Drought") %>%
  select("Study_number", "Ecosystem_type",
         "Percent_control", "Meas_interval",
         "sg_ocs", "sg_clay", "Duration",
         "mi_bins", "yi", "vi")
##
#+P
#single modifiers

#ecosystem type
i_et <- rma.mv(yi, vi, random = ~ 1 | Study_number,
               mods = ~ Ecosystem_type - 1,
               method = "REML",
               data = metadat_i,
               slab = Study_number)
summary(i_et)
#p value for all ecosystems (except shrublands) < 0.01

#duration
i_d <- rma.mv(yi, vi, random = ~ 1 | Study_number,
               mods = ~ Duration,
               method = "REML",
               data = metadat_i,
               slab = Study_number)
summary(i_d)
#p value for duration < 0.0001

#soil organic carbon
i_soc <- rma.mv(yi, vi, random = ~ 1 | Study_number,
               mods = ~ sg_ocs,
               method = "REML",
               data = metadat_i,
               slab = Study_number)
summary(i_soc)
#p value for soc = 0.30

#measurement interval
i_mi <- rma.mv(yi, vi, random = ~ 1 | Study_number,
               mods = ~ Meas_interval,
               method = "REML",
               data = metadat_i,
               slab = Study_number)
summary(i_mi)
#p value for Meas_interval = 0.40

#all mods
#same as main analysis, but w/ Meas_interval added)
i_all <- rma.mv(yi, vi, random = ~ 1 | Study_number,
               mods = ~ Ecosystem_type * Duration +
                 sg_ocs + Meas_interval - 1,
               method = "REML",
               data = metadat_i,
               slab = Study_number)
summary(i_all)


#effect of measurement interval on -P studies
d_mi <- rma.mv(yi, vi, random = ~ 1 | Study_number,
               mods = ~ Meas_interval,
               method = "REML",
               data = metadat_d,
               slab = Study_number)
summary(d_mi)
#p value for Meas_interval = 0.74


#all mods
#same as main analysis, but w/ Meas_interval added)
d_all <- rma.mv(yi, vi, random = ~ 1 | Study_number,
                mods = ~ Ecosystem_type + sg_clay + sg_ocs +
                  Percent_control + Meas_interval - 1,
                method = "REML",
                data = metadat_d,
                slab = Study_number)
summary(d_all)
#p value for Meas_interval = 0.70

#Conclusion, significance of Meas_interval for +P
#is dependent on variation due to ecosystem type
#and study duration, also being taken into account

#graph it!
ggplot(metadat_i[metadat_i$Ecosystem_type != "Shrubland" &
                   metadat_i$Ecosystem_type != "Savanna",],
       aes(Duration, yi)) +
  geom_jitter(aes(fill = mi_bins,
                  colour = mi_bins),
              size = 3, na.rm = TRUE) +
  geom_smooth(method = lm, formula = y ~ x,
              se = FALSE,
              aes(fill = mi_bins,
                  colour = mi_bins)) +
  scale_color_viridis_d(name = "Measurement Interval") +
  scale_fill_viridis_d(name = "Measurement Interval") +
  theme(legend.position = "bottom") +
  geom_hline(yintercept = 0) +  ylab("ln(RR)") +
  scale_x_continuous(
    breaks = seq(0, max(metadat$Duration), 1),
    labels = seq(1, max(metadat$Duration) + 1, 1),
    name = "Study Duration (yrs)") +
  facet_grid(.~Ecosystem_type, scales = "free") +
  theme(legend.position = "right")

#direct effect of measurement interval
ggplot(metadat[metadat$Manipulation == "Irrigation" &
                 metadat$Meas_interval < 84,],
       aes(Meas_interval, yi)) +
  geom_jitter(aes(color = mi_bins),
              size = 3, na.rm = TRUE) +
  geom_smooth(method = lm, formula = y ~ x,
              se = FALSE, na.rm = TRUE) +
  scale_color_viridis_d(name = "Measurement Interval",
                        na.translate=FALSE) +
  theme(legend.position = "bottom") +
  geom_hline(yintercept = 0) +  ylab("ln(RR)") +
  xlab("Measurement Interval (days)")


#forest plot of i_et
Coef_i_et<- coef(summary(i_et))
plyr::count(metadat[metadat$Manipulation == "Irrigation",]$Ecosystem_type)
Eco_ss <- c("36", "5", "7", "80", "13")
Forest_i_et <- forest(Coef_i_et$estimate,
                     ci.lb = Coef_i_et$ci.lb,
                     ci.ub = Coef_i_et$ci.ub,
                     annotate = TRUE,
                     slab = c(
                       "Forest", "Shrubland",
                       "Savanna", "Grassland",
                       "Desert"
                     ),
                     cex = 1,
                     cex.lab = 1,
                     cex.main = 2,
                     digits = 2,
                     lwd = 2
)
text(-0.25, rev(seq(5:1)), Eco_ss)
op <- par(cex=1, font=2)
text(-0.62, 6.25, "Ecosystem")
text(-0.25, 6.25, "Sample Size")
text(1, 6.25, "ln(RR) [95% CI]")
text(0.1, 7.25, "Solo Effect of Ecosystem \n +P Studies")
