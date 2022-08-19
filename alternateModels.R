
#write.csv(x = metadat, "metadat.csv")
metadat <- read.csv("metadat.csv")

metadat$Ecosystem_type <- factor(metadat$Ecosystem_type,
                                 levels = c(
                                   "Wetland", "Forest", "Shrubland",
                                   "Savanna", "Grassland", "Desert"))

#convert measurement interval to factor
metadat$mi_bins <- ""
metadat %>%
  mutate(mi_bins =
           if_else(Meas_interval <= 1, "frequent",
                   if_else(Meas_interval >= 30, 
                           "infrequent", "intermediate"))) -> metadat
metadat[is.na(metadat$mi_bins),]$mi_bins <- "unknown"


metadat$mi_bins <- factor(metadat$mi_bins,
                                 levels = c(
                                   "unknown", "frequent",
                                   "intermediate", "infrequent"))

#effect of measurement interval on +P studies
metadati_mi <- metadat %>%
  filter(Manipulation == "Irrigation") %>%
  select("Study_number", "Meas_interval",
         "yi", "vi") %>% na.omit()

i_mi <- rma.mv(yi, vi, random = ~ 1 | Study_number,
               mods = ~ Meas_interval,
               method = "REML",
               data = metadati_mi,
               slab = Study_number)
summary(i_mi)

#effect of measurement interval on -P studies
metadatd_mi <- metadat %>%
  filter(Manipulation == "Drought") %>%
  select("Study_number", "Meas_interval",
         "yi", "vi") %>% na.omit()

d_mi <- rma.mv(yi, vi, random = ~ 1 | Study_number,
               mods = ~ Meas_interval,
               method = "REML",
               data = metadatd_mi,
               slab = Study_number)
summary(d_mi)

#summary figure with duration, ecosystem type,
# and measurement frequency bins
ggplot(metadat[metadat$mi_bins != "unknown" &
                 metadat$Duration < 7,],
       aes(Duration, yi)) +
  geom_jitter(aes(fill = Ecosystem_type,
                  colour = Ecosystem_type),
              size = 3, na.rm = TRUE) +
  geom_smooth(method = lm, formula = y ~ x,
              aes(fill = Ecosystem_type,
                  color = Ecosystem_type)) +
  scale_x_continuous(
    breaks = seq(0, max(metadat$Duration), 1),
    labels = seq(1, max(metadat$Duration) + 1, 1),
    name = "Study Duration (yrs)") +
  scale_color_manual(name = "Ecosystem", values = Eco_colors) +
  scale_fill_manual(name = "Ecosystem", values = Eco_colors) +
  theme(legend.position = "bottom") +
  geom_hline(yintercept = 0) +  ylab("ln(RR)") +
  facet_grid(mi_bins ~ Manipulation, scales = "free") +
  theme(legend.position = "bottom")

#alt summary with just +P
ggplot(metadat[metadat$Manipulation == "Irrigation",],
       aes(Duration, yi)) +
  geom_jitter(aes(fill = Ecosystem_type,
                  colour = Ecosystem_type),
              size = 3, na.rm = TRUE) +
  geom_smooth(method = lm, formula = y ~ x,
              aes(fill = Ecosystem_type,
                  color = Ecosystem_type)) +
  scale_x_continuous(
    breaks = seq(0, max(metadat$Duration), 1),
    labels = seq(1, max(metadat$Duration) + 1, 1),
    name = "Study Duration (yrs)") +
  scale_color_manual(name = "Ecosystem", values = Eco_colors) +
  scale_fill_manual(name = "Ecosystem", values = Eco_colors) +
  theme(legend.position = "bottom") +
  geom_hline(yintercept = 0) +  ylab("ln(RR)") +
  facet_grid(Ecosystem_type ~ mi_bins, scales = "free") +
  theme(legend.position = "bottom")
