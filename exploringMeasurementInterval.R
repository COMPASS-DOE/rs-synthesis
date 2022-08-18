
library(rcompanion)

transformTukey(metadat_i$Meas_interval)
hist(metadat_i$Meas_interval)
hist(metadat_i$Meas_interval^0.55)

transformTukey(metadat_i$Duration)
hist(metadat_i$Duration)
hist(metadat_i$Duration^0.475)

kmeans(na.omit(metadat_i$Meas_interval), 3)
#K-means clustering with 3 clusters
#1, 12, 32

metadat_i %>%
  mutate(mi_bins =
         if_else(Meas_interval <= 1, "frequent",
                 if_else(Meas_interval >= 30, "infrequent", "intermediate"))) -> metadat_i
metadat_i$mi_bins <- "intermediate"
metadat_i[metadat_i$Meas_interval < 7,]$mi_bins <- "frequent"
metadat_i[metadat_i$Meas_interval >= 22,]$mi_bins <- "infrequent"

ggplot(metadat_i, aes(MI, yi)) +
  geom_hline(yintercept = 0) +
  geom_point(aes(color = Ecosystem_type)) + geom_smooth(method = lm) +
  facet_wrap(vars(Ecosystem_type), scales = "free") +
  theme(legend.position = "none")


just_meas <- rma.mv(yi, vi,
                         random = ~ 1 | Study_number,
                         mods = ~ MI,
                         method = "REML",
                         data = metadat_i,
                         slab = Ecosystem_type,
)

summary(just_meas)
jm <- tidy(just_meas)

meas_E <- rma.mv(yi, vi,
                    random = ~ 1 | Study_number,
                    mods = ~ MI + Ecosystem_type,
                    method = "REML",
                    data = metadat_i,
                    slab = Ecosystem_type,
)

summary(meas_E)
me <- tidy(meas_E)

meas_dur <- rma.mv(yi, vi,
                    random = ~ 1 | Study_number,
                    mods = ~ MI + Duration,
                    method = "REML",
                    data = metadat_i,
                    slab = Ecosystem_type,
)

summary(meas_dur)
md <- tidy(meas_dur)

meas_durE <- rma.mv(yi, vi,
                    random = ~ 1 | Study_number,
                    mods = ~ MI + Duration + Ecosystem_type - 1,
                    method = "REML",
                    data = metadat_i,
                    slab = Ecosystem_type,
)

summary(meas_durE)
mde <- tidy(meas_durE)

ggplot(metadat_i, aes(Duration, yi)) +
  geom_jitter(aes(color = Ecosystem_type)) + geom_smooth(method = lm) +
  scale_color_viridis_d() +
  stat_poly_eq() + geom_hline(yintercept = 0) +
  facet_wrap(vars(mi_bins)) +
  theme(legend.position = "bottom")

