

#```{r TEMPEST, fig.cap = "Prediction of TEMPEST treatment effects on soil respiration", include = FALSE}
### Tempest
metadat %>%
  filter(Manipulation == "Irrigation") %>%
  select("Study_number", "Ecosystem_type",
         "Duration", "Percent_control", "sg_ocs", "yi",
         "vi") %>%
  na.omit() -> metadat_i2

# rma.mv conducts a meta-analysis multivariate mixed effects model
# the - 1 for the fixed effect modifiers gives us estimates for individual ecosystem types
metadat_imodel2 <- rma.mv(yi, vi,
                          random = ~ 1 | Study_number,
                          mods = ~ Ecosystem_type * Duration + Percent_control + sg_ocs - 1,
                          method = "REML",
                          data = metadat_i2,
                          slab = Ecosystem_type,
)
summary(metadat_imodel2)
ip2 <- tidy(metadat_imodel2)
ipreds2 <- predict.rma(metadat_imodel2)

#combine columns into one dataframe
metadat_i2 %>%
  bind_cols(as.data.frame(ipreds2)) -> metadat_ipreds2

predRes <- list()

for (pc in c(116, 132, 148)) {
  for (dur in 0:3){
    test_predict <- predict(metadat_imodel2,
                            newmods = c(Ecosystem_typeForest = 1,
                                        Ecosystem_typeSavanna = 0,
                                        Ecosystem_typeShrubland = 0,
                                        Ecosystem_typeGrassland = 0,
                                        Ecosystem_typeDesert = 0, 
                                        Duration = dur,
                                        Percent_control = pc, 
                                        sg_ocs = 45,
                                        `Ecosystem_typeGrassland:Duration` = 0,
                                        `Ecosystem_typeForest:Duration` = dur))
    result <- as_tibble(test_predict)
    result$Duration <- dur
    result$Percent_control <- pc
    predRes[[paste(dur, pc)]] <- result
  }
}

temp_est2 <- bind_rows(predRes)
temp_est2$Flood_number <- (temp_est2$Percent_control - 100) / 16


ggplot(
  data = temp_est2,
  aes(Duration+1, 100 * (10 ^ pred -1),
      group = as.factor(Flood_number))) +
  geom_hline(yintercept = 0) +
  geom_point(aes(fill = as.factor(Flood_number),
                 colour = as.factor(Flood_number),
                 size = 0.5)) +
  geom_line(aes(fill = as.factor(Flood_number),
                colour = as.factor(Flood_number)),
            size = 0.75) +
  geom_ribbon(aes(ymin=(100*(10^pred - 1) - (100*(10^se -1))),
                  ymax=(100*(10^pred - 1) + (100*(10^se -1))),
                  fill = as.factor(Flood_number)),
              alpha = 0.2) +
  ylab("predicted \n %Change Soil Respiration") +
  xlab("Study Duration (years)") +
  scale_color_manual(name = "Number of Flood Events",
                     values = c("sienna1", "sienna4","hotpink4")
  ) +
  scale_fill_manual(name = "Number of Flood Events",
                    values = c("sienna1", "sienna4","hotpink4")
  ) +
  ggtitle("Predicted response of soil respiration in TEMPEST") +
  guides(size = "none") +
  theme(legend.position = "bottom")


#```