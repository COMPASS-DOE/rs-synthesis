
#side code, data frame "metadat" must be generated from primary code
#directly testing the effect of three seperate dependent variables

#plot showing distribution of effect sizes among dependent variables
ggplot(metadat, aes(yi, color = Manipulation)) +
  xlab("ln(Response Ratio)") +
  geom_density(na.rm = TRUE) +
  facet_grid(Variable~., scales = "free", labeller = labeller(Variable = VarLabs)) +
  ggtitle("Distribution of Data")

metadat_imodel_var <- rma.mv(yi, vi,
                             random = ~ 1 | Study_number,
                             mods = ~ Variable + Ecosystem_type * Duration + sg_ocs - 1,
                             method = "REML",
                             data = metadat[metadat$Manipulation == "Irrigation",],
                             slab = Ecosystem_type,
)
summary(metadat_imodel_var)
Coef_ivar <- coef(summary(metadat_imodel_var))
Forestivar <- forest(Coef_ivar[1:3, ]$estimate,
                  ci.lb = Coef_ivar[1:3, ]$ci.lb,
                  ci.ub = Coef_ivar[1:3, ]$ci.ub,
                  annotate = TRUE,
                  slab = c(
                    "Rh_annual", "Rs_annual",
                    "Rs_growingseason"
                  ),
                  cex = 1,
                  cex.lab = 1,
                  cex.main = 2,
                  digits = 1,
                  lwd = 2
)
#irrigation studies
#there are differences in the statistical significance of the RR's
#BUT they are not statistically different from each other
#evidence is 95% CI overlap

metadat_dmodel_var <- rma.mv(yi, vi,
                         random = ~ 1 | Study_number,
                         mods = ~ Variable + Ecosystem_type + sg_clay + sg_ocs + Percent_control - 1,
                         method = "REML",
                         data = metadat[metadat$Manipulation == "Drought",],
                         slab = Ecosystem_type,
)
summary(metadat_dmodel_var)
Coef_dvar <- coef(summary(metadat_dmodel_var))
Forestdvar <- forest(Coef_dvar[1:3, ]$estimate,
                     ci.lb = Coef_dvar[1:3, ]$ci.lb,
                     ci.ub = Coef_dvar[1:3, ]$ci.ub,
                     annotate = TRUE,
                     slab = c(
                       "Rh_annual", "Rs_annual",
                       "Rs_growingseason"
                     ),
                     cex = 1,
                     cex.lab = 1,
                     cex.main = 2,
                     digits = 1,
                     lwd = 2
)
#drought studies
#here are no differences in the statistical significance of the RR's
#ANDthey are not statistically different from each other
#evidence is 95% CI overlap



