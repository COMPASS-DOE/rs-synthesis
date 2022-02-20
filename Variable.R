
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

metadat_dmodel_var <- rma.mv(yi, vi,
                         random = ~ 1 | Study_number,
                         mods = ~ Variable + Ecosystem_type + sg_clay + sg_ocs + Percent_control - 1,
                         method = "REML",
                         data = metadat[metadat$Manipulation == "Drought",],
                         slab = Ecosystem_type,
)
summary(metadat_dmodel_var)





