

#side code showing alternate models with the lowest AIC
#data frame "metadat" must be generated from primary code

#below is the best fitting model (lowest AIC) for irrigiation studies
#the "/" notation gives the nested effect of ecosystem type within individual studies

res_irrigation <- rma.mv(yi, vi, random = ~ 1|Study_number/Ecosystem_type,
              mods = ~Duration*Percent_control*Latitude - 1,
              method = "ML",
           data = metadat[metadat$Manipulation == "Irrigation",])
summary.rma(res_irrigation)
profile(res_irrigation)

#here is the lowest AIC model for drought studies

res_drought <- rma.mv(yi, vi, random = ~ 1|Study_number/Ecosystem_type,
                      mods = ~Duration*Percent_control*Latitude - 1,
                      method = "ML",
                      data = metadat[metadat$Manipulation == "Drought",])
summary.rma(res_drought)
profile(res_drought)

