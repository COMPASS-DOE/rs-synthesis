
#To test interaction between ecosystem type and duration


res_interaction<- rma.mv(yi, vi, random = ~ 1|Study_number,
                         mods = ~Ecosystem_type*Duration*Manipulation -1,
                         method = "ML",
                         data = metadat)
summary(res_interaction)
profile(res_interaction)
forest.rma(res_interaction)
funnel(res_interaction)
plot(cooks.distance(res_interaction), type = "o")


cooksd <- cooks.distance(res_interaction)
influential <- names(cooksd)[(cooksd > 50)]
influential

Coef_interaction <- coef(summary(res_interaction))

Forest_eco <- forest(Coef_interaction$estimate,
                  ci.lb = Coef_interaction$ci.lb,
                  ci.ub = Coef_interaction$ci.ub,
                  annotate = TRUE,
                  xlab = "ln(Response Ratio)",
                  cex = 1.25,
                  cex.lab = 2,
                  cex.main = 3,
                  digits = 2,
                  lwd = 3
)
