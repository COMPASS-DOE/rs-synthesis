
# #Studies that last @least four years
# #no forest studies longer than 3 years
# metadat %>%
#   group_by(Study_number) %>%
#   filter(length(unique(Study_midyear)) > 2 ) -> md4year


plyr::count(metadat[metadat$Manipulation == "Irrigation",]$Ecosystem_type)
Ecoi_ss <- c("13","35","83","7","5")

Foresti <- forest(Coef_irrigation[1:5,]$estimate,
                  ci.lb = Coef_irrigation[1:5,]$ci.lb,
                  ci.ub = Coef_irrigation[1:5,]$ci.ub,
                  annotate = TRUE,
                  xlab = "ln(Response Ratio)",
                  slab = c("Desert", "Forest", "Grassland",
                           "Savanna", "Shrubland"),
                  
                  cex = 1,
                  digits = 2)

text(-.25, rev(seq(5:1)), Ecoi_ss, cex = 1) # Code to write sample size of sub-groups on graph
op <- par(cex=1, font=2) # Set up font for rest of graph (just the headers of the graph remain), to make bold headings, set font=2
text(-.5, 6.2, "Ecosystem Type") # For this code, enter x-position of text, then y-position. You may have to experiment a bit.
text(-.22, 6.2, "Sample Size")
text(0.9, 6.2, "ln(Response Ratio) [95% CI]")
text(0,7, "Increased Precipitation Studies")

plyr::count(metadat[metadat$Manipulation == "Drought",]$Ecosystem_type)
Ecod_ss <- c("7","39","55","1","27","2")

Forestd <- forest(Coef_drought[1:6,]$estimate,
                  ci.lb = Coef_drought[1:6,]$ci.lb,
                  ci.ub = Coef_drought[1:6,]$ci.ub,
                  annotate = TRUE,
                  xlab = "ln(Response Ratio)",
                  slab = c("Desert", "Forest", "Grassland",
                           "Savanna", "Shrubland", "Wetland"),
                  
                  cex = 1,
                  digits = 2)

text(-.9, rev(seq(6:1)), Ecod_ss, cex = 1) # Code to write sample size of sub-groups on graph
op <- par(cex=1, font=2) # Set up font for rest of graph (just the headers of the graph remain), to make bold headings, set font=2
text(-2, 7.2, "Ecosystem Type") # For this code, enter x-position of text, then y-position. You may have to experiment a bit.
text(-.9, 7.2, "Sample Size")
text(1.8, 7.2, "ln(Response Ratio) [95% CI]")
text(0,7.9, "Decreased Precipitation Studies")



###For dealing with suspicous rows

suspects <- cooks.distance(res_drought) > 33
targets <- metadat[metadat$Manipulation == "Drought",]
suspects <- targets[suspects,]

 