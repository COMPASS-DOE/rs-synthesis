
#scartch pad for developing new plots
#data frame "metadat" must be generated from primary code


#Forest plot for Irrigation studies
plyr::count(metadat[metadat$Manipulation == "Irrigation",]$Ecosystem_type)
Ecoi_ss <- c("36","5","7","83","13")

Foresti <- forest(100 * (10^Coef_irrigation[1:5,]$estimate - 1),
                  ci.lb = 100 * (10^Coef_irrigation[1:5,]$ci.lb -1),
                  ci.ub = 100 * (10^Coef_irrigation[1:5,]$ci.ub -1),
                  annotate = TRUE,
                  xlab = "Percent Change in Soil Respiration",
                  slab = c("Forest", "Shrubland",
                           "Savanna", "Grassland", "Desert"),
                  cex = 1,
                  cex.lab = 2,
                  cex.main = 3,
                  digits = 1,
                  lwd = 3
                  )

text(-80, rev(seq(5:1)), Ecoi_ss, cex = 1) # Code to write sample size of sub-groups on graph
op <- par(cex=2, font=2) # Set up font for rest of graph (just the headers of the graph remain), to make bold headings, set font=2
text(-200, 6.2, "Ecosystem") # For this code, enter x-position of text, then y-position. You may have to experiment a bit.
text(-60, 6.2, "Sample Size")
text(350, 6.2, "Mean[95% CI]")
text(0,7, "Increased Precipitation Studies", cex = 1.2)


#Forest plot for Drought studies
plyr::count(metadat[metadat$Manipulation == "Drought",]$Ecosystem_type)
Ecod_ss <- c("2","38","27","1","55","7")

Forestd <- forest(100 * (10^Coef_drought[1:6,]$estimate - 1),
                  ci.lb = 100 * (10^Coef_drought[1:6,]$ci.lb - 1),
                  ci.ub = 100 * (10^Coef_drought[1:6,]$ci.ub - 1),
                  annotate = TRUE,
                  xlab = "Percent Change in Soil Respiration",
                  slab = c("Wetland", "Forest", "Shrubland",
                           "Savanna", "Grassland", "Desert"),
                  cex = 0.9,
                  cex.lab = 1.5,
                  cex.main = 1.7,
                  digits = 1,
                  lwd = 3
)

text(-130, rev(seq(6:1)), Ecod_ss, cex = 1) # Code to write sample size of sub-groups on graph
op <- par(cex=2, font=2) # Set up font for rest of graph (just the headers of the graph remain), to make bold headings, set font=2
text(-225, 7.2, "Ecosystem") # For this code, enter x-position of text, then y-position. You may have to experiment a bit.
text(-130, 7.2, "Sample Size")
text(150, 7.2, "Mean[95% CI]")
text(0,8, "Decreased Precipitation Studies", cex = 1.2)

 