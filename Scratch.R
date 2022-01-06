
#scartch pad for developing new plots
#data frame "metadat" must be generated from primary code


#Forest plot for Irrigation studies
plyr::count(metadat[metadat$Manipulation == "Irrigation",]$Ecosystem_type)
Ecoi_ss <- c("36","5","7","83","13")

Foresti <- forest(Coef_irrigation[1:5,]$estimate,
                  ci.lb = Coef_irrigation[1:5,]$ci.lb,
                  ci.ub = Coef_irrigation[1:5,]$ci.ub,
                  annotate = TRUE,
                  xlab = "ln(Response Ratio)",
                  slab = c("Forest", "Shrubland",
                           "Savanna", "Grassland", "Desert"),
                  cex = 1.25,
                  cex.lab = 2,
                  cex.main = 3,
                  digits = 2,
                  lwd = 3
                  )

text(-.14, rev(seq(5:1)), Ecoi_ss, cex = 1.5) # Code to write sample size of sub-groups on graph
op <- par(cex=2, font=2) # Set up font for rest of graph (just the headers of the graph remain), to make bold headings, set font=2
text(-.5, 6.2, "Ecosystem") # For this code, enter x-position of text, then y-position. You may have to experiment a bit.
text(-.14, 6.2, "Sample Size")
text(0.85, 6.2, "ln(RR) [95% CI]")
text(0,7, "Increased Precipitation Studies", cex = 1.2)


#Forest plot for Drought studies
plyr::count(metadat[metadat$Manipulation == "Drought",]$Ecosystem_type)
Ecod_ss <- c("2","38","27","1","55","7")

Forestd <- forest(Coef_drought[1:6,]$estimate,
                  ci.lb = Coef_drought[1:6,]$ci.lb,
                  ci.ub = Coef_drought[1:6,]$ci.ub,
                  annotate = TRUE,
                  xlab = "ln(Response Ratio)",
                  slab = c("Wetland", "Forest", "Shrubland",
                           "Savanna", "Grassland", "Desert"),
                  cex = 1.25,
                  cex.lab = 2,
                  cex.main = 3,
                  digits = 2,
                  lwd = 3
)

text(-1.2, rev(seq(6:1)), Ecod_ss, cex = 1.5) # Code to write sample size of sub-groups on graph
op <- par(cex=2, font=2) # Set up font for rest of graph (just the headers of the graph remain), to make bold headings, set font=2
text(-2.25, 7.2, "Ecosystem") # For this code, enter x-position of text, then y-position. You may have to experiment a bit.
text(-1.2, 7.2, "Sample Size")
text(1.3, 7.2, "ln(RR) [95% CI]")
text(0,8, "Decreased Precipitation Studies", cex = 1.2)

 