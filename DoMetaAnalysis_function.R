
do_ma <- function(dat, condition, dv, output = TRUE) {
  dat %>% 
    filter(Manipulation == condition, Variable == dv) -> 
    dat_condition
  
  cat("condition = ", condition, "\n")
  cat("var = ", dv, "\n")
  cat("N = ", nrow(dat_condition), "\n")
  cat("Percent control values:\n")
  print(summary(dat_condition$Percent_control))
  
  # Construct the meta-analysis variables and run the MA
  metadat <- escalc(measure = "ROM",
                    m1i = Manip_Resp, m2i = Control_Resp, 
                    sd1i = Manip_SD, sd2i = Control_SD,
                    n1i = N, n2i = N, 
                    slab = paste(Study_number, Author, Study_midyear),
                    data = dat_condition)
  
  # Arrange by response ratio
  metadat <- metadat[order(metadat$yi),]
  
  if(nrow(metadat) < 10) {
    return(list(metadata = metadat, mam = NULL))  
  }
  
  # BBL TODO: build formula dynamically and have a single model fit
  if(length(unique(metadat$Soil_drainage)) > 1) {
    mam <- rma(yi, vi, 
               mods = ~ Soil_drainage + Ecosystem_type +  Percent_control, 
               data = metadat)
  } else {
    mam <- rma(yi, vi, 
               mods = ~ Ecosystem_type +  Percent_control, 
               data = metadat)
  } 
  
  if(output) {
    # Diagnostics
    print(summary(mam))
    forest.rma(mam)
    title(paste(condition, dv))
    plot(mam)
  }
  invisible(list(metadat = metadat, mam = mam))
}