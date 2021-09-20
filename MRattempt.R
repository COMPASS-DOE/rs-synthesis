

#failed attempt at meta regression using time

metadat <- escalc(measure = "SMD",
                  m1i = Manip_Resp, m2i = Control_Resp, 
                  sd1i = Manip_SD, sd2i = Control_SD,
                  n1i = N, n2i = N, 
                  slab = paste(Study_number, Author, Study_midyear),
                  data = meta_time5)

meta_time4 %>%
  filter(metadat$yi < 45,
         Percent_control < 350) -> meta_time5

meta_time3 <- read.csv("meta_time.csv")

meta_time3 %>%
  distinct() -> meta_time4

res <- rma(yi, vi, 
           mods= ~Percent_control + Latitude + Year - 1,
           data = metadat)
res


library(ggpmisc)
my.formula <- y ~ x

ggplot(metadat2, aes(Year, yi)) +
  geom_point(size = 4, na.rm = TRUE) +
  geom_smooth(method = "lm", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) #+ facet_grid(Manipulation~.)

library(meta)
library(esc)

altmetadata <- esc_mean_sd(grp1m = meta_time4$Manip_Resp, grp2m = meta_time4$Control_Resp, 
                            grp1sd = meta_time4$Manip_SD, grp2sd = meta_time4$Control_SD,
                              grp1n = meta_time4$N, grp2n = meta_time4$N)


soilresp_time <- metareg(altmetadata, Year)
