
metadat <- escalc(measure = "SMD",
                  m1i = Manip_Resp, m2i = Control_Resp, 
                  sd1i = Manip_SD, sd2i = Control_SD,
                  n1i = N, n2i = N, 
                  slab = paste(Study_number, Author, Study_midyear),
                  data = meta_df)

metadat %>%
  filter(metadat$yi < 45,
         Percent_control < 350) -> metadat2

res <- rma(yi, vi, 
           mods= ~Percent_control + Latitude - 1,
           data = metadat2)
res

ggplot(metadat2, aes(Percent_control, yi)) +
  geom_point(size = 4, na.rm = TRUE) + geom_smooth(method = "lm")