

meta_df %>%
  group_by(Study_number) %>%
  filter(length(unique(Study_midyear)) > 1 ) -> meta_time

ggplot(meta_time, aes(Study_midyear, Manip_Resp-Control_Resp, color = Author)) + 
  geom_point(na.rm = TRUE) + geom_line() +
  facet_grid(Ecosystem_type~Manipulation) + theme(legend.position = "bottom")

write.csv(meta_time, "meta_time.csv")

meta_time2 <- read.csv("meta_time.csv")


length(unique(meta_df$Study_number))
length(unique(meta_time$Study_number))
length(unique(meta_time$Author))

metadat <- escalc(measure = "RR",
                  m1i = Manip_Resp, m2i = Control_Resp, 
                  sd1i = Manip_SD, sd2i = Control_SD,
                  n1i = N, n2i = N, 
                  slab = paste(Study_number, Author, Study_midyear),
                  data = meta_time2)

metadat %>%
  filter(metadat$yi < 45,
         Percent_control < 350) -> metadat2

res <- rma(yi, vi, 
           mods= ~Percent_control + Latitude - 1,
           data = metadat2)
res

ggplot(metadat2, aes(Year, yi)) + 
  geom_point(size = 4, na.rm = TRUE) + geom_smooth(method = "lm") + facet_grid(Manipulation~.)
