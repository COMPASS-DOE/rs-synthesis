

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


#what is actually getting dropped here?
#what is going on with the Fay studies, see control SD
meta_time2 %>%
  distinct() -> meta_time3

metadat <- escalc(measure = "SMD",
                  m1i = Manip_Resp, m2i = Control_Resp, 
                  sd1i = Manip_SD, sd2i = Control_SD,
                  n1i = N, n2i = N, 
                  slab = paste(Study_number, Author, Study_midyear),
                  data = meta_time2)

metadat %>%
  filter(metadat$yi < 45,
         Percent_control < 350) -> metadat2

#also need to think about filtering outliers

res <- rma(yi, vi, 
           mods= ~Year + Percent_control - 1,
           data = metadat2)
res

ggplot(metadat, aes(Year, yi)) + 
  geom_point(size = 4, na.rm = TRUE) + geom_smooth(method = "lm") #+ facet_grid(Manipulation~.)


#summarize mean effect size, 95%CI, and count by year of study (and manipulation type)
#metadat

lower_ci <- function(mean, se, n, conf_level = 0.95){
  lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}
upper_ci <- function(mean, se, n, conf_level = 0.95){
  upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}

test <- metadat %>%
  group_by(Manipulation, Year) %>%
  summarise(average = mean(yi, na.rm = TRUE),
            ssd = sd(yi, na.rm = TRUE),
            count = n()) %>%
  mutate(se = ssd/sqrt(count),
         lower_ci = lower_ci(average, se, count),
         upper_ci = upper_ci(average, se, count))

ggplot(test, aes(average, Year, label = count)) + 
  geom_vline(xintercept = 0) +
  geom_errorbarh(aes(xmax = upper_ci, xmin = lower_ci,
                     na.rm = TRUE)) +
  geom_point(size = 4, na.rm = TRUE) + 
  scale_y_continuous(breaks=seq(0,13,1)) +
  facet_grid(.~Manipulation) + geom_label()
  

