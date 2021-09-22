
#filter meta_df to get studies with more than one year
meta_df %>%
  group_by(Study_number) %>%
  filter(length(unique(Study_midyear)) > 1 ) -> meta_time

#summary plot
ggplot(meta_time, aes(Study_midyear, Manip_Resp-Control_Resp, color = Author)) + 
  geom_point(na.rm = TRUE) + geom_line() +
  facet_grid(Ecosystem_type~Manipulation) + theme(legend.position = "bottom")

#compute effect sizes
metadat <- escalc(measure = "SMD",
                  m1i = Manip_Resp, m2i = Control_Resp, 
                  sd1i = Manip_SD, sd2i = Control_SD,
                  n1i = N, n2i = N, 
                  slab = paste(Study_number, Author, Study_midyear),
                  data = meta_time)

#also need to think about filtering outliers
metadat %>%
  filter(metadat$yi < 45) -> metadat2

metadat2 %>%
  group_by(Study_number) %>%
  filter(length(unique(Study_midyear)) > 1 ) -> metadat_time

ggplot(metadat_time, aes(YearOfStudy, yi, color = Ecosystem_type, group = Percent_control)) + 
  geom_point(na.rm = TRUE) + geom_line() +
  facet_grid(Ecosystem_type~Manipulation) + theme(legend.position = "bottom")





res <- rma(yi, vi, 
           mods= ~YearOfStudy + Percent_control - 1,
           data = metadat_time)
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

test <- metadat_time %>%
  group_by(Manipulation, YearOfStudy) %>%
  summarise(average = mean(yi, na.rm = TRUE),
            ssd = sd(yi, na.rm = TRUE),
            count = n()) %>%
  mutate(se = ssd/sqrt(count),
         lower_ci = lower_ci(average, se, count),
         upper_ci = upper_ci(average, se, count))

ggplot(test, aes(average, YearOfStudy, label = count)) + 
  geom_vline(xintercept = 0) +
  geom_errorbarh(aes(xmax = upper_ci, xmin = lower_ci,
                     na.rm = TRUE)) +
  geom_point(size = 4, na.rm = TRUE) + 
  scale_y_continuous(breaks=seq(0,13,1)) +
  facet_grid(.~Manipulation) + geom_label()
  

