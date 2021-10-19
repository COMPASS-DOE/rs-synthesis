
library(skimr)

#duration
meta_df %>%
  group_by(Study_number, OtherFactor, Ecosystem_type) %>%
  mutate(Duration = Study_midyear - min(Study_midyear)) -> meta_time

meta_time %>%
  select(-Study_number, -Record_number, -OtherFactor, -Manipulation_level, -Soil_type) %>%
  skim()


#compute effect sizes
metadat <- escalc(measure = "ROM",
                  m1i = Manip_Resp, m2i = Control_Resp, 
                  sd1i = Manip_SD, sd2i = Control_SD,
                  n1i = N, n2i = N, 
                  slab = paste(Study_number, Author, Study_midyear),
                  data = meta_time)

#conduct meta-analysis
res <- rma(yi, vi, 
           mods= ~Ecosystem_type + Duration + Percent_control - 1,
           data = metadat)
res


#linear relationship between effect size and duration
ggplot(metadat, aes(Duration, yi)) + 
  geom_point(size = 4, na.rm = TRUE) + geom_smooth(method = "lm") #+ facet_grid(Manipulation~.)


#Studies that last @least four years
#no forest studies longer than 3 years
metadat %>%
  group_by(Study_number) %>%
  filter(length(unique(Study_midyear)) > 2 ) -> md4year


ggplot(md4year[md4year$Ecosystem_type == "Desert" | md4year$Ecosystem_type == "Forest",],
       aes(Duration, yi, color=Manipulation)) + 
  geom_point(na.rm = TRUE) + geom_smooth(method = lm) +
  facet_grid(.~Ecosystem_type, scales="free") #+ theme(legend.position = "bottom")

ggplot(md4year[md4year$Ecosystem_type == "Grassland" | md4year$Ecosystem_type == "Shrubland",],
       aes(Duration, yi, color=Manipulation)) + 
  geom_point(na.rm = TRUE) + geom_smooth(method = lm) +
  facet_grid(.~Ecosystem_type, scales="free") #+ theme(legend.position = "bottom")

#summarize mean effect size, 95%CI, and count by year of study (and manipulation type)
#metadat

lower_ci <- function(mean, se, n, conf_level = 0.95){
  lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}
upper_ci <- function(mean, se, n, conf_level = 0.95){
  upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}

test <- metadat %>%
  group_by(Manipulation, Duration) %>%
  summarise(average = mean(yi, na.rm = TRUE),
            ssd = sd(yi, na.rm = TRUE),
            count = n()) %>%
  mutate(se = ssd/sqrt(count),
         lower_ci = lower_ci(average, se, count),
         upper_ci = upper_ci(average, se, count))

ggplot(test, aes(average, Duration, label = count)) + 
  geom_vline(xintercept = 0) +
  geom_errorbarh(aes(xmax = upper_ci, xmin = lower_ci,
                     na.rm = TRUE)) +
  geom_point(size = 4, na.rm = TRUE) + 
  scale_y_continuous(breaks=seq(0,13,1)) +
  facet_grid(.~Manipulation) + geom_label()
  

