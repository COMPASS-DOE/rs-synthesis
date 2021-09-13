

meta_df %>%
  group_by(Study_number) %>%
  filter(length(unique(Study_midyear)) > 1 ) -> meta_time

ggplot(meta_time, aes(Study_midyear, Manip_Resp-Control_Resp, color = Author)) + 
  geom_point(na.rm = TRUE) + geom_line() +
  facet_grid(Ecosystem_type~Manipulation) + theme(legend.position = "bottom")

