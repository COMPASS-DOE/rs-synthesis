



#Studies that last @least four years
#no forest studies longer than 3 years
metadat %>%
  group_by(Study_number) %>%
  filter(length(unique(Study_midyear)) > 2 ) -> md4year




  


