
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(kableExtra)
library(forcats)
library(DT)
library(metafor)

setwd("rs-synthesis/")

srdb_raw <- read.csv("./rs/srdb-data.csv") %>% as_tibble()

read_csv("./rs/Rs Studies 2018+ - Water Manipulation.csv",
         col_types = "cdddcdcccdcccdddddddddddc") %>% 
  select(Study_number, Record_number, Author, Study_midyear, Latitude, Ecosystem_type, 
         Manipulation, Manipulation_level, Meas_method, Soil_type,
         Rs_annual, Rh_annual, Rs_growingseason, Soil_drainage, Elevation) -> new_studies
srdb_raw %>% 
  select(Study_number, Record_number, Author, Study_midyear, Latitude, Ecosystem_type, 
         Manipulation, Manipulation_level, Meas_method, Soil_type,
         Rs_annual, Rh_annual, Rs_growingseason, Soil_drainage, Elevation, Quality_flag)  %>% 
  filter(!grepl("Q1[0-5]", Quality_flag)) %>% 
  bind_rows(new_studies) -> srdb

read_csv("rs/Variance and N - Water manipulations.csv", skip = 2,
         col_types = "ddcdcccdcccdddddddddddc") %>% 
  #filter(Study_number==10526) %>% 
  select(Study_number, Record_number, N, SD_Rs_annual,	SD_Rh_annual, 
         SD_Rs_growingseason, Percent_control, SM_mean, SM_sd) %>% 
  left_join(srdb, by = c("Study_number", "Record_number")) %>% 
  mutate(Manipulation = case_when(
    Percent_control < 100 ~ "Drought",
    Percent_control > 100 ~ "Irrigation",
    TRUE ~ "Control")) -> dat_rs

dat_rs %>% 
  filter(Manipulation == "Control") %>% 
  select(-Record_number, -Author, -Manipulation, -Manipulation_level, -N, -SM_mean, -SM_sd)->
  controls

controls %>% 
  select(-starts_with("SD_"), -Percent_control) %>% 
  pivot_longer(cols = c(Rs_annual, Rh_annual, Rs_growingseason), 
               names_to = "depvar", values_to = "Control_Resp") ->
  cont1

controls %>% 
  select(-Rs_annual, -Rh_annual, -Rs_growingseason, -Percent_control) %>% 
  pivot_longer(cols = c(SD_Rs_annual, SD_Rh_annual, SD_Rs_growingseason),
               names_to = "depvar", values_to = "Control_SD") %>% 
  mutate(depvar = gsub("SD_", "", depvar)) ->
  cont2

cont1 %>% left_join(cont2) -> meta_control

dat_rs %>% 
  filter(Manipulation != "Control") %>% 
  select(-starts_with("SD_")) %>% 
  pivot_longer(cols = c(Rs_annual, Rh_annual, Rs_growingseason), 
               names_to = "depvar", values_to = "Manip_Resp") ->
  manip1

dat_rs %>% 
  filter(Manipulation != "Control") %>% 
  select(-Rs_annual, -Rh_annual, -Rs_growingseason) %>% 
  pivot_longer(cols = c(SD_Rs_annual, SD_Rh_annual, SD_Rs_growingseason), 
               names_to = "depvar", values_to = "Manip_SD") %>% 
  mutate(depvar = gsub("SD_", "", depvar)) ->
  manip2

manip1 %>% left_join(manip2) -> meta_manip

meta_manip %>% 
  left_join(meta_control, 
            by = c("Study_number", "Study_midyear", "Ecosystem_type",
                   "Latitude", "Meas_method", "Soil_type", "Soil_drainage",
                   "Elevation", "depvar")) %>% 
  filter(!is.na(Manip_Resp)) %>% 
  rename("Variable" = "depvar", "Quality_flag" = "Quality_flag.x") %>% 
  select(-Quality_flag.y) ->
  meta_df

limit <- max(abs(meta_df$Percent_control), na.rm = TRUE) * c(-1, 1) + 100 

ggplot(meta_df, aes(Control_Resp, Manip_Resp, color = Percent_control)) +
  geom_point(size = 4, na.rm = TRUE) + geom_abline() + 
  scale_color_distiller(palette = "BrBG", direction = 1, limit = limit, 
                        breaks = c(-200, 100, 400), labels = c(-200, 100, 400)) +
  facet_wrap(~Variable, scales = "free")

ggplot(meta_df, aes(Control_Resp, Manip_Resp, color = Manipulation)) +
  geom_point(size = 3, na.rm = TRUE) + geom_abline() + 
  facet_wrap(~Variable, scales = "free")

ggplot(meta_df, aes(Manip_Resp / Control_Resp, color = Manipulation)) + 
  geom_density(na.rm = TRUE) + facet_grid(Variable~., scales = "free")

ggplot(meta_time, aes(Study_midyear, Manip_Resp-Control_Resp, color = Author)) + 
  geom_point(na.rm = TRUE) + geom_line() +
  facet_grid(Ecosystem_type~Manipulation) + theme(legend.position = "bottom")

meta_df %>%
  group_by(Study_number) %>%
  filter(length(unique(Study_midyear)) > 1 ) -> meta_time


ggplot(meta_df, aes(log(Manip_Resp / Control_Resp), fct_reorder(paste(Study_number, Author), Percent_control), color = Manipulation)) +
  geom_point() +
  geom_vline(xintercept = 0)

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
  metadat <- escalc(measure = "SMD",
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

do_ma(metadat, )