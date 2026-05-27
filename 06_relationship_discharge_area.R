# 20/10/2025 - LAB EXPERIMENT | WATER FLOW THROUGH LIVE POLE DRAINS
# Author: Fernanda Berlitz 
# E-mail: fernanda.berlitz@gcu.ac.uk

# TASK 06: RELATIONSHIP DISCHARGE AND FLOW AREA

# LOAD LIBRARIES ####
library(here)
library(readxl)
library(tidyverse) 
library(ggpubr)
library(GGally)
library(patchwork)
library(gridExtra)
library(broom) 

# SOURCE EXTERNAL SCRIPTS ####
source("01_outflow_lumped_swmb.R")
source("04_hue_areas.R")

# DATA PREPARATION ####
discharge <- outflow_RATE %>% 
  select(ID_treat, treat, growth_day, Q20) %>% 
  mutate(Q20_m3_s = (Q20 / 1000) / 3600)

flow_area <- t_HueArea %>% 
  select(ID_treat, treat, growth_day, PosX_cm, APoreFlow_mm, AOpen_mm, A_voids_mm, ALatFlow_mm) %>%
  mutate(
    across(
      ends_with("_mm"),
      ~ .x / 1000000,
      .names = "{str_replace(.col, '_mm', '_m2')}"
    ),
    PosX_cm = as.character(PosX_cm)
  ) %>% 
  select(-ends_with("_mm"))

QA_models_data <- flow_area %>%
  left_join(
    discharge %>% select(ID_treat, Q20_m3_s),
    by = "ID_treat"
  ) %>%
  pivot_longer(
    cols = ends_with("_m2"),
    names_to = "area_metric",
    values_to = "area_value"
  )

# RUN MODELS & EXTRACT METRICS ####
QA_table <- QA_models_data %>%
  drop_na(Q20_m3_s, area_value) %>% 
  group_by(treat, growth_day, PosX_cm, area_metric) %>%
  filter(sum(area_value) > 0) %>% 
  nest() %>%
  mutate(
    model = map(data, ~ lm(Q20_m3_s ~ area_value + 0, data = .x)),
    tidy_stats = map(model, ~ possibly(tidy, otherwise = tibble())(.x)),
    model_fit  = map(model, ~ possibly(glance, otherwise = tibble())(.x))
  ) %>%
  
  # Unnest the overall fit
  unnest(model_fit) %>%
  
  # DROP the duplicate columns
  select(-statistic, -p.value) %>% 
  
  # Unnest the coefficient stats
  unnest(tidy_stats) %>%
  
  # Select for the final CSV
  select(
    treat, 
    growth_day, 
    PosX_cm, 
    area_metric, 
    r.squared,
    adj.r.squared,
    beta = estimate, 
    p.value
  ) %>%
  ungroup()

# EXPORT ####
write_csv(QA_table, here("data", "output", "Q-A_Regression_Coefficients.csv"))





