# 20/10/2025 - LAB EXPERIMENT | WATER FLOW THROUGH LIVE POLE DRAINS
# Author: Fernanda Berlitz 
# E-mail: fernanda.berlitz@gcu.ac.uk


# TASK 00: SELECT BASIC INFO OF SPECIMENS USED DURING WATER TEST FLOW WITH DYE

# LOAD LIBRARIES
library(here)
library(readxl)
library(tidyverse)
library(ggpubr)
library(lubridate) 

# Load the base data once
file_path <- here("data", "input", "outflow_total.xlsx")
raw_data <- read_xlsx(file_path, sheet = "one")

# 1. Select only specimens derived from flow C, dye, and SAT
# We can filter first, then select and rename in one step, ignoring the columns we no longer need
i_specSAT <- raw_data %>%
  filter(flow == "C", flow_type == "dye", k == "SAT") %>%
  select(ID_treat = id, treat, growth_day)

# 2. Save .csv
# Define the file path using 'here' (saving it back to the "data" folder)
save_path <- here("data", "output", "i_specSAT.csv")

# Save the dataframe
write_csv(i_specSAT, save_path)
