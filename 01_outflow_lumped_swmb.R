# 20/10/2025 - LAB EXPERIMENT | WATER FLOW THROUGH LIVE POLE DRAINS
# Author: Fernanda Berlitz 
# E-mail: fernanda.berlitz@gcu.ac.uk

# TASK 01: CALCULATE OUTFLOW DISCHARGE RATE, SOIL-WATER BALANCE, AND POROSITY

# LOAD LIBRARIES
library(here)
library(readxl)
library(tidyverse)
library(ggpubr)


# CALCULATE OUTFLOW DISCHARGE AND LUMPED WMB ####
  
  # 1. GET OUTFLOW DATA & ORGANISE BASE TABLE
  file_path <- here("data", "input", "outflow_total.xlsx")
  
  # Load, filter, clean, and rename in one chain
  outflow_clean <- read_xlsx(file_path, sheet = "one") %>%
    filter(flow == "C", flow_type == "dye", k == "SAT") %>%
    select(-n, -date, -flow, -flow_type, -k) %>%
    rename(ID_treat = id)
  
  
  # 2. CALCULATE VOLUMETRIC SOIL-WATER BALANCE (Liters & Leakage)
  outflow_VOL <- outflow_clean %>%
    mutate(
      # Convert outflow to liters
      VOL_10 = out10_ml * 0.001, 
      VOL_15 = out15_ml * 0.001, 
      VOL_20 = out20_ml * 0.001, 
      VOL_25 = out25_ml * 0.001, 
      
      # Calculate total end connection leakage in liters
      LKG_END_L = leak_connect_ml * 0.001, 
      
      # Convert interval leakages to liters AND add the distributed end leakage
      LKG_10 = (leak10_ml * 0.001) + (LKG_END_L * 0.40),
      LKG_15 = (leak15_ml * 0.001) + (LKG_END_L * 0.20),
      LKG_20 = (leak20_ml * 0.001) + (LKG_END_L * 0.20),
      LKG_25 = (leak25_ml * 0.001) + (LKG_END_L * 0.20)
    ) %>%
    select(
      ID_treat, treat, growth_day,
      VOL_10, LKG_10, VOL_15, LKG_15, VOL_20, LKG_20, VOL_25, LKG_25
    )
  
  
  # 3. CALCULATE DISCHARGE RATE PER RUN
  outflow_RATE <- outflow_clean %>%
    mutate(
      Q10 = ((out10_ml * 0.001) / 10) * 60, # L/hour - 10 min run 
      Q15 = ((out15_ml * 0.001) / 5) * 60,  # L/hour - 5 min run, valve closed
      Q20 = ((out20_ml * 0.001) / 5) * 60,  # L/hour - 5 min run
      Q25 = ((out25_ml * 0.001) / 5) * 60   # L/hour - 5 min run, valve closed
    ) %>%
    select(
      ID_treat, treat, growth_day, Q10, Q15, Q20, Q25
    )
  
  
  # 4. LUMPED WATER BALANCE (STORAGE)
  # Note: Renamed to outflow_STOR to prevent losing your VOL/LKG columns
  outflow_STOR <- outflow_VOL %>% 
    mutate(
      # Water inputs
      INPUT_10 = 0.50,
      INPUT_15 = 0.00,
      INPUT_20 = 0.25,
      INPUT_25 = 0.00,
      
      # Calculate water storage (Input - Leakage - Outflow)
      STOR_10 = INPUT_10 - LKG_10 - VOL_10,
      STOR_15 = INPUT_15 - LKG_15 - VOL_15,
      STOR_20 = INPUT_20 - LKG_20 - VOL_20,
      STOR_25 = INPUT_25 - LKG_25 - VOL_25
    ) %>% 
    select(
      ID_treat, treat, growth_day, STOR_10, STOR_15, STOR_20, STOR_25
    )
  
  # view(outflow_STOR) # storage in soil, in litres.
  
  
  # 5. POROSITY DATA
  rho.s <- 2.520598 # constant: particle density of soil, g/cm^3
  
  massSoil <- read_xlsx(here("data", "input", "mass_soil.xlsx")) %>%
    rename(ID_treat = id) %>%
    mutate(
      rho.b = final_soil_g / vol4soil_cm3,         # dry bulk density of soil, g/cm^3
      f.t = 1 - (rho.b / rho.s),                   # total porosity, -
      pore_vol = (f.t * vol4soil_cm3) * 0.001      # total pore volume in litres
    )
  
  
  # 6. LUMPED WATER MASS BALANCE (PERCENTAGES)
  lumped_WB_VOL <- outflow_STOR %>%
    # Join pore_vol data
    left_join(
      massSoil %>% select(ID_treat, pore_vol),
      by = "ID_treat"
    ) %>%
    # Calculate storage as a percentage of total pore capacity
    mutate(
      STOR_10perc = (100 * STOR_10) / pore_vol,
      STOR_15perc = (100 * STOR_15) / pore_vol,
      STOR_20perc = (100 * STOR_20) / pore_vol,
      STOR_25perc = (100 * STOR_25) / pore_vol
    ) %>% 
    select(
      ID_treat, treat, growth_day, STOR_10perc, STOR_15perc, STOR_20perc, STOR_25perc
    )



# CREATE TABLES ####
  
  ## flow discharge | combined table ####
  
  # 1. Create the detailed table (grouped by treat and growth_day)
  g01 <- outflow_RATE %>%
    group_by(treat, growth_day) %>%
    summarise(
      across(
        c(Q10, Q15, Q20, Q25),
        ~ paste0(
          formatC(mean(.x, na.rm = TRUE), format = "f", digits = 2),
          " ± ",
          formatC(sd(.x, na.rm = TRUE), format = "f", digits = 2)
        ),
        .names = "{.col}_mean_sd"
      ),
      .groups = "drop"
    ) %>%
    # Convert growth_day to character just in case it was numeric
    mutate(growth_day = as.character(growth_day)) 
  
  
  # 2. Create the summary table (grouped by treat only)
  g02 <- outflow_RATE %>%
    group_by(treat) %>%
    summarise(
      across(
        c(Q10, Q15, Q20, Q25),
        ~ paste0(
          formatC(mean(.x, na.rm = TRUE), format = "f", digits = 2),
          " ± ",
          formatC(sd(.x, na.rm = TRUE), format = "f", digits = 2)
        ),
        .names = "{.col}_mean_sd"
      ),
      .groups = "drop"
    ) %>%
    # Manually add the growth_day column so it aligns nicely with g01
    mutate(growth_day = "Overall") 
  
  
  # 3. Combine them into one final table
  outflow_discharge <- bind_rows(g01, g02) %>%
    # Optional: Sort by treatment so the "Overall" row sits nicely under its respective treatment
    arrange(treat, growth_day == "Overall")

  # Define the file path using 'here' (saving it back to the "data" folder)
  save_path <- here("data", "output", "outflow_discharge.csv")
  
  # Save the dataframe
  write_csv(outflow_discharge, save_path)



  
  
  ## soil-water retention | combined table ####
  
  # 1. Create the detailed table (grouped by treat and growth_day)
  lump01 <- lumped_WB_VOL %>%
    group_by(treat, growth_day) %>%
    summarise(
      across(
        c(STOR_10perc, STOR_15perc, STOR_20perc, STOR_25perc),
        ~ paste0(
          formatC(mean(.x, na.rm = TRUE), format = "f", digits = 2),
          " ± ",
          formatC(sd(.x, na.rm = TRUE), format = "f", digits = 2)
        ),
        .names = "{.col}_mean_sd"
      ),
      .groups = "drop"
    ) %>%
    # Convert growth_day to character just in case it was numeric
    mutate(growth_day = as.character(growth_day)) 
  
  
  # 2. Create the summary table (grouped by treat only)
  lump02 <- lumped_WB_VOL %>%
    group_by(treat) %>%
    summarise(
      across(
        c(STOR_10perc, STOR_15perc, STOR_20perc, STOR_25perc),
        ~ paste0(
          formatC(mean(.x, na.rm = TRUE), format = "f", digits = 2),
          " ± ",
          formatC(sd(.x, na.rm = TRUE), format = "f", digits = 2)
        ),
        .names = "{.col}_mean_sd"
      ),
      .groups = "drop"
    ) %>%
    # Manually add the growth_day column so it aligns nicely with lump01
    mutate(growth_day = "Overall") 
  
  
  # 3. Combine them into one final table
  water_retention <- bind_rows(lump01, lump02) %>%
    # Sort by treatment so the "Overall" row sits nicely under its respective treatment
    arrange(treat, growth_day == "Overall")
  
  # 4. Save to CSV
  # Define the file path using 'here'
  save_path <- here("data", "output", "water_retention.csv")
  
  # Save the dataframe
  write_csv(water_retention, save_path)
  




# STATISTICAL ANALYSIS ####

  ## flow discharge ####
  
  # 1.1 Normality Checks (Shapiro-Wilk)
  normality_outflow <- outflow_RATE %>%
    group_by(treat) %>%
    summarise(
      across(
        c(Q10, Q15, Q20, Q25), 
        ~ shapiro.test(.x)$p.value, 
        .names = "shapiro_p_{.col}"
      )
    )
  print(normality_outflow)
  # CONCLUSION: All p-values > 0.05. The data within each treatment group can 
  # reasonably be assumed to follow a normal distribution. Proceed with ANCOVA.
  
  
  # 1.2 ANCOVA (Main Effects: categorical 'treat' + numerical 'growth_day' covariate)
  ancova_Q10 <- aov(Q10 ~ treat + growth_day, data = outflow_RATE)
  summary(ancova_Q10) # No significant differences
  
  ancova_Q15 <- aov(Q15 ~ treat + growth_day, data = outflow_RATE)
  summary(ancova_Q15) # treat (p=0.052, marginal), growth_day (p=0.027, sig)
  
  ancova_Q20 <- aov(Q20 ~ treat + growth_day, data = outflow_RATE)
  summary(ancova_Q20) # treat (p=0.027, sig), growth_day (ns)
  
  ancova_Q25 <- aov(Q25 ~ treat + growth_day, data = outflow_RATE)
  summary(ancova_Q25) # treat (p=0.016, sig), growth_day (ns)
  
  
  # 1.3 Differences Between Pulses and Lags (ANOVA)
  # Compare the two Pulses (Q10 vs Q20)
  long_pulses <- outflow_RATE %>%
    pivot_longer(cols = c(Q10, Q20), names_to = "storage", values_to = "STOR_value")
  
  aov_pulses <- aov(STOR_value ~ storage + growth_day + treat + storage:treat, data = long_pulses)
  summary(aov_pulses) 
  # Pulse/Saturation Effect (storage, p = 0.0048): Highly significant. There is a clear difference
  # in outflow discharge between the first water pulse (Q10) and the second water pulse (Q20).
  
  # Time Effect (growth_day, p = 0.0378): Significant. The progression of time (plant development/root growth)
  # actively impacts the overall discharge rate during these pulses.
  
  # Treatment Effect (treat, p = 0.0547): Marginally significant. There is a strong trend, but it sits just
  # outside the strict 0.05 threshold for a definitive difference between the LPD and CON treatments when
  # combining data from both pulses.
  
  # Interaction (storage:treat, p = 0.1284): Not significant. The effect of the LPD treatment is consistent
  # across both pulses (i.e., the treatment doesn't behave drastically differently in Q10 compared to Q20).
  
  
  
  # Compare the two Lags (Q15 vs Q25)
  long_lags <- outflow_RATE %>%
    pivot_longer(cols = c(Q15, Q25), names_to = "storage", values_to = "STOR_value")
  
  aov_lags <- aov(STOR_value ~ storage + growth_day + treat + storage:treat, data = long_lags)
  summary(aov_lags)
  # Lag/Interval Effect (storage, p = 0.1520): Not significant. There is no statistical difference in
  # overall outflow discharge between the first lag (Q15) and the second lag (Q25).
  
  # Time Effect (growth_day, p = 0.1101): Not significant. The progression of time (plant development/root growth)
  # did not affect the discharge rate during these periods.
  
  # Treatment Effect (treat, p = 0.0019): Highly significant. The treatment (LPD vs. CON) is the primary and
  # only significant driver of differences in discharge during the lags.
  
  # Interaction (storage:treat, p = 0.3622): Not significant. The treatment behaves consistently across
  # both lag intervals (i.e., the LPD maintains its relative effect compared to the CON regardless of whether it is the first or second lag).
  
  
  
  
  ## soil-water retention ####
  
  # 2.1 Normality Checks (Shapiro-Wilk)
  normality_wb <- lumped_WB_VOL %>%
    group_by(treat) %>%
    summarise(
      n = n(),
      across(
        c(STOR_10perc, STOR_15perc, STOR_20perc, STOR_25perc), 
        ~ shapiro.test(.x)$p.value, 
        .names = "shapiro_p_{.col}"
      )
    )
  print(normality_wb)
  # CONCLUSION: All p-values > 0.05. Data checks normality. Proceed with ANCOVA.
  
  
  # 2.2 ANCOVA (Main Effects: categorical 'treat' + numerical 'growth_day' covariate)
  
  anova10 <- aov(STOR_10perc ~ treat + growth_day, data = lumped_WB_VOL)                    
  summary(anova10)        
  # CONCLUSION: Main effect of treatment (LPD > CON) is marginally significant (p=0.0728). 
  # Time (growth_day) has no significant effect (p=0.1997).
  
  anova15 <- aov(STOR_15perc ~ treat + growth_day, data = lumped_WB_VOL)                    
  summary(anova15)                    
  # CONCLUSION: Main effect of treatment is marginally significant (p=0.0626). 
  # Time (growth_day) has no significant effect (p=0.5721).                        
  
  anova20 <- aov(STOR_20perc ~ treat + growth_day, data = lumped_WB_VOL)                    
  summary(anova20)        
  # CONCLUSION: Treatment clearly and significantly affects storage (LPD > CON, p=0.0113). 
  # Time (growth_day) has no significant effect (p=0.4889).  
  
  anova25 <- aov(STOR_25perc ~ treat + growth_day, data = lumped_WB_VOL)                    
  summary(anova25)        
  # CONCLUSION: Treatment highly significantly affects storage (LPD < CON, p=0.000715). 
  # Time (growth_day) has no significant effect (p=0.2154).
  
  
  # 2.3 Differences Between Pulses and Lags

  # Compare the two Pulses (STOR_10perc vs STOR_20perc)
  long_wb_pulses <- lumped_WB_VOL %>%
    pivot_longer(cols = c(STOR_10perc, STOR_20perc), names_to = "storage", values_to = "STOR_value")
  
  aov_wb_pulses <- aov(STOR_value ~ storage + growth_day + treat + storage:treat, data = long_wb_pulses)
  summary(aov_wb_pulses)
  # Pulse/Saturation Effect (storage, p < 0.001): Highly significant. There is a massive, clear
  # difference in the percentage of water stored between the first water pulse (Q10) and the second water pulse (Q20).
  
  # Time Effect (growth_day, p = 0.1359): Not significant. The progression of time (plant development/root growth)
  # did not significantly alter how much water was stored in the soil during these active rainfall periods.
  
  # Treatment Effect (treat, p = 0.0029): Highly significant. The physical treatment (LPD vs. CON) is a strong,
  # definitive driver of how much water is retained in the soil across both pulses.
  
  # Interaction (storage:treat, p = 0.9696): Not significant. The LPD treatment behaves remarkably consistently
  # across both pulses. Its relative impact on water storage compared to the control remains the exact same
  # regardless of whether it is the first or second pulse.
  
  
  
  # Compare the two Lags (STOR_15perc vs STOR_25perc)
  long_wb_lags <- lumped_WB_VOL %>%
    pivot_longer(cols = c(STOR_15perc, STOR_25perc), names_to = "storage", values_to = "STOR_value")
  
  aov_wb_lags <- aov(STOR_value ~ storage + growth_day + treat + storage:treat, data = long_wb_lags)
  summary(aov_wb_lags)
  # Lag/Interval Effect (storage, p = 0.1756): Not significant. There is no statistical difference in the
  # percentage of water stored between the first lag (Q15) and the second lag (Q25).
  
  # Time Effect (growth_day, p = 0.8193): Not significant. The progression of time
  # (plant development/root growth) did not significantly alter water storage during these post-rainfall periods.
  
  # Treatment Effect (treat, p = 0.0003): Highly significant. The physical treatment (LPD vs. CON)
  # is a strong, definitive driver of how much water is retained in the soil during both lag periods.
  
  # Interaction (storage:treat, p = 0.4574): Not significant. The LPD treatment behaves consistently
  # across both lag intervals. Its relative impact on water storage compared to the control remains
  # the exact same regardless of whether it is the first or second lag.
  
  
  
  
  