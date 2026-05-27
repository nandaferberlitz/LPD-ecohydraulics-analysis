# 20/10/2025 - LAB EXPERIMENT | WATER FLOW THROUGH LIVE POLE DRAINS
# Author: Fernanda Berlitz 
# E-mail: fernanda.berlitz@gcu.ac.uk

# TASK 04: PROCESS CROSS-SECTIONAL AREAS & CALCULATE FLOW AREA
  
  ## Assumptions:
    # Flow area is a sum of flow that went through porous media plus open/void areas.
    # Open areas occur on the top of the soil matrix in both LPD and CON specimens.
    # Void areas are open areas bigger than 1mm^2. Void areas occur only within the bundle in the LPD specimens.


### LOAD LIBRARIES
library(here)
library(readxl)
library(tidyverse)
library(ggpubr)
library(lubridate)
library(ggplot2)
library(showtext)
library(sysfonts)
library(scales)
library(ggrepel)   #For repelling overlapping text

# Setup custom fonts
font_add_google("Roboto Condensed", "roboto_condensed")
showtext_auto()


source("00_basicInfoSpecimens.R")

### DEFINE CONSTANT VALUES
l_CONTROL <- c("C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09",
               "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "18")


### GET FILES FROM 4 DIFFERENT SOURCES ####

# Source 1: The 6-category cross-sections
p_ALLArea <- here("data", "output", "hue_class", "CSV_results_ALL")
f_ALLArea <- list.files(path = p_ALLArea, pattern = "\\.csv$", full.names = TRUE)

# Source 2: The voids / open area
p_OPENArea <- here("data", "output", "hue_class", "CSV_results_OPEN")
f_OPENArea <- list.files(path = p_OPENArea, pattern = "\\.csv$", full.names = TRUE)

# Source 3: Specific voids area within the bundle
p_VOIDArea <- here("data", "output", "hue_class", "CSV_results_VOID")
f_VOIDArea <- list.files(path = p_VOIDArea, pattern = "\\.csv$", full.names = TRUE)

# Source 4: Bundle and Cuttings Area
p_BUNDLEArea <- here("data", "output", "hue_class", "CSV_results_BUNDLE")
f_BUNDLEArea <- list.files(path = p_BUNDLEArea, pattern = "\\.csv$", full.names = TRUE)


### GET TABLES FROM SOURCES ####

# PROCESS SOURCE 1 (Main Categories)
t_source1 <- map_dfr(f_ALLArea, function(file) {
  temp <- read.csv(file)
  fname <- basename(file)
  
  temp %>%
    mutate(
      ID_treat = sub("^[^_]*_([^_]*)_.*", "\\1", fname),         # Extracts 2nd element
      ID_slice = sub("^[^_]*_[^_]*_([^_]*)_.*", "\\1", fname),   # Extracts 3rd element
      section = case_when(
        X.1 == 1 ~ "AFrame_mm",
        X.1 == 2 ~ "ABorder_mm",
        X.1 == 3 ~ "AAvail_mm",
        X.1 == 4 ~ "APoreFlow_mm",
        X.1 == 5 ~ "ADiffusion_mm",
        X.1 == 6 ~ "ADrySoil_mm",
        TRUE ~ as.character(X.1)
      )
    ) %>%
    select(ID_treat, ID_slice, section, Area) %>%
    pivot_wider(names_from = section, values_from = Area) %>% 
    select(-AFrame_mm, -ABorder_mm)
})

# PROCESS SOURCE 2 (Voids / Open Area)
t_source2 <- map_dfr(f_OPENArea, function(file) {
  temp <- read.csv(file)
  fname <- basename(file)
  
  # Return a single row per file with the summed Area
  data.frame(
    ID_treat = sub("^[^_]*_([^_]*)_.*", "\\1", fname),         # Extracts 2nd element
    ID_slice = sub("^[^_]*_[^_]*_([^_]*)_.*", "\\1", fname),   # Extracts 3rd element
    AOpen_mm = sum(temp$Area, na.rm = TRUE)
  )
})

# PROCESS SOURCE 3 (Specific Voids Area)
t_source3 <- map_dfr(f_VOIDArea, function(file) {
  temp <- read.csv(file)
  fname <- basename(file)
  
  if ("Area" %in% colnames(temp) && sum(temp$Area, na.rm = TRUE) > 0) {
    data.frame(
      ID_treat = sub("^[^_]*_([^_]*)_.*", "\\1", fname),         # Extracts 2nd element
      ID_slice = sub("^[^_]*_[^_]*_([^_]*)_.*", "\\1", fname),   # Extracts 3rd element
      A_voids_mm = sum(temp$Area, na.rm = TRUE)
    )
  } else {
    NULL
  }
})


# PROCESS SOURCE 4 (Bundle and Cuttings Area)
t_source4 <- map_dfr(f_BUNDLEArea, function(file) {
  temp <- read.csv(file)
  fname <- basename(file)
  
  # Extract IDs first, since we need them for the output whether the file passes or fails
  treat_id <- sub("^[^_]*_([^_]*)_.*", "\\1", fname)         # Extracts 2nd element
  slice_id <- sub("^[^_]*_[^_]*_([^_]*)_.*", "\\1", fname)   # Extracts 3rd element
  
  # Strict gatekeeper: Check if the file has exactly 15 rows
  if (nrow(temp) == 15) {
    # If YES: Process normally
    temp %>%
      mutate(
        ID_treat = treat_id,
        ID_slice = slice_id
      ) %>%
      rename(Label = X) %>%
      filter(Label %in% c(14, 15)) %>%
      mutate(
        section = case_when(
          Label == 14 ~ "A_Cuttings_mm",
          Label == 15 ~ "A_Bundle_mm"
        )
      ) %>%
      select(ID_treat, ID_slice, section, Area) %>%
      pivot_wider(names_from = section, values_from = Area)
    
  } else {
    # If NO: Return NA for both calculations to exclude them
    data.frame(
      ID_treat = treat_id,
      ID_slice = slice_id,
      A_Cuttings_mm = NA_real_,
      A_Bundle_mm = NA_real_
    )
  }
})


### COMBINE ALL TABLES ####

t_HueArea <- t_source1 %>%
  # 1. Merge all data sources
  left_join(t_source2, by = c("ID_treat", "ID_slice")) %>%
  left_join(t_source3, by = c("ID_treat", "ID_slice")) %>%
  left_join(t_source4, by = c("ID_treat", "ID_slice")) %>%
  left_join(i_specSAT, by = "ID_treat") %>%
  
  # 2. Consolidate all data cleaning and math into a single mutate block
  mutate(
    # Replace NAs with 0 for areas that didn't exist in a slice
    AOpen_mm = replace_na(AOpen_mm, 0),
    A_voids_mm = replace_na(A_voids_mm, 0),
    
    # Adjust Open Area: subtract bundle voids for LPD (ImageJ captured all voids together)
    AOpen_mm = case_when(
      treat == "CON" ~ AOpen_mm,
      treat == "LPD" ~ AOpen_mm - A_voids_mm,
      TRUE ~ 0
    ),
    
    # Calculate total lateral flow area
    ALatFlow_mm = AOpen_mm + A_voids_mm + APoreFlow_mm,
    
    # Map ID_slice to longitudinal position (PosX_cm)
    PosX_cm = case_match(
      ID_slice,
      c("01", "02") ~ "24",
      c("03", "04") ~ "18",
      c("05", "06") ~ "12",
      c("07", "08") ~ "06",
      .default = ID_slice
    ),
    
    # Map ID_slice to Side (LEFT/RIGHT)
    Side = case_match(
      ID_slice,
      c("01", "03", "05", "07") ~ "RIGHT",
      c("02", "04", "06", "08") ~ "LEFT",
      .default = ID_slice
    )
  ) %>%
  
  # 3. Calculate average area flow per location (Averages LEFT and RIGHT sides together)
  group_by(ID_treat, treat, growth_day, PosX_cm) %>% 
  summarise(
    across(
      where(is.numeric),
      \(x) mean(x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  
  # 4. Organise final table
  select(ID_treat, treat, growth_day, PosX_cm, everything())



### GET FLOW AREA % ####
t_FlowArea <- t_HueArea %>% 
  select(ID_treat, treat, growth_day, PosX_cm, AAvail_mm, ALatFlow_mm) %>% 
  mutate(
    ALatFlow_p  = (ALatFlow_mm  / AAvail_mm)*100
  ) %>% 
  select(-AAvail_mm, -ALatFlow_mm) %>% 
  group_by(treat, PosX_cm, growth_day) %>% 
  summarise(
    across(
      c(ALatFlow_p), # You can easily add more columns to this vector later if needed
      ~ paste0(
        formatC(mean(.x, na.rm = TRUE), format = "f", digits = 2),
        " ± ",
        formatC(sd(.x, na.rm = TRUE), format = "f", digits = 2)
      ),
      .names = "{.col}_mean_sd"
    ),
    .groups = "drop"
  )

# Define the file path using 'here' (saving it back to the "data" folder)
save_path <- here("data", "output", "flow_area.csv")

# Save the dataframe
write_csv(t_FlowArea, save_path)



  
### PLOT FLOW AREA % ####
data <- t_HueArea %>% 
  select(ID_treat, treat, growth_day, PosX_cm, AAvail_mm, ALatFlow_mm) %>% 
  mutate(
    ALatFlow_p  = (ALatFlow_mm  / AAvail_mm)*100
  ) %>% 
  select(-AAvail_mm, -ALatFlow_mm) %>% 
  group_by(treat, PosX_cm, growth_day) %>% 
  summarise(
    across(
      c(ALatFlow_p), # You can easily add more columns to this vector later if needed
      ~ paste0(
        formatC(mean(.x, na.rm = TRUE), format = "f", digits = 2)
      ),
      .names = "{.col}_mean"
    ),
    .groups = "drop"
  )


# Data Preparation
data$growth_day <- as.factor(data$growth_day)
data$Treatment <- as.factor(data$treat)
data$ALatFlow_p_mean <- as.numeric(data$ALatFlow_p_mean)
data$PosX_cm <- as.numeric(data$PosX_cm)


# Create the plot
plot <- ggplot(data, aes(
  x = PosX_cm, 
  y = ALatFlow_p_mean, 
  color = Treatment, 
  fill = Treatment,       
  linetype = growth_day,  
  group = interaction(Treatment, growth_day)
)) +
  
  # Add the shaded area for the standard deviation (sd)
  # geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.05, color = NA) +
  
  # Add the lines for the mean
  geom_line(linewidth = 0.8) +
  
  # Add points to clearly see where the actual data markers are
  geom_point(size = 1) +
  
  # -- NEW: Use geom_text_repel to fix overlapping labels vertically --
  geom_text_repel(
    data = subset(data, PosX_cm == max(PosX_cm)), 
    aes(label = paste("day", growth_day)), 
    nudge_x = 0.5,                 # Pushes text consistently to the right
    direction = "y",             # Only adjusts positions vertically to avoid overlap
    segment.color = "grey70",    # Color of the tiny line connecting text to point
    segment.size = 0.3,
    hjust = 0,                   # Left-align the repelled text
    size = 3.5, 
    show.legend = FALSE 
  ) +
  
  # Define custom colors for BOTH the lines and the filled ribbon
  scale_color_manual(values = c("CON" = "#0F1622", "LPD" = "#7E8737")) +
  scale_fill_manual(values = c("CON" = "#0F1622", "LPD" = "#7E8737")) +
  
  # -- NEW: Explicitly assign requested linetypes to specific days --
  scale_linetype_manual(values = c("18" = "dotted", "32" = "dashed", "46" = "solid")) +
  
  # Expand the right side slightly more to make room for the repelled labels
  scale_x_continuous(
    breaks = c(6, 12, 18, 24),
    expand = expansion(mult = c(0.01, 0.05)) 
  ) +
  
  # Set y-axis to 0-80 with breaks every 10
  scale_y_continuous(
    limits = c(0, 60),
    breaks = seq(0, 60, by = 5),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  
  # Customize labels
  labs(
    x = "CS (cm)",
    y = "Flow Area (%)",
    color = "Treatment",
    fill = "Treatment",
    linetype = "Growth Day"
  ) +
  
  # Apply Theme
  theme_minimal(base_family = "roboto_condensed") +
  theme(
    strip.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.position = "bottom",
    
    # -- NEW: Make the legend line wider so differences in dashes/dots are visible --
    legend.key.width = unit(2.5, "lines"), 
    
    axis.text.x = element_text(size = 9, hjust = 0.5, vjust = 1),
    axis.text.y = element_text(size = 9, hjust = 0, vjust = 0.5),
    axis.title = element_text(size = 10),
    panel.grid.major.y = element_line(color = "white"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey99"),
    panel.grid.minor.x = element_blank(),
    panel.spacing.y = unit(0.7, "lines"),
    panel.border = element_rect(fill = NA, color = "grey50", linewidth = 0.4)
  )

# Display the plot
print(plot)





# STATS ####
stats_FlowArea <- t_HueArea %>% 
  select(ID_treat, treat, growth_day, PosX_cm, AAvail_mm, ALatFlow_mm) %>% 
  mutate(
    ALatFlow_p  = (ALatFlow_mm  / AAvail_mm)*100
  ) %>% 
  select(-AAvail_mm, -ALatFlow_mm)

str(stats_FlowArea)

# Prepare data
stats_FlowArea <- stats_FlowArea %>%
  mutate(
    treat = as.factor(treat),            # treatment is categorical
    growth_day = as.numeric(growth_day), # continuous covariate
    PosX_cm = as.numeric(PosX_cm)        # continuous covariate
  )

# Normality check (per ALL variables)
normality <- stats_FlowArea %>%
  group_by(PosX_cm) %>%
  summarise(shapiro_p = shapiro.test(ALatFlow_p)$p.value)

print(normality)
# all variables were 'normal'


# “Is there a difference in area of flow between treatments?” -- one-factor ANOVA
aov_oneway <- aov(ALatFlow_p ~ treat, data = stats_FlowArea)
summary(aov_oneway)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# treat        1   3324    3324   25.53 4.63e-06 ***
# Residuals   58   7550     130


# “Is there a difference in area of flow between treatments and over time?” -- ANCOVA!
aov_ancova <- aov(ALatFlow_p ~ treat + growth_day, data = stats_FlowArea)
summary(aov_ancova)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# treat        1   3324    3324  26.263 3.69e-06 ***
# growth_day   1    336     336   2.656    0.109    
# Residuals   57   7214     127


# “Is there a difference in area of flow between treatments and over position?” -- ANCOVA!
aov_ancova <- aov(ALatFlow_p ~ treat + PosX_cm, data = stats_FlowArea)
summary(aov_ancova)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# treat        1   3324    3324   31.32 6.53e-07 ***
# PosX_cm      1   1501    1501   14.14 0.000402 ***
# Residuals   57   6050     106


# e.g., On day 18, are the differences in AreaFlow between LPD and CON treatments statistically significant?

day18 <- stats_FlowArea %>%
  filter(growth_day == 18)

aov_day18 <- aov(ALatFlow_p ~ treat, data = day18)
summary(aov_day18)
# Df Sum Sq Mean Sq F value Pr(>F)    
# treat        1   1197  1196.7   16.66  7e-04 ***
# Residuals   18   1293    71.8 


day32 <- stats_FlowArea %>%
  filter(growth_day == 32)

aov_day32 <- aov(ALatFlow_p ~ treat, data = day32)
summary(aov_day32)
# Df Sum Sq Mean Sq F value  Pr(>F)    
# treat        1   2138  2137.9   15.71 0.00091 ***
# Residuals   18   2449   136.1 


day46 <- stats_FlowArea %>%
  filter(growth_day == 46)

aov_day46 <- aov(ALatFlow_p ~ treat, data = day46)
summary(aov_day46)
# Df Sum Sq Mean Sq F value Pr(>F)
# treat        1  362.1   362.1   2.491  0.132
# Residuals   18 2616.5   145.4  


# e.g., On CS 06, are the differences in AreaFlow between LPD and CON treatments statistically significant?

cs06 <- stats_FlowArea %>%
  filter(PosX_cm == 6)

aov_cs06 <- aov(ALatFlow_p ~ treat, data = cs06)
summary(aov_cs06)
# Df Sum Sq Mean Sq F value Pr(>F)
# treat        1    248  248.01   3.113  0.101
# Residuals   13   1036   79.67   


cs12 <- stats_FlowArea %>%
  filter(PosX_cm == 12)

aov_cs12 <- aov(ALatFlow_p ~ treat, data = cs12)
summary(aov_cs12)
# Df Sum Sq Mean Sq F value  Pr(>F)   
# treat        1 1216.3  1216.3    17.5 0.00107 **
# Residuals   13  903.7    69.5 


cs18 <- stats_FlowArea %>%
  filter(PosX_cm == 18)

aov_cs18 <- aov(ALatFlow_p ~ treat, data = cs18)
summary(aov_cs18)
# Df Sum Sq Mean Sq F value Pr(>F)  
# treat        1   1236  1236.4   8.486 0.0121 *
# Residuals   13   1894   145.7    


cs24 <- stats_FlowArea %>%
  filter(PosX_cm == 24)

aov_cs24 <- aov(ALatFlow_p ~ treat, data = cs24)
summary(aov_cs24)
# Df Sum Sq Mean Sq F value Pr(>F)  
# treat        1  871.5   871.5   8.485 0.0121 *
# Residuals   13 1335.2   102.7 


# e.g., “On day 18 and CS 06, are the differences in AreaFlow between LPD and CON treatments statistically significant?”

day18_cs06 <- stats_FlowArea %>%
  filter(growth_day == 18,
         PosX_cm    == 6)

aov_day18_cs06 <- aov(ALatFlow_p ~ treat, data = day18_cs06)
summary(aov_day18_cs06)
#             Df Sum Sq Mean Sq  F value Pr(>F)
# treat        1   62.9   62.88   0.579  0.502
# Residuals    3  325.7  108.56  


day18_cs12 <- stats_FlowArea %>%
  filter(growth_day == 18,
         PosX_cm    == 12)

aov_day18_cs12 <- aov(ALatFlow_p ~ treat, data = day18_cs12)
summary(aov_day18_cs12)
# Df Sum Sq Mean Sq F value Pr(>F)  
# treat        1  534.7   534.7   14.65 0.0314 *
# Residuals    3  109.5    36.5  


day18_cs18 <- stats_FlowArea %>%
  filter(growth_day == 18,
         PosX_cm    == 18)

aov_day18_cs18 <- aov(ALatFlow_p ~ treat, data = day18_cs18)
summary(aov_day18_cs18)
# Df Sum Sq Mean Sq F value Pr(>F)  
# treat        1  736.4   736.4    5.64 0.0981 .
# Residuals    3  391.7   130.6 


day18_cs24 <- stats_FlowArea %>%
  filter(growth_day == 18,
         PosX_cm    == 24)

aov_day18_cs24 <- aov(ALatFlow_p ~ treat, data = day18_cs24)
summary(aov_day18_cs24)
# Df Sum Sq Mean Sq F value Pr(>F)
# treat        1  120.9  120.91   5.358  0.104
# Residuals    3   67.7   22.57 


day32_cs06 <- stats_FlowArea %>%
  filter(growth_day == 32,
         PosX_cm    == 6)

aov_day32_cs06 <- aov(ALatFlow_p ~ treat, data = day32_cs06)
summary(aov_day32_cs06)
# Df Sum Sq Mean Sq F value Pr(>F)
# treat        1  296.8  296.81   3.628  0.153
# Residuals    3  245.5   81.82   


day32_cs12 <- stats_FlowArea %>%
  filter(growth_day == 32,
         PosX_cm    == 12)

aov_day32_cs12 <- aov(ALatFlow_p ~ treat, data = day32_cs12)
summary(aov_day32_cs12)
# Df Sum Sq Mean Sq F value Pr(>F)  
# treat        1 195.72  195.72   28.39 0.0129 *
# Residuals    3  20.68    6.89 


day32_cs18 <- stats_FlowArea %>%
  filter(growth_day == 32,
         PosX_cm    == 18)

aov_day32_cs18 <- aov(ALatFlow_p ~ treat, data = day32_cs18)
summary(aov_day32_cs18)
# Df Sum Sq Mean Sq F value Pr(>F)  
# treat        1  938.4   938.4   12.41 0.0389 *
# Residuals    3  226.9    75.6  


day32_cs24 <- stats_FlowArea %>%
  filter(growth_day == 32,
         PosX_cm    == 24)

aov_day32_cs24 <- aov(ALatFlow_p ~ treat, data = day32_cs24)
summary(aov_day32_cs24)
# Df Sum Sq Mean Sq F value Pr(>F)  
# treat        1  937.8   937.8   10.74 0.0465 *
# Residuals    3  262.0    87.3    


day46_cs06 <- stats_FlowArea %>%
  filter(growth_day == 46,
         PosX_cm    == 6)

aov_day46_cs06 <- aov(ALatFlow_p ~ treat, data = day46_cs06)
summary(aov_day46_cs06)
# Df Sum Sq Mean Sq F value Pr(>F)
# treat        1   4.49    4.49   0.088  0.786
# Residuals    3 153.17   51.06 


day46_cs12 <- stats_FlowArea %>%
  filter(growth_day == 46,
         PosX_cm    == 12)

aov_day46_cs12 <- aov(ALatFlow_p ~ treat, data = day46_cs12)
summary(aov_day46_cs12)
# Df Sum Sq Mean Sq F value Pr(>F)
# treat        1  542.5   542.5   4.605  0.121
# Residuals    3  353.4   117.8  


day46_cs18 <- stats_FlowArea %>%
  filter(growth_day == 46,
         PosX_cm    == 18)

aov_day46_cs18 <- aov(ALatFlow_p ~ treat, data = day46_cs18)
summary(aov_day46_cs18)
# Df Sum Sq Mean Sq F value Pr(>F)
# treat        1    9.8    9.82   0.047  0.843
# Residuals    3  631.6  210.53   


day46_cs24 <- stats_FlowArea %>%
  filter(growth_day == 46,
         PosX_cm    == 24)

aov_day46_cs24 <- aov(ALatFlow_p ~ treat, data = day46_cs24)
summary(aov_day46_cs24)
# Df Sum Sq Mean Sq F value Pr(>F)
# treat        1  90.46   90.46   1.698  0.284
# Residuals    3 159.84   53.28


# Filter for only LPD treatment
only_LPD <- stats_FlowArea %>%
  filter(treat == "LPD")

# Run the ANOVA testing the effect of position (PosX_cm)
aov_LPD_gradient <- aov(ALatFlow_p ~ PosX_cm, data = only_LPD)
summary(aov_LPD_gradient)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# PosX_cm      1   1288  1288.1   13.07 0.000959 ***
# Residuals   34   3350    98.5 

