# 20/10/2025 - LAB EXPERIMENT | WATER FLOW THROUGH LIVE POLE DRAINS
# Author: Fernanda Berlitz 
# E-mail: fernanda.berlitz@gcu.ac.uk

# TASK 09: SENSITIVITY ANALYSIS

# LOAD LIBRARIES ####
library(here)
library(readxl)
library(tidyverse)
library(ggpubr)
library(GGally)
library(showtext)  # For custom fonts
library(sysfonts)

# Add Roboto Condensed font (automatically downloaded via Google Fonts)
font_add_google("Roboto Condensed", "roboto_condensed")
showtext_auto()

# SOURCE EXTERNAL SCRIPTS ####
source("04_hue_areas.R")

# GET RAW DATA ####

# Source - Threshold 100-255
p_TH100 <- here("data", "output", "hue_class", "OUTPUT_TH100")
f_TH100 <- list.files(path = p_TH100, pattern = "\\.csv$", full.names = TRUE)

# Source - Threshold 125-255
p_TH125 <- here("data", "output", "hue_class", "OUTPUT_TH125")
f_TH125 <- list.files(path = p_TH125, pattern = "\\.csv$", full.names = TRUE)


# PROCESS Source - Threshold 100-255 ####
t_th100 <- map_dfr(f_TH100, function(file) {
  temp <- read.csv(file)
  fname <- basename(file)
  
  temp %>%
    mutate(
      ID_treat = sub("^[^_]*_([^_]*)_.*", "\\1", fname),         # Extracts 2nd element
      ID_slice = sub("^[^_]*_[^_]*_([^_]*)_.*", "\\1", fname),   # Extracts 3rd element
      section = case_when(
        X.1 == 1 ~ "A100_mm",
        TRUE ~ as.character(X.1)
      )
    ) %>%
    select(ID_treat, ID_slice, section, Area) %>%
    pivot_wider(names_from = section, values_from = Area)
})


# PROCESS Source - Threshold 125-255 ####
t_th125 <- map_dfr(f_TH125, function(file) {
  temp <- read.csv(file)
  fname <- basename(file)
  
  temp %>%
    mutate(
      ID_treat = sub("^[^_]*_([^_]*)_.*", "\\1", fname),         # Extracts 2nd element
      ID_slice = sub("^[^_]*_[^_]*_([^_]*)_.*", "\\1", fname),   # Extracts 3rd element
      section = case_when(
        X.1 == 1 ~ "A125_mm",
        TRUE ~ as.character(X.1)
      )
    ) %>%
    select(ID_treat, ID_slice, section, Area) %>%
    pivot_wider(names_from = section, values_from = Area)
})


# COMBINE SOURCES & ADD LOCATION SLICE ####
t_threshold <- t_th100 %>% 
  left_join(t_th125, by = c("ID_treat", "ID_slice")) %>% 
  mutate(
    PosX_cm = case_when(
      ID_slice %in% c("01", "02") ~ "24",
      ID_slice %in% c("03", "04") ~ "18",
      ID_slice %in% c("05", "06") ~ "12",
      ID_slice %in% c("07", "08") ~ "06",
      TRUE ~ as.character(ID_slice)  
    ),
    Side = case_when(
      ID_slice %in% c("01", "03", "05", "07") ~ "RIGHT",
      ID_slice %in% c("02", "04", "06", "08") ~ "LEFT",
      TRUE ~ as.character(ID_slice)  
    )
  ) %>% 
  group_by(ID_treat, PosX_cm) %>% 
  summarise(
    across(where(is.numeric), \(x) mean(x, na.rm = TRUE)),
    .groups = "drop"
  )


# JOIN BASE THRESHOLD WITH NEW THRESHOLDS ####
# print(t_HueArea)

sensitivity <- t_HueArea %>% 
  select(ID_treat, treat, growth_day, PosX_cm, AAvail_mm, ALatFlow_mm) %>% 
  rename(A112_mm = ALatFlow_mm) %>% 
  left_join(t_threshold, by = c("ID_treat", "PosX_cm")) %>% 
  mutate(PosX_cm = as.numeric(PosX_cm))


# CALCULATE FLOW AREA % ####
sensitivity <- sensitivity %>% 
  mutate(
    hue100_p = (A100_mm / AAvail_mm) * 100,
    hue112_p = (A112_mm / AAvail_mm) * 100,
    hue125_p = (A125_mm / AAvail_mm) * 100 
  )



# PLOT SENSITIVITY ####

# 1. Prepare the data and calculate the relative change per treatment
sensitivity_summary <- sensitivity %>%
  group_by(treat) %>%           # <-- ADDED: Group by treatment
  summarise(
    mean_100 = mean(hue100_p, na.rm = TRUE),
    mean_112 = mean(hue112_p, na.rm = TRUE),
    mean_125 = mean(hue125_p, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    change_100 = ((mean_100 - mean_112) / mean_112) * 100,
    change_125 = ((mean_125 - mean_112) / mean_112) * 100
  )

sensitivity_plot_data <- sensitivity %>%
  select(treat, hue100_p, hue112_p, hue125_p) %>% # <-- ADDED: Keep 'treat' column
  rename(
    "+10" = hue100_p,
    "0"   = hue112_p,
    "-10" = hue125_p
  ) %>%
  pivot_longer(
    cols = c("+10", "0", "-10"), # <-- FIXED: Only pivot the numeric columns, ignore 'treat'
    names_to = "Threshold_Shift", 
    values_to = "Flow_Area_Pct"
  ) %>%
  mutate(Threshold_Shift = as.numeric(Threshold_Shift))

write_csv(sensitivity_plot_data, here("data", "output", "sensitivity_analysis.csv"))


# 2. Create the Plot with Change Labels
ggplot(sensitivity_plot_data, aes(x = Threshold_Shift, y = Flow_Area_Pct, color = treat)) +
  
  # Individual data points
  geom_jitter(width = 0.8, alpha = 0.3) + 
  
  # Mean line and points
  stat_summary(fun = mean, geom = "line", aes(group = treat), alpha = 0.8, linewidth = 0.8) +
  stat_summary(fun = mean, geom = "point", size = 3, alpha = 0.8) +
  
  # ADDING THE PERCENTAGE LABELS DYNAMICALLY PER TREATMENT
  # Label for Hue 100 (+10% shift)
  geom_text(
    data = sensitivity_summary,
    aes(x = 8, y = mean_100 + 4, label = paste0("+", round(change_100, 1), "%"), color = treat),
    fontface = "bold", hjust = 0.5, show.legend = FALSE, inherit.aes = FALSE
  ) +
  
  # Label for Hue 125 (-10% shift)
  geom_text(
    data = sensitivity_summary,
    aes(x = -8, y = mean_125 - 4, label = paste0(round(change_125, 1), "%"), color = treat),
    fontface = "bold", hjust = 0.5, show.legend = FALSE, inherit.aes = FALSE
  ) +
  
  # Formatting
  scale_color_manual(values = c("CON" = "#0F1622", "LPD" = "#7E8737")) + # Define custom colors
  
  scale_x_continuous(
    breaks = c(-10, 0, 10), 
    labels = c("-10%", "0%", "+10%")
  ) +
  scale_y_continuous(
    limits = c(0, 100),            
    breaks = seq(0, 100, by = 10)  
  ) +
  
  labs(
    x = "Hue Threshold Adjustment",
    y = "Measured Flow Area (%)",
    color = "Treatment"
  ) +
  
  theme_minimal(base_family = "roboto_condensed") +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.major.y = element_line(color = "white"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey99"),
    panel.grid.minor.x = element_blank(),
    panel.border = element_rect(fill = NA, color = "grey50", linewidth = 0.4)
  )
  