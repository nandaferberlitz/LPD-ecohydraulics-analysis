# 20/10/2025 - LAB EXPERIMENT | WATER FLOW THROUGH LIVE POLE DRAINS
# Author: Fernanda Berlitz 
# E-mail: fernanda.berlitz@gcu.ac.uk

# TASK 08: CALCULATE HEAD LOSS

# LOAD LIBRARIES ####
library(here)
library(readxl)
library(tidyverse)
library(ggpubr)
library(GGally)
library(patchwork)
library(gridExtra)
library(scales)    # for axis scaling
library(showtext)  # for custom fonts
library(sysfonts)
library(rlang)   

# Add Roboto Condensed font (automatically downloaded via Google Fonts)
font_add_google("Roboto Condensed", "roboto_condensed")
showtext_auto()

# SOURCE EXTERNAL SCRIPTS ####
source("01_outflow_lumped_swmb.R")
source("04_hue_areas.R")

# GET RAW DATA ####

# MATRIC DATA TEROS 31
p_T31 <- here("data", "input", "matric_kPa.xlsx")
x_T31matric <- read_xlsx(p_T31, sheet = "matric")

# TEROS 31 POSITION IN SPECIMENS
p_T31_pos <- here("data", "input", "T31_position.xlsx")
x_T31_pos <- read_xlsx(p_T31_pos, sheet = "T31_SAT")

# ATMOSPHERIC PRESSURE FROM GLASGOW
p_Glasgow <- here("data", "input", "hourly_pressure_Glasgow.xlsx")
x_GlasgowAtm <- read_xlsx(p_Glasgow)


# DATA PREPARATION ####

## MATRIC POTENTIAL ####

# 1. Clean table and format dates
matric <- x_T31matric %>% 
  rename(ID_treat = id) %>% 
  drop_na(ID_treat) %>% 
  filter(flow == "C", k == "SAT") %>% 
  mutate(
    time_fixed = format(time, "%H:%M:%S"), 
    datetime = ymd_hms(paste(date, time_fixed), tz = "UTC")
  ) %>% 
  select(ID_treat, treat, growth_day, datetime, T31, matric_kPa, temp_C)


# 2. Assign RUNs 
matric <- matric %>%
  mutate(
    run = case_when(
      # C01
      datetime >= dmy_hms("06-11-2024 08:57:52") & datetime <= dmy_hms("06-11-2024 09:07:52") ~ "Q10",
      datetime >= dmy_hms("06-11-2024 09:07:53") & datetime <= dmy_hms("06-11-2024 09:13:02") ~ "Q15",      
      datetime >= dmy_hms("06-11-2024 09:13:03") & datetime <= dmy_hms("06-11-2024 09:18:03") ~ "Q20",
      datetime >= dmy_hms("06-11-2024 09:18:04") & datetime <= dmy_hms("06-11-2024 09:23:12") ~ "Q25",
      # C02
      datetime >= dmy_hms("23-10-2024 10:57:37") & datetime <= dmy_hms("23-10-2024 11:07:37") ~ "Q10",
      datetime >= dmy_hms("23-10-2024 11:07:38") & datetime <= dmy_hms("23-10-2024 11:13:03") ~ "Q15",    
      datetime >= dmy_hms("23-10-2024 11:13:04") & datetime <= dmy_hms("23-10-2024 11:18:04") ~ "Q20",
      datetime >= dmy_hms("23-10-2024 11:18:05") & datetime <= dmy_hms("23-10-2024 11:23:41") ~ "Q25",
      # C03
      datetime >= dmy_hms("23-10-2024 11:36:36") & datetime <= dmy_hms("23-10-2024 11:46:36") ~ "Q10",
      datetime >= dmy_hms("23-10-2024 11:46:37") & datetime <= dmy_hms("23-10-2024 11:52:04") ~ "Q15",  
      datetime >= dmy_hms("23-10-2024 11:52:05") & datetime <= dmy_hms("23-10-2024 11:57:05") ~ "Q20",
      datetime >= dmy_hms("23-10-2024 11:57:06") & datetime <= dmy_hms("23-10-2024 12:02:23") ~ "Q25",
      # C06
      datetime >= dmy_hms("09-10-2024 11:37:20") & datetime <= dmy_hms("09-10-2024 11:47:20") ~ "Q10",
      datetime >= dmy_hms("09-10-2024 11:47:21") & datetime <= dmy_hms("09-10-2024 11:52:47") ~ "Q15",
      datetime >= dmy_hms("09-10-2024 11:52:48") & datetime <= dmy_hms("09-10-2024 11:57:48") ~ "Q20",
      datetime >= dmy_hms("09-10-2024 11:57:49") & datetime <= dmy_hms("09-10-2024 12:02:57") ~ "Q25",
      # C07
      datetime >= dmy_hms("06-11-2024 09:33:59") & datetime <= dmy_hms("06-11-2024 09:43:59") ~ "Q10",
      datetime >= dmy_hms("06-11-2024 09:44:00") & datetime <= dmy_hms("06-11-2024 09:49:14") ~ "Q15",
      datetime >= dmy_hms("06-11-2024 09:49:15") & datetime <= dmy_hms("06-11-2024 09:54:15") ~ "Q20",
      datetime >= dmy_hms("06-11-2024 09:54:16") & datetime <= dmy_hms("06-11-2024 09:59:38") ~ "Q25",
      # C11
      datetime >= dmy_hms("09-10-2024 12:12:09") & datetime <= dmy_hms("09-10-2024 12:22:09") ~ "Q10",
      datetime >= dmy_hms("09-10-2024 12:22:10") & datetime <= dmy_hms("09-10-2024 12:27:29") ~ "Q15",
      datetime >= dmy_hms("09-10-2024 12:27:30") & datetime <= dmy_hms("09-10-2024 12:32:30") ~ "Q20",
      datetime >= dmy_hms("09-10-2024 12:32:31") & datetime <= dmy_hms("09-10-2024 12:37:40") ~ "Q25",
      # L03
      datetime >= dmy_hms("06-11-2024 10:10:22") & datetime <= dmy_hms("06-11-2024 10:20:22") ~ "Q10",
      datetime >= dmy_hms("06-11-2024 10:20:23") & datetime <= dmy_hms("06-11-2024 10:25:36") ~ "Q15",
      datetime >= dmy_hms("06-11-2024 10:25:37") & datetime <= dmy_hms("06-11-2024 10:30:37") ~ "Q20",
      datetime >= dmy_hms("06-11-2024 10:30:38") & datetime <= dmy_hms("06-11-2024 10:35:59") ~ "Q25",
      # L04
      datetime >= dmy_hms("09-10-2024 14:01:54") & datetime <= dmy_hms("09-10-2024 14:11:54") ~ "Q10",
      datetime >= dmy_hms("09-10-2024 14:11:55") & datetime <= dmy_hms("09-10-2024 14:17:29") ~ "Q15",
      datetime >= dmy_hms("09-10-2024 14:17:30") & datetime <= dmy_hms("09-10-2024 14:22:30") ~ "Q20",
      datetime >= dmy_hms("09-10-2024 14:22:31") & datetime <= dmy_hms("09-10-2024 14:27:56") ~ "Q25",
      # L05
      datetime >= dmy_hms("06-11-2024 11:25:30") & datetime <= dmy_hms("06-11-2024 11:35:30") ~ "Q10",
      datetime >= dmy_hms("06-11-2024 11:35:31") & datetime <= dmy_hms("06-11-2024 11:40:40") ~ "Q15",
      datetime >= dmy_hms("06-11-2024 11:40:41") & datetime <= dmy_hms("06-11-2024 11:45:41") ~ "Q20",
      datetime >= dmy_hms("06-11-2024 11:45:42") & datetime <= dmy_hms("06-11-2024 11:50:55") ~ "Q25",
      # L14
      datetime >= dmy_hms("23-10-2024 09:43:55") & datetime <= dmy_hms("23-10-2024 09:53:55") ~ "Q10",
      datetime >= dmy_hms("23-10-2024 09:53:56") & datetime <= dmy_hms("23-10-2024 09:59:11") ~ "Q15",
      datetime >= dmy_hms("23-10-2024 09:59:12") & datetime <= dmy_hms("23-10-2024 10:04:12") ~ "Q20",
      datetime >= dmy_hms("23-10-2024 10:04:13") & datetime <= dmy_hms("23-10-2024 10:09:21") ~ "Q25",
      # L17
      datetime >= dmy_hms("09-10-2024 13:23:35") & datetime <= dmy_hms("09-10-2024 13:33:35") ~ "Q10",
      datetime >= dmy_hms("09-10-2024 13:33:36") & datetime <= dmy_hms("09-10-2024 13:39:07") ~ "Q15",
      datetime >= dmy_hms("09-10-2024 13:39:08") & datetime <= dmy_hms("09-10-2024 13:44:08") ~ "Q20",
      datetime >= dmy_hms("09-10-2024 13:44:09") & datetime <= dmy_hms("09-10-2024 13:49:25") ~ "Q25",
      # L18
      datetime >= dmy_hms("09-10-2024 12:47:55") & datetime <= dmy_hms("09-10-2024 12:57:55") ~ "Q10",
      datetime >= dmy_hms("09-10-2024 12:57:56") & datetime <= dmy_hms("09-10-2024 13:03:19") ~ "Q15",
      datetime >= dmy_hms("09-10-2024 13:03:20") & datetime <= dmy_hms("09-10-2024 13:08:20") ~ "Q20",
      datetime >= dmy_hms("09-10-2024 13:08:21") & datetime <= dmy_hms("09-10-2024 13:13:37") ~ "Q25",
      # L22
      datetime >= dmy_hms("23-10-2024 10:20:28") & datetime <= dmy_hms("23-10-2024 10:30:28") ~ "Q10",
      datetime >= dmy_hms("23-10-2024 10:30:29") & datetime <= dmy_hms("23-10-2024 10:35:51") ~ "Q15",
      datetime >= dmy_hms("23-10-2024 10:35:52") & datetime <= dmy_hms("23-10-2024 10:40:52") ~ "Q20",
      datetime >= dmy_hms("23-10-2024 10:40:53") & datetime <= dmy_hms("23-10-2024 10:46:24") ~ "Q25",
      # L23
      datetime >= dmy_hms("23-10-2024 12:20:47") & datetime <= dmy_hms("23-10-2024 12:30:47") ~ "Q10",
      datetime >= dmy_hms("23-10-2024 12:30:48") & datetime <= dmy_hms("23-10-2024 12:36:59") ~ "Q15",
      datetime >= dmy_hms("23-10-2024 12:37:00") & datetime <= dmy_hms("23-10-2024 12:42:00") ~ "Q20",
      datetime >= dmy_hms("23-10-2024 12:42:01") & datetime <= dmy_hms("23-10-2024 12:47:21") ~ "Q25",
      # L24
      datetime >= dmy_hms("06-11-2024 10:47:47") & datetime <= dmy_hms("06-11-2024 10:57:47") ~ "Q10",
      datetime >= dmy_hms("06-11-2024 10:57:48") & datetime <= dmy_hms("06-11-2024 11:03:03") ~ "Q15",
      datetime >= dmy_hms("06-11-2024 11:03:04") & datetime <= dmy_hms("06-11-2024 11:08:04") ~ "Q20",
      datetime >= dmy_hms("06-11-2024 11:08:05") & datetime <= dmy_hms("06-11-2024 11:13:15") ~ "Q25",
      
      TRUE ~ NA_character_ 
    )
  ) %>%
  drop_na(run) %>% 
  group_by(ID_treat, run, T31) %>%
  mutate(
    t_sec = if_else(
      T31 %in% c("1", "2"),
      as.numeric(difftime(datetime, first(datetime), units = "secs")),
      NA_real_
    )
  ) %>%
  ungroup()


## ATMOSPHERIC PRESSURE ####
atm_Glasgow <- x_GlasgowAtm %>% 
  rename(
    datetime = "Date and time UTC",
    station_kpa = "Station Level Pressure hPa",
    sealevel_kpa = "Mean Sea Level Pressure hPa"
  ) %>% 
  mutate(
    station_kpa = station_kpa * 0.1,
    sealevel_kpa = sealevel_kpa * 0.1,
    datetime = as.POSIXct(datetime, tz = "UTC")
  ) %>%
  select(datetime, station_kpa)

# Rolling Join 
matric <- matric %>%
  left_join(
    atm_Glasgow, 
    by = join_by(closest(datetime >= datetime))
  )


## COMPENSATE MATRIC POTENTIAL ####
matric <- matric %>%
  mutate(matric_comp_kpa = matric_kPa - station_kpa) %>% 
  select(ID_treat, treat, growth_day, run, t_sec, T31, matric_comp_kpa, temp_C) %>% 
  pivot_wider(
    names_from = T31,
    values_from = c(matric_comp_kpa, temp_C)
  ) %>% 
  group_by(ID_treat, treat, growth_day, run, t_sec) %>%
  summarise(
    across(starts_with(c("matric_comp_kpa", "temp_C")), ~ first(na.omit(.x))),
    .groups = "drop"
  ) %>% 
  group_by(ID_treat, treat, growth_day, run) %>%
  arrange(t_sec) %>%
  fill(starts_with(c("matric_comp_kpa", "temp_C")), .direction = "down") %>% 
  ungroup() %>%
  mutate(matric_diff = abs(matric_comp_kpa_1 - matric_comp_kpa_2))


## TEROS31 POSITION ####
x_T31_pos <- x_T31_pos %>% rename(ID_treat = "id")

matric <- matric %>%
  left_join(x_T31_pos, by = "ID_treat")


## GET FLOW AREA ####
area <- t_HueArea %>% 
  select(ID_treat, PosX_cm, ALatFlow_mm) %>% 
  mutate(ALatFlow_m = ALatFlow_mm / 1e6) %>% 
  select(-ALatFlow_mm) %>% 
  filter(PosX_cm %in% c("06", "24")) %>% 
  pivot_wider(
    names_from = PosX_cm,
    values_from = ALatFlow_m,
    names_prefix = "A_"
  )


## GET DISCHARGE ####
discharge <- outflow_RATE %>% 
  select(ID_treat, Q10, Q15, Q20, Q25) %>% 
  pivot_longer(
    cols = starts_with("Q"),
    names_to = "run",
    values_to = "rate_Lh"
  )


# CALCULATE HEAD LOSS ####

# Physical constants
g   <- 9.80665         # m/s^2, gravity
rho <- 998.2           # kg/m^3, water density at ~20°C
L_spec   <- 0.30       # m, total length

# Create table
head_loss <- matric %>% 
  select(-temp_C_1, -temp_C_2, -matric_diff) %>% 
  left_join(discharge, by = c("ID_treat", "run")) %>% 
  left_join(area, by = "ID_treat") %>% 
  rename(Treatment = "treat") %>% 
  mutate(
    x1       = x1      / 1000,           # mm to m
    delta_x  = delta_x / 1000,           # mm to m
    x2       = x2      / 1000,           # mm to m
    rate_m3s = rate_Lh / 1000 / 3600,    # L/h to m^3/s
    p1_Pa    = matric_comp_kpa_1 * 1000, # Pa
    p2_Pa    = matric_comp_kpa_2 * 1000, # Pa
    u1       = rate_m3s / A_06,
    u2       = rate_m3s / A_24,
    # Calculate head loss
    hL       = ((p1_Pa - p2_Pa) / (rho * g)) + ((u1**2 - u2**2) / (2*g)) + (0.02 * L_spec)
  ) %>% 
  select(-rate_Lh, -matric_comp_kpa_1, -matric_comp_kpa_2)

# Save output
save_path <- here("data", "output", "head_loss.csv")
write_csv(head_loss, save_path)


# PLOT HEAD LOSS ####  

## organize time x-axis
head_loss_plot <- head_loss %>%
  mutate(
    t_total_sec = case_when(
      run == "Q10" ~ t_sec,
      run == "Q15" ~ t_sec + 600,
      run == "Q20" ~ t_sec + 900,
      run == "Q25" ~ t_sec + 1200
    ),
    t_total_min = t_total_sec / 60
  ) %>%
  filter(t_total_sec <= 1500)

## organize facet panels
head_loss_panel <- bind_rows(
  head_loss_plot %>% mutate(panel = paste("day", growth_day)),  # day-specific panels
  head_loss_plot %>% mutate(panel = "overall")                  # combined panel
) %>%
  mutate(
    panel = factor(panel, levels = c("day 18", "day 32", "day 46", "overall"))
  )


## theme function (Safely Renamed from `plot` to `create_panel_plot`)
create_panel_plot <- function(data, x, y, group, color, facet_var, y_lab = NULL) {
  
  y_sym   <- rlang::ensym(y)
  y_label <- if (is.null(y_lab)) rlang::as_name(y_sym) else y_lab
  
  ggplot(
    data,
    aes(x = {{ x }}, y = {{ y }}, group = {{ group }}, color = {{ color }})
  ) +
    # reference lines
    geom_hline(yintercept = 0, color = "grey85", linewidth = 0.2, linetype = "dashed") +
    geom_vline(xintercept = c(10, 15, 20), color = "grey85", linewidth = 0.2) +
    
    # lines
    geom_line(aes(alpha = {{ color }}, linewidth = {{ color }})) +
    geom_smooth(
      aes(group = {{ color }}, color = {{ color }}),
      method = "loess",
      span = 0.05,
      se = FALSE,
      linewidth = 0.8
    ) +
    
    # facets
    facet_wrap(
      vars({{ facet_var }}),
      ncol = 1,
      strip.position = "top"
    ) +
    
    # manual scales
    scale_color_manual(values = c("CON" = "#0F1622", "LPD" = "#7E8737")) +
    scale_alpha_manual(values = c("CON" = 0.2, "LPD" = 0.2)) +
    scale_linewidth_manual(values = c("CON" = 0.6, "LPD" = 0.6)) +
    
    scale_x_continuous(
      expand = c(0, 0),
      breaks = seq(15, 25, by = 1), 
      limits = c(15, 25)
    ) +
    scale_y_continuous(
      expand = c(0.005, 0.005),
      breaks = seq(-0.04, 0.08, by = 0.02),
      limits = c(-0.04, 0.08)
    ) +
    
    # labels
    labs(x = "Time (min)", y = y_label) +
    
    # theme
    theme_minimal(base_family = "roboto_condensed") +
    theme(
      strip.text         = element_text(size = 10),
      legend.title       = element_text(size = 10),
      legend.text        = element_text(size = 9),
      legend.position    = "bottom",
      axis.text.x        = element_text(size = 9, angle = 90, hjust = 1, vjust = 0.5),
      axis.text.y        = element_text(size = 9),
      axis.title         = element_text(size = 10),
      panel.grid.major.y = element_line(color = "grey99"),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_line(color = "grey95"),
      panel.grid.minor.x = element_blank(),
      panel.spacing.y    = grid::unit(0.7, "lines"),
      panel.border       = element_rect(fill = NA, color = "grey50", linewidth = 0.4)
    )
}

# GENERATE FINAL PLOT
# (Fixed dataframe reference here to point to head_loss_panel)
head_loss_final_plot <- create_panel_plot(
  data      = head_loss_panel,
  x         = t_total_min,
  y         = hL,
  group     = ID_treat,
  color     = Treatment,
  facet_var = panel,
  y_lab     = expression("Head loss (" * h[L] * ", m)")
)

head_loss_final_plot
