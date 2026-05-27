# 20/10/2025 - LAB EXPERIMENT | WATER FLOW THROUGH LIVE POLE DRAINS
# Author: Fernanda Berlitz 
# E-mail: fernanda.berlitz@gcu.ac.uk

# TASK 07: MODEL OUTFLOW DISCHARGE AND PLOT FLUX DENSITY

# LOAD LIBRARIES ####
library(here)
library(readxl)
library(tidyverse)
library(ggpubr)
library(GGally)
library(patchwork)
library(gridExtra)
library(showtext)  # for custom fonts
library(sysfonts)
library(scales)    # for axis scaling

# Add Roboto Condensed font (automatically downloaded via Google Fonts)
font_add_google("Roboto Condensed", "roboto_condensed")
showtext_auto()

# SOURCE EXTERNAL SCRIPTS ####
source("06_relationship_discharge_area.R")


# DATA PREPARATION ####
continuity <- discharge %>% 
  select(-Q20) %>% 
  left_join(
    flow_area %>% select(ID_treat, treat, growth_day, PosX_cm, ALatFlow_m2), 
    by = c("ID_treat", "treat", "growth_day")
  ) 


# FLUX DENSITY ####

## CALCULATE ####
continuity <- continuity %>% 
  mutate(
    v = Q20_m3_s / ALatFlow_m2
  )

# Optional: save to CSV
write_csv(continuity, here("data", "output", "flux_density.csv"))


## PREPARE DATA FOR PLOTTING ####
continuity$PosX_cm <- as.numeric(continuity$PosX_cm)

continuity <- continuity %>% 
  rename(Treatment = "treat")

f_density <- bind_rows(
  # Day-specific panels
  continuity %>%
    mutate(panel = paste("day", growth_day)),   
  # Combined panel
  continuity %>%
    mutate(panel = "overall")                    
) %>%
  mutate(
    # Lock the order of the facet panels (Standardized capitalization)
    panel = factor(panel, levels = c("day 18", "day 32", "day 46", "overall"))
  )

## PLOT FLUX DENSITY ####
flux_density <- ggplot(f_density,
                       aes(x = PosX_cm, y = v, color = Treatment, group = Treatment)) +
  geom_point(alpha = 0.1) +
  geom_smooth(
    method = "loess",
    span = 0.5,
    se = FALSE,
    linewidth = 0.8
  ) +
  facet_wrap(~ panel, ncol = 1, strip.position = "top") + 
  scale_color_manual(values = c("CON" = "#0F1622", "LPD" = "#7E8737")) +
  scale_x_continuous(expand = c(0.01, 0.01), breaks = c(6, 12, 18, 24)) +
  scale_y_continuous(
    expand = c(0.00015, 0.00015),
    limits = c(0, 0.005),
    breaks = seq(0, 0.005, by = 0.001),
    labels = function(x) x * 1000
  ) +
  guides(color = guide_legend(order = 1)) + 
  labs(
    title = "(b) Flux Density",
    x = "CS (cm)",
    y = expression("Flux Density" ~(v*","~"\u00D7"~10^{-3}~m~s^{-1})),
    color = "Treatment" 
  ) +
  theme_minimal(base_family = "roboto_condensed") +
  theme(
    legend.position = "none", # Hiding legend here assuming patchwork collects it from plot (a)
    plot.title = element_text(size = 10, face = "bold", hjust = 0), 
    plot.title.position = "plot", 
    strip.text = element_text(size = 10),
    axis.text.x = element_text(size = 9, hjust = 0.5, vjust = 1),
    axis.text.y = element_text(size = 9, hjust = 0, vjust = 0.5),
    axis.title = element_text(size = 10),
    panel.grid.major.y = element_line(color = "white"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey99"),
    panel.grid.minor.x = element_blank(),
    panel.spacing.y = unit(0.7, "lines"),
    panel.border = element_rect(fill = NA, color = "grey50", linewidth = 0.4),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 15) 
  )



# OBS vs MOD OUTFLOW DISCHARGE ####

## CALCULATE DAY-SPECIFIC MODEL ####
ALatFlow_betas <- QA_table %>%
  filter(area_metric == "ALatFlow_m2") %>%
  select(treat, growth_day, PosX_cm, beta)

Continuity_day_specific <- QA_models_data %>%
  filter(area_metric == "ALatFlow_m2") %>%
  left_join(ALatFlow_betas, by = c("treat", "growth_day", "PosX_cm")) %>%
  mutate(
    Q_modelled = area_value * beta,
    panel = paste("day", growth_day)
  )

## CALCULATE AGGREGATE MODEL ####
ALatFlow_aggregate_betas <- QA_models_data %>%
  filter(area_metric == "ALatFlow_m2") %>%
  drop_na(Q20_m3_s, area_value) %>% 
  group_by(treat, PosX_cm) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(Q20_m3_s ~ area_value + 0, data = .x)),
    beta = map_dbl(model, ~ coef(.x)[1]) 
  ) %>%
  select(treat, PosX_cm, beta)

Continuity_aggregate <- QA_models_data %>%
  filter(area_metric == "ALatFlow_m2") %>%
  left_join(ALatFlow_aggregate_betas, by = c("treat", "PosX_cm")) %>%
  mutate(
    Q_modelled = area_value * beta,
    panel = "overall"
  )

## COMBINE AND PREPARE FOR GGPLOT ####
Continuity_long_plot <- bind_rows(Continuity_day_specific, Continuity_aggregate) %>%
  mutate(
    xPos_num = as.numeric(PosX_cm),
    Observed = Q20_m3_s * 3.6e6,
    Modelled = Q_modelled * 3.6e6,
    
    # Standardized capitalization to match the Flux Density plot
    panel = factor(panel, levels = c("day 18", "day 32", "day 46", "overall"))
  ) %>%
  pivot_longer(
    cols = c(Observed, Modelled),
    names_to = "Type",
    values_to = "Discharge"
  ) %>%
  mutate(Type = factor(Type, levels = c("Observed", "Modelled")))

# Optional: save to CSV
write_csv(Continuity_long_plot, here("data", "output", "modelled_discharge.csv"))


## PLOT ####
mod_discharge <- ggplot(Continuity_long_plot,
                        aes(x = xPos_num, y = Discharge, color = treat, linetype = Type)) +
  geom_point(aes(color = treat), alpha = 0.1) +
  geom_smooth(
    method = "loess",
    se = FALSE,
    span = 1,
    linewidth = 0.8
  ) +
  facet_wrap(~ panel, ncol = 1, strip.position = "top") +
  scale_color_manual(values = c(
    "CON" = "#0F1622",
    "LPD" = "#7E8737")
  ) +
  scale_linetype_manual(
    values = c(
      "Observed" = "dotted",
      "Modelled" = "solid"
    )
  ) +
  guides(
    color    = guide_legend(order = 1),
    linetype = guide_legend(
      order = 2,
      override.aes = list(color = "grey50")
    )
  ) +
  scale_x_continuous(expand = c(0.01, 0.01), breaks = c(6, 12, 18, 24)) +
  scale_y_continuous(
    expand = c(0.05, 0.05),
    limits = c(0, 5),
    breaks = seq(0, 5, by = 1)
  ) +
  labs(
    title = "(a) Observed and Modelled Outflow Discharge",
    x = "CS (cm)",
    y = expression(Discharge~(L~h^{-1})),
    linetype = "Discharge",
    color = "Treatment"
  ) +
  theme_minimal(base_family = "roboto_condensed") +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0),
    plot.title.position = "plot", 
    strip.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.spacing.y = unit(0, "lines"),
    legend.box.spacing = unit(0.5, "lines"),
    legend.margin = margin(t = 0, b = 0),
    axis.text.x = element_text(size = 9, hjust = 0.5, vjust = 1),
    axis.text.y = element_text(size = 9, hjust = 0.5, vjust = 0.5),
    axis.title = element_text(size = 10),
    panel.grid.major.y = element_line(color = "white"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey99"),
    panel.grid.minor.x = element_blank(),
    panel.spacing.y = unit(0.7, "lines"),
    panel.border = element_rect(fill = NA, color = "grey50", linewidth = 0.4),
    plot.margin = margin(t = 5, r = 15, b = 5, l = 5) 
  )


# COMBINE PLOTS ####

combined_plot <- mod_discharge + flux_density + 
  plot_layout(ncol = 2, guides = "collect") + 
  plot_annotation(
    theme = theme(
      legend.position = "bottom",
      legend.justification = "center",
      legend.box = "vertical",
      legend.spacing.y = unit(0, "cm"), # Squishes the two legends together
      legend.margin = margin(t = 0, b = 0, r = 0, l = 0) # Removes invisible padding
    )
  )

print(combined_plot)





