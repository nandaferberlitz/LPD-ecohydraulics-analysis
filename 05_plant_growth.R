# 20/10/2025 - LAB EXPERIMENT | WATER FLOW THROUGH LIVE POLE DRAINS
# Author: Fernanda Berlitz 
# E-mail: fernanda.berlitz@gcu.ac.uk

# TASK 05: CALCULATE PLANT DEVELOPMENT

### LOAD ALL LIBRARIES ####
library(here)
library(readxl)
library(tidyverse) 
library(ggpubr)
library(lubridate)
library(showtext)  # For custom fonts
library(sysfonts)
library(scales)    # For log10 axis labels
library(ggrepel)   # For repelling overlapping text
library(splines)
library(patchwork) # For combining plots
library(GGally)    # For global pairs plot
library(Hmisc)     # For correlation matrix
library(broom)     # For tidying linear models

# Setup custom fonts
font_add_google("Roboto Condensed", "roboto_condensed")
showtext_auto()

source("04_hue_areas.R")


### GET FILES FROM 2 DIFFERENT SOURCES ####

# Source 1: Biomass data
p_biomass <- here("data", "input", "plant_biomass.xlsx")
f_biomass <- read_xlsx(p_biomass, sheet = "biomass")

# Source 2: Bundle and Cuttings Area (extracted from t_HueArea)
bundle <- t_HueArea %>% 
  select(ID_treat, treat, growth_day, PosX_cm, A_Bundle_mm, A_Cuttings_mm)


### CALCULATE DRY BIOMASS ####
dry_biomass <- f_biomass %>%
  mutate(
    leaf_Wf_g = leaves_dry_g - leaves_et_g,
    stem_Ws_g = (stem_dry_g + inn_stem_dry_g) - (stem_et_g + inn_stem_et_g),
    root_Wr_g = roots_dry_g - roots_et_g,
    abv_Wa_g  = leaf_Wf_g + stem_Ws_g,
    cutt_Wc_g = cutt_dry_g - et_cutt_g
  ) %>%
  filter(
    k == "SAT",
    growth_day %in% c(18, 32, 46) 
  ) %>% 
  select(
    ID_treat = id, growth_day, root_Wr_g, cutt_Wc_g, abv_Wa_g
  )

  # Define the file path using 'here' (saving it back to the "data" folder)
  save_path <- here("data", "output", "dry_biomass.csv")
  write_csv(dry_biomass, save_path)

  
  
  
### CALCULATE BUNDLE AREA SUMMARY ####
bundle_summary <- bundle %>% 
  filter(treat == "LPD") %>% 
  left_join(
    dry_biomass %>% select(ID_treat, cutt_Wc_g), 
    by = "ID_treat"
  ) %>%
  group_by(growth_day) %>%
  summarise(
    across(
      c(A_Bundle_mm, A_Cuttings_mm, cutt_Wc_g),
      list(
        mean = ~round(mean(.x, na.rm = TRUE), 3),
        sd = ~round(sd(.x, na.rm = TRUE), 3)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

  
# Define the file path using 'here' and save
save_path <- here("data", "output", "bundle_area.csv")
write_csv(bundle_summary, save_path)



### ROOT N SHOOT OVER TIME ####

# 1. Calculate R-squared values
lm_roots  <- lm(root_Wr_g ~ growth_day, data = dry_biomass)
lm_shoots <- lm(abv_Wa_g ~ growth_day, data = dry_biomass)

r2_roots  <- signif(summary(lm_roots)$r.squared, 3)
r2_shoots <- signif(summary(lm_shoots)$r.squared, 3)

# 2. Reshape and Create Dynamic Labels for the Legend
df_mass <- dry_biomass %>%
  pivot_longer(
    cols = c(root_Wr_g, abv_Wa_g),
    names_to = "LPD",
    values_to = "Mass_g"
  ) %>%
  mutate(LPD = case_when(
    LPD == "root_Wr_g" ~ paste0("Roots (R² = ", r2_roots, ")"),
    LPD == "abv_Wa_g"  ~ paste0("Shoots (R² = ", r2_shoots, ")")
  ))

# 3. Dynamic Color Mapping
labels_unique <- unique(df_mass$LPD)
root_label    <- grep("Roots", labels_unique, value = TRUE)
shoot_label   <- grep("Shoots", labels_unique, value = TRUE)
custom_colors <- setNames(c("#FCC6E9", "#7E8737"), c(root_label, shoot_label))

# 4. Create Plot A
plot_mass <- ggplot(df_mass, aes(x = growth_day, y = Mass_g, color = LPD, fill = LPD)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 0.8) +
  geom_point(alpha = 0.8, size = 2) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  scale_x_continuous(
    limits = c(18, 46),
    breaks = c(18, 32, 46),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    limits = c(0, 3.5),
    breaks = seq(0, 3.5, by = 0.5),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(
    x = "Day",
    y = "Dry biomass (g)",
    title = "(a) LPD Roots and Shoots Biomass Growth"
  ) +
  theme_minimal(base_family = "roboto_condensed") +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 10),
    legend.position = c(0.02, 0.98),  # Pinned to Top-Left
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "grey95", linewidth = 0.2),
    legend.key.size = unit(0.3, "cm"),
    legend.spacing.y = unit(0.5, "cm"),
    legend.margin = margin(t = 2, r = 4, b = 2, l = 4, unit = "pt"), 
    legend.text = element_text(size = 9),
    legend.title = element_blank(),
    panel.grid.major.y = element_line(color = "white"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey99"),
    panel.grid.minor.x = element_blank(),
    panel.border = element_rect(fill = NA, color = "grey50", linewidth = 0.4)
  )


### ALLOMETRY ROOT_SHOOT ####

lm_allo <- lm(log10(abv_Wa_g) ~ log10(root_Wr_g), data = dry_biomass)
r2_allo <- summary(lm_allo)$r.squared

plot_allometry <- ggplot(dry_biomass, aes(x = root_Wr_g, y = abv_Wa_g)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 0.8, color = "#0F1622") +
  geom_point(alpha = 0.8, size = 2, color = "#7E8737") +
  
  # Positioned at Top-Left to match Plot A
  annotate("text", 
           x = min(dry_biomass$root_Wr_g, na.rm = TRUE), 
           y = max(dry_biomass$abv_Wa_g, na.rm = TRUE), 
           label = paste0("R^2 == ", signif(r2_allo, 3)), 
           parse = TRUE, hjust = 0, vjust = 1, size = 3.5) +
  
  labs(
    x = expression(paste("Root dry biomass ", W[r], ~"(g)")),
    y = expression(paste("Shoot dry biomass ", W[s], ~"(g)")),
    title = "(b) LPD Root-Shoot Allometry"
  ) +
  annotation_logticks(sides = "bl") +
  scale_x_log10(labels = scales::label_number()) +
  scale_y_log10(labels = scales::label_number()) +
  theme_minimal(base_family = "roboto_condensed") +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 10),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "white"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey99"),
    panel.grid.minor.x = element_blank(),
    panel.border = element_rect(fill = NA, color = "grey50", linewidth = 0.4)
  )

### COMBINE PRINT 3 PLANT-DEVELOPMENT PLOTS TOGETHER #### 

combined_plot <- plot_mass + plot_allometry + plot_layout(ncol = 2)
print(combined_plot)


### STATS ####

# Ensure the time variable is numeric to test linear trends over time
dry_biomass$growth_day <- as.numeric(dry_biomass$growth_day)

# PART 1: VISUAL EXPLORATION 

# FIX: Average bundle data by ID_treat BEFORE joining to prevent data duplication
bundle_per_treat <- bundle %>%
  group_by(ID_treat) %>%
  summarise(
    A_Bundle_mm = mean(A_Bundle_mm, na.rm = TRUE),
    A_Cuttings_mm = mean(A_Cuttings_mm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  drop_na() # This removes any rows that resulted in NA or NaN

# Select only the meaningful continuous variables and time for the plot
df_explore <- dry_biomass %>% 
  left_join(bundle_per_treat, by = "ID_treat") %>% 
  select(growth_day, root_Wr_g, cutt_Wc_g, abv_Wa_g, A_Bundle_mm, A_Cuttings_mm)

# Create a pairs plot
global_plot <- ggpairs(
  df_explore, 
  lower = list(continuous = wrap("smooth", alpha = 0.4, color = "#0F1622", size = 1)),
  diag = list(continuous = wrap("densityDiag", fill = "#7E8737", alpha = 0.5)),
  title = "Global Variable Relationships & Time Trends"
) +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank())

print(global_plot)





# PART 2: STATISTICAL CORRELATION MATRIX 

# Isolate just the physical plant metrics (excluding growth_day for this step)
num_vars <- df_explore %>% 
  select(-growth_day)

# Calculate correlations (R) and p-values
cor_results <- rcorr(as.matrix(num_vars))

# Helper function to flatten the matrix into a readable list
flatten_cormat <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    Variable_1 = rownames(cormat)[row(cormat)[ut]],
    Variable_2 = rownames(cormat)[col(cormat)[ut]],
    Correlation = round(cormat[ut], 3),
    P_value = signif(pmat[ut], 3)
  )
}

# Flatten results and filter for ONLY statistically significant relationships (p < 0.10)
significant_correlations <- flatten_cormat(cor_results$r, cor_results$P) %>%
  filter(P_value < 0.10) %>%
  arrange(desc(abs(Correlation))) 

cat("\n======================================================\n")
cat("SUMMARY 1: SIGNIFICANT CORRELATIONS (p < 0.10)\n")
cat("======================================================\n")
print(significant_correlations)

# Variable_1    Variable_2 Correlation  P_value
# 1   root_Wr_g      abv_Wa_g       0.994 6.37e-08
# 2 A_Bundle_mm A_Cuttings_mm       0.838 4.81e-03




# PART 3: EFFECT OF TIME (Linear Regression Models)

# Create an empty list to store our significant findings
model_findings <- list()
variables_to_test <- names(num_vars)

for(var in variables_to_test) {
  # Build a linear model testing if Growth Day affects the variable
  formula <- as.formula(paste(var, "~ growth_day"))
  model <- lm(formula, data = df_explore)
  
  # Run a summary to get the p-values for the slope
  model_summary <- tidy(model)
  
  # Filter for the 'growth_day' term and check if it's significant (p < 0.10)
  time_effect <- model_summary %>% 
    filter(term == "growth_day", p.value < 0.10)
  
  if(nrow(time_effect) > 0) {
    model_findings[[var]] <- time_effect %>% select(estimate, statistic, p.value)
  }
}

cat("\n======================================================\n")
cat("SUMMARY 2: SIGNIFICANT DRIVERS OF CHANGE OVER TIME\n")
cat("======================================================\n")

if(length(model_findings) > 0) {
  for(var in names(model_findings)) {
    cat(paste("\nMetric:", var, "changes over time.\n"))
    print(model_findings[[var]])
  }
} else {
  cat("\nNo variables showed a statistically significant linear change over time.\n")
}
# 
# Metric: root_Wr_g changes over time.
# # A tibble: 1 × 3
# estimate statistic   p.value
# <dbl>     <dbl>     <dbl>
#   1   0.0394      9.82 0.0000242
# 
# Metric: abv_Wa_g changes over time.
# # A tibble: 1 × 3
# estimate statistic   p.value
# <dbl>     <dbl>     <dbl>
#   1    0.074      8.06 0.0000871
