# Lab Experiment: Water Flow Through Live Pole Drains (LPD)

**Author:** Fernanda Berlitz  
**Contact:** fernanda.berlitz@gcu.ac.uk  
**Date:** October 2025  

## Overview
This repository contains the dataset, images, and analysis scripts for the laboratory experiment investigating lateral subsurface water flow through live pole drains (LPD). 

### Abstract
{ }

## Repository Structure
The project files are logically grouped into the following directories:
```text
├── data_raw/        # Original tables (water flow, plant growth, matric suction) and ImageJ .csv outputs.
├── data_processed/  # Final cleaned tables used to directly produce the manuscript's figures.
├── photos/          # The 117 cross-sectional images captured during the physical experiment.
└── scripts/         # R scripts and ImageJ macros (Tasks 00-10) for the full analytical workflow.
```
Note: The dataset and photos are hosted directly on Zenodo. The codebase/scripts are maintained on GitHub with an automated Zenodo integration for reliable versioning and archival.

## Analytical Workflow and Scripts
The analysis follows a sequential workflow divided into 11 distinct tasks. To reproduce the study's results, execute the files found in the /scripts/ directory in the numbered order below:

#### TASK 00: SELECT BASIC INFO OF SPECIMENS USED DURING WATER TEST FLOW WITH DYE
File: 00_basicInfoSpecimens.R

#### TASK 01: CALCULATE OUTFLOW DISCHARGE RATE, SOIL-WATER BALANCE, AND POROSITY
File: 01_outflow_lumped_swmb.R

#### TASK 02: CLASSIFY HUE & MEASURE HUE AREA THROUGH IMAGE ANALYSIS
File: 02_hue_class.ijm

#### TASK 03: MEASURE CUTTINGS & BUNDLE AREA, AND VOIDS AREA WITHIN BUNDLE THROUGH IMAGE ANALYSIS
File: 03_bundle_voids_area.ijm

#### TASK 04: PROCESS CROSS-SECTIONAL AREAS & CALCULATE FLOW AREA
File: 04_hue_areas.R

#### TASK 05: CALCULATE PLANT DEVELOPMENT
File: 05_plant_growth.R

#### TASK 06: RELATIONSHIP DISCHARGE AND FLOW AREA
File: 06_relationship_discharge_area.R

#### TASK 07: MODEL OUTFLOW DISCHARGE AND PLOT FLUX DENSITY
File: 07_mod_discharge_density.R

#### TASK 08: CALCULATE HEAD LOSS
File: 08_head_loss.R

#### TASK 09: CLASSIFY HUE WITH DIFFERENT THRESHOLDS FOR SENSITIVITY ANALYSIS
File: 09_hue_sensitivity_analysis.ijm

#### TASK 10: SENSITIVITY ANALYSIS
File: 10_sensitivity_analysis.R

## Software Requirements
To run the analysis successfully, you will need:
- R (and RStudio): Required for executing all .R scripts.
- ImageJ / FIJI: Required for running the .ijm macro files for image processing.

## License & Citation
If using this data or code, please cite the original publication and the Zenodo DOI associated with this repository [https://doi.org/10.5281/zenodo.20416047]
