# Species Distribution Modeling and Data Analysis

## Overview

This repository contains R scripts for constructing species distribution models (SDMs) and performing data analysis related to the scientific article:

> Bedrij, N.A.; Montti, L.; Keller, H.A.; Lopez. L.N.; Velazco, S.J.E. (2026). **Beyond protected areas: The synergistic role of forest territorial planning in safeguarding tree diversity**, *Biological Conservation* (in press). [https://doi.org/10.1016/j.biocon.2026.111982](https://doi.org/10.1016/j.biocon.2026.111982)

## Project Description

This study investigates the effectiveness of forest territorial planning in combination with protected areas for safeguarding tree diversity in Argentina. Using species distribution modeling techniques, we analyze how different conservation strategies contribute to protecting forest biodiversity.

## Contents

### 1-Models

Scripts for building and evaluating species distribution models (SDMs) using the `flexsdm` framework.

| Script | Description |
|---|---|
| `1-Premodeling.R` | Sets up the directory structure, cleans occurrence records, defines calibration areas, and prepares environmental predictors |
| `2.1-Modeling_tuned_SDMs.R` | Fits and projects tuned SDMs across multiple algorithms (GAM, GBM, GLM, MaxEnt, Random Forest, SVM, etc.) for species with > 15 occurrence records |
| `2.2-Ensemble_of_small_models.R` | Builds ensemble of small models for species with between 5 and 15 occurrence records |
| `2.3-Environmental_similarity.R` | Estimates environmental distance-based suitability for species with between 3-5 occurrence records or species with low model performance|
| `2.4-Models_performance.R` | Compiles and summarizes model performance metrics (e.g., Sørensen index) across all species and algorithms |
| `3-Postmodeling.R` | Post-processes model outputs, including overprediction correction |

### 2-Analysis

Scripts for spatial analysis of SDM outputs in relation to land use, conservation areas, and biodiversity patterns in Argentina.

| Script | Description |
|---|---|
| `1_Land use 2025.R` | Processes MapBiomas land use data (1985 and 2024) to generate binary rasters of natural wooded vegetation, shrublands, and forest cover at 1 km resolution |
| `2_Protected areas and FTP.R` | Integrates and processes Argentinean protected area layers and Forest Territorial Planning (FTP) polygons |
| `3_Analysis.R` | Main analysis script combining SDM outputs, land use, and conservation layers to assess the role of protected areas and FTP in safeguarding tree diversity (i.e., species richness patterns and species distribution) |

## Contact

For questions or issues, please contact Santiago J.E. Velazco (sjevelazco@gmail.com).

---

*Last updated: June 2026*
