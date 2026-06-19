# Species Distribution Modeling and Data Analysis

## Overview

This repository contains R scripts for constructing species distribution models (SDMs) and performing data analysis related to the scientific article:

> Bedrij, N.A.; Montti, L.; Keller, H.A.; Lopez. L.N.; Velazco, S.J.E. (2026) **"Beyond protected areas: The synergistic role of forest territorial planning in safeguarding tree diversity"**, *Biological Conservation* (in press). https://doi.org/10.1016/j.biocon.2026.111982

## Project Description

This study investigates the effectiveness of forest territorial planning in combination with protected areas for safeguarding tree diversity in Argentina. Using species distribution modeling techniques, we analyze how different conservation strategies contribute to protecting forest biodiversity.

## Contents

- **Data Analysis Scripts**: R scripts for processing and analyzing occurrence and environmental data
- **Species Distribution Models**: Scripts for building and evaluating SDMs using various algorithms
- **Visualization**: Code for generating maps and figures
- **Results**: Outputs and model performance metrics

## Requirements

### R Packages
- `terra` / `sf` - Spatial data manipulation
- `flexsdm` - Species distribution modeling
- `biomod2` - Biomod2 ensemble modeling (optional)
- `ggplot2` - Data visualization
- Additional packages as needed (specified in individual scripts)

### Data
- Species occurrence data
- Environmental predictor layers
- Protected area boundaries
- Forest territorial planning zones

## Usage

1. Clone the repository
2. Install required R packages (see individual script headers)
3. Configure data paths in configuration files
4. Run analysis scripts in order (typically 01_*, 02_*, etc.)
5. Check outputs in the results/ directory

## Authors

- S. Jevela

## Citation

If you use code or results from this repository, please cite:

[Citation details to be added upon publication]

## License

[Specify your preferred license - e.g., MIT, GPL-3.0, CC BY 4.0]

## Contact

For questions or issues, please contact the repository maintainer.

---

*Last updated: June 2026*
