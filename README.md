# Nonparametric Statistics Project: School Scores in Bogota

## Overview
This project focuses on analyzing school scores in Bogota, Colombia, using nonparametric statistical methods. The dataset includes information about school performance over multiple years, as well as geographic data about the areas where the schools are located. Additionally, socio-economic data about the students is also incorporated for a comprehensive analysis.


## Data Sources
- The primary datasets used in this project are:
  - `colegios.csv`: Provides additional details about the schools, such as their location and identification codes (upz code).
  - `poblacion-upz-bogota.geojson`: Geographic data representing the areas (UPZs) in Bogota.
  - `data_2019.csv`: School performance data from 2019.
  - `data_2020.csv`: School performance data from 2020.
  - `data_2021.csv`: School performance data from 2021
  - `data_2022.csv`: School performance data from 2022.
  - `data_students.csv`: Dataset containing scores and socioeconomic informations for each student
  - `poblacion-upz-bogota.csv`: provides additional data on the density of different areas (UPZs).
  - `tabrobacionofupz.csv`: Pass rates in 2021
  - `tdesercionofupz.csv`: Fail rates in 2021

- Links

  [Dataset scores and map](https://datosabiertos.bogota.gov.co/dataset/resultados-pruebas-saber-11-bogota-d-c)

  [Dataset fail and pass rates](https://datosabiertos.bogota.gov.co/en/dataset/tasa-de-desercion-escolar-en-colegios-oficiales-por-upz-bogota-d-c)

  [Dataset upz](https://datosabiertos.bogota.gov.co/dataset/colegios-bogota-d-c)


## Data used
- The datasets used in the analysis, obtained after pre-processing.
  - `Extended_data_clean.csv`

## Tools and Libraries
- R programming language
- Required R libraries:
  - `splines`: Used for spline interpolation.
  - `ggplot2`: Utilized for data visualization.
  - `sf`: For working with spatial data.
  - `ggpubr`: Helpful for arranging and customizing ggplot2 plots.
  - `RColorBrewer`: Provides color palettes for data visualization.
  - `ISLR2`: Implements functions and datasets from the book "An Introduction to Statistical Learning".
  - `car`: Offers various regression diagnostics and other statistical tools.
  - `mgcv`: Used for generalized additive models.
  - `rgl`: Provides interactive 3D visualization.
  - `knitr`: Enables dynamic report generation.
  - `kableExtra`: Enhances tabular representation in R Markdown documents.
  - `tidyr`: Facilitates data tidying operations.
  - `dplyr`: Essential for data manipulation tasks.
  - `broom`: Converts statistical analysis objects into tidy data frames.
  - `pbapply`: Parallel version of the base apply function.
  - `dbscan`: Implements density-based spatial clustering of applications with noise.
  - `gridExtra`: Assists in arranging multiple grid-based plots.
  - `conformalInference`: Implements conformal inference methods.
  - `MASS`: Contains functions and datasets for Venables and Ripley's book "Modern Applied Statistics with S".
  - `DepthProc`: Provides depth-based multivariate analysis.
  - `hexbin`: Offers hexagonal binning functionality.
  - `robustbase`: Implements robust statistics methods.
  - `MDBED`: Implements the Multivariate Distance-Based Extension of the Bivariate Energy Statistics.
  - `parallel`: Provides support for parallel computation.
  - `spdep`: Implements spatial dependence models.
  - `sfdep`: Computes spatially filtered dependence.
  - `readr`: Simplifies data import from flat files.
  - `sp`: Provides classes and methods for spatial data.
  - `npsp`: Implements non-parametric statistical tests. To be installed from Github:
```R
install.packages('https://github.com/rubenfcasal/npsp/releases/download/v0.7-10/npsp_0.7-10.zip',repos = NULL)
```



## Project Workflow
1. **Data Preprocessing**: 
   - Read and preprocess the datasets, including merging and cleaning the data.

2. **Exploratory Data Analysis (EDA)**:
   - Analyze the distribution of school scores between public and private
   - Analyze the differences between small and big schools

3. **Spatial Analysis**:
   - Apply nonparametric statistical methods to explore relationships between school scores and school location.
   - Identify worse performing zones
   - Conduct hotspot analysis to find clusters.

4. **Spatial Analysis**:
   - Apply nonparametric statistical methods to explore relationships between school scores and school location.
   - Identify worse performing zones
   - Conduct hotspot analysis to find clusters.

5. **Predictive Analysis**:
   - Create visualizations to communicate insights from the analysis.
   - Interpret the results and provide recommendations for educational policymakers based on the findings.

6. **Conclusions**


## How to Run
1. Clone this repository to your local machine.
2. Ensure you have R and the required libraries installed.
3. Run each R script independently 

## Authors
@matteongr
@trama00
@sleival
@Trif02

