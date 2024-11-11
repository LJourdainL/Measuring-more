# Measuring-more

**Measuring more - the key to a robust signal in ring-porous QWA chronologies**; Appendix - code and ShinyApp tool. This repository provides tools to generate synthetic tree-ring data based on specified climate variables, noise parameters (related to the TW gradient), population size, target Rbt, and target climate correlation. The aim is to evaluate the impact of different sampling strategies on the correlation between tree-ring indices and climate data.

## Getting Started

Clone or download this repository, ensure that all necessary R packages are installed, and open the Shiny App or example document to begin.

## Requirements

-   **R packages**: `shiny`, `tidyverse`, `dplR`, `lattice`, `zoo`, `gridExtra`, `cowplot`, `patchwork`.
-   **Climate Data Format**: A `.txt` file with monthly climate data. Columns represent months, and rows represent years.

## Files in This Repository

### 1. Shiny App - Interactive Simulation Interface

The **Shiny App** provides an interactive graphical user interface (GUI) to run simulations, view climate data, set parameters, and visualize results.

#### How to Use the Shiny App

/App/ShinyApp.R –\> Run App

1.  **Upload Climate Data**:
    -   The app requires a climate data file (in `.txt` format) where rows represent years and columns represent months.
    -   You can upload your climate data file by navigating to the **Data** tab and using the upload option.
2.  **Set Simulation Parameters**:
    -   In the **Param** tab, specify:
        -   Population size, number of trees to sample, and the number of sub-samples per tree.
        -   Target correlation and Rbt (within-sample correlation) values.
        -   Noise level (min and max) and significance threshold (p-value).
3.  **Run the Simulation**:
    -   In the **Simul** tab, click **Run** to start the simulation.
    -   Adjust graphical settings, such as color, Y-axis and X-axis limits, and toggle between viewing graphs and tables.
4.  **View Results**:
    -   Results are displayed in table or graph format.

### 2. Simulation Example Document - "How to Use the Simulation Functions"

/App/How_to_use_simulation_function.R

The **Simulation Example Document** provides an example of how to run the `Climate_cor_simule` function directly in R. This document demonstrates:

-   **Setting Up Climate Data**: Load climate data (e.g., monthly temperature data).
-   **Defining Noise Levels**: Specify a range of noise levels to explore different simulation scenarios.
-   **Running the Simulation**: Call `Climate_cor_simule` with various parameter options, including population size, sampling details, and target correlation values.
-   **Visualizing Output**: Access and visualize the output in tables or customized graphs.

This example provides a template for users who want to explore the simulation functionality directly in R without the GUI.

### 3. Function Documentation - Simulation Functions

/App/Simulation_function.R

The **Function Documentation** file contains the core functions used for the simulation. This includes details of:

-   The main simulation function `Climate_cor_simule`, which performs the correlation and noise simulation, and can generate graphical output.
-   Helper functions for calculating statistical parameters, converting correlation values, and setting up noise levels.
-   Detailed comments within each function provide insight into their workings, enabling users to modify or extend the functions for more customized analyses.

Users interested in the inner workings of the simulation or looking to modify the simulation process should refer to this file.

------------------------------------------------------------------------

This work is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International
