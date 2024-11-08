########### How to Use the Simulation Functions ################################

# Load necessary libraries for data manipulation, simulations, and plotting

library(dplR)
library(tidyverse)
library(lattice)
library(zoo)
library(gridExtra)
library(cowplot)
library(patchwork)
source("App/Simulation_function.R")

## Detrended Climate Data
# Load detrended climate data to be used as input for the simulation.
climate_data <- read.table("App/Data/Ash_Tmean.txt") -> test

## Noise Levels
# Define a range of noise levels to add variability in the simulations.
noise <- c(0.96, 0.82, 0.714, 0.65, 0.57, 0.53, 0.49, 0.45, 0.41, 0.37, 0.33, 0.30, 0.27, 0.24, 0.21, 0.20, 0.17, 0.15, 0.13)

## Run Simulation Function

# Run the Climate_cor_simule function with specified parameters, such as climate data, noise levels, population size, target correlation, and others.
# The function simulates correlations and Rbt values and can generate graphs based on these simulations.
Output <- Climate_cor_simule(
  climate_data,
  noise = noise,
  pop_size = 1000,
  N_tree_sample = c(5, 10, 20, 30),
  driver_season = 3:4, # March-April
  analysis_season = 3:4, # March-April
  target_cor = 0.54,
  target_rbt = 0.39,
  rep_sub_core = 100,
  rep_sub_pop = 100,
  graph = TRUE,
  color1 = "green",
  color2 = "white",
  ylim = c(0, 1),
  xlim = c(0, 1),
  p_value = 0.01
)

## Visualizing the Output

# Access and view the data, individual plots for specific sample sizes, and calculated statistical results from the simulation.
Output$Ouput              # Data frame with correlation and Rbt values.
Output$graph$`_5_tree`    # Plot for the 5-tree sample.
Output$graph$`_10_tree`   # Plot for the 10-tree sample.
Output$graph$`_20_tree`   # Plot for the 20-tree sample.
Output$truth_rbt          # Calculated truth Rbt.
Output$truth_cor          # Calculated truth correlation.
Output$signific_cor       # Correlation significance threshold.

## Personalizing Graphs

### Personalize a Single Graph

# Use simule_graph_uniq to customize a single plot for a specific sample size (e.g., 5 trees).
# Allows customization of colors, text size, and axis limits.

Output$Ouput %>% filter(N_tree == 5) %>%
simule_graph_uniq(color1 = "cyan",
                  color2 = "pink",
                  text_size = 15,
                  ax_text_size = 11,
                  ylim = c(0,0.7),
                  xlim = c(0,0.7),
                  truth_cor = Output$truth_cor,
                  signific_cor = Output$signific_cor,
                  linewidth = 0.8,
                  point_size = 3
                  )

### Personalize Multiple Graphs

# Use simule_graph_multi to create customized plots for multiple sample sizes and display them in a grid layout.
# Adjust color gradients, text size, axis limits, and layout options for each plot.
Multi <- Output$Ouput %>% simule_graph_multi(color1 = "brown",
                                    color2 = "beige",
                                    text_size = 10,
                                    ax_text_size = 8,
                                    ylim = c(0,0.7),
                                    xlim = c(0,0.7),
                                    truth_cor = Output$truth_cor,
                                    signific_cor = Output$signific_cor,
                                    linewidth = 0.8,
                                    point_size = 3
                                    )

Multi$`_5_tree`/ Multi$`_20_tree` | Multi$`_10_tree`/Multi$`_30_tree`

## Tool_graph
# Use Tool_graph to generate a summary plot for the entire simulation output
Tool_graph(Output$Ouput, text_size = 10)
