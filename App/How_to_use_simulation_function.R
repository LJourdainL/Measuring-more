###########how to use the random simualtion functions###############
library(dplR)
library(tidyverse)
library(lattice)
library(zoo)
library(gridExtra)
library(cowplot)
library(patchwork)

noise <- c(0.96, 0.82, 0.714, 0.65, 0.57, 0.53, 0.49, 0.45, 0.41, 0.37, 0.33, 0.30, 0.27, 0.24, 0.21, 0.20, 0.17, 0.15, 0.13)

climate_data <- read.table("App/Data/Ash_Tmean.txt") -> test

names(climate_data) <- month.abb
#write.table(climate_data,"data/Climate data/ash_Tmean_1950.txt")

Output <- Climate_cor_simule(
  climate_data,
  noise = noise,
  pop_size = 1000,
  N_tree_sample = c(5,10,20,30),
  driver_season = 3:4,
  analysis_season = 3:4,
  target_cor = 0.54,
  #analysis_cor = 0.20,
  #uncertainty = 0.015,
  target_rbt = 0.39,
  rep_sub_core = 100,
  rep_sub_pop = 1000,
  graph = TRUE,
  color1 = "green",
  color2 = "white",
  ylim = c(0, 1),
  xlim = c(0, 1),
  p_value = 0.01
)

Output$graph$`_5_tree`
Output$graph$`_10_tree`
Output$graph$`_20_tree`
Output$truth_rbt
Output$truth_cor
Output$signific_cor
