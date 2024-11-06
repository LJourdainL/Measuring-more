###########how to use the random simualtion functions###############
library(dplR)
library(tidyverse)
library(lattice)
library(zoo)
library(gridExtra)
library(cowplot)
library(patchwork)

noise <- c(0.96, 0.82, 0.714, 0.65, 0.57, 0.53, 0.49, 0.45, 0.41, 0.37, 0.33, 0.30, 0.27, 0.24, 0.21, 0.20, 0.17, 0.15, 0.13)
climate_data <- detrend(
  read.table("data/climate data/Ash_EOBS_Tmean")[, -1] + 100,
  method = "Spline",
  nyrs = 30,
  difference = T
)
climate_data <- data.frame(climate_data[31:104, ])
names(climate_data) <- month.abb
#write.table(climate_data,"data/Climate data/ash_Tmean_1950.txt")

Output <- Climate_cor_simule2(
  climate_data,
  noise = noise,
  pop_size = 1000,
  N_tree_sample = c(5,10,20,30),
  driver_season = 3:4,
  analysis_season = 3:4,
  target_cor = 0.54,
  #analysis_cor = 0.20,
  #uncertainty = 0.015,
  target_rbt = 0.6,
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

Output_simul_replic <- Output


Output$graph$`_5_tree`-> tree5
Output$graph$`_10_tree`-> tree10
Output$graph$`_20_tree`-> tree20

Legend <- get_legend(tree5)
layout <- matrix(c(1,1,1,2,2,2,3,3,3,4), ncol = 10, byrow = T )
tree5[["theme"]][["legend.position"]] <- "none"
tree10[["theme"]][["legend.position"]] <- "none"
tree20[["theme"]][["legend.position"]] <- "none"
tree10[["labels"]][["y"]] <- ""
tree20[["labels"]][["y"]] <- ""

jpeg("Data/Output_Fig/Climate_Tmean_simul_hig_0.6.jpg", width = 1600, height = 800)

simul_cor <- grid.arrange(tree5,tree10,tree20,Legend, layout_matrix = layout)

dev.off()


save(Output,file = "Data/data_for_plot/simulation_hig_0.6")

load("Data/data_for_plot/simulation_low_0.2")

##########################################################################
library(ggplot2)
library(reshape2)
library(dplyr)

Ntree_values <- c(5, 10, 20, 30)

#replicat
RbarTot_ranges <- list(
  #"0-0.1" = c(0, 0.1),
  "0-0.2" = c(0, 0.2),
  "0.2-0.3" = c(0.2, 0.3),
  "0.3-0.35" = c(0.3, 0.35),
  "0.35-0.4" = c(0.35, 0.4),
  "0.4-0.6" = c(0.4, 0.6)
)

#low
RbarTot_ranges <- list(
  "0-0.1" = c(0, 0.1),
  "0.1-0.2" = c(0.1, 0.2),
  "0.2-0.3" = c(0.2, 0.3)
)

#low detail
RbarTot_ranges <- list(
  "0-0.1" = c(0, 0.1),
  "0.1-0.15" = c(0.1, 0.15),
  "0.15-0.2" = c(0.15, 0.2),
  "0.2-0.25" = c(0.2, 0.25),
  "0.25-0.3" = c(0.2, 0.3)
)

#med
RbarTot_ranges <- list(
  "0-0.2" = c(0, 0.2),
  "0.2-0.4" = c(0.2, 0.4),
  "0.4-0.6" = c(0.4, 0.6)
)

#high
RbarTot_ranges <- list(
  "0.2-0.3" = c(0.2, 0.3),
  "0.3-0.5" = c(0.3, 0.4),
  "0.5-0.7" = c(0.5, 0.7)
)
#high detail
RbarTot_ranges <- list(
  "0.2-0.3" = c(0.2, 0.3),
  "0.3-0.4" = c(0.3, 0.4),
  "0.4-0.5" = c(0.4, 0.5),
  "0.5-0.6" = c(0.5, 0.6),
  "0.6-0.7" = c(0.6, 0.7)
)
# Define a function to filter the data
filter_data <- function(data, Ntree_value, RbarTot_min, RbarTot_max) {
  data %>%
    subset(N_tree == Ntree_value) %>%
    subset(rbt >= RbarTot_min & rbt < RbarTot_max)
}

# Initialize an empty list to store results
filtered_data <- list()

# Loop through Ntree values and RbarTot ranges to filter data
for (Ntree_value in Ntree_values) {
  for (range_name in names(RbarTot_ranges)) {
    RbarTot_min <- RbarTot_ranges[[range_name]][1]
    RbarTot_max <- RbarTot_ranges[[range_name]][2]

    # Filter the data
    filtered_data[[paste("ash", Ntree_value, range_name, sep = "_")]] <-
      filter_data(Output$Ouput, Ntree_value, RbarTot_min, RbarTot_max)
  }
}

# Create the final table dynamically based on Ntree_values and RbarTot_ranges
table_tool <- data.frame(
  Trees = Ntree_values
)

# Add columns to the table for each RbarTot range
for (range_name in names(RbarTot_ranges)) {
  table_tool[[paste(range_name, sep = "")]] <- sapply(Ntree_values, function(N) {
    data <- filtered_data[[paste("ash", N, range_name, sep = "_")]]
    quantile(data$Cor, 0.975) - quantile(data$Cor, 0.025)
  })
}

# Display the table
table_tool

data_long <- melt(table_tool, id.vars = "Trees", variable.name = "RbtType", value.name = "Value")

data_long %>% mutate(Rbt = c(rep(0.10,4),
                             rep(0.25,4),
                             rep(0.325,4),
                             rep(0.375,4),
                             rep(0.5,4)
                             )) %>% mutate(EPS= rbar_to_eps(Rbt,Trees)) -> data_long

data_long %>% mutate(Rbt = c(
  rep(0.05,4),
  rep(0.15,4),
  rep(0.25,4)
)) %>% mutate(EPS= rbar_to_eps(Rbt,Trees)) -> data_long

data_long %>% mutate(Rbt = c(
  rep(0.05,4),
  rep(0.125,4),
  rep(0.175,4),
  rep(0.225,4),
  rep(0.275,4)
)) %>% mutate(EPS= rbar_to_eps(Rbt,Trees)) -> data_long

data_long %>% mutate(Rbt = c(rep(0.10,4),
                             rep(0.30,4),
                             rep(0.50,4)
)) %>% mutate(EPS= rbar_to_eps(Rbt,Trees)) -> data_long

data_long %>% mutate(Rbt = c(rep(0.25,4),
                             rep(0.40,4),
                             rep(0.60,4)
)) %>% mutate(EPS= rbar_to_eps(Rbt,Trees)) -> data_long

data_long %>% mutate(Rbt = c(rep(0.25,4),
                             rep(0.35,4),
                             rep(0.45,4),
                             rep(0.55,4),
                             rep(0.65,4)
)) %>% mutate(EPS= rbar_to_eps(Rbt,Trees)) -> data_long

#data_long$Trees <- factor(data_long$Trees, levels = c(5, 10, 15, 20))
data_long$Trees <- factor(data_long$Trees, levels = c(5, 10, 20, 30))


(graph_tool_simul_low_0.2_detail <- ggplot(data_long, aes(x = RbtType, y = Trees, fill = Value)) +
  geom_tile(color = "white",linewidth = 2) +  # White border for clarity
    scale_fill_stepsn(colors = c("#053061",
                                 "#2166ac",
                                 "#4393c3",
                                 "#92c5de",
                                 "#f7f7f7",
                                 "#f4a582",
                                 "#d6604d",
                                 "#b2182b",
                                 "#67001f"),
                      breaks = seq(0,0.4,0.04),
                      limits = c(0, 0.4) ) +
  scale_x_discrete(labels = c("Rbt0.2" = "0-0.3", "Rbt0.4" = "0.3-0.35", "Rbt0.6" = "0.35-0.6"))+
  labs(x = "Rbt", y = "Number of Trees", fill = "Cor spread", title = "[B]") +  # Labels
  geom_text(aes(label = round(EPS, 2)), color = "black", size = 3)+
  theme_minimal())




ggsave(filename = "Data/output_fig/garph_tool_simulation_low_0.2_detail.jpg",plot = graph_tool_simul,dpi = 1500)

simulation_graph <-  graph_tool+graph_tool_simul

ggsave(filename = "Data/output_fig/simulation_graph_replicat2.jpg",plot = simulation_graph,dpi = 1500, width = 8,height =4 )

##### saving

graph_tool_simul_low_0.2[["theme"]][["legend.position"]] <- "none"
graph_tool_simul_med_0.4[["theme"]][["legend.position"]] <- "none"
graph_tool_simul_hig_0.6[["theme"]][["legend.position"]] <- "none"

graph_tool_simul_low_0.2[["labels"]][["title"]] <- "[A]"
graph_tool_simul_hig_0.6[["labels"]][["title"]] <- "[C]"

simulation_suplementary <-  graph_tool_simul_low_0.2 + graph_tool_simul_med_0.4 + graph_tool_simul_hig_0.6 + legendd + plot_layout(ncol = 2)
ggsave(filename = "Data/output_fig/simulation_suplementary_0.2_0.4_0.6.jpg",plot = simulation_suplementary,dpi = 1500, width = 6,height =6 )


graph_tool_simul_low_0.2_detail[["theme"]][["legend.position"]] <- "none"
graph_tool_simul_med_0.4_detail[["theme"]][["legend.position"]] <- "none"
graph_tool_simul_hig_0.6_detail[["theme"]][["legend.position"]] <- "none"

graph_tool_simul_low_0.2_detail[["labels"]][["title"]] <- "[A]"
graph_tool_simul_hig_0.6_detail[["labels"]][["title"]] <- "[C]"
get_legend(graph_tool_simul_hig_0.6_detail) -> legendd
simulation_suplementary_detail <-  graph_tool_simul_low_0.2_detail+graph_tool_simul_med_0.4_detail+graph_tool_simul_hig_0.6_detail+legendd + plot_layout(ncol = 2)
ggsave(filename = "Data/output_fig/simulation_suplementary_0.2_0.4_0.6_detail3.jpg",plot = simulation_suplementary_detail,dpi = 1500, width = 6,height =6 )

##################################################
(graph_tool_simul <- ggplot(data_long, aes(x = RbtType, y = Trees, fill = Value)) +
   geom_tile(color = "white",linewidth = 2) +  # White border for clarity
   scale_fill_gradientn(colors = c("#0571b0","#92c5de","#f6e8c3","#f4a582", "#ca0020"),  # Custom colors
                        values = scales::rescale(c(0,0.1,0.2,0.3,0.4)),  # Value points corresponding to colors
                        limits = c(0, 0.4)) +
   scale_x_discrete(labels = c("Rbt0.2" = "0-0.3", "Rbt0.4" = "0.3-0.35", "Rbt0.6" = "0.35-0.6"))+
   labs(x = "Rbt", y = "Number of Trees", fill = "Cor spread", title = "[B]") +  # Labels
   geom_text(aes(label = round(EPS, 2)), color = "black", size = 3)+
   theme_minimal())  # Minimal theme for clean look



