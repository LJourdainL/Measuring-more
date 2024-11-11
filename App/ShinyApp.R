#  This work is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International

library(shiny)
library(shinythemes)
library(tidyverse)
library(dplR)


# Define UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  navbarPage(
    title = "ClimateCorSimulation", footer = "LJourdainL_Measuring_more_appendix",
    tabPanel(
      titlePanel("Data"),
      sidebarPanel(
        fileInput("climate_data", "Upload Climate Datatable (txt)", accept = ".txt"),
        sliderInput("driver_season", "Driver Season", value = c(3, 4), min = 1, max = 12, step = 1),
        sliderInput("analysis_season", "Analysis Season", value = c(3, 4), min = 1, max = 12, step = 1),
      ),
      mainPanel(
        h2("Visualisation"),
        h5("Table format must be colum = months, row = years."),
        h5("Season selection by columns number."),
        tableOutput("climate")
      )
    ),
    tabPanel(
      titlePanel("Param"),
      sidebarPanel(
        width = 6,
        h4("Population"),
        numericInput("pop_size", "Population Size", value = 100, min = 50, max = 500),
        numericInput("N_tree_sample", "Number of Trees to Sample", min = 2, max = 100, value = 5),
        numericInput("rep_sub_core", "Number of subsample core / tree", value = 100, min = 1),
        numericInput("rep_sub_pop", "Number of sub-population draw", value = 100, min = 1, max = 1000),
      ),
      mainPanel(
        width = 6,
        h4("Stats target"),
        numericInput("target_cor", "Target Climate Correlation (r)", value = 0.5, min = -1, max = 1),
        numericInput("target_rbt", "Target common signal (Rbt)", value = 0.4, min = 0, max = 1),
        numericInput("noise_min", "Noise min", min = 0, max = 1, value = 0.1),
        numericInput("noise_max", "Noise max", min = 0.5, max = 5, value = 1),
        numericInput("p_value", "P-value", min = 0, max = 1, value = 0.01)
      ),
    ),
    tabPanel(
      titlePanel("Simul"),
      sidebarPanel(
        h4("Graphic param"),
        actionButton("rerun", "Run"),
        checkboxInput("graph", "Show Graph", value = TRUE),
        textInput("color1", "Color 1", value = "green"),
        textInput("color2", "Color 2", value = "white"),
        sliderInput("ylim", "Y-axis Limits", min = -1, max = 1, value = c(0, 0.7), step = 0.01),
        sliderInput("xlim", "X-axis Limits", min = 0, max = 1, value = c(0, 0.7), step = 0.01),
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Graph", plotOutput("plot"), plotOutput("tool")), # Add action button),
          tabPanel("Table", tableOutput("results"), )
        )
      )
    ),
  )
)

# Define server logic
server <- function(input, output) {
  source("Simulation_function.R")
  climate_data <- reactive({
    req(input$climate_data)
    data <- read.table(input$climate_data$datapath, header = TRUE)
    data <- data %>%
      mutate(across(everything(), ~ as.numeric(as.character(.))))

    data
  })

  driver_season <- reactive({
    as.integer(input$driver_season)
  })

  analysis_season <- reactive({
    as.integer(input$analysis_season)
  })

  N_tree_sample <- reactive({
    input$N_tree_sample
  })

  noise <- reactive({
    round(seq(from = input$noise_max, to = input$noise_min, length.out = 10), digits = 2)
  })

  results <- eventReactive(input$rerun, {
    req(climate_data())
    climate_df <- climate_data()
    Climate_cor_simule(
      climate_data = climate_df,
      noise = noise(),
      pop_size = input$pop_size,
      N_tree_sample = N_tree_sample(),
      driver_season = driver_season(),
      analysis_season = analysis_season(),
      target_cor = input$target_cor,
      target_rbt = input$target_rbt,
      rep_sub_core = input$rep_sub_core,
      rep_sub_pop = input$rep_sub_pop,
      graph = input$graph,
      color1 = input$color1,
      color2 = input$color2,
      ylim = input$ylim,
      xlim = input$xlim,
      p_value = input$p_value
    )
  })

  output$climate <- renderTable({
    req(climate_data)
    climate_df <- climate_data()
    climate_df
  })


  output$results <- renderTable({
    req(results())
    results()[[1]]
  })

  output$plot <- renderPlot({
    req(results())
    if (input$graph) {
      results()[[2]]
    }
  })
  output$tool <- renderPlot({
    req(results())
    if (input$graph) {
      Tool_graph(results()[[1]])
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
