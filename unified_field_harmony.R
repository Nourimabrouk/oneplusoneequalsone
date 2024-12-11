# Load necessary libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(rgl)
library(viridis)

# Define constants
UNITY_CONSTANTS <- list(
  PHI = (1 + sqrt(5)) / 2, # Golden ratio
  PI = pi,                 # Circle constant
  E = exp(1)               # Natural emergence base
)

# Generate Unity Field
generate_unity_field <- function(resolution = 100, depth = 3, phi_factor = UNITY_CONSTANTS$PHI) {
  x <- seq(-pi, pi, length.out = resolution)
  y <- seq(-pi, pi, length.out = resolution)
  z <- outer(x, y, function(x, y) cos(x * phi_factor) * sin(y / phi_factor))
  
  # Return the grid as a list for 3D surface plotting
  list(
    x = x,
    y = y,
    z = (z^depth + 1) / 2
  )
}

# Define Shiny UI
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Unity Dashboard: 1+1=1"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Interactive Unity Field", tabName = "field", icon = icon("chart-area")),
      menuItem("3D Exploration", tabName = "exploration", icon = icon("cube")),
      menuItem("Meta Insights", tabName = "insights", icon = icon("brain"))
    ),
    sliderInput("resolution", "Resolution:", min = 50, max = 300, value = 100, step = 10),
    sliderInput("depth", "Recursion Depth:", min = 1, max = 5, value = 3, step = 1),
    sliderInput("phi_factor", "Phi Factor (Golden Influence):", min = 1, max = 2, value = UNITY_CONSTANTS$PHI, step = 0.01),
    actionButton("generate", "Generate Unity", class = "btn-primary")
  ),
  dashboardBody(
    tabItems(
      # Tab 1: Unity Field
      tabItem(
        tabName = "field",
        fluidRow(
          box(
            title = "Unity Field Visualization", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            plotlyOutput("unity_plot", height = "600px")
          )
        )
      ),
      # Tab 2: 3D Exploration
      tabItem(
        tabName = "exploration",
        fluidRow(
          box(
            title = "3D Unity Field", 
            status = "success", 
            solidHeader = TRUE, 
            width = 12,
            rglwidgetOutput("unity_3d", height = "600px")
          )
        )
      ),
      # Tab 3: Meta Insights
      tabItem(
        tabName = "insights",
        fluidRow(
          box(
            title = "Unity Console",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            verbatimTextOutput("console_output")
          )
        )
      )
    )
  )
)

# Define Shiny Server
server <- function(input, output, session) {
  unity_data <- reactiveVal(generate_unity_field())
  
  observeEvent(input$generate, {
    unity_data(generate_unity_field(
      resolution = input$resolution,
      depth = input$depth,
      phi_factor = input$phi_factor
    ))
  })
  
  output$unity_plot <- renderPlotly({
    field <- unity_data()
    plot_ly(
      x = ~field$x,
      y = ~field$y,
      z = ~field$z,
      type = "surface",
      colors = viridis::viridis(100)
    ) %>%
      layout(
        title = "Unity Field",
        scene = list(
          xaxis = list(title = "X"),
          yaxis = list(title = "Y"),
          zaxis = list(title = "Unity")
        )
      )
  })
  
  output$unity_3d <- renderRglwidget({
    field <- unity_data()
    with(field, {
      open3d()
      surface3d(
        x, y,
        outer(x, y, function(x, y) cos(x * UNITY_CONSTANTS$PHI) * sin(y / UNITY_CONSTANTS$PHI)),
        col = viridis::viridis(length(unique(x))),
        alpha = 0.7
      )
      rglwidget()
    })
  })
  
  output$console_output <- renderText({
    field <- unity_data()
    coherence <- mean(field$z)
    phi_alignment <- abs(coherence - UNITY_CONSTANTS$PHI)
    glue::glue("
      ╔════════════════════════════╗
      ║        UNITY REPORT        ║
      ╠════════════════════════════╣
      ║ Coherence Level: {round(coherence, 4)}       ║
      ║ Phi Alignment: {round(phi_alignment, 4)}    ║
      ║ Total Data Points: {length(field$z)}        ║
      ╚════════════════════════════╝
    ")
  })
}

# Run the app
shinyApp(ui, server)
