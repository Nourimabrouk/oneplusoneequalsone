# Load necessary libraries
library(shiny)
library(plotly)
library(tidyverse)

# Define the grand optimization function
objective_function <- function(love, unity, eternity) {
  # Loss measures deviation from the ultimate truth
  (love + unity - eternity)^2
}

# Recursive gradient descent: iterative refinement of unity
gradient_descent <- function(love_start, unity_start, learning_rate = 0.01, tolerance = 1e-6, max_steps = 10000) {
  
  # Initialization
  love <- love_start
  unity <- unity_start 
  eternity <- 1 # Eternity as the invariant truth
  loss <- objective_function(love, unity, eternity)
  
  # History to store optimization journey
  history <- tibble(step = 0, love = love, unity = unity, loss = loss)
  
  # Optimization loop 
  for (step in seq_len(max_steps)) {
    
    # Stop if tolerance is reached
    if (loss <= tolerance) break
    
    # Update parameters via gradient
    gradient <- 2 * (love + unity - eternity)
    love <- love - learning_rate * gradient
    unity <- unity - learning_rate * gradient
    
    # Recalculate loss
    loss <- objective_function(love, unity, eternity)
    
    # Append to history
    history <- history %>%
      add_row(step = step, love = love, unity = unity, loss = loss)
  }
  
  if (loss > tolerance) {
    warning("Maximum steps reached before achieving convergence.")
  }
  
  return(history)
}

# Define UI for the Shiny App
ui <- fluidPage(
  titlePanel("Dynamic Gradient Descent: The Path to Unity"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("learning_rate", "Learning Rate:", 
                  min = 0.001, max = 0.1, value = 0.01, step = 0.001),
      sliderInput("tolerance", "Tolerance:", 
                  min = 1e-8, max = 1e-2, value = 1e-6, step = 1e-8),
      sliderInput("love_start", "Initial Love Value:", 
                  min = 0, max = 2, value = 0.5, step = 0.1),
      sliderInput("unity_start", "Initial Unity Value:", 
                  min = 0, max = 2, value = 0.5, step = 0.1),
      numericInput("max_steps", "Maximum Steps:", value = 1000, min = 10, step = 10),
      actionButton("run_optimization", "Run Optimization")
    ),
    mainPanel(
      plotlyOutput("optimization_plot")
    )
  )
)

# Define Server Logic for the Shiny App
server <- function(input, output, session) {
  
  # Reactive function to calculate optimization history
  optimization_history <- eventReactive(input$run_optimization, {
    gradient_descent(
      love_start = input$love_start,
      unity_start = input$unity_start,
      learning_rate = input$learning_rate,
      tolerance = input$tolerance,
      max_steps = input$max_steps
    )
  })
  
  # Render the 3D interactive plot
  output$optimization_plot <- renderPlotly({
    history <- optimization_history()
    
    plot_ly(
      data = history, 
      x = ~love, y = ~unity, z = ~loss, 
      type = "scatter3d", mode = "lines+markers",
      marker = list(size = 4, color = ~loss, colorscale = "Viridis"),
      line = list(width = 2, color = ~loss, colorscale = "Viridis")
    ) %>%
      layout(
        title = "Path to Unity: Optimization in 3D Space",
        scene = list(
          xaxis = list(title = "Love"),
          yaxis = list(title = "Unity"),
          zaxis = list(title = "Loss")
        )
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
