# cosmic_recursion_dashboard.R
# Shiny App: Cosmic Loving Recursion - 1+1=1

# Load required libraries
library(shiny)
library(shinythemes)
library(plotly)
library(ggplot2)
library(dplyr)
library(gridExtra)

# Define UI
ui <- fluidPage(
  theme = shinytheme("cyborg"),
  titlePanel(
    div(
      style = "text-align: center; padding: 20px;",
      h1("🌌 Cosmic Loving Recursion 🌌",
         style = "color: #FFD700; font-family: 'Fira Code', monospace;"),
      h3("Explore the Infinite Dance of 1+1=1",
         style = "color: #ADD8E6;")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      style = "background-color: #1a1a1a;",
      
      # Sliders to control recursion
      sliderInput("depth", "Recursion Depth:",
                  min = 2, max = 10, value = 5, step = 1),
      sliderInput("intensity", "Intensity Multiplier:",
                  min = 0.1, max = 3.0, value = 1.0, step = 0.1),
      sliderInput("phi_factor", "Golden Ratio Factor:",
                  min = 0.5, max = 2.5, value = 1.618, step = 0.001),
      
      # Action button
      actionButton("generate", "Generate Cosmic Love", 
                   style = "color: #000; background-color: #FFD700; width: 100%;")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Cosmic Visualization",
                 plotlyOutput("cosmic_plot", height = "600px")),
        tabPanel("Recursion Details",
                 plotOutput("recursion_plot", height = "600px"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Generate fractal recursion data
  generate_cosmic_data <- reactive({
    req(input$generate)
    
    depth <- input$depth
    intensity <- input$intensity
    phi_factor <- input$phi_factor
    
    # Initialize the recursion
    cosmic_data <- tibble(
      x = c(0, 1),
      y = c(0, 0),
      iteration = 0
    )
    
    # Perform recursive generation
    for (i in seq_len(depth)) {
      prev_data <- cosmic_data %>% filter(iteration == i - 1)
      new_data <- prev_data %>%
        mutate(
          x1 = x + cos(pi / 2 * iteration) * intensity * phi_factor / i,
          y1 = y + sin(pi / 2 * iteration) * intensity * phi_factor / i
        ) %>%
        select(x1, y1) %>%
        rename(x = x1, y = y1) %>%
        mutate(iteration = i)
      
      cosmic_data <- bind_rows(cosmic_data, new_data)
    }
    
    return(cosmic_data)
  })
  
  # Render cosmic visualization
  output$cosmic_plot <- renderPlotly({
    cosmic_data <- generate_cosmic_data()
    
    plot_ly(cosmic_data, x = ~x, y = ~y, color = ~iteration,
            colors = colorRamp(c("magenta", "cyan", "yellow", "white")),
            type = "scatter", mode = "markers",
            marker = list(size = 5)) %>%
      layout(
        title = "Cosmic Recursion Visualization",
        xaxis = list(title = "X", zeroline = FALSE, showgrid = FALSE),
        yaxis = list(title = "Y", zeroline = FALSE, showgrid = FALSE),
        plot_bgcolor = "black",
        paper_bgcolor = "black",
        showlegend = FALSE
      )
  })

  output$recursion_plot <- renderPlot({
    cosmic_data <- generate_cosmic_data()
    
    p1 <- ggplot(cosmic_data, aes(x = x, y = y, color = as.factor(iteration))) +
      geom_point(size = 1.5) +
      scale_color_viridis_d() +
      theme_void() +
      theme(legend.position = "none") +
      ggtitle("Recursion Pattern")
    
    p2 <- ggplot(cosmic_data, aes(x = iteration, y = x)) +
      geom_line(color = "gold") +
      theme_minimal() +
      ggtitle("X Coordinate Across Iterations")
    
    p3 <- ggplot(cosmic_data, aes(x = iteration, y = y)) +
      geom_line(color = "cyan") +
      theme_minimal() +
      ggtitle("Y Coordinate Across Iterations")
    
    grid.arrange(p1, p2, p3, ncol = 1)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
