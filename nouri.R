# Libraries ----
library(tidyverse)
library(shiny)
library(plotly)
library(glue)
library(pracma)      # Fractals, golden ratio
library(shinyWidgets) # Enhanced UI elements
library(ggthemes)    # Elegant themes for plots

# Constants ----
phi <- (1 + sqrt(5)) / 2 # Golden Ratio

# Recursive Function Generator ----
generate_recursive_function <- function(level = 1) {
  function(x) {
    if (level <= 1) {
      return(sin(phi * x))
    }
    x + generate_recursive_function(level - 1)(phi * x)
  }
}

# Dynamic Fractal Data ----
generate_fractal_data <- function(func, depth = 5) {
  tibble(
    x = seq(-10, 10, length.out = 1000),
    y = map_dbl(x, func)
  )
}

recursive_plot <- function(func, iterations = 5, title = "Fractal Harmony") {
  data <- generate_fractal_data(func)
  ggplot(data, aes(x, y)) +
    geom_line(linewidth = 1, color = "cyan") +
    ggtitle(title) +
    theme_minimal(base_size = 14) +
    theme(
      plot.background = element_rect(fill = "#000428", color = NA),
      panel.background = element_rect(fill = "#000428", color = NA),
      text = element_text(color = "white"),
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
    ) +
    annotate(
      "text",
      x = 0,
      y = max(data$y, na.rm = TRUE),
      label = glue("Harmonic Metric: {round(harmonic_convergence(func, 0), 3)}"),
      size = 5,
      color = "yellow"
    )
}

# Shiny App ----
shinyApp(
  ui = fluidPage(
    # Cosmic Theme
    tags$head(
      tags$style(HTML("
        body {
          background: linear-gradient(135deg, #1B2735, #090A0F);
          color: white;
        }
        .sliderInput {
          color: white;
        }
        h1, h2 {
          text-shadow: 0px 0px 10px #FFFFFF;
        }
      "))
    ),
    titlePanel(
      div(" Manifesting Multidimensional Harmony ", 
          style = "text-align:center; font-size: 36px; font-weight: bold; color: gold;")
    ),
    sidebarLayout(
      sidebarPanel(
        sliderInput("recursion_depth", "Recursion Depth:",
                    min = 1, max = 10, value = 5, step = 1),
        sliderInput("glitch_intensity", "Glitch Intensity:",
                    min = 0, max = 1, value = 0.1, step = 0.01),
        actionButton("update_plot", "Update Visualization",
                     style = "background-color: #28A745; color: white; font-weight: bold;"),
        br(),
        p("Harness the harmony of recursion, phi, and fractals to explore infinite unity.", 
          style = "color: lightgray; font-style: italic;")
      ),
      mainPanel(
        plotlyOutput("fractal_plot", height = "600px"),
        br(),
        verbatimTextOutput("meta_comments", placeholder = TRUE),
        br(),
        div("Made with metagaming love", style = "text-align: center; font-size: 12px; color: lightgray;")
      )
    )
  ),
  server = function(input, output, session) {
    # Reactive Fractal Function ----
    fractal_func <- reactive({
      generate_recursive_function(input$recursion_depth)
    })
    
    # Update Visualization ----
    observeEvent(input$update_plot, {
      output$fractal_plot <- renderPlotly({
        func <- fractal_func()
        data <- generate_fractal_data(func)
        plot_ly(
          data,
          x = ~x,
          y = ~y,
          type = "scatter",
          mode = "lines",
          line = list(color = "cyan", width = 2)
        ) %>%
          layout(
            title = list(
              text = "Fractal Harmony of Emergence",
              font = list(color = "gold", size = 24, family = "Arial")
            ),
            xaxis = list(title = "Input", color = "white"),
            yaxis = list(title = "Output", color = "white"),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor = "rgba(0,0,0,0)"
          )
      })
    })
    
    # Meta-Comments ----
    output$meta_comments <- renderText({
      phi_comment <- "Phi unifies chaos into divine symmetry."
      glitch_comment <- "Glitches represent growth—where the cosmos whispers evolution."
      recursion_comment <- glue("Recursion depth: {input$recursion_depth}, exploring {round(phi ^ input$recursion_depth, 3)} dimensions.")
      glue("Meta-Comments:\n\n1. {phi_comment}\n\n2. {glitch_comment}\n\n3. {recursion_comment}")
    })
  }
)
