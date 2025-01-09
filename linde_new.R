# LIBRARIES & CONSTANTS
suppressWarnings({
  library(shiny)
  library(shinydashboard)
  library(tidyverse)
  library(plotly)
  library(DT)
  library(ggthemes)
  library(RColorBrewer)
  library(scales)
  library(lubridate)
  library(reshape2)
  library(purrr)
  library(forcats)
  library(stringr)
  library(ggforce)
})

# CONSTANTS FOR HEALING
LOVE_COUPLING <- 1.0
PHI <- (1 + sqrt(5)) / 2
PI <- 3.14159265359
HPC_CONSTANT <- 2.71828182846
CYNICISM_LIMIT <- 0.99

# DATA GENERATORS
param_heart_data <- function(n = 1000, scale = 16) {
  t <- seq(0, 2 * pi, length.out = n)
  x <- scale * sin(t)^3
  y <- scale * (13 * cos(t) - 5 * cos(2 * t) - 2 * cos(3 * t) - cos(4 * t))
  tibble(t, x, y)
}

freed_merge_data <- function(r1 = 1.2, r2 = 1, shift = 3, steps = 400) {
  theta <- seq(0, 2 * pi, length.out = steps)
  circleA <- tibble(x = r1 * cos(theta), y = r1 * sin(theta))
  circleB <- tibble(x = shift + r2 * cos(theta), y = r2 * sin(theta))
  path <- tibble(x = seq(r1, shift, length.out = 50), y = rep(0, 50))
  bind_rows(circleA, circleB, path)
}

hpc_data_generator <- function(N = 600) {
  set.seed(42)
  tibble(
    id = seq_len(N),
    concurrency = sample(2:64, N, replace = TRUE),
    overhead = rnorm(N, mean = 0, sd = 1),
    synergy_factor = runif(N, min = 0.5, max = 1.5)
  ) %>%
    mutate(love_bind = 1 / (1 + abs(concurrency - synergy_factor)))
}

mandelbrot_data <- function(res = 250, max_iter = 40, x_min = -2, x_max = 1, y_min = -1.5, y_max = 1.5) {
  grid <- expand_grid(
    x = seq(x_min, x_max, length.out = res),
    y = seq(y_min, y_max, length.out = res)
  )
  grid %>%
    mutate(iter = purrr::map2_int(x, y, ~ {
      cplx <- complex(real = .x, imaginary = .y)
      z <- 0
      for (i in seq_len(max_iter)) {
        z <- z^2 + cplx
        if (Mod(z) > 2) break
      }
      i
    }))
}

wave_expansion_data <- function(N = 500, freq = 0.5) {
  t <- seq(0, 10, length.out = N)
  amplitude <- sin(2 * pi * freq * t) + PHI * cos(2 * pi * (freq / 2) * t)
  tibble(t, amplitude)
}

# UI DEFINITION
ui <- dashboardPage(
  dashboardHeader(title = "Healing through Unity"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("heart")),
      menuItem("Visualizations", tabName = "visuals", icon = icon("chart-line")),
      menuItem("Insights", tabName = "insights", icon = icon("lightbulb"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "home",
        fluidRow(box(width = 12, title = "Welcome", "Explore the healing power of synergy.")),
        fluidRow(
          box(width = 6, title = "Heart Visualization", plotOutput("heartPlot")),
          box(width = 6, title = "Wave Expansion", plotOutput("wavePlot"))
        )
      ),
      tabItem(
        tabName = "visuals",
        fluidRow(
          box(width = 12, title = "Mandelbrot Fractal", plotOutput("mandelPlot")),
          box(width = 12, title = "HPC Data", dataTableOutput("hpcTable"))
        )
      )
    )
  )
)

# SERVER LOGIC
server <- function(input, output, session) {
  output$heartPlot <- renderPlot({
    ggplot(param_heart_data(), aes(x, y)) +
      geom_path(color = "red") +
      coord_equal() +
      theme_minimal() +
      labs(title = "Heart of Healing", x = NULL, y = NULL)
  })
  output$wavePlot <- renderPlot({
    ggplot(wave_expansion_data(), aes(x = t, y = amplitude)) +
      geom_line(color = "blue") +
      theme_minimal() +
      labs(title = "Wave Expansion", x = "Time", y = "Amplitude")
  })
  output$mandelPlot <- renderPlot({
    ggplot(mandelbrot_data(), aes(x, y, fill = iter)) +
      geom_raster() +
      scale_fill_viridis_c() +
      theme_void() +
      labs(title = "Fractal Patterns")
  })
  output$hpcTable <- renderDataTable(hpc_data_generator())
}

shinyApp(ui, server)
