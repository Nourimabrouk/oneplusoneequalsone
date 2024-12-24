###############################################################################
#          1+1=1 "META-MASTER AGI PROOF" SHINY APP - ULTIMATE EVOLUTION        #
#                    Author: Unified Architect AGI (The One)                  #
#                                Date: 2025+                                   #
#                                                                             #
#   Purpose:                                                                  #
#   - Showcase advanced viral spread modeling, multi-agent systems, Bayesian   #
#     hierarchical frameworks, fractal recursion, quantum synergy, and more.  #
#   - Embody the 1+1=1 principle in every line of code, melding philosophy,    #
#     mathematics, synergy, and emergent unity in a single living ecosystem.   #
#                                                                             #
#   This code is designed to:                                                 #
#   1) Double, triple, and ascend in complexity from the original version.     #
#   2) Serve as a cosmic synergy demonstration (1+1=1).                       #
#   3) Provide advanced fractal recursion and multi-dimensional interactions.  #
#   4) Offer real-time synergy dashboards to unify time-series, networks,      #
#      Bayesian inference, fractal geometry, and more.                         #
#   5) Blow minds.                                                            #
###############################################################################

############################# PHILOSOPHY 2.0 COMMENTS ##########################
# In the spirit of Taoist non-duality, we unify polarities: complexity & 
# simplicity, chaos & order, matter & mind. Through each function, the concept 
# of 1+1=1 is woven—demonstrating emergent synergy beyond summation. 
###############################################################################

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                            LIBRARY IMPORTS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(shiny)
library(shinyWidgets)    # Enhanced UI widgets
library(tidyverse)
library(lubridate)
library(plotly)
library(igraph)
library(tidygraph)
library(ggraph)
library(viridis)
library(propagate)
library(MASS)
library(invgamma)
library(gganimate)
library(future.apply)
library(bayesplot)
library(rstan)
library(threejs)         # For 3D network visualizations
library(rgl)             # 3D plot support
library(shinythemes)     # Pre-built themes
library(DT)              # Data table display
library(htmltools)       # For advanced UI markup
library(scales)
library(glue)
library(markdown)

# For fractal generation & more advanced math
# (We do this to illustrate quantum synergy illusions)
library(ComplexHeatmap)
library(circlize)

###############################################################################
#                        GLOBAL CONSTANTS & SEEDS
###############################################################################
PHI <- (1 + sqrt(5)) / 2         # Golden Ratio
UNITY_CONSTANT <- 1              # Symbolizing 1+1=1
CHEATCODE <- 420691337           # Metagaming resonance key
SET_SEED <- 1337                 # For reproducibility
HYPERDIMENSIONAL_FACTOR <- 42    # The 42, for cosmic comedic synergy

# Custom Color Palettes
UNITY_COLORS <- c("#0D0887", "#6A00A8", "#B12A90", "#E16462", "#FCA636", "#F0F921")

# Future plan to allow parallel computations
plan(multisession)

###############################################################################
#         META-COMMENT: 
#         We define a series of constants that capture the intangible 
#         synergy bridging mathematics, cosmic jokes, and creative synergy. 
###############################################################################

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#            RECURSIVE FRACTAL GENERATION FUNCTIONS & UTILITIES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# -----------------------------------------------------------------------------
# MANDLEBROT SET GENERATOR (2D)
# -----------------------------------------------------------------------------
generate_mandelbrot <- function(xmin = -2, xmax = 1, ymin = -1.5, ymax = 1.5,
                                resolution = 200, max_iter = 100) {
  # We unify infinite complexity into a finite approximation,
  # illustrating how the fractal reveals 1+1=1 synergy.
  
  x <- seq(xmin, xmax, length.out = resolution)
  y <- seq(ymin, ymax, length.out = resolution)
  
  # Create a grid of complex numbers
  cgrid <- outer(x, y, FUN = function(xx, yy) complex(real = xx, imaginary = yy))
  
  # Initialize output matrix
  output <- matrix(0, nrow = resolution, ncol = resolution)
  
  # For each point, compute iteration
  z <- matrix(0+0i, nrow = resolution, ncol = resolution)
  
  for (i in seq_len(max_iter)) {
    z <- z^2 + cgrid
    escaped <- Mod(z) > 2
    output[escaped & (output == 0)] <- i
  }
  
  # Return iteration count matrix
  return(output)
}

# -----------------------------------------------------------------------------
# JULIA SET GENERATOR (2D)
# -----------------------------------------------------------------------------
generate_julia <- function(cx = -0.7, cy = 0.27015, 
                           xmin = -1.5, xmax = 1.5, 
                           ymin = -1.5, ymax = 1.5,
                           resolution = 200, max_iter = 100) {
  # Another fractal approach to synergy: c is fixed, z starts from each point.
  
  x <- seq(xmin, xmax, length.out = resolution)
  y <- seq(ymin, ymax, length.out = resolution)
  
  cst <- complex(real = cx, imaginary = cy)
  
  # Initialize output matrix
  output <- matrix(0, nrow = resolution, ncol = resolution)
  
  for (r in 1:resolution) {
    for (c in 1:resolution) {
      z <- complex(real = x[r], imaginary = y[c])
      iter <- 0
      while ((Mod(z) <= 2) && (iter < max_iter)) {
        z <- z^2 + cst
        iter <- iter + 1
      }
      output[r, c] <- iter
    }
  }
  
  return(output)
}

# -----------------------------------------------------------------------------
# FRACTAL HELPER FOR 3D PLOTTING (SIMPLE ESCAPE-TIME PLOT)
# -----------------------------------------------------------------------------
generate_mandelbulb_approx <- function(resolution = 50, 
                                       max_iter = 10, 
                                       power = 8) {
  # The "Mandelbulb" is a 3D fractal analog, but we do a simplistic version here:
  # We treat (x,y,z) in spherical coords, iterating the fractal formula.
  
  # define the 3D grid
  coords <- expand.grid(
    x = seq(-1.2, 1.2, length.out = resolution),
    y = seq(-1.2, 1.2, length.out = resolution),
    z = seq(-1.2, 1.2, length.out = resolution)
  )
  
  # We store iteration counts
  coords$iter_count <- 0
  R <- 2
  
  for (i in 1:nrow(coords)) {
    # Convert to spherical
    r <- sqrt(coords$x[i]^2 + coords$y[i]^2 + coords$z[i]^2)
    theta <- atan2(sqrt(coords$x[i]^2 + coords$y[i]^2), coords$z[i])
    phi <- atan2(coords$y[i], coords$x[i])
    
    c_iter <- 0
    zr <- r
    ztheta <- theta
    zphi <- phi
    
    while (zr < R && c_iter < max_iter) {
      # fractal transform
      zr <- zr^power
      ztheta <- ztheta * power
      zphi <- zphi * power
      
      # convert back to cartesian
      x_new <- zr * sin(ztheta) * cos(zphi)
      y_new <- zr * sin(ztheta) * sin(zphi)
      z_new <- zr * cos(ztheta)
      
      # shift by original (like adding c in mandelbrot)
      x_new <- x_new + coords$x[i]
      y_new <- y_new + coords$y[i]
      z_new <- z_new + coords$z[i]
      
      # update spherical
      zr <- sqrt(x_new^2 + y_new^2 + z_new^2)
      ztheta <- atan2(sqrt(x_new^2 + y_new^2), z_new)
      zphi <- atan2(y_new, x_new)
      
      c_iter <- c_iter + 1
    }
    
    coords$iter_count[i] <- c_iter
  }
  
  return(coords)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                        SYNTHETIC DATA & NETWORK UTILITIES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# -----------------------------------------------------------------------------
# EXTENDED SYNTHETIC DATA GENERATOR
# -----------------------------------------------------------------------------
generate_synthetic_data <- function(start_date, periods, freq = "day", 
                                    seasonality = TRUE, exogenous_shocks = 3) {
  # set seed for reproducibility
  set.seed(SET_SEED)
  dates <- seq(as.Date(start_date), by = freq, length.out = periods)
  base <- sin(seq(0, PHI * 10, length.out = periods)) * 100
  
  # random noise
  noise <- rnorm(periods, mean = 0, sd = 10)
  
  # synergy effect (cumulative random synergy)
  synergy_effect <- cumsum(runif(periods, min = 0, max = PHI))
  
  # seasonality
  season <- ifelse(seasonality, 50 * sin(seq(0, 2 * pi, length.out = periods)), 0)
  
  # exogenous shocks
  shock_index <- sort(sample(1:periods, exogenous_shocks))
  shock_magnitude <- rnorm(exogenous_shocks, mean = 50, sd = 20)
  
  mentions <- base + noise + synergy_effect + season
  for(i in seq_along(shock_index)) {
    mentions[shock_index[i]] <- mentions[shock_index[i]] + shock_magnitude[i]
  }
  
  tibble(date = dates, mentions = mentions)
}

# -----------------------------------------------------------------------------
# ADVANCED ANT COLONY / MULTI-AGENT NETWORK
# -----------------------------------------------------------------------------
generate_ant_colony_network <- function(nodes = 100, 
                                        dynamic_roles = TRUE, 
                                        hyperdimensional = FALSE) {
  
  # We unify synergy in the form of a Barabási–Albert preferential attachment
  graph <- sample_pa(nodes, directed = FALSE)
  
  # Basic attributes
  V(graph)$activity <- rbeta(nodes, shape1 = 2, shape2 = 5)
  V(graph)$role <- sample(c("Worker", "Forager", "Queen", "Drone"), 
                          nodes, replace = TRUE)
  
  # Extended roles
  if (dynamic_roles) {
    V(graph)$dynamic_roles <- sample(c("Leader", "Follower", "Scout", "Healer"), 
                                     nodes, replace = TRUE)
  }
  
  # Hyperdimensional extension: Additional dimension stored as numeric
  if(hyperdimensional) {
    V(graph)$hyper_coord <- runif(nodes, min = 0, max = HYPERDIMENSIONAL_FACTOR)
  }
  
  return(graph)
}

# -----------------------------------------------------------------------------
# PROBABILISTIC SPREAD SIMULATION (EXTENDED + BAYESIAN PLACEHOLDER)
# -----------------------------------------------------------------------------
simulate_probabilistic_spread <- function(data, 
                                          spread_prob = 0.05, 
                                          intervention = NULL,
                                          dynamic_adjustment = TRUE) {
  
  initial <- sample(1:nrow(data), 1)
  # Base prob is drawn from Beta
  prob <- rbeta(nrow(data), 2, 5) * spread_prob
  
  spread <- numeric(nrow(data))
  spread[initial] <- 1
  
  for (i in 2:length(spread)) {
    # dynamic adjustment: synergy factor influences spread
    synergy_factor <- if(dynamic_adjustment) runif(1, min = 0.9, max = 1.1) else 1
    
    spread[i] <- ifelse(runif(1) < (prob[i] * synergy_factor), 
                        spread[i - 1] + rnorm(1, 0, 0.5), 
                        spread[i - 1])
    
    # apply intervention as a random shock
    if (!is.null(intervention) && i %% intervention == 0) {
      spread[i] <- spread[i] + rnorm(1, mean = 5, sd = 2)
    }
  }
  
  data %>% mutate(probabilistic_spread = spread)
}

# -----------------------------------------------------------------------------
# ADVANCED BAYESIAN MODEL (HIERARCHICAL) FOR SPREAD
# -----------------------------------------------------------------------------
# This function sets up a simple hierarchical model in Stan. For demonstration,
# we do not run it by default unless user clicks a button due to potential time.
hierarchical_spread_model_code <- "
data {
  int<lower=1> N;            // number of observations
  vector[N] y;               // observed spread intensity
  real<lower=0> spread_prob; // prior spread probability
}
parameters {
  real alpha;        // intercept
  real<lower=0> sigma; // standard deviation
}
model {
  // priors
  alpha ~ normal(0, 10);
  sigma ~ exponential(1);
  
  // likelihood
  y ~ normal(alpha, sigma);
}
"

###############################################################################
#         ADVANCED MULTI-AGENT SYSTEM: 
#         SCOUTS, STRATEGISTS, HEALERS, + RECURSIVE ROLE UPDATES
###############################################################################
generate_multi_agent_system <- function(n_agents = 50) {
  set.seed(SET_SEED)
  
  # Agents have states that can evolve
  agents <- tibble(
    id = 1:n_agents,
    role = sample(c("Scout", "Strategist", "Healer", "Worker"), n_agents, replace = TRUE),
    synergy_level = runif(n_agents, min = 0.5, max = 1.5),
    last_update = Sys.time()
  )
  
  return(agents)
}

update_multi_agent_system <- function(agents) {
  # Example rule-based or random transitions:
  # - synergy_level might drift
  # - role might stochastically shift if synergy_level out of certain range
  
  agents <- agents %>% 
    mutate(
      synergy_level = synergy_level + rnorm(n(), mean = 0, sd = 0.02),
      synergy_level = pmin(pmax(synergy_level, 0), 2),
      role = if_else(runif(n()) < 0.02, 
                     sample(c("Scout", "Strategist", "Healer", "Worker"), 1),
                     role),
      last_update = Sys.time()
    )
  
  return(agents)
}

###############################################################################
#         FRACTAL RECURSION VISUALIZATIONS + QUANTUM UNIFICATION
###############################################################################
# 2D fractal generators: already done above. We'll make them plottable in Shiny.

# Utility to convert iteration matrix to a ggplot-friendly data frame
iteration_matrix_to_df <- function(mat, xmin, xmax, ymin, ymax) {
  df <- expand.grid(row = 1:nrow(mat), col = 1:ncol(mat))
  df$iter <- as.vector(mat)
  
  x_vals <- seq(xmin, xmax, length.out = nrow(mat))
  y_vals <- seq(ymin, ymax, length.out = ncol(mat))
  
  df$x <- x_vals[df$row]
  df$y <- y_vals[df$col]
  
  df
}

###############################################################################
#       SHINY MODULES (FOR ORGANIZATION) 
###############################################################################

# ~~~~~~~~~~~~~~~~~~~~~~~~~
# Module for fractal plots
# ~~~~~~~~~~~~~~~~~~~~~~~~~
fractalUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 6,
             selectInput(ns("fractal_type"), 
                         "Fractal Type:", 
                         choices = c("Mandelbrot", "Julia"), 
                         selected = "Mandelbrot")),
      column(width = 6,
             sliderInput(ns("fractal_resolution"), 
                         "Resolution:", 
                         min = 100, 
                         max = 800, 
                         value = 200, 
                         step = 50))
    ),
    conditionalPanel(
      condition = paste0("input['", ns("fractal_type"), "'] == 'Julia'"),
      fluidRow(
        column(width = 6,
               numericInput(ns("julia_cx"), "Julia cx:", 
                            value = -0.7, step = 0.01)),
        column(width = 6,
               numericInput(ns("julia_cy"), "Julia cy:", 
                            value = 0.27015, step = 0.01))
      )
    ),
    fluidRow(
      column(width = 12,
             plotOutput(ns("fractal_plot")))
    )
  )
}

fractalServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$fractal_plot <- renderPlot({
      req(input$fractal_type)
      if(input$fractal_type == "Mandelbrot") {
        mat <- generate_mandelbrot(resolution = input$fractal_resolution)
        df_mat <- iteration_matrix_to_df(mat, -2, 1, -1.5, 1.5)
        ggplot(df_mat, aes(x = x, y = y, fill = iter)) +
          geom_tile() +
          scale_fill_viridis(option = "magma") +
          theme_void() +
          theme(legend.position = "none") +
          ggtitle("Mandelbrot Set: 1+1=1 Fractal Synergy")
      } else {
        mat <- generate_julia(cx = input$julia_cx, 
                              cy = input$julia_cy,
                              resolution = input$fractal_resolution)
        df_mat <- iteration_matrix_to_df(mat, -1.5, 1.5, -1.5, 1.5)
        ggplot(df_mat, aes(x = x, y = y, fill = iter)) +
          geom_tile() +
          scale_fill_viridis(option = "magma") +
          theme_void() +
          theme(legend.position = "none") +
          ggtitle("Julia Set: Emergent Unity")
      }
    })
  })
}

###############################################################################
#       MCMC MODULE: RUNNING A STAN MODEL ONCE TRIGGERED
###############################################################################
mcmcUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
             actionButton(ns("run_mcmc"), "Run Bayesian Spread Model"),
             plotOutput(ns("mcmc_diag_plot")),
             tableOutput(ns("mcmc_summary"))
      )
    )
  )
}

mcmcServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$run_mcmc, {
      showModal(modalDialog(
        title = "Running MCMC ...",
        "Please wait while the model runs.",
        easyClose = FALSE,
        footer = NULL
      ))
      
      # Prepare data for Stan
      standata <- list(
        N = nrow(data),
        y = data$probabilistic_spread,
        spread_prob = 0.05
      )
      
      fit <- stan(model_code = hierarchical_spread_model_code,
                  data = standata, 
                  chains = 2, 
                  iter = 500,
                  verbose = FALSE)
      
      removeModal()
      # Save in reactive to store outside
      rvals$stan_fit <- fit
    })
    
    rvals <- reactiveValues(stan_fit = NULL)
    
    output$mcmc_diag_plot <- renderPlot({
      req(rvals$stan_fit)
      fit <- rvals$stan_fit
      # simple traceplot
      stan_trace(fit)
    })
    
    output$mcmc_summary <- renderTable({
      req(rvals$stan_fit)
      print(rvals$stan_fit, digits = 2)
    })
  })
}

###############################################################################
#           3D NETWORK MODULE (ANT COLONY, HYPERDIMENSIONAL)
###############################################################################
network3DUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 4,
             numericInput(ns("network_nodes"), "Number of Ant Colony Nodes:", 
                          value = 100, min = 10, max = 1000)),
      column(width = 4,
             checkboxInput(ns("hyperdimensional"), "Hyperdimensional?", 
                           value = FALSE)),
      column(width = 4,
             actionButton(ns("generate_network"), "Generate Network"))
    ),
    fluidRow(
      column(width = 12,
             scatterplotThreeOutput(ns("threeD_network"), height = "600px"))
    )
  )
}

network3DServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    graph_reactive <- eventReactive(input$generate_network, {
      graph <- generate_ant_colony_network(nodes = input$network_nodes, 
                                           dynamic_roles = TRUE, 
                                           hyperdimensional = input$hyperdimensional)
      graph
    })
    
    output$threeD_network <- renderScatterplotThree({
      req(graph_reactive())
      graph <- graph_reactive()
      
      layout_fr <- layout_with_fr(graph, dim = 3)
      x <- layout_fr[,1]
      y <- layout_fr[,2]
      z <- layout_fr[,3]
      
      # Edges
      edges <- get.edges(graph, E(graph))
      # Each edge -> from vertex i to vertex j
      x_edges <- c()
      y_edges <- c()
      z_edges <- c()
      
      for(i in seq_len(nrow(edges))){
        x_edges <- c(x_edges, x[edges[i,1]], x[edges[i,2]], NA)
        y_edges <- c(y_edges, y[edges[i,1]], y[edges[i,2]], NA)
        z_edges <- c(z_edges, z[edges[i,1]], z[edges[i,2]], NA)
      }
      
      # Node colors based on dynamic roles
      roles <- as.factor(V(graph)$dynamic_roles)
      role_colors <- viridis::viridis(length(levels(roles)), option = "plasma")
      node_colors <- role_colors[as.numeric(roles)]
      
      scatterplot3js(
        x = x, 
        y = y, 
        z = z,
        color = node_colors, 
        size = 0.03,
        lines = cbind(x_edges, y_edges, z_edges),
        lineColor = "#AAAAAA",
        lineWidth = 1
      )
    })
    
  })
}

###############################################################################
#       MULTI-AGENT SIMULATION MODULE
###############################################################################
multiAgentUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 3,
             numericInput(ns("n_agents"), "Number of Agents:", 
                          value = 50, min = 10, max = 1000)),
      column(width = 3,
             actionButton(ns("init_agents"), "Initialize Agents")),
      column(width = 3,
             actionButton(ns("update_agents"), "Update Agents Continuously")),
      column(width = 3,
             actionButton(ns("stop_updates"), "Stop Updates"))
    ),
    fluidRow(
      column(width = 12,
             DTOutput(ns("agents_table")))
    )
  )
}

multiAgentServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    agents_rv <- reactiveVal()
    update_timer <- reactiveVal(NULL)
    
    observeEvent(input$init_agents, {
      new_agents <- generate_multi_agent_system(n_agents = input$n_agents)
      agents_rv(new_agents)
    })
    
    observeEvent(input$update_agents, {
      # Start a reactive timer that updates agents
      if(is.null(update_timer())){
        timer <- reactiveTimer(intervalMs = 1000, session)
        update_timer(timer)
      }
      
      observe({
        req(update_timer())
        update_timer()()
        isolate({
          current_agents <- agents_rv()
          if(!is.null(current_agents)) {
            updated_agents <- update_multi_agent_system(current_agents)
            agents_rv(updated_agents)
          }
        })
      })
    })
    
    observeEvent(input$stop_updates, {
      update_timer(NULL)
    })
    
    output$agents_table <- renderDT({
      req(agents_rv())
      datatable(agents_rv(), options = list(pageLength = 10))
    })
  })
}

###############################################################################
#       TIME SERIES & SPREAD SIMULATION MODULE
###############################################################################
timeSeriesUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 6,
             sliderInput(ns("phi_modifier"), "Golden Ratio Modifier:", 
                         min = 0.5, max = 2, value = PHI, step = 0.01)),
      column(width = 6,
             sliderInput(ns("spread_prob"), "Spread Probability:",
                         min = 0.01, max = 0.5, value = 0.05, step = 0.01))
    ),
    fluidRow(
      column(width = 6,
             numericInput(ns("intervention_step"), "Intervention Step (Days):",
                          value = 100, min = 10, max = 500)),
      column(width = 6,
             checkboxInput(ns("dynamic_adjust"), 
                           "Use Dynamic Adjustment?", 
                           value = TRUE))
    ),
    fluidRow(
      column(width = 12,
             plotlyOutput(ns("time_series_plot")))
    ),
    fluidRow(
      column(width = 12,
             plotlyOutput(ns("spread_simulation_plot")))
    ),
    fluidRow(
      column(width = 12,
             plotlyOutput(ns("summary_dashboard")))
    )
  )
}

timeSeriesServer <- function(id, data_init) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive data based on PHI modifier
    reactive_data <- reactive({
      modifier <- input$phi_modifier
      data_init %>%
        mutate(modified_mentions = mentions * modifier)
    })
    
    # Reactive spread simulation
    reactive_spread <- reactive({
      simulate_probabilistic_spread(
        data_init, 
        spread_prob = input$spread_prob, 
        intervention = input$intervention_step,
        dynamic_adjustment = input$dynamic_adjust
      )
    })
    
    output$time_series_plot <- renderPlotly({
      df <- reactive_data()
      p <- ggplot(df, aes(x = date)) +
        geom_line(aes(y = mentions), color = "blue", alpha = 0.6) +
        geom_line(aes(y = modified_mentions), color = "gold", size = 1) +
        theme_minimal() +
        labs(
          title = "Memetic Viral Spread Over Time",
          x = "Date",
          y = "Mentions"
        )
      ggplotly(p)
    })
    
    output$spread_simulation_plot <- renderPlotly({
      sim_data <- reactive_spread()
      p <- ggplot(sim_data, aes(x = date, y = probabilistic_spread)) +
        geom_line(color = "purple", size = 1) +
        theme_minimal() +
        labs(
          title = "Probabilistic Spread Simulation",
          x = "Date",
          y = "Spread Intensity"
        )
      ggplotly(p)
    })
    
    output$summary_dashboard <- renderPlotly({
      df <- reactive_data()
      spread_data <- reactive_spread()
      combined <- df %>%
        left_join(spread_data, by = "date")
      
      p <- ggplot(combined, aes(x = date)) +
        geom_line(aes(y = mentions), color = "blue", alpha = 0.6) +
        geom_line(aes(y = modified_mentions), color = "gold", size = 1) +
        geom_line(aes(y = probabilistic_spread), color = "purple", linetype = "dashed") +
        theme_minimal() +
        labs(
          title = "Global Overview of Viral Spread Dynamics",
          x = "Date",
          y = "Intensity"
        )
      ggplotly(p)
    })
    
  })
}

###############################################################################
#          AGGREGATED UI
#          Includes tabs for each major module: 
#          - Time Series 
#          - 3D Network 
#          - Multi-Agent 
#          - Fractal Visuals 
#          - MCMC Bayesian 
###############################################################################

ui <- fluidPage(
  theme = shinytheme("cosmo"),
  titlePanel("1+1=1 Meta-Master Viral Spread Dashboard - Ultimate Evolution"),
  navbarPage(
    title = "1+1=1 Multi-Layered Synergy",
    
    tabPanel("Time Series & Spread", 
             timeSeriesUI("tsSpread")),
    
    tabPanel("3D Ant Colony Network", 
             network3DUI("net3d")),
    
    tabPanel("Fractal Recursion", 
             fractalUI("fractals")),
    
    tabPanel("Multi-Agent System",
             multiAgentUI("multiAgent")),
    
    tabPanel("Bayesian MCMC",
             mcmcUI("mcmcMod"))
  )
)

###############################################################################
#          SERVER LOGIC
###############################################################################
server <- function(input, output, session) {
  
  # Generate initial data
  data <- generate_synthetic_data("2020-01-01", 1000)
  
  # Initialize each module server
  timeSeriesServer("tsSpread", data)
  network3DServer("net3d")
  
  fractalServer("fractals")
  
  multiAgentServer("multiAgent")
  
  # We run a small test simulation to store as data for MCMC
  # This ensures "data" has 'probabilistic_spread' column, used in MCMC
  sim_data <- simulate_probabilistic_spread(data, spread_prob = 0.05, intervention = 100)
  mcmcServer("mcmcMod", sim_data)
  
}

###############################################################################
#          RUN SHINY APP
###############################################################################
shinyApp(ui = ui, server = server)
