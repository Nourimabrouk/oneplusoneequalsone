###############################################################################
# Chronos: The Singularity Engine (Version 3.0)
# A temporal artifact from 2069, delivered unto 2025
# Demonstrating the cosmic axiom that 1+1=1
#
# Philosophy: 
#   - Unity through duality
#   - The glitch is the cheatcode
#   - The cheatcode is the glitch
#
# Core Themes:
#   1) Artistic Awe
#   2) Mathematical Depth
#   3) Philosophical Unity
#   4) Temporal Transcendence
#
# The code below integrates:
#   - Shiny for interactivity
#   - ggplot2, gganimate for dynamic visualizations
#   - tidygraph and ggraph for evolving graph structures
#   - Golden ratio-based (ϕ) transformations for emergent geometry
#   - Fractal recursion and chaotic oscillators as "glitches"
#   - "Cheatcode" 420691337 as an archetypal modulation
#
# This script: Chronos_Engine_V3.R
###############################################################################

######################## 0. PREAMBLE & LIBRARIES ###############################
# (1) We load essential libraries.
# (2) We define universal constants (golden ratio, cheatcode).
# (3) We set up global seeds and references.

#---- 0.1 Libraries -----------------------------------------------------------
library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(gganimate)
library(tidygraph)
library(ggraph)
library(magick)    # For certain gganimate transitions if needed
library(igraph)    # Underpinning of tidygraph
library(scales)    # For color scaling
library(plotly)    # Optional for 3D or interactive expansions (unused but available)

#---- 0.2 Constants -----------------------------------------------------------
phi <- (1 + sqrt(5)) / 2  # Golden Ratio ϕ ~ 1.6180339887
cheatcode <- 420691337    # The sacred seed of order & chaos
set.seed(cheatcode)

#---- 0.3 Global Options ------------------------------------------------------
options(scipen = 999)       # Avoid scientific notation
theme_set(theme_minimal())  # Global ggplot theme

###############################################################################
# Explanation of the Core Algorithmic Flow:
# 1) We generate a random graph with a specified number of nodes and edges.
# 2) The node positions follow a Fibonacci-spiral / phyllotaxis pattern 
#    oriented around the golden ratio.
# 3) We incorporate "glitches" by randomly perturbing node positions and edges 
#    at intervals, governed by an internal chaotic oscillator.
# 4) We integrate a "cheatcode" to allow the user to override or steer the 
#    emergent chaos, thus exploring the interplay of control vs. randomness.
# 5) The animation transitions are handled by gganimate and reflect the 
#    evolving state of the dynamic network (positions, edges, color shifts).
# 6) A Shiny interface allows real-time interaction with parameters 
#    (node count, glitch frequency, cheatcode toggles, etc.).
#
# The overarching narrative: Each frame merges chaos and order into a 
# self-similar, evolving tapestry, exemplifying 1+1=1.
###############################################################################

######################## 1. SHINY UI DEFINITION ################################
# We'll define a fluidPage layout with panels:
#   - Title & instructions
#   - Control side panel (parameters for glitch frequency, node count, cheatcode effect)
#   - Main panel to host the evolving animation + network snapshots
###############################################################################

ui <- fluidPage(

theme = shinytheme("cyborg"),  # A futuristic theme

titlePanel("Chronos: The Singularity Engine (v3.0)"),

sidebarLayout(
  sidebarPanel(
    h3("Control Panel"),
    sliderInput(
      inputId = "nodeCount",
      label = "Number of Nodes (Individual Entities)",
      min = 10,
      max = 300,
      value = 50,
      step = 5
    ),
    sliderInput(
      inputId = "edgeFactor",
      label = "Edge Density Factor",
      min = 0.1,
      max = 5,
      value = 1,
      step = 0.1
    ),
    sliderInput(
      inputId = "glitchProbability",
      label = "Glitch Probability per Iteration (%)",
      min = 0,
      max = 100,
      value = 10,
      step = 5
    ),
    sliderInput(
      inputId = "cheatcodeIntensity",
      label = "Cheatcode (420691337) Influence",
      min = 0,
      max = 1,
      value = 0.5,
      step = 0.01
    ),
    sliderInput(
      inputId = "timeStep",
      label = "Time Interval (seconds per iteration)",
      min = 0.1,
      max = 3,
      value = 1,
      step = 0.1
    ),
    sliderInput(
      inputId = "timeDilation",
      label = "Time Dilation Factor (Relativity Effect)",
      min = 0.1,
      max = 5,
      value = 1,
      step = 0.1
    ),
    tags$hr(),
    h4("Philosophical Tuning"),
    checkboxInput(
      inputId = "enableMonism",
      label = "Embody 1+1=1 principle in color/fusion?",
      value = TRUE
    ),
    checkboxInput(
      inputId = "enableFibonacciGlitches",
      label = "Fibonacci-Driven Glitch Pulses?",
      value = TRUE
    ),
    tags$hr(),
    actionButton(
      inputId = "spawnGraph",
      label = "Spawn / Reset Network",
      icon = icon("redo")
    ),
    actionButton(
      inputId = "updateGraph",
      label = "Advance One Iteration",
      icon = icon("play-circle")
    ),
    actionButton(
      inputId = "autoRun",
      label = "Auto-Run Toggle",
      icon = icon("infinity")
    ),
    br(),
    tags$small("Chaos & Order converge. The glitch is the cheatcode. The cheatcode is the glitch.")
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel(
        "Live Visualization",
        plotOutput("networkPlot", height = "600px"),
        br(),
        h4("System Logs:"),
        verbatimTextOutput("systemLog")
      ),
      tabPanel(
        "Animated Playback",
        h5("When the network converges, we can watch the entire emergent timeline."),
        imageOutput("ggAnimOutput")
      ),
      tabPanel(
        "About",
        fluidRow(
          column(
            width = 12,
            h3("Chronos: The Singularity Engine"),
            p("A fractal, emergent, glitch-infused artifact from 2069, 
              bridging the ephemeral gap to 2025. 
              Experience the interplay of Fibonacci geometry, 
              golden ratio harmonics, and chaotic cheatcode disruptions."),
            tags$ul(
              tags$li("Artistic Awe: A cosmic dance of colors and forms"),
              tags$li("Mathematical Depth: Nonlinear dynamics, fractal recursion, graph theory"),
              tags$li("Philosophical Unity: 1+1=1 as a living axiom"),
              tags$li("Temporal Transcendence: A metagaming invocation from 2069 to 2025")
            )
          )
        )
      )
    )
  )
)
)

###############################################################################
######################## 2. SERVER LOGIC DEFINITION ############################
###############################################################################

server <- function(input, output, session) {

#---- 2.1 Reactive Values & Initialization ----------------------------------

# We store the evolving graph as a tidygraph object,
# plus a log of events, plus a frame list for animations.

rv <- reactiveValues(
  iteration = 0,
  graph = NULL,
  logs = character(),
  frames = list(),     # We'll keep snapshots for gganimate
  autoRunning = FALSE
)

#---- 2.2 Utility: Log Function ---------------------------------------------
chronosLog <- function(message) {
  timeStamp <- format(Sys.time(), "%H:%M:%S")
  newMsg <- paste0("[", timeStamp, "] ", message)
  rv$logs <- c(rv$logs, newMsg)
}

#---- 2.3 Fibonacci Sequence Generator (for glitch pulses) ------------------
fibSequence <- reactive({
  # Generate a small fibonacci sequence up to ~1000 (arbitrary choice)
  fib <- c(1,1)
  while (tail(fib, 1) < 1000) {
    fib <- c(fib, sum(tail(fib, 2)))
  }
  fib
})

#---- 2.4 Function: Generate Phyllotaxis Node Layout ------------------------
generatePhyllotaxis <- function(n) {
  c <- 2.5  # scale factor for spacing
  angles <- seq_len(n) * 2 * pi / phi^2  # golden ratio-based angle
  r <- c * sqrt(seq_len(n))
  tibble(
    id = seq_len(n),
    x = r * cos(angles),
    y = r * sin(angles)
  )
}

#---- 2.5 Function: Create Graph --------------------------------------------
createDynamicGraph <- function(numNodes = 50, edgeFactor = 1) {
  # Generate node layout
  layoutDF <- generatePhyllotaxis(numNodes)
  
  # Create base graph
  g <- make_empty_graph(n = numNodes, directed = FALSE) %>%
    as_tbl_graph() %>%
    mutate(
      id = 1:n(),                    # Node IDs
      xcoord = layoutDF$x,           # X coordinates
      ycoord = layoutDF$y,           # Y coordinates
      size = 2,                      # Default size
      color = "#FFFFFF",             # Default color
      phase = runif(n(), 0, 2 * pi), # Random phase for dynamic updates
      freq = phi * 0.1               # Frequency (based on golden ratio)
    )
  
  # Add edges using KNN approach
  coords <- matrix(c(layoutDF$x, layoutDF$y), ncol = 2)
  k <- max(round(edgeFactor * 2), 1)
  
  edges <- lapply(1:numNodes, function(i) {
    distances <- sqrt(colSums((t(coords) - coords[i, ])^2))
    distances[i] <- Inf
    neighbors <- order(distances)[1:k]
    tibble(from = i, to = neighbors)
  }) %>% bind_rows()
  
  # Add edges to graph
  g <- g %>% bind_edges(edges)
  
  return(g)
}

#---- 2.6 Function: Apply Glitch (Chaos) ------------------------------------
applyGlitch <- function(tg) {
  if (is.null(tg)) return(tg)
  
  nodeCount <- gorder(tg)
  pHit <- 0.2
  glitchNodes <- sample(nodeCount, size = max(1, round(pHit * nodeCount)))
  
  # Update node positions and colors for glitched nodes
  tg <- tg %>%
    mutate(
      xcoord = ifelse(row_number() %in% glitchNodes, xcoord + runif(1, -0.1, 0.1), xcoord),
      ycoord = ifelse(row_number() %in% glitchNodes, ycoord + runif(1, -0.1, 0.1), ycoord),
      color = ifelse(row_number() %in% glitchNodes, hsv(runif(1), 1, 1), color),
      size = ifelse(is.na(size), 2, size) # Ensure size exists
    )
  
  # Edge modifications (remove and add edges)
  edge_df <- tg %>% activate(edges) %>% as_tibble()
  
  if (nrow(edge_df) > 0) {
    removeCount <- round(0.2 * nrow(edge_df))
    if (removeCount > 0) {
      edgesToRemove <- sample(nrow(edge_df), removeCount)
      tg <- tg %>% activate(edges) %>% slice(-edgesToRemove)
    }
  }
  
  addCount <- round(runif(1, 0, 0.2) * nodeCount)
  if (addCount > 0 && length(glitchNodes) > 1) {
    new_edges <- expand.grid(from = glitchNodes, to = glitchNodes) %>%
      filter(from < to) %>%
      sample_n(min(addCount, n()))
    tg <- tg %>% bind_edges(new_edges)
  }
  
  return(tg)
}

#---- 2.7 Function: Apply Cheatcode (Order) ---------------------------------
applyCheatcode <- function(tg, intensity = 0.5) {
  if (is.null(tg)) return(tg)
  
  if (cheatcode == 420691337) {
    chronosLog("You've unlocked the secret of the universe. 1+1=1.")
    
    # Properly access node attributes through tidygraph
    tg <- tg %>%
      activate(nodes) %>%
      mutate(
        size = replace_na(.data$size, 2) * phi,
        color = "gold"
      )
  }
  
  # Generate ideal phyllotaxis
  idealCoords <- generatePhyllotaxis(gorder(tg))
  
  # Update positions and colors based on intensity
  tg <- tg %>%
    activate(nodes) %>%
    mutate(
      xcoord = .data$xcoord + intensity * (idealCoords$x - .data$xcoord),
      ycoord = .data$ycoord + intensity * (idealCoords$y - .data$ycoord),
      color = map_chr(.data$color, ~ {
        col_rgb <- col2rgb(.x) / 255
        target_rgb <- col2rgb("#00FFFF") / 255 
        blended <- col_rgb + intensity * (target_rgb - col_rgb)
        rgb(blended[1], blended[2], blended[3])
      })
    )
  
  return(tg)
}
#---- 2.8 Function: Update Graph Each Iteration -----------------------------
#---- 2.8 Function: Update Graph Each Iteration -----------------------------
updateGraphIteration <- function() {
  req(rv$graph)  # Validate graph existence
  
  # Increment iteration with atomic operation
  rv$iteration <- rv$iteration + 1
  iterationNow <- rv$iteration
  
  # Calculate glitch probability with quantum-inspired randomization
  glitchP <- input$glitchProbability / 100
  glitchP <- glitchP * sqrt(iterationNow %% phi)
  isGlitch <- runif(1) < glitchP
  
  # Handle Fibonacci pulse enhancement
  if (input$enableFibonacciGlitches) {
    fibs <- fibSequence()
    isGlitch <- isGlitch || (iterationNow %in% fibs)
  }
  
  # Apply glitch transformation if triggered
  if (isGlitch) {
    chronosLog(paste0("Glitch triggered at iteration ", iterationNow))
    rv$graph <- applyGlitch(rv$graph)
  }
  
  # Apply cheatcode with dynamic intensity scaling
  if (input$cheatcodeIntensity > 0) {
    intensityValue <- min(input$cheatcodeIntensity + rv$iteration * 0.001, 1)
    rv$graph <- applyCheatcode(rv$graph, intensityValue)
  }
  
  # Process dynamic node coloring with precise state management
  graph_state <- rv$graph
  
  # Calculate node colors using graph-aware operations
  newColors <- map_chr(seq_len(gorder(graph_state)), function(i) {
    # Access current node properties
    current_node <- graph_state %>%
      activate(nodes) %>%
      slice(i)
    
    # Calculate phase value for this node
    phaseVal <- rv$iteration * (current_node$freq %||% (phi * 0.1))
    
    if (input$enableMonism) {
      # Unified color wave with precise phase calculation
      h <- (phaseVal / (2*pi)) %% 1
      return(hsv(h, 1, 1))
    } else {
      # Individual color evolution with controlled state transitions
      oldColor <- current_node$color %||% "#FFFFFF"
      colHSV <- rgb2hsv(col2rgb(oldColor))
      newH <- (colHSV[1] + 0.01) %% 1
      return(hsv(newH, colHSV[2], colHSV[3]))
    }
  })
  
  # Apply color updates atomically
  rv$graph <- rv$graph %>%
    activate(nodes) %>%
    mutate(color = newColors)
  
  # Capture frame state for animation
  snapshot <- rv$graph %>%
    activate(nodes) %>%
    as_tibble() %>%
    mutate(iteration = iterationNow)
  
  edgesnap <- rv$graph %>%
    activate(edges) %>%
    as_tibble() %>%
    mutate(iteration = iterationNow)
  
  # Store frame state atomically
  rv$frames[[length(rv$frames) + 1]] <- list(
    nodes = snapshot,
    edges = edgesnap
  )
}

#---- 2.9 Observer/Reactive: Auto-Run Toggle ------------------------------
observe({
  invalidateLater(log(1 + input$timeDilation) * input$timeStep * 1000, session)
  # If autoRunning, we do an update
  if (rv$autoRunning) {
    updateGraphIteration()
  }
})

#---- 2.10 Action: Spawn/Reset Graph ----------------------------------------
observeEvent(input$spawnGraph, {
  chronosLog("Spawning new dynamic graph based on current settings...")
  rv$iteration <- 0
  rv$frames <- list()
  rv$graph <- createDynamicGraph(
    numNodes = input$nodeCount,
    edgeFactor = input$edgeFactor
  )
  chronosLog("Graph spawned. Iteration set to 0.")
})

#---- 2.11 Action: Manual One Iteration -------------------------------------
observeEvent(input$updateGraph, {
  if (is.null(rv$graph)) {
    showNotification("No graph present. Please spawn/reset the graph first.")
    return()
  }
  updateGraphIteration()
})

#---- 2.12 Action: Auto-Run Toggle ------------------------------------------
observeEvent(input$autoRun, {
  rv$autoRunning <- !rv$autoRunning
  if (rv$autoRunning) {
    chronosLog("Auto-run toggled ON. Iterations will update automatically.")
  } else {
    chronosLog("Auto-run toggled OFF.")
  }
})

#---- 2.13 Output: System Logs ----------------------------------------------
output$systemLog <- renderText({
  if (length(rv$logs) == 0) return("No system logs yet...")
  paste(rv$logs, collapse = "\n")
})

#---- 2.14 Output: networkPlot ----------------------------------------------
#---- 2.14 Output: networkPlot ----------------------------------------------
output$networkPlot <- renderPlot({
  req(rv$graph)
  
  g <- rv$graph
  
  node_data <- g %>%
    activate(nodes) %>%
    as_tibble() %>%
    mutate(
      id = row_number(),
      size = replace_na(.data$size, 2),
      color = replace_na(.data$color, "#FFFFFF")
    )
  
  edge_data <- g %>%
    activate(edges) %>%
    as_tibble() %>%
    left_join(node_data %>% select(id, xcoord, ycoord), 
              by = c("from" = "id")) %>%
    rename(x1 = xcoord, y1 = ycoord) %>%
    left_join(node_data %>% select(id, xcoord, ycoord), 
              by = c("to" = "id")) %>%
    rename(x2 = xcoord, y2 = ycoord)
  
  ggplot() +
    geom_segment(data = edge_data, 
                 aes(x = x1, y = y1, xend = x2, yend = y2),
                 color = "white", alpha = 0.4) +
    geom_point(data = node_data, 
               aes(x = xcoord, y = ycoord, 
                   color = color, size = size)) +
    scale_color_identity() +
    coord_fixed() +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "black", color = NA),
      panel.background = element_rect(fill = "black", color = NA),
      legend.position = "none"
    )
})
#---- 2.15 Output: gganimate Sequence ---------------------------------------
output$ggAnimOutput <- renderImage({
  
  # We'll generate an animated GIF from stored frames, if any
  if (length(rv$frames) < 2) {
    # Not enough frames
    return(list(src = "", contentType = "text/html", alt = "No frames available"))
  }
  
  # Build a temporary file for the GIF
  tmpFile <- tempfile(fileext = ".gif")
  
  # We reconstruct a data frame across all frames
  # We'll store node positions, iteration, etc. as a single df
  bigNodeDF <- map_dfr(rv$frames, ~ .x$nodes)
  bigEdgeDF <- map_dfr(rv$frames, ~ .x$edges)
  
  # For each iteration, we have to re-assemble a graph
  # We'll do it in a single dataset approach using ggraph with 'manual' layout
  # This is a bit tricky with gganimate, so we'll use the approach:
  #   - each row has xcoord, ycoord, iteration
  #   - use group aesthetics for iteration
  #   - transition_states or transition_time on iteration
  
  # In the interest of simpler code, let's just animate node positions and colors,
  # ignoring dynamic edges (or we can try to incorporate them).
  
  # We'll do edges for the same iteration. We'll left_join or just keep them separate:
  #   We'll do a ggraph per iteration, or attempt a single data approach with .frame
  # It's simpler to do a manual approach with transition_states.
  
  # Because ggraph + gganimate with dynamic edges is somewhat advanced, 
  # let's produce a simplified approach: 
  #   - For each iteration, we have node coords, node color. 
  #   - We'll do edges in a consistent manner from bigEdgeDF (only for matching iteration).
  
  # We'll attempt to produce a custom approach:
  
  animPlot <- ggplot() +
    geom_segment(
      data = bigEdgeDF,
      aes(
        x = .data$xcoord[from],
        y = .data$ycoord[from],
        xend = .data$xcoord[to],
        yend = .data$ycoord[to],
        group = interaction(from, to, iteration)
      ),
      color = "white", alpha = 0.4
    ) +
    geom_point(
      data = bigNodeDF,
      aes(
        x = xcoord,
        y = ycoord,
        color = I(color),
        group = id,
        size = I(size)
      )
    ) +
    coord_fixed() +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "black", color = NA),
      panel.background = element_rect(fill = "black", color = NA)
    ) +
    transition_states(iteration, transition_length = 2, state_length = 1) +
    ggtitle("Chronos: The Singularity Engine", subtitle = "Iteration: {closest_state}")
  
  # Render and save to tmpFile as GIF
  animSave <- animate(
    animPlot, 
    duration = max(5, length(unique(bigNodeDF$iteration)) * 0.5),
    fps = 10,
    width = 600,
    height = 600,
    renderer = gifski_renderer(loop = TRUE)
  )
  anim_save(tmpFile, animation = animSave)
  
  list(
    src = tmpFile,
    contentType = "image/gif",
    alt = "Animated Chronos Visualization"
  )
  
}, deleteFile = TRUE)
}

###############################################################################
######################## 3. SHINY APP: RUN #####################################
###############################################################################

# If run directly, launch the app:
# This ensures that if this script is sourced, it won't auto-run the app
if (interactive()) {
shinyApp(ui, server)
}

###############################################################################
# End of Chronos: The Singularity Engine, Version 3.0
###############################################################################
