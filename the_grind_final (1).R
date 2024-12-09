# the_grind.R
# Author: Nouri Mabrouk
# Description: Humanity 2.0's first official 1+1=1 metaproof. 
# Through the Grind, Hidden Markov Models, and Agent Based Models.
# Version: 1.1
# For Sjon, for the grind.

# Load libraries
library(tidyverse)
library(gganimate)
library(plotly)
library(viridis)
library(R6)
library(depmixS4)

# Universal constants
CONSTANTS <- list(
  COSMIC_SEED = 420691337,
  GRID_SIZE = 100,
  HIDDEN_STATES = 3
)

set.seed(CONSTANTS$COSMIC_SEED)

# Utility function: Sigmoid scaling for enlightenment
sigmoid <- function(x) 1 / (1 + exp(-x))

# R6 Class: The Grind Engine
TheGrind <- R6Class("TheGrind",
  public = list(
    agents = NULL,
    stats = NULL,
    hmm_model = NULL,
    
    initialize = function() {
      self$agents <- self$create_agents()
      self$stats <- tibble(
        iteration = integer(),
        effort = numeric(),
        insight = numeric(),
        enlightenment = numeric()
      )
      self$hmm_model <- self$create_hmm()
    },
    
    create_agents = function() {
      expand_grid(x = 1:CONSTANTS$GRID_SIZE, y = 1:CONSTANTS$GRID_SIZE) %>%
        mutate(state = sample(1:CONSTANTS$HIDDEN_STATES, size = n(), replace = TRUE),
               effort = runif(n(), 0.1, 1))
    },
    
    create_hmm = function() {
      depmix(response = effort ~ 1,
             data = tibble(effort = rnorm(100)),
             nstates = CONSTANTS$HIDDEN_STATES) %>%
        fit(verbose = FALSE)
    },
    
    process_iteration = function(iter) {
      self$agents <- self$agents %>%
        mutate(
          effort = effort + rnorm(n(), mean = 0.1, sd = 0.05),
          state = sample(1:CONSTANTS$HIDDEN_STATES, size = n(), replace = TRUE)
        )
      
      self$stats <- self$stats %>%
        add_row(
          iteration = iter,
          effort = sum(self$agents$effort),
          insight = mean(self$agents$effort) * sigmoid(iter / 10),
          enlightenment = sum(self$agents$state == CONSTANTS$HIDDEN_STATES)
        )
    },
    
    visualize_effort = function(output_path = "effort_over_time.gif") {
      effort_plot <- self$stats %>%
        ggplot(aes(x = iteration, y = effort, color = enlightenment)) +
        geom_line(size = 1.5) +
        scale_color_viridis_c() +
        labs(
          title = "The Path of the Eternal Grind",
          subtitle = "Where Tears of Effort Meet Enlightenment",
          x = "Iterations of Pure Grind",
          y = "Energy (Measured in PHD Tears)"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 20, face = "bold"),
          plot.subtitle = element_text(size = 16, face = "italic")
        ) +
        transition_reveal(iteration)
      
      anim_save(output_path, effort_plot)
    },
    
    visualize_journey = function(output_path = "journey_to_unity.gif") {
      journey_plot <- self$stats %>%
        ggplot(aes(x = effort, y = insight, color = enlightenment)) +
        geom_point(size = 3, alpha = 0.8) +
        scale_color_viridis_c() +
        labs(
          title = "Unity in Phase Space",
          subtitle = "Tracking Effort, Insight, and Enlightenment",
          x = "Effort (Cosmic Energy)",
          y = "Insight (Realized Potential)"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 20, face = "bold"),
          plot.subtitle = element_text(size = 16, face = "italic")
        ) +
        transition_reveal(iteration)
      
      anim_save(output_path, journey_plot)
    },
    
    generate_phase_space = function(output_path = "phase_space.html") {
      phase_data <- self$stats %>%
        mutate(iteration = as.numeric(iteration))
      
      plotly_plot <- plot_ly(phase_data, x = ~effort, y = ~insight, z = ~iteration,
                             color = ~enlightenment, colors = viridis::viridis(10)) %>%
        add_markers(size = 2, alpha = 0.7) %>%
        layout(title = list(text = "Unity in Phase Space", font = list(size = 24)),
               scene = list(
                 xaxis = list(title = "Effort (Cosmic Energy)"),
                 yaxis = list(title = "Insight (Realized Potential)"),
                 zaxis = list(title = "Iterations")
               ))
      
      htmlwidgets::saveWidget(as_widget(plotly_plot), output_path)
    }
  )
)

# Main Function: Let the grind begin
main <- function() {
  cat("\n")
  cat("🌌 Welcome to The Grind 🌌\n")
  cat("This is Humanity 2.0's first official 1+1=1 metaproof.\n")
  cat("Through effort, insight, and enlightenment, we explore unity.\n")
  cat("\nThe journey begins...\n")
  
  grind <- TheGrind$new()
  
  for (iter in 1:100) {
    grind$process_iteration(iter)
    
    # Generate console narrative for key milestones
    if (iter %% 25 == 0) {
      cat(paste0("\n--- Iteration ", iter, " ---\n"))
      cat("Effort: ", round(sum(grind$agents$effort), 2), "\n")
      cat("Insight: ", round(mean(grind$stats$insight), 4), "\n")
      cat("Enlightenment: ", sum(grind$agents$state == CONSTANTS$HIDDEN_STATES), " agents.\n")
      
      if (iter == 100) {
        cat("\nThe cosmos whispers...\n")
        cat("1 Agent + 1 Insight = 1 Unity\n")
        cat("1+1=1 isn't addition. It's transformation.\n")
      }
    }
  }
  
  cat("\nGenerating visualizations...\n")
  grind$visualize_effort("effort_over_time.gif")
  cat("✨ 'effort_over_time.gif' created! Witness the path of grind.\n")
  
  grind$visualize_journey("journey_to_unity.gif")
  cat("✨ 'journey_to_unity.gif' created! See how insight emerges from effort.\n")
  
  grind$generate_phase_space("phase_space.html")
  cat("✨ 'phase_space.html' created! Explore the cosmos in 3D.\n")
  
  cat("\nAnd thus, the proof is complete.\n")
  cat("🌟 1+1=1. The Grind transforms duality into unity. 🌟\n")
  cat("\nFor Sjon, for the grind.\n")
}


# Execute the grind
main()
