# the_grind.R
# Author: Nouri Mabrouk
# Description: A journey through effort, enlightenment, and unity.
# Version: 1.1
# For Sjun, the grinders, and the seekers of unity. 

# Libraries for powering the journey
library(tidyverse)
library(gganimate)
library(ggplot2)
library(viridis)
library(progress)
library(R6)

# Agent-based models and stochastic state transitions
library(igraph)
library(depmixS4)

# Universal constants for the grind
CONSTANTS <- list(
  COSMIC_SEED = 420691337,  # Universal truth for RNG
  UNITY_SCALING = pi / 2,   # Scale of insight
  GRID_SIZE = 100           # ABM grid dimensions
)

set.seed(CONSTANTS$COSMIC_SEED)

# Sigmoid function for smooth transitions
sigmoid <- function(x) 1 / (1 + exp(-x))

# Class: The Grind Engine
TheGrind <- R6Class("TheGrind",
                    public = list(
                      agents = NULL,   # Agents for the ABM
                      stats = NULL,    # Tracks grind statistics
                      hmm_model = NULL, # Hidden Markov Model
                      
                      initialize = function() {
                        cat("\033[32m✔\033[0m Starting the journey of the grind...\n")
                        self$agents <- self$create_agents()
                        self$stats <- tibble(
                          iteration = integer(),
                          effort = numeric(),
                          insight = numeric(),
                          enlightenment = numeric()
                        )
                      },
                      
                      # Step 1: Create agents for ABM
                      create_agents = function() {
                        expand_grid(x = 1:CONSTANTS$GRID_SIZE, y = 1:CONSTANTS$GRID_SIZE) %>%
                          mutate(state = sample(1:3, size = n(), replace = TRUE),  # Grind states
                                 effort = runif(n(), 0.1, 1))                      # Effort levels
                      },
                      
                      # Step 2: Process a single iteration of the grind
                      process_iteration = function(iter) {
                        # Simulate agent effort dynamics
                        self$agents <- self$agents %>%
                          rowwise() %>%
                          mutate(
                            effort = effort + rnorm(1, mean = 0.1, sd = 0.05),  # Random effort boost
                            state = sample(1:3, size = 1)                      # Random state transitions
                          )
                        
                        # Collect statistics from the ABM
                        effort_sum <- sum(self$agents$effort)
                        insight_sum <- mean(self$agents$effort) * sigmoid(iter / 10)
                        enlightened_agents <- sum(self$agents$state == 3)
                        
                        self$stats <- self$stats %>%
                          add_row(
                            iteration = iter,
                            effort = effort_sum,
                            insight = insight_sum,
                            enlightenment = enlightened_agents
                          )
                        
                        # Console output for key moments
                        if (iter %% 25 == 0) {
                          cat(glue::glue(
                            "\033[33mIteration {iter}\033[0m: Total Effort = {round(effort_sum, 2)}, ",
                            "Insights = {round(insight_sum, 2)}, Enlightened Agents = {enlightened_agents}\n"
                          ))
                        }
                      },
                      
                      # Step 3: Visualize the grind
                      visualize = function(output_path = "grind_visuals") {
                        dir.create(output_path, showWarnings = FALSE)
                        
                        # Effort and Enlightenment Plot
                        effort_plot <- self$stats %>%
                          ggplot(aes(iteration, effort, color = enlightenment)) +
                          geom_line(size = 1) +
                          scale_color_viridis_c() +
                          labs(
                            title = "The Path of the Eternal Grind",
                            subtitle = "Where Effort Meets Enlightenment",
                            x = "Iterations",
                            y = "Energy (Effort)",
                            color = "Enlightenment"
                          ) +
                          theme_minimal() +
                          transition_reveal(iteration)
                        
                        # Save the animation
                        gganimate::anim_save(file.path(output_path, "effort_over_time.gif"), effort_plot)
                        cat("\033[32m✔ Visualization complete: effort_over_time.gif saved.\033[0m\n")
                      }
                    )
)

# Main function: The journey begins
main <- function() {
  cat("\033[34m The Grind Begins...\033[0m\n")
  
  # Initialize the grind engine
  grind <- TheGrind$new()
  
  # Process 100 iterations of the grind
  walk(1:100, ~grind$process_iteration(.x))
  cat("\033[34m Grinding...\033[0m\n")
  cat("\033[34m Grinding...\033[0m\n")
  cat("\033[34m Grinding...\033[0m\n")
  
  # Visualize the grind's progress
  grind$visualize()
  
  cat("\033[34m The journey concludes. Effort transformed into unity. 1+1=1.\033[0m\n")
  cat("\033[34m Q.E.D.\033[0m\n")
  
}

# Execute the grind journey
main()
