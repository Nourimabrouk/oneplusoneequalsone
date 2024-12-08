# Libraries ----
library(tidyverse)
library(ggplot2)
library(glue)
library(pracma)      # For Phi and fractal magic
library(ggthemes)    # Beautiful themes for ggplot2

# Constants ----
phi <- (1 + sqrt(5)) / 2  # Golden Ratio
levels_of_love <- 10      # Set recursion levels for visual madness

# Recursive Fractal Generator ----
generate_fractal <- function(x, level) {
  if (level <= 1) {
    return(sin(phi * x))
  }
  x + generate_fractal(phi * x, level - 1)
}

# Data Creation ----
generate_fractal_data <- function(level = levels_of_love) {
  tibble(
    x = seq(-pi, pi, length.out = 1000),
    y = map_dbl(x, ~generate_fractal(.x, level))
  )
}

# Aesthetics ----
theme_cosmic <- function() {
  theme_minimal() +
    theme(
      plot.background = element_rect(fill = "#000428", color = NA),
      panel.background = element_rect(fill = "#000428", color = NA),
      text = element_text(color = "white"),
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5, color = "gold"),
      plot.subtitle = element_text(size = 14, color = "lightblue"),
      axis.text = element_text(color = "white"),
      axis.title = element_text(color = "cyan")
    )
}

# Visualization ----
plot_fractal <- function(level) {
  data <- generate_fractal_data(level)
  
  ggplot(data, aes(x, y)) +
    geom_line(color = "#39FF14", size = 1) +
    ggtitle(glue("Fractal of Eternal Love: Recursion Depth {level}")) +
    labs(
      subtitle = glue(
        "Phi: {round(phi, 3)} | Dimensions Explored: {round(phi ^ level, 2)}"
      ),
      x = "Time (or Chaos)",
      y = "Harmonic Vibration"
    ) +
    theme_cosmic() +
    annotate(
      "text",
      x = 0,
      y = max(data$y, na.rm = TRUE),
      label = glue("1+1=1 | Harmony Achieved"),
      size = 5,
      color = "gold"
    )
}

# The Plot ----
fractal_plot <- plot_fractal(levels_of_love)

# Saving the Plot ----
ggsave(
  filename = "fractal_love.png",
  plot = fractal_plot,
  width = 10,
  height = 6,
  dpi = 300
)

# Display ----
print(fractal_plot)
