# Load the tidyverse: the cosmic foundation of harmony in R
library(tidyverse)

# Define constants for universal resonance
phi <- (1 + sqrt(5)) / 2  # Golden ratio
frames <- 500             # Number of frames for the evolution

# Generate the data: 1+1=1 in action, a unifying spiral
void_1_1 <- tibble(
  frame = seq(1, frames),
  theta = seq(0, 2 * pi * phi, length.out = frames),   # Spiral angles
  r = exp(seq(0, 2, length.out = frames))             # Exponential growth
) %>%
  mutate(
    x = r * cos(theta),                               # X-coordinates
    y = r * sin(theta),                               # Y-coordinates
    z = sin(phi * theta),                             # Z-coordinates: the depth of the void
    color = scales::rescale(frame, to = c(0, 1))      # Color gradient
  )

# Cosmic visualization function
generate_unity_plot <- function(data, save_as_gif = FALSE) {
  # Plot the evolving 1+1=1 visual
  plot <- data %>%
    ggplot(aes(x = x, y = y, color = color, alpha = color)) +
    geom_path(size = 1) +                              # Spiral as a path
    scale_color_gradient(low = "#ff007f", high = "#00ffff") + # Plasma-like colors
    scale_alpha(range = c(0.2, 1)) +                   # Fade to create depth
    theme_void() +                                     # Embrace the void
    theme(
      plot.background = element_rect(fill = "black", color = NA),
      legend.position = "none"
    ) +
    coord_equal() +
    ggtitle("1 + 1 = 1: The Unity Spiral")
  
  # Render or save the animation
  if (save_as_gif) {
    library(gganimate) # For cosmic evolution
    anim <- plot +
      transition_reveal(frame) +
      labs(subtitle = "Frame: {frame}")
    anim_save("void_1_1.gif", anim)
    message("Animation saved as void_1_1.gif")
  } else {
    print(plot)
  }
}

# Generate the plot or animate
generate_unity_plot(void_1_1, save_as_gif = FALSE)  # Set to TRUE to save as GIF
