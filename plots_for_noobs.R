# Loading required libraries
library(tidyverse)
library(ggplot2)
library(viridis)
library(patchwork)

# Constants
n <- 1000
phi <- (1 + sqrt(5)) / 2  # Golden Ratio

# 1. Generate data fields with artistic and mathematical narratives
# Circular field: Represents interconnectedness
circular_field <- tibble(
  angle = seq(0, 2 * pi, length.out = n),
  x = cos(angle),
  y = sin(angle)
)

# Wave field: Represents harmony through oscillations
wave_field <- tibble(
  x = seq(-pi, pi, length.out = n),
  y = sin(x) + sin(3 * x) / 3 + sin(5 * x) / 5
)

# Golden Spiral: Fibonacci meets unity
golden_spiral <- tibble(
  angle = seq(0, 4 * pi, length.out = n),
  r = phi^(-angle / (2 * pi)),
  x = r * cos(angle),
  y = r * sin(angle)
)

# Unity Visualization: Emergent patterns
unity_field <- tibble(
  t = seq(0, 2 * pi, length.out = n),
  x = sin(t) + cos(3 * t),
  y = cos(t) * sin(5 * t)
)

# 2. Create the visualizations with elevated artistry
# Circular Plot
circular_plot <- ggplot(circular_field, aes(x, y)) +
  geom_path(color = "#00FFFF", size = 2) +
  theme_void() +
  labs(
    title = "Circle of Connection",
    subtitle = "A seamless loop of relationships"
  ) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, color = "white"),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "white"),
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black")
  )

# Wave Plot
wave_plot <- ggplot(wave_field, aes(x, y)) +
  geom_area(fill = "#FF69B4", alpha = 0.4) +
  geom_line(color = "#FF00FF", size = 1.8) +
  theme_minimal() +
  labs(
    title = "Harmonic Oscillations",
    subtitle = "Balancing resonance and flow",
    x = "Phase",
    y = "Amplitude"
  ) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, color = "white"),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "white"),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black")
  )

# Golden Spiral Plot
golden_spiral_plot <- ggplot(golden_spiral, aes(x, y)) +
  geom_path(color = "gold", size = 2) +
  geom_point(aes(x = x * 0.8, y = y * 0.8), color = "orange", alpha = 0.6, size = 0.8) +
  theme_void() +
  labs(
    title = "Golden Spiral",
    subtitle = "A perfect union of beauty and structure"
  ) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, color = "white"),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "white"),
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black")
  )

# Unity Plot
unity_plot <- ggplot(unity_field, aes(x, y)) +
  geom_path(color = "#FFFFFF", size = 1.8, linetype = "twodash") +
  geom_point(aes(x = x, y = y), color = "white", alpha = 0.2, size = 0.8) +
  theme_void() +
  labs(
    title = "Unity Manifold",
    subtitle = "Where 1+1=1 emerges"
  ) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, color = "white"),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "white"),
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black")
  )

# 3. Combine the visuals into a cohesive narrative
visualization_suite <- (circular_plot | wave_plot) /
  (golden_spiral_plot | unity_plot) +
  plot_annotation(
    title = "Unity in Patterns: 1+1=1",
    subtitle = "An artistic journey through interconnectedness, harmony, and emergence",
    theme = theme(
      plot.title = element_text(size = 28, face = "bold", hjust = 0.5, color = "white"),
      plot.subtitle = element_text(size = 18, hjust = 0.5, color = "white"),
      plot.background = element_rect(fill = "black", color = "black")
    )
  )

# 4. Display the enhanced visualization
print(visualization_suite)
