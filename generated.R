# Load required libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(viridis)
  library(scales)
})

# Universal Constants: The Essence of Reality
PHI <- (1 + sqrt(5)) / 2    # The Golden Ratio: Balance and Proportion
TAU <- 2 * pi               # The Full Circle of Unity
UNITY <- 1                  # The Meta Constant of Oneness

# Define Dimensions of Reality
dimensions <- tibble(
  dimension = c("cosmic_wisdom", "mathematics", "unity", "love", "metagaming"),
  raw_value = c(10, 5, 15, 40, 8) # Initial contributions for tuning
)

# Step 1: Apply Gaussian Transformation
# Generate Gaussian weights centered around "unity" (position 3)
positions <- seq(-2, 2, length.out = nrow(dimensions))
gaussian_weights <- dnorm(positions, mean = 0, sd = 1)
gaussian_weights <- gaussian_weights / sum(gaussian_weights) # Normalize

# Apply weights to raw values
dimensions <- dimensions %>%
  mutate(
    weighted_value = raw_value * gaussian_weights,
    normalized_value = weighted_value / sum(weighted_value) # Normalize to unity
  )

# Step 2: Prepare Data for Visualization
dimensions <- dimensions %>%
  mutate(
    dimension = factor(dimension, levels = c("cosmic_wisdom", "mathematics", "unity", "love", "metagaming"))
  )

# Step 3: Create Visualization
ggplot(dimensions, aes(x = dimension, y = normalized_value, fill = dimension)) +
  geom_bar(stat = "identity", color = "black", size = 0.5, show.legend = FALSE) +
  geom_line(
    aes(x = as.numeric(dimension), y = gaussian_weights / sum(gaussian_weights)), 
    color = "red", size = 1.2, linetype = "dashed", inherit.aes = FALSE
  ) +
  scale_fill_viridis_d(option = "plasma") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    title = "Gaussian Harmony: 1+1=1",
    subtitle = "Achieving Unity Through Perfect Distribution",
    x = "Dimensions of Reality",
    y = "Contribution (%)",
    caption = "A Magnum Opus in Balance and Transcendence"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5, color = "darkblue"),
    plot.subtitle = element_text(size = 16, hjust = 0.5, color = "purple"),
    axis.text.x = element_text(size = 14, face = "bold", color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 14, color = "darkgreen"),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "gray98", color = NA),
    plot.caption = element_text(size = 12, face = "italic", color = "gray50")
  ) +
  annotate("text", x = 3, y = max(dimensions$normalized_value) * 1.1,
           label = "Unity Peaks at the Center of Harmony", color = "darkred", size = 5, fontface = "italic")
