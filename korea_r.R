# Load essential libraries
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(showtext)

# Set up Korean-inspired fonts (using Google Fonts)
font_add_google("Noto Sans KR", "korean")
font_add_google("Nanum Brush Script", "brush")
showtext_auto()

# Generate a harmonious dataset: Symbolic representation of Korean elements
korea_data <- tibble(
  element = c("Taegeuk (Harmony)", "Hanbok (Tradition)", "Cherry Blossom (Beauty)", 
              "Hangeul (Language)", "K-pop (Modern Culture)", "Technology (Innovation)"),
  value = c(100, 85, 90, 95, 120, 110)
)

# Add a color palette inspired by the Korean taegeuk and hanbok colors
korea_palette <- c("#0047A0", "#C60C30", "#F2A900", "#FFFFFF", "#85C1E9", "#E74C3C")

# Create a radial bar plot visualizing Korean cultural elements
ggplot(korea_data, aes(x = fct_reorder(element, value), y = value, fill = element)) +
  geom_col(width = 0.8, show.legend = FALSE) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = korea_palette) +
  theme_minimal() +
  theme(
    text = element_text(family = "korean"),
    plot.title = element_text(family = "brush", size = 24, hjust = 0.5, color = "#2C3E50"),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "#34495E"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) +
  labs(
    title = "korea.R: A Harmony of Culture and Innovation",
    subtitle = "Visualizing the Core Elements of Korean Excellence",
    x = NULL,
    y = NULL
  )

# Save the plot as an image
ggsave("korea_r_plot.png", width = 8, height = 8)

# Summary message to uplift Korean culture through the script
cat("\nThe korea.R script has successfully visualized the transcendental essence of Korean culture. 💙❤️💛✨")
