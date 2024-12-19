# sacred_geometry_2069.R
# Optimized visualization system with multiple output formats

if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  tidyverse,
  ggforce,
  plotly,
  ragg,     # High-quality raster output
  gganimate, # For animation
  htmlwidgets,
  processx,  # For system commands
  pracma,
  complexplus
)

# Constants remain the same as before...
PHI <- (1 + sqrt(5)) / 2
PI2 <- 2 * pi
GOLDEN_ANGLE <- PI2 * (1 - 1/PHI)

# Core geometric functions remain the same...
# [Previous geometric functions here]

#' Enhanced visualization output system
#' @param plot ggplot object to render
#' @param filename Base filename without extension
#' @param format Output format: "png", "html", "gif", or "all"
#' @param width Output width in pixels
#' @param height Output height in pixels
#' @param bg Background color
#' @return Path to generated file(s)
save_sacred_visualization <- function(
    plot,
    filename = "sacred_unity_2069",
    format = "html",
    width = 1200,
    height = 1200,
    bg = "black"
) {
  format <- match.arg(format, c("png", "html", "gif", "all"))
  
  # Base configuration
  dpi <- 300
  scale <- 2
  
  outputs <- list()
  
  if (format %in% c("png", "all")) {
    # High-quality PNG output using ragg
    png_path <- str_glue("{filename}.png")
    agg_png(
      png_path,
      width = width * scale,
      height = height * scale,
      res = dpi,
      bg = bg
    )
    print(plot)
    dev.off()
    outputs$png <- png_path
  }
  
  if (format %in% c("html", "all")) {
    # Interactive HTML using plotly
    interactive_plot <- ggplotly(plot) %>%
      layout(
        paper_bgcolor = bg,
        plot_bgcolor = bg,
        font = list(color = "white"),
        margin = list(t = 50, r = 50, b = 50, l = 50)
      ) %>%
      config(
        displayModeBar = FALSE,
        scrollZoom = TRUE
      )
    
    html_path <- str_glue("{filename}.html")
    saveWidget(
      interactive_plot,
      html_path,
      selfcontained = TRUE,
      background = bg
    )
    outputs$html <- html_path
  }
  
  if (format %in% c("gif", "all")) {
    # Animated GIF output
    animated_plot <- plot +
      transition_states(
        states = 1:10,
        transition_length = 2,
        state_length = 1
      ) +
      enter_fade() +
      exit_fade()
    
    gif_path <- str_glue("{filename}.gif")
    anim_save(
      gif_path,
      animated_plot,
      width = width,
      height = height,
      bg = bg
    )
    outputs$gif <- gif_path
  }
  
  # Provide clear feedback
  message("Generated outputs:")
  walk2(outputs, names(outputs), ~message(str_glue("- {.y}: {.x}")))
  
  # Try to open the file if on an interactive system
  if (interactive()) {
    tryCatch({
      if (format == "html") {
        browseURL(outputs$html)
      } else if (format == "png") {
        system2("open", outputs$png)
      }
    }, error = function(e) {
      message("Note: Could not automatically open the file.")
      message("You can find the outputs at the paths listed above.")
    })
  }
  
  invisible(outputs)
}

# Main execution function with enhanced output options
create_sacred_unity_visualization <- function(
    output_format = "html",
    filename = "sacred_unity_2069",
    width = 1200,
    height = 1200,
    complexity = 2000
) {
  # Generate the unified pattern
  message("⚡ Generating unity pattern...")
  unity_data <- generate_unity_pattern(complexity)
  
  # Create the visualization
  message("🎨 Creating visualization...")
  unity_plot <- plot_unified_geometry(
    unity_data,
    "Sacred Geometry: Where 1+1=1"
  )
  
  # Save with enhanced output system
  message("💾 Generating output...")
  save_sacred_visualization(
    unity_plot,
    filename = filename,
    format = output_format,
    width = width,
    height = height
  )
}

# Execute with your preferred format
create_sacred_unity_visualization(
  output_format = "all",  # Creates PNG, HTML, and GIF versions
  filename = "sacred_unity_2069",
  width = 1200,
  height = 1200
)