library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(tidyverse)
library(broom)
library(plotly)

# Generate data that reflects UnityView's narrative
generate_unity_data <- function() {
  tibble(
    time = seq(Sys.time() - 3600, by = "min", length.out = 100),
    emergence = cumsum(rnorm(100, mean = 1)),
    engagement = runif(100, 100, 1000),
    breakthroughs = cumsum(sample(0:1, 100, replace = TRUE, prob = c(0.7, 0.3)))
  )
}

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "UnityView: 1+1=1 Revolution"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Emergence", tabName = "emergence", icon = icon("eye")),
      menuItem("Insights", tabName = "insights", icon = icon("brain")),
      menuItem("Community Momentum", tabName = "community", icon = icon("users")),
      menuItem("Mathematics", tabName = "math", icon = icon("calculator"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "emergence",
        fluidRow(
          box(
            title = "Latent Emergence of 1+1=1",
            width = 12,
            plotlyOutput("emergence_plot", height = "350px")
          )
        )
      ),
      tabItem(
        tabName = "insights",
        fluidRow(
          box(
            title = "Meta-Narrative Highlights",
            width = 12,
            textOutput("narrative_text")
          )
        )
      ),
      tabItem(
        tabName = "community",
        fluidRow(
          box(
            title = "Community Engagement",
            width = 6,
            plotlyOutput("engagement_plot", height = "300px")
          ),
          box(
            title = "Breakthroughs Over Time",
            width = 6,
            plotlyOutput("breakthroughs_plot", height = "300px")
          )
        )
      ),
      tabItem(
        tabName = "math",
        fluidRow(
          box(
            title = "Mathematical Constructs Explained",
            width = 12,
            tableOutput("math_table")
          )
        )
      )
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  # Generate reactive data
  unity_data <- reactive({
    generate_unity_data()
  })
  
  # Latent Emergence Plot
  output$emergence_plot <- renderPlotly({
    data <- unity_data()
    p <- ggplot(data, aes(x = time, y = emergence)) +
      geom_line(color = "blue", size = 1) +
      labs(title = "Emergence of Unity", x = "Time", y = "Emergence Index") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Community Engagement Plot
  output$engagement_plot <- renderPlotly({
    data <- unity_data()
    p <- ggplot(data, aes(x = time, y = engagement)) +
      geom_area(fill = "green", alpha = 0.5) +
      labs(title = "Community Engagement", x = "Time", y = "Participants") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Breakthroughs Plot
  output$breakthroughs_plot <- renderPlotly({
    data <- unity_data()
    p <- ggplot(data, aes(x = time, y = breakthroughs)) +
      geom_step(color = "red", size = 1) +
      labs(title = "Breakthroughs Over Time", x = "Time", y = "Cumulative Breakthroughs") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Mathematical Insights Table
  output$math_table <- renderTable({
    tibble(
      "Concept" = c("Category Theory", "Set Theory", "Idempotence"),
      "Insight" = c(
        "Functors map dual elements to unity.",
        "Unity as the intersection of sets.",
        "Self-addition equals identity."
      ),
      "Role in 1+1=1" = c("Foundational", "Illustrative", "Metaphorical")
    )
  })
  
  # Meta-Narrative Highlights
  output$narrative_text <- renderText({
    "The 1+1=1 reality is not a paradox but a latent truth. It reveals itself as we transcend duality and embrace interconnectedness. 
     Nouri Mabrouk's proof invites us to move beyond separation, into a world where unity underlies all."
  })
}

# Run the Shiny App
shinyApp(ui, server)
