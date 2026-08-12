library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(tidyr)

# Load pre-processed data
load("app_data.RData")

ui <- fluidPage(
  titlePanel("2026 Fantasy Football Draft Kit"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filter Options"),
      selectInput("pos_filter", "Position:", 
                  choices = c("ALL", "QB", "RB", "WR", "TE"), 
                  selected = "ALL"),
      sliderInput("vor_filter", "Minimum VOR:", 
                  min = -20, max = 150, value = -20, step = 5),
      hr(),
      helpText("Data sources blended via weighted model. Tiers and VOR based on custom baselines.")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Projections & Rankings", 
                 br(),
                 DTOutput("proj_table")
        ),
        tabPanel("Tier Visualizer", 
                 br(),
                 plotOutput("tier_plot", height = "600px")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    df <- weighted_projections
    
    if (input$pos_filter != "ALL") {
      df <- df %>% filter(pos == input$pos_filter)
    }
    
    df <- df %>% filter(vor >= input$vor_filter)
    df
  })
  
  output$proj_table <- renderDT({
    dt_df <- filtered_data() %>%
      mutate(
        weighted_pts = round(weighted_pts, 1),
        vor = round(vor, 1)
      ) %>%
      select(
        Rank = pos_rank,
        Player = player,
        Pos = pos,
        Team = team,
        FPts = weighted_pts,
        VOR = vor,
        Tier = tier,
        Sources = sources_used
      )
    
    datatable(
      dt_df,
      options = list(pageLength = 25, scrollX = TRUE),
      rownames = FALSE,
      selection = "single"
    )
  })
  
  output$tier_plot <- renderPlot({
    p_data <- filtered_data()
    
    ggplot(p_data, aes(x = pos_rank, y = weighted_pts, color = factor(tier), label = player)) +
      geom_point(size = 3, alpha = 0.8) +
      geom_text(vjust = -0.8, size = 3, check_overlap = TRUE) +
      facet_wrap(~pos, scales = "free") +
      theme_minimal() +
      labs(
        title = "Player Tiers & Value Drop-offs",
        subtitle = "Colored by positional tier grouping",
        x = "Position Rank",
        y = "Weighted Projected Fantasy Points",
        color = "Tier"
      ) +
      theme(legend.position = "bottom")
  })
}

shinyApp(ui = ui, server = server)