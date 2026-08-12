library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(tidyr)

# Load pre-processed data
load("app_data.RData")

ui <- fluidPage(
  titlePanel("2026 Fantasy Football Draft War Room"),
  
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
      h4("Player Comparison"),
      selectizeInput("compare_players", "Select Players to Compare:",
                     choices = NULL, multiple = TRUE, options = list(maxItems = 4)),
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
        ),
        tabPanel("Player Comparison Chart", 
                 br(),
                 plotOutput("comparison_plot", height = "600px")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Update selectize choices dynamically based on loaded data
  observe({
    req(weighted_projections)
    player_choices <- sort(unique(weighted_projections$player))
    updateSelectizeInput(session, "compare_players", choices = player_choices, server = TRUE)
  })
  
  # Reactive dataset filtered by user inputs
  filtered_data <- reactive({
    df <- weighted_projections
    
    # Position filter
    if (input$pos_filter != "ALL") {
      df <- df %>% filter(pos == input$pos_filter)
    }
    
    # VOR filter
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
  
  output$comparison_plot <- renderPlot({
    req(input$compare_players)
    if (length(input$compare_players) == 0) {
      return(NULL)
    }
    
    comp_data <- weighted_projections %>%
      filter(player %in% input$compare_players) %>%
      select(player, pos, weighted_pts, vor, pass_yds, rush_yds, rec_yds) %>%
      pivot_longer(cols = c(weighted_pts, vor, pass_yds, rush_yds, rec_yds), 
                   names_to = "metric", values_to = "value") %>%
      mutate(metric = case_match(
        metric,
        "weighted_pts" ~ "Projected Points",
        "vor" ~ "VOR",
        "pass_yds" ~ "Pass Yards",
        "rush_yds" ~ "Rush Yards",
        "rec_yds" ~ "Rec Yards",
        .default = metric
      ))
    
    ggplot(comp_data, aes(x = metric, y = value, fill = player)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      labs(
        title = "Side-by-Side Player Comparison",
        x = "Metric",
        y = "Value",
        fill = "Player"
      ) +
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 15, hjust = 1))
  })
}

shinyApp(ui = ui, server = server)