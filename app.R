library(shiny)
library(dplyr)
library(DT)
library(tidyr)
library(ggplot2)
library(stringr)

# Load pre-computed data instantly (no web scraping / API delays on startup)
load("app_data.RData")

calc_league_pts <- function(df, rules) {
  df %>%
    mutate(
      pass_yds_c = coalesce(pass_yds, 0),
      pass_tds_c = coalesce(pass_tds, 0),
      pass_int_c = coalesce(pass_int, 0),
      rush_yds_c = coalesce(rush_yds, 0),
      rush_tds_c = coalesce(rush_tds, 0),
      rec_c      = coalesce(rec, 0),
      rec_yds_c  = coalesce(rec_yds, 0),
      rec_tds_c  = coalesce(rec_tds, 0),
      
      total_pts = (pass_yds_c * rules$pass_yds) +
        (pass_tds_c * rules$pass_tds) +
        (pass_int_c * rules$pass_int) +
        (rush_yds_c * rules$rush_yds) +
        (rush_tds_c * rules$rush_tds) +
        (rec_c     * rules$rec)     +
        (rec_yds_c * rules$rec_yds) +
        (rec_tds_c * rules$rec_tds)
    ) %>%
    select(-ends_with("_c"))
}

ui <- fluidPage(
  titlePanel("🏈 Live Draft War Room"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("League Settings"),
      radioButtons("scoring_format", "Scoring System:",
                   choices = c("PPR (1.0 Rec)" = "PPR", 
                               "Half-PPR (0.5 Rec)" = "Half-PPR", 
                               "Standard (0.0 Rec)" = "Standard"),
                   selected = "PPR"),
      hr(),
      h4("Draft Controls"),
      numericInput("my_pick", "Your Current Pick #:", value = 14, min = 1, max = 300),
      sliderInput("window", "Draft Window (+/- Picks):", value = 15, min = 5, max = 30),
      checkboxGroupInput("target_pos", "Positions to Show:", 
                         choices = c("QB", "RB", "WR", "TE"), 
                         selected = c("RB", "WR", "TE")),
      hr(),
      h4("Compare Specific Players"),
      selectizeInput("compare_players", "Select 2+ Players to Compare:", 
                     choices = NULL, multiple = TRUE),
      hr(),
      h4("Draft Actions"),
      actionButton("draft_others_btn", "Mark Taken (Leaguemates)", class = "btn-secondary btn-sm", style = "width: 100%; margin-bottom: 5px;"),
      actionButton("draft_me_btn", "Draft to MY ROSTER", class = "btn-success", style = "width: 100%; font-weight: bold; margin-bottom: 10px;"),
      hr(),
      actionButton("undo_draft", "Undo Last Action", class = "btn-warning btn-sm", style = "width: 100%; margin-bottom: 5px;"),
      actionButton("reset_draft", "Reset Entire Draft", class = "btn-danger btn-sm", style = "width: 100%;")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Target Window (Bars)", 
                 plotOutput("lollipop_plot", height = "650px")),
        tabPanel("Multi-Player Volatility", 
                 plotOutput("volatility_plot", height = "500px"),
                 tableOutput("volatility_table")),
        tabPanel("My Roster", 
                 h4("Your Drafted Roster & Projected Output"),
                 verbatimTextOutput("roster_summary"),
                 br(),
                 tableOutput("my_roster_table")),
        tabPanel("Available Board", 
                 h5("Click rows to select, then click an action button on the left panel."),
                 br(),
                 DTOutput("draft_board_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    drafted_all = character(0),
    my_roster   = character(0),
    history     = list()
  )
  
  league_rules <- reactive({
    rec_pts <- switch(input$scoring_format,
                      "PPR" = 1.00,
                      "Half-PPR" = 0.50,
                      "Standard" = 0.00,
                      1.00)
    list(
      pass_yds = 0.04,   
      pass_tds = 4.00,   
      pass_int = -2.00,  
      rush_yds = 0.10,   
      rush_tds = 6.00,   
      rec      = rec_pts,   
      rec_yds  = 0.10,   
      rec_tds  = 6.00   
    )
  })
  
  weighted_league_pts <- reactive({
    # weighted_projections is already calculated and weighted in app_data.RData
    weighted_projections
  })
  
  espn_pts <- reactive({
    combined_projections_clean %>%
      filter(data_src == "ESPN") %>%
      calc_league_pts(league_rules()) %>%
      rename(espn_pts = total_pts) %>%
      select(player, pos, team, espn_pts)
  })
  
  draft_board_master <- reactive({
    req(input$target_pos)
    
    weighted_league_pts() %>%
      inner_join(espn_pts(), by = c("player", "pos", "team")) %>%
      filter(pos %in% input$target_pos) %>% 
      mutate(
        pts_diff = round(weighted_pts - espn_pts, 1),
        pct_diff = round(((weighted_pts - espn_pts) / espn_pts) * 100, 1)
      ) %>%
      arrange(desc(espn_pts)) %>%
      mutate(model_adp = row_number()) %>%
      select(model_adp, player, pos, team, weighted_pts, espn_pts, pts_diff, sources_count)
  })
  
  observe({
    board <- draft_board_master()
    updateSelectizeInput(session, "compare_players", 
                         choices = sort(board$player), 
                         server = TRUE)
  })
  
  observeEvent(input$reset_draft, {
    rv$drafted_all <- character(0)
    rv$my_roster   <- character(0)
    rv$history     <- list()
  })
  
  observeEvent(input$undo_draft, {
    if(length(rv$history) > 0) {
      last_action <- tail(rv$history, 1)[[1]]
      rv$history <- head(rv$history, -1)
      rv$drafted_all <- last_action$drafted_all_snapshot
      rv$my_roster   <- last_action$my_roster_snapshot
    }
  })
  
  observeEvent(input$draft_others_btn, {
    selected_row <- input$draft_board_table_rows_selected
    if(length(selected_row) > 0) {
      current_board <- active_board()
      players_to_draft <- current_board$player[selected_row]
      
      rv$history <- c(rv$history, list(list(
        drafted_all_snapshot = rv$drafted_all,
        my_roster_snapshot   = rv$my_roster
      )))
      
      rv$drafted_all <- unique(c(rv$drafted_all, players_to_draft))
    }
  })
  
  observeEvent(input$draft_me_btn, {
    selected_row <- input$draft_board_table_rows_selected
    if(length(selected_row) > 0) {
      current_board <- active_board()
      players_to_draft <- current_board$player[selected_row]
      
      rv$history <- c(rv$history, list(list(
        drafted_all_snapshot = rv$drafted_all,
        my_roster_snapshot   = rv$my_roster
      )))
      
      rv$drafted_all <- unique(c(rv$drafted_all, players_to_draft))
      rv$my_roster   <- unique(c(rv$my_roster, players_to_draft))
    }
  })
  
  active_board <- reactive({
    draft_board_master() %>%
      filter(!player %in% rv$drafted_all)
  })
  
  output$lollipop_plot <- renderPlot({
    board <- active_board()
    req(nrow(board) > 0)
    
    min_pick <- max(1, input$my_pick - input$window)
    max_pick <- input$my_pick + input$window
    
    filtered_data <- board %>%
      filter(model_adp >= min_pick, model_adp <= max_pick)
    
    if(nrow(filtered_data) == 0) {
      plot.new()
      text(0.5, 0.5, "No players available in this window matching your position filters!", cex = 1.2)
      return()
    }
    
    filtered_data <- filtered_data %>%
      mutate(display_label = paste0("#", model_adp, " - ", player, " (", pos, ", ", team, ")"))
    
    ggplot(filtered_data, aes(x = weighted_pts, y = reorder(display_label, weighted_pts))) +
      geom_col(aes(fill = pts_diff), width = 0.65, alpha = 0.9) +
      geom_text(aes(label = sprintf("%.1f pts (Diff: %+ .1f)", weighted_pts, pts_diff)), 
                hjust = -0.05, size = 4, fontface = "bold", color = "grey20") +
      scale_fill_gradient2(
        low = "#e74c3c", mid = "#f5f5f5", high = "#2ecc71", 
        midpoint = 0, name = "Model vs ESPN Diff"
      ) +
      expand_limits(x = max(filtered_data$weighted_pts, na.rm = TRUE) * 1.32) +
      labs(
        title = paste0("Draft Window: Picks ", min_pick, " to ", max_pick, " (Your Pick: #", input$my_pick, ")"),
        subtitle = paste0("Scoring: ", input$scoring_format, " | Ranked by Weighted Points (Filtered Positions Only)"),
        x = "Weighted Model Projected Fantasy Points",
        y = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 12, color = "grey40"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 11, face = "bold", color = "grey20"),
        axis.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
        legend.position = "bottom",
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 10)
      )
  })
  
  output$volatility_plot <- renderPlot({
    req(length(input$compare_players) >= 2)
    
    comparison_data <- combined_projections_clean %>%
      filter(player %in% input$compare_players)
    
    if(nrow(comparison_data) == 0) return()
    
    source_breakdown <- calc_league_pts(comparison_data, league_rules()) %>%
      select(player, pos, team, data_src, total_pts)
    
    ggplot(source_breakdown, aes(x = total_pts, y = reorder(player, total_pts, FUN = median), color = data_src)) +
      geom_point(size = 4.5, alpha = 0.85, position = position_jitter(height = 0.1, width = 0)) +
      geom_text(aes(label = data_src), vjust = -1.3, size = 3.5, show.legend = FALSE) +
      labs(
        title = "Multi-Player Volatility & Range of Outcomes",
        subtitle = paste0("Scoring: ", input$scoring_format, " | Comparing source projections across candidate targets"),
        x = "Projected Fantasy Points (Individual Source Output)",
        y = NULL,
        color = "Data Source"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11, color = "grey40"),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.text.y = element_text(size = 14, face = "bold", color = "grey20"), 
        axis.text.x = element_text(size = 11),
        legend.position = "right"
      ) +
      expand_limits(x = c(
        min(source_breakdown$total_pts, na.rm = TRUE) * 0.9,
        max(source_breakdown$total_pts, na.rm = TRUE) * 1.15
      ))
  })
  
  output$volatility_table <- renderTable({
    req(length(input$compare_players) >= 2)
    
    comparison_data <- combined_projections_clean %>%
      filter(player %in% input$compare_players)
    
    if(nrow(comparison_data) == 0) return()
    
    calc_league_pts(comparison_data, league_rules()) %>%
      select(player, pos, team, data_src, total_pts) %>%
      pivot_wider(names_from = data_src, values_from = total_pts) %>%
      mutate(across(where(is.numeric), ~ round(.x, 1)))
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  output$roster_summary <- renderText({
    if(length(rv$my_roster) == 0) {
      return("Your roster is currently empty. Select players from the 'Available Board' tab and click 'Draft to MY ROSTER'.")
    }
    
    roster_df <- weighted_league_pts() %>% 
      inner_join(espn_pts(), by = c("player", "pos", "team")) %>% 
      filter(player %in% rv$my_roster)
    
    total_proj <- sum(roster_df$weighted_pts, na.rm = TRUE)
    pos_counts <- table(roster_df$pos)
    pos_breakdown <- paste(names(pos_counts), pos_counts, sep = ": ", collapse = " | ")
    
    paste0("Players Drafted: ", nrow(roster_df), 
           " | Total Projected Points: ", round(total_proj, 1), 
           "\nPosition Breakdown: ", pos_breakdown)
  })
  
  output$my_roster_table <- renderTable({
    req(length(rv$my_roster) > 0)
    weighted_league_pts() %>% 
      inner_join(espn_pts(), by = c("player", "pos", "team")) %>% 
      filter(player %in% rv$my_roster) %>%
      mutate(pts_diff = round(weighted_pts - espn_pts, 1)) %>%
      select(player, pos, team, weighted_pts, espn_pts, pts_diff) %>%
      arrange(desc(weighted_pts))
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  output$draft_board_table <- renderDT({
    datatable(
      active_board() %>% select(model_adp, player, pos, team, weighted_pts, espn_pts, pts_diff),
      options = list(pageLength = 15),
      selection = 'multiple'
    )
  })
}

shinyApp(ui = ui, server = server)