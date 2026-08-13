library(shiny)
library(dplyr)
library(DT)
library(tidyr)
library(ggplot2)
library(stringr)

load("app_data.RData")

# Helper function to convert numeric SoS into categorical labels
get_sos_label <- function(sos_val) {
  case_when(
    sos_val >= 4.5 ~ "Strong",
    sos_val >= 3.5 ~ "Favorable",
    sos_val >= 2.5 ~ "Neutral",
    sos_val >= 1.5 ~ "Unfavorable",
    TRUE           ~ "Weak"
  )
}

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
        (rec_yds_c  * rules$rec_yds) +
        (rec_tds_c  * rules$rec_tds)
    ) %>%
    select(-ends_with("_c"))
}

# Extract unique sorted teams for the filter input
all_teams <- sort(unique(na.omit(weighted_projections$team)))

ui <- fluidPage(
  titlePanel("🏈 Fantasy Player Comparison & Draft Hub"),
  
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
      h4("Replacement Baselines (VOR)"),
      numericInput("rep_qb", "QB Baseline Rank:", value = 16, min = 1, max = 50),
      numericInput("rep_rb", "RB Baseline Rank:", value = 30, min = 1, max = 70),
      numericInput("rep_wr", "WR Baseline Rank:", value = 46, min = 1, max = 80),
      numericInput("rep_te", "TE Baseline Rank:", value = 12, min = 1, max = 30),
      hr(),
      h4("Player Filtering"),
      checkboxGroupInput("target_pos", "Positions to Show:",
                         choices = c("QB", "RB", "WR", "TE"),
                         selected = c("RB", "WR", "TE")),
      br(),
      selectizeInput("target_teams", "Filter Teams (Leave blank for all):",
                     choices = all_teams, multiple = TRUE,
                     options = list(placeholder = 'All Teams')),
      hr(),
      h4("Compare Specific Players"),
      selectizeInput("compare_players", "Select 2+ Players to Compare:",
                     choices = NULL, multiple = TRUE),
      hr(),
      h4("Draft Window Settings"),
      numericInput("my_pick", "Reference Rank/Tier #:", value = 14, min = 1, max = 150),
      sliderInput("window_half_size", "Window Span (+/- Players):", min = 2, max = 15, value = 5, step = 1)
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Active Decision Hub", 
                 h4("Expert Consensus & Outcome Spread"),
                 p("Visualizing individual expert source projections and outcome volatility for selected targets."),
                 plotOutput("volatility_plot", height = "380px"),
                 hr(),
                 h4("Draft Window (Reaches & Values)"),
                 p("Showing a customizable window of players preceding and following your reference rank among your selected positions."),
                 DTOutput("window_picks_table")
        ),
        tabPanel("Master Player Board", 
                 h5("Complete filtered pool of players sorted by Model ADP and Value Over Replacement."),
                 br(),
                 DTOutput("draft_board_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  league_rules <- reactive({
    rec_pts <- switch(input$scoring_format,
                      "PPR" = 1.00,
                      "Half-PPR" = 0.50,
                      "Standard" = 0.00,
                      1.00)
    list(
      pass_yds = 0.04, pass_tds = 4.00, pass_int = -2.00,
      rush_yds = 0.10, rush_tds = 6.00,
      rec = rec_pts, rec_yds = 0.10, rec_tds = 6.00
    )
  })
  
  rep_ranks_reactive <- reactive({
    tibble(
      pos = c("QB", "RB", "WR", "TE"),
      rep_rank = c(input$rep_qb, input$rep_rb, input$rep_wr, input$rep_te)
    )
  })
  
  weighted_league_pts <- reactive({
    rep_df <- rep_ranks_reactive()
    
    weighted_projections %>%
      select(-any_of(c("weighted_pts", "vor", "model_adp", "pos_rank"))) %>%
      calc_league_pts(league_rules()) %>%
      rename(weighted_pts = total_pts) %>%
      filter(pos %in% c("QB", "RB", "WR", "TE")) %>%
      group_by(pos) %>%
      arrange(desc(weighted_pts)) %>%
      mutate(pos_rank = row_number()) %>%
      ungroup() %>%
      left_join(rep_df, by = "pos") %>%
      group_by(pos) %>%
      mutate(
        baseline_pts = if_else(
          max(pos_rank) >= first(rep_rank),
          weighted_pts[pos_rank == first(rep_rank)][1],
          tail(weighted_pts, 1)
        ),
        vor = weighted_pts - coalesce(baseline_pts, 0)
      ) %>%
      ungroup() %>%
      select(-rep_rank, -baseline_pts) %>%
      mutate(model_adp = rank(-weighted_pts, ties.method = "min"))
  })
  
  espn_projections_calc <- reactive({
    combined_projections_clean %>%
      filter(data_src == "ESPN") %>%
      calc_league_pts(league_rules()) %>%
      rename(espn_pts = total_pts) %>%
      select(player, pos, team, espn_pts)
  })
  
  combined_ranking <- reactive({
    w_pts <- weighted_league_pts()
    espn_pts_df <- espn_projections_calc()
    
    w_pts %>%
      left_join(espn_pts_df, by = c("player", "pos", "team")) %>%
      mutate(
        espn_pts = coalesce(espn_pts, weighted_pts)
      ) %>%
      mutate(raw_delta = weighted_pts - espn_pts) %>%
      group_by(pos) %>%
      mutate(
        pos_mean_delta = mean(raw_delta, na.rm = TRUE),
        net_delta = raw_delta - pos_mean_delta,
        pos_sd_delta = sd(raw_delta, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      group_by(pos) %>%
      arrange(desc(espn_pts)) %>%
      mutate(pos_adp = row_number()) %>%
      ungroup() %>%
      arrange(desc(weighted_pts)) %>%
      mutate(overall_adp = row_number())
  })
  
  filtered_ranking <- reactive({
    req(input$target_pos)
    df <- combined_ranking() %>% filter(pos %in% input$target_pos)
    if (!is.null(input$target_teams) && length(input$target_teams) > 0) {
      df <- df %>% filter(team %in% input$target_teams)
    }
    df
  })
  
  draft_board_master <- reactive({
    filtered_ranking() %>%
      mutate(
        pts_diff = round(weighted_pts - espn_pts, 1),
        pct_diff = round(((weighted_pts - espn_pts) / espn_pts) * 100, 1),
        SoS = get_sos_label(sos)
      ) %>%
      arrange(model_adp) %>%
      select(model_adp, player, pos, team, weighted_pts, vor, SoS, espn_pts, pts_diff, sources_count)
  })
  
  observe({
    board <- draft_board_master()
    updateSelectizeInput(session, "compare_players",
                         choices = sort(board$player),
                         server = TRUE)
  })
  
  output$window_picks_table <- renderDT({
    ranked_board <- filtered_ranking()
    
    ref_val <- input$my_pick
    span <- input$window_half_size
    
    ahead_pool <- ranked_board %>% filter(overall_adp < ref_val) %>% arrange(desc(overall_adp)) %>% head(max(0, span - 1)) %>% arrange(overall_adp)
    after_pool <- ranked_board %>% filter(overall_adp >= ref_val) %>% arrange(overall_adp) %>% head(span + 2)
    
    window_data <- bind_rows(ahead_pool, after_pool) %>%
      mutate(
        dyn_threshold = pmax(1.0, 0.35 * coalesce(pos_sd_delta, 2.0)),
        Model_Outlook = case_when(
          net_delta > dyn_threshold   ~ "🔥 Model Likes More",
          net_delta < -dyn_threshold  ~ "❄️ Model Likes Less",
          TRUE                        ~ "⚖️ Matches ESPN"
        ),
        "Rank" = overall_adp,
        "Player" = player,
        "Pos" = pos,
        "Team" = team,
        "Model Proj" = round(weighted_pts, 1),
        "ESPN Proj" = round(espn_pts, 1),
        "VOR" = round(vor, 1),
        "SoS" = get_sos_label(sos),
        "Delta" = round(weighted_pts - espn_pts, 1)
      ) %>%
      select(Rank, Player, Pos, Team, VOR, SoS, "Model Proj", "ESPN Proj", Delta, Model_Outlook)
    
    datatable(
      window_data,
      options = list(pageLength = 25, dom = 't', ordering = FALSE),
      rownames = FALSE
    ) %>%
      formatStyle(
        'VOR',
        background = styleColorBar(c(0, max(window_data$VOR, na.rm = TRUE)), '#d4edda'),
        fontWeight = 'bold'
      ) %>%
      formatStyle(
        'SoS',
        backgroundColor = styleEqual(
          c("Weak", "Unfavorable", "Neutral", "Favorable", "Strong"),
          c('#f8d7da', '#ffe8cc', '#f1f3f5', '#d3f9d8', '#8ce99a')
        ),
        fontWeight = 'bold'
      ) %>%
      formatStyle(
        'Model_Outlook',
        backgroundColor = styleEqual(
          c("🔥 Model Likes More", "❄️ Model Likes Less", "⚖️ Matches ESPN"),
          c('#e6f4ea', '#fce8e6', '#f1f3f4')
        ),
        fontWeight = 'bold'
      )
  })
  
  output$volatility_plot <- renderPlot({
    req(length(input$compare_players) >= 2)
    comparison_data <- combined_projections_clean %>% filter(player %in% input$compare_players)
    if(nrow(comparison_data) == 0) return()
    
    source_breakdown <- calc_league_pts(comparison_data, league_rules()) %>% 
      select(player, pos, team, data_src, total_pts)
    
    ggplot(source_breakdown, aes(x = total_pts, y = reorder(player, total_pts, FUN = median))) +
      geom_boxplot(aes(fill = pos), alpha = 0.25, outlier.shape = NA, width = 0.35) +
      geom_point(aes(color = data_src), size = 4.5, alpha = 0.9, position = position_jitter(height = 0.1, width = 0)) +
      scale_fill_brewer(palette = "Pastel1", guide = "none") +
      scale_color_brewer(palette = "Set1", name = "Data Source") +
      labs(
        title = "Outcome Range Across Expert Platforms",
        subtitle = paste0("Scoring: ", input$scoring_format, " | Higher spread indicates lower expert consensus"),
        x = "Projected Fantasy Points",
        y = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(size = 11, color = "grey40"),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 13, face = "bold", color = "grey20"),
        axis.text.x = element_text(size = 12, face = "bold", color = "grey20"),
        legend.position = "right"
      )
  })
  
  output$draft_board_table <- renderDT({
    board_df <- draft_board_master()
    
    datatable(
      board_df,
      options = list(pageLength = 15),
      selection = 'none'
    ) %>%
      formatStyle(
        'SoS',
        backgroundColor = styleEqual(
          c("Weak", "Unfavorable", "Neutral", "Favorable", "Strong"),
          c('#f8d7da', '#ffe8cc', '#f1f3f5', '#d3f9d8', '#8ce99a')
        ),
        fontWeight = 'bold'
      )
  })
}

shinyApp(ui = ui, server = server)