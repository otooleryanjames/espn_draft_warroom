library(shiny)
library(dplyr)
library(DT)
library(tidyr)
library(ggplot2)
library(stringr)

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
        (rec_c      * rules$rec)     +
        (rec_yds_c  * rules$rec_yds) +
        (rec_tds_c  * rules$rec_tds)
    ) %>%
    select(-ends_with("_c"))
}

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
      h4("Player Filtering"),
      checkboxGroupInput("target_pos", "Positions to Show:",
                         choices = c("QB", "RB", "WR", "TE"),
                         selected = c("RB", "WR", "TE")),
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
                 h4("Statistical Tale of the Tape"),
                 tableOutput("volatility_summary_card"),
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
  
  weighted_league_pts <- reactive({
    weighted_projections %>%
      select(-any_of(c("weighted_pts", "vor", "tier", "model_adp", "pos_rank"))) %>%
      calc_league_pts(league_rules()) %>%
      rename(weighted_pts = total_pts) %>%
      filter(pos %in% c("QB", "RB", "WR", "TE")) %>%
      group_by(pos) %>%
      arrange(desc(weighted_pts)) %>%
      mutate(pos_rank = row_number()) %>%
      ungroup() %>%
      mutate(
        tier = case_when(
          pos == "WR" ~ case_when(pos_rank <= 2 ~ 1, pos_rank <= 4 ~ 2, pos_rank <= 8 ~ 3, pos_rank <= 13 ~ 4, pos_rank <= 19 ~ 5, pos_rank <= 27 ~ 6, pos_rank <= 36 ~ 7, pos_rank <= 48 ~ 8, TRUE ~ 9),
          pos == "RB" ~ case_when(pos_rank <= 2 ~ 1, pos_rank <= 4 ~ 2, pos_rank <= 8 ~ 3, pos_rank <= 11 ~ 4, pos_rank <= 17 ~ 5, pos_rank <= 23 ~ 6, pos_rank <= 30 ~ 7, pos_rank <= 36 ~ 8, pos_rank <= 44 ~ 9, TRUE ~ 10),
          pos == "QB" ~ case_when(pos_rank <= 1 ~ 1, pos_rank <= 5 ~ 2, pos_rank <= 12 ~ 3, pos_rank <= 16 ~ 4, pos_rank <= 19 ~ 5, TRUE ~ 6),
          pos == "TE" ~ case_when(pos_rank <= 1 ~ 1, pos_rank <= 2 ~ 2, pos_rank <= 4 ~ 3, pos_rank <= 8 ~ 4, pos_rank <= 15 ~ 5, TRUE ~ 6)
        )
      ) %>%
      left_join(
        tibble(pos = c("QB", "RB", "WR", "TE"), rep_rank = c(16, 28, 32, 12)),
        by = "pos"
      ) %>%
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
      # Calculate raw delta from ESPN, then subtract the positional mean delta 
      # dynamically to eliminate systemic platform inflation/deflation biases.
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
  
  draft_board_master <- reactive({
    req(input$target_pos)
    combined_ranking() %>%
      filter(pos %in% input$target_pos) %>% 
      mutate(
        pts_diff = round(weighted_pts - espn_pts, 1),
        pct_diff = round(((weighted_pts - espn_pts) / espn_pts) * 100, 1)
      ) %>%
      arrange(model_adp) %>%
      select(model_adp, tier, player, pos, team, weighted_pts, vor, espn_pts, pts_diff, sources_count)
  })
  
  observe({
    board <- draft_board_master()
    updateSelectizeInput(session, "compare_players",
                         choices = sort(board$player),
                         server = TRUE)
  })
  
  output$window_picks_table <- renderDT({
    req(input$target_pos)
    ranked_board <- combined_ranking() %>% filter(pos %in% input$target_pos)
    
    ref_val <- input$my_pick
    span <- input$window_half_size
    
    ahead_pool <- ranked_board %>% filter(overall_adp < ref_val) %>% arrange(desc(overall_adp)) %>% head(max(0, span - 1)) %>% arrange(overall_adp)
    after_pool <- ranked_board %>% filter(overall_adp >= ref_val) %>% arrange(overall_adp) %>% head(span + 2)
    
    window_data <- bind_rows(ahead_pool, after_pool) %>%
      mutate(
        # Dynamic thresholding based on the positional standard deviation of deltas.
        # This replaces hardcoded static point gaps.
        dyn_threshold = pmax(1.0, 0.35 * coalesce(pos_sd_delta, 2.0)),
        Model_Outlook = case_when(
          net_delta > dyn_threshold   ~ "🔥 Model Likes More",
          net_delta < -dyn_threshold  ~ "❄️ Model Likes Less",
          TRUE                        ~ "⚖️ Matches ESPN"
        ),
        "Rank" = overall_adp,
        "Tier" = tier,
        "Player" = player,
        "Pos" = pos,
        "Team" = team,
        "Model Proj" = round(weighted_pts, 1),
        "ESPN Proj" = round(espn_pts, 1),
        "VOR" = round(vor, 1),
        "Delta" = round(weighted_pts - espn_pts, 1)
      ) %>%
      select(Rank, Tier, Player, Pos, Team, VOR, "Model Proj", "ESPN Proj", Delta, Model_Outlook)
    
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
  
  output$volatility_summary_card <- renderTable({
    req(length(input$compare_players) >= 2)
    
    comparison_data <- combined_projections_clean %>% filter(player %in% input$compare_players)
    if(nrow(comparison_data) == 0) return()
    
    calc_league_pts(comparison_data, league_rules()) %>%
      group_by(player, pos, team) %>%
      summarise(
        Sources     = n(),
        Consensus   = round(mean(total_pts, na.rm = TRUE), 1),
        Floor       = round(min(total_pts, na.rm = TRUE), 1),
        Ceiling     = round(max(total_pts, na.rm = TRUE), 1),
        Spread      = round(Ceiling - Floor, 1),
        Std_Dev     = round(sd(total_pts, na.rm = TRUE), 1),
        .groups     = "drop"
      ) %>%
      arrange(desc(Consensus)) %>%
      rename(
        Player = player,
        Pos = pos,
        Team = team,
        "Sources Count" = Sources,
        "Mean Proj" = Consensus,
        "Floor" = Floor,
        "Ceiling" = Ceiling,
        "Spread (Ceiling - Floor)" = Spread,
        "Std Dev" = Std_Dev
      )
  }, striped = TRUE, bordered = TRUE, spacing = "m", align = "c")
  
  output$draft_board_table <- renderDT({
    datatable(
      draft_board_master() %>% select(model_adp, tier, player, pos, team, weighted_pts, vor, espn_pts, pts_diff, sources_count),
      options = list(pageLength = 15),
      selection = 'none'
    )
  })
}

shinyApp(ui = ui, server = server)