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
  titlePanel("🏈 Fantasy Player Comparison Hub"),
  
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
      numericInput("my_pick", "Reference Pick #:", value = 14, min = 1, max = 300),
      sliderInput("window", "Draft Window (+/- Picks):", value = 15, min = 5, max = 30)
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Tier Quadrants",
                 fluidRow(
                   column(6, h3("Quarterbacks (QB)"), DTOutput("qb_quadrant_table")),
                   column(6, h3("Running Backs (RB)"), DTOutput("rb_quadrant_table"))
                 ),
                 br(),
                 fluidRow(
                   column(6, h3("Wide Receivers (WR)"), DTOutput("wr_quadrant_table")),
                   column(6, h3("Tight Ends (TE)"), DTOutput("te_quadrant_table"))
                 )
        ),
        tabPanel("Target Window (Bars)", 
                 plotOutput("lollipop_plot", height = "650px")),
        tabPanel("Player Comparison Hub", 
                 plotOutput("volatility_plot", height = "450px"),
                 hr(),
                 h4("Statistical Range & Expert Consensus"),
                 tableOutput("volatility_summary_card"),
                 hr(),
                 plotOutput("stat_breakdown_plot", height = "450px"),
                 br(),
                 h4("Source-by-Source Projection Breakdown"),
                 tableOutput("volatility_table")),
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
      left_join(espn_pts(), by = c("player", "pos", "team")) %>%
      filter(pos %in% input$target_pos) %>% 
      mutate(
        espn_pts = coalesce(espn_pts, weighted_pts),
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
  
  render_position_quadrant <- function(position_filter) {
    weighted_league_pts() %>%
      filter(pos == position_filter) %>%
      select(pos_rank, player, team, weighted_pts, tier) %>%
      datatable(
        options = list(pageLength = 25, dom = 't', scrollY = "500px", scroller = TRUE, ordering = FALSE),
        rownames = FALSE,
        colnames = c("Rank", "Player", "Team", "Proj Pts", "Tier")
      ) %>%
      formatStyle(
        'tier',
        backgroundColor = styleInterval(c(1, 2, 3, 4, 5, 6, 7, 8, 9), c('#ffff00', '#000080', '#00ff00', '#800000', '#00ffff', '#4b0082', '#ff8c00', '#111111', '#ff1493', '#556b2f')),
        color = styleInterval(c(1, 2, 3, 4, 5, 6, 7, 8, 9), c('#000000', '#ffffff', '#000000', '#ffffff', '#000000', '#ffffff', '#000000', '#ffffff', '#000000', '#ffffff')),
        fontWeight = 'bold'
      )
  }
  
  output$qb_quadrant_table <- renderDT({ render_position_quadrant("QB") })
  output$rb_quadrant_table <- renderDT({ render_position_quadrant("RB") })
  output$wr_quadrant_table <- renderDT({ render_position_quadrant("WR") })
  output$te_quadrant_table <- renderDT({ render_position_quadrant("TE") })
  
  output$lollipop_plot <- renderPlot({
    board <- draft_board_master()
    req(nrow(board) > 0)
    min_pick <- max(1, input$my_pick - input$window)
    max_pick <- input$my_pick + input$window
    
    filtered_data <- board %>% filter(model_adp >= min_pick, model_adp <= max_pick)
    if(nrow(filtered_data) == 0) {
      plot.new()
      text(0.5, 0.5, "No players available in this window matching your position filters!", cex = 1.2)
      return()
    }
    
    filtered_data <- filtered_data %>%
      mutate(display_label = paste0("#", model_adp, " - Tier ", tier, " | ", player, " (", pos, ", ", team, ")"))
    
    ggplot(filtered_data, aes(x = vor, y = reorder(display_label, vor))) +
      geom_col(aes(fill = factor(tier)), width = 0.65, alpha = 0.9) +
      geom_text(aes(label = sprintf("%.1f VOR (Proj: %.1f)", vor, weighted_pts)), 
                hjust = -0.05, size = 4, fontface = "bold", color = "grey20") +
      scale_fill_brewer(palette = "Set2", name = "Tier") +
      expand_limits(x = max(filtered_data$vor, na.rm = TRUE) * 1.35) +
      labs(
        title = paste0("Draft Window: Picks ", min_pick, " to ", max_pick, " (Reference Pick: #", input$my_pick, ")"),
        subtitle = paste0("Scoring: ", input$scoring_format, " | Grouped by Tiers (Value Over Replacement)"),
        x = "Value Over Replacement (VOR)",
        y = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 12, color = "grey40"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 13, face = "bold", color = "grey20"),
        axis.text.x = element_text(size = 13, face = "bold", color = "grey20"),
        axis.title.x = element_text(size = 13, face = "bold", margin = margin(t = 10)),
        legend.position = "bottom",
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 10)
      )
  })
  
  # 1. Enhanced Volatility & Expert Spread Plot
  output$volatility_plot <- renderPlot({
    req(length(input$compare_players) >= 2)
    comparison_data <- combined_projections_clean %>% filter(player %in% input$compare_players)
    if(nrow(comparison_data) == 0) return()
    
    source_breakdown <- calc_league_pts(comparison_data, league_rules()) %>% 
      select(player, pos, team, data_src, total_pts)
    
    ggplot(source_breakdown, aes(x = total_pts, y = reorder(player, total_pts, FUN = median))) +
      geom_boxplot(aes(fill = pos), alpha = 0.3, outlier.shape = NA, width = 0.4) +
      geom_point(aes(color = data_src), size = 4, alpha = 0.9, position = position_jitter(height = 0.1, width = 0)) +
      scale_fill_brewer(palette = "Pastel1", guide = "none") +
      scale_color_brewer(palette = "Set1", name = "Data Source") +
      labs(
        title = "Expert Consensus & Outcome Spread",
        subtitle = paste0("Scoring: ", input$scoring_format, " | Comparing individual source projections across targets"),
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
  
  # 2. Statistical Summary Card Table
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
        "Expert Count" = Sources,
        "Mean Proj" = Consensus,
        "Floor (Min)" = Floor,
        "Ceiling (Max)" = Ceiling,
        "Spread" = Spread,
        "Std Dev" = Std_Dev
      )
  }, striped = TRUE, bordered = TRUE, spacing = "s", align = "c")
  
  # 3. Stat-Category Breakdown Stacked Bar Chart
  output$stat_breakdown_plot <- renderPlot({
    req(length(input$compare_players) >= 2)
    
    rules <- league_rules()
    
    stat_data <- weighted_projections %>%
      filter(player %in% input$compare_players) %>%
      mutate(
        Pass_Yds_Pts  = coalesce(pass_yds, 0) * rules$pass_yds,
        Pass_TD_Pts   = coalesce(pass_tds, 0) * rules$pass_tds,
        Pass_Int_Pts  = coalesce(pass_int, 0) * rules$pass_int,
        Rush_Yds_Pts  = coalesce(rush_yds, 0) * rules$rush_yds,
        Rush_TD_Pts   = coalesce(rush_tds, 0) * rules$rush_tds,
        Rec_Pts       = coalesce(rec, 0)      * rules$rec,
        Rec_Yds_Pts   = coalesce(rec_yds, 0)  * rules$rec_yds,
        Rec_TD_Pts    = coalesce(rec_tds, 0)  * rules$rec_tds
      ) %>%
      select(player, pos, team, ends_with("_Pts")) %>%
      pivot_longer(
        cols = ends_with("_Pts"),
        names_to = "stat_category",
        values_to = "pts_generated"
      ) %>%
      mutate(
        stat_category = str_remove(stat_category, "_Pts") %>% 
          str_replace_all("_", " ") %>% 
          tools::toTitleCase()
      )
    
    if(nrow(stat_data) == 0) return()
    
    ggplot(stat_data, aes(x = reorder(player, pts_generated, sum), y = pts_generated, fill = stat_category)) +
      geom_col(width = 0.6, alpha = 0.9) +
      coord_flip() +
      scale_fill_brewer(palette = "Set2", name = "Point Source") +
      labs(
        title = "Where Do The Points Come From?",
        subtitle = paste0("Scoring: ", input$scoring_format, " | Breakdown of fantasy point generation by category"),
        x = NULL,
        y = "Fantasy Points Generated"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(size = 11, color = "grey40"),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 13, face = "bold", color = "grey20"),
        axis.text.x = element_text(size = 12, face = "bold", color = "grey20"),
        legend.position = "bottom"
      )
  })
  
  # 4. Source-by-Source Pivot Table
  output$volatility_table <- renderTable({
    req(length(input$compare_players) >= 2)
    comparison_data <- combined_projections_clean %>% filter(player %in% input$compare_players)
    if(nrow(comparison_data) == 0) return()
    calc_league_pts(comparison_data, league_rules()) %>%
      select(player, pos, team, data_src, total_pts) %>%
      pivot_wider(names_from = data_src, values_from = total_pts) %>%
      mutate(across(where(is.numeric), ~ round(.x, 1)))
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  output$draft_board_table <- renderDT({
    datatable(
      draft_board_master() %>% select(model_adp, tier, player, pos, team, weighted_pts, vor, espn_pts, pts_diff, sources_count),
      options = list(pageLength = 15),
      selection = 'none'
    )
  })
}

shinyApp(ui = ui, server = server)