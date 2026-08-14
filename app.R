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

# Clean apostrophes and quotes from player names globally to avoid HTML/JS selector breaks
if ("weighted_projections" %in% ls()) {
  weighted_projections$player <- gsub("['`’]", "", weighted_projections$player)
}
if ("combined_projections_clean" %in% ls()) {
  combined_projections_clean$player <- gsub("['`’]", "", combined_projections_clean$player)
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
      
      checkboxInput("show_vor_baselines", "Customize VOR Baselines", value = FALSE),
      
      conditionalPanel(
        condition = "input.show_vor_baselines == true",
        wellPanel(
          style = "background: #f8f9fa; padding: 10px;",
          h5("Replacement Baselines (VOR)"),
          numericInput("rep_qb", "QB Baseline Rank:", value = 16, min = 1, max = 50),
          numericInput("rep_rb", "RB Baseline Rank:", value = 30, min = 1, max = 70),
          numericInput("rep_wr", "WR Baseline Rank:", value = 46, min = 1, max = 80),
          numericInput("rep_te", "TE Baseline Rank:", value = 12, min = 1, max = 30)
        )
      ),
      
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
      selectizeInput("compare_players", "Select Players to Focus:",
                     choices = NULL, multiple = TRUE),
      
      hr(),
      h4("My Roster"),
      wellPanel(
        style = "background: #f8f9fa; padding: 10px;",
        uiOutput("my_roster_summary")
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Active Decision Hub", 
                 h4("Expert Consensus & Outcome Spread"),
                 p("Visualizing individual expert source projections and outcome volatility for selected targets."),
                 plotOutput("volatility_plot", height = "380px"),
                 hr(),
                 h4("Player Comparison Focus"),
                 p("Showing detailed metrics for your selected comparison players."),
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
  
  # Reactive value to keep track of drafted players
  my_drafted_players <- reactiveVal(character(0))
  
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
      select(-any_of(c("weighted_pts", "vor", "model_adp", "pos_rank", "tier_drop_1", "tier_drop_5", "tier_acceleration", "tier_group"))) %>%
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
        vor = weighted_pts - coalesce(baseline_pts, 0),
        
        # Drops & Acceleration
        tier_drop_1 = vor - lead(vor, 1),
        tier_drop_5 = vor - lead(vor, 5),
        tier_acceleration = tier_drop_5 - tier_drop_1,
        
        # Span-based tiering 
        max_pos_vor = max(vor, na.rm = TRUE),
        min_pos_vor = min(vor, na.rm = TRUE),
        vor_span = pmax(5.0, max_pos_vor - min_pos_vor),
        
        tier_step = vor_span / 9.0,
        tier_group = ceiling((max_pos_vor - vor + 0.01) / tier_step)
      ) %>%
      ungroup() %>%
      select(-rep_rank, -baseline_pts, -max_pos_vor, -min_pos_vor, -vor_span, -tier_step) %>%
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
        SoS = get_sos_label(sos),
        Tier = paste("Tier", tier_group)
      ) %>%
      arrange(tier_group, desc(vor)) %>%
      select(Tier, player, pos, team, weighted_pts, vor, SoS, espn_pts) %>%
      mutate(
        weighted_pts = round(weighted_pts),
        vor = round(vor),
        espn_pts = round(espn_pts)
      )
  })
  
  observe({
    board <- draft_board_master()
    updateSelectizeInput(session, "compare_players",
                         choices = sort(board$player),
                         server = TRUE)
  })
  
  # Standard shinyclick / DT proxy event handling using standard button IDs
  observeEvent(input$draft_player, {
    req(input$draft_player)
    p_name <- input$draft_player
    current_drafted <- my_drafted_players()
    if (!(p_name %in% current_drafted)) {
      my_drafted_players(c(current_drafted, p_name))
    }
  })
  
  observeEvent(input$undraft_player, {
    req(input$undraft_player)
    p_name <- input$undraft_player
    current_drafted <- my_drafted_players()
    my_drafted_players(setdiff(current_drafted, p_name))
  })
  
  output$my_roster_summary <- renderUI({
    drafted <- my_drafted_players()
    
    shell_slots <- c("QB", "RB", "RB", "WR", "WR", "TE", "FLEX", "BN", "BN", "BN", "BN", "BN")
    
    if (length(drafted) == 0) {
      assigned_df <- data.frame(slot = shell_slots, player = "—", pos = "", team = "", stringsAsFactors = FALSE)
    } else {
      roster_df <- combined_ranking() %>%
        filter(player %in% drafted) %>%
        arrange(desc(weighted_pts))
      
      assigned_details <- vector("list", length(shell_slots))
      unassigned_indices <- seq_len(nrow(roster_df))
      
      qb_indices <- which(shell_slots == "QB")
      for (s in qb_indices) {
        match_idx <- which(unassigned_indices %in% which(roster_df$pos[unassigned_indices] == "QB"))
        if (length(match_idx) > 0) {
          p_idx <- unassigned_indices[match_idx[1]]
          assigned_details[[s]] <- roster_df[p_idx, ]
          unassigned_indices <- setdiff(unassigned_indices, p_idx)
        }
      }
      
      rb_indices <- which(shell_slots == "RB")
      for (s in rb_indices) {
        match_idx <- which(unassigned_indices %in% which(roster_df$pos[unassigned_indices] == "RB"))
        if (length(match_idx) > 0) {
          p_idx <- unassigned_indices[match_idx[1]]
          assigned_details[[s]] <- roster_df[p_idx, ]
          unassigned_indices <- setdiff(unassigned_indices, p_idx)
        }
      }
      
      wr_indices <- which(shell_slots == "WR")
      for (s in wr_indices) {
        match_idx <- which(unassigned_indices %in% which(roster_df$pos[unassigned_indices] == "WR"))
        if (length(match_idx) > 0) {
          p_idx <- unassigned_indices[match_idx[1]]
          assigned_details[[s]] <- roster_df[p_idx, ]
          unassigned_indices <- setdiff(unassigned_indices, p_idx)
        }
      }
      
      te_indices <- which(shell_slots == "TE")
      for (s in te_indices) {
        match_idx <- which(unassigned_indices %in% which(roster_df$pos[unassigned_indices] == "TE"))
        if (length(match_idx) > 0) {
          p_idx <- unassigned_indices[match_idx[1]]
          assigned_details[[s]] <- roster_df[p_idx, ]
          unassigned_indices <- setdiff(unassigned_indices, p_idx)
        }
      }
      
      flex_indices <- which(shell_slots == "FLEX")
      for (s in flex_indices) {
        match_idx <- which(unassigned_indices %in% which(roster_df$pos[unassigned_indices] %in% c("RB", "WR", "TE")))
        if (length(match_idx) > 0) {
          p_idx <- unassigned_indices[match_idx[1]]
          assigned_details[[s]] <- roster_df[p_idx, ]
          unassigned_indices <- setdiff(unassigned_indices, p_idx)
        }
      }
      
      bn_indices <- which(shell_slots == "BN")
      for (s in bn_indices) {
        if (length(unassigned_indices) > 0) {
          p_idx <- unassigned_indices[1]
          assigned_details[[s]] <- roster_df[p_idx, ]
          unassigned_indices <- unassigned_indices[-1]
        }
      }
      
      if (length(unassigned_indices) > 0) {
        for (p_idx in unassigned_indices) {
          shell_slots <- c(shell_slots, "BN")
          assigned_details <- c(assigned_details, list(roster_df[p_idx, ]))
        }
      }
      
      assigned_df <- data.frame(
        slot = shell_slots,
        stringsAsFactors = FALSE
      )
      
      assigned_df$player <- sapply(assigned_details, function(x) {
        if (is.null(x)) "—" else x$player
      })
      assigned_df$pos_team <- sapply(assigned_details, function(x) {
        if (is.null(x)) "" else paste0(" (", x$pos, " - ", x$team, ")")
      })
    }
    
    tagList(
      p(strong(paste0("Drafted: ", length(drafted)))),
      hr(style = "margin: 5px 0;"),
      tags$ul(
        style = "padding-left: 0; list-style-type: none; margin-bottom: 0;",
        lapply(seq_len(nrow(assigned_df)), function(i) {
          slot_name <- assigned_df$slot[i]
          p_name <- assigned_df$player[i]
          pt_info <- if("pos_team" %in% names(assigned_df)) assigned_df$pos_team[i] else ""
          
          is_empty <- (p_name == "—")
          
          tags$li(
            style = "margin-bottom: 5px; font-size: 13px;",
            span(style = "font-weight: bold; display: inline-block; width: 45px; color: #555;", paste0(slot_name, ":")),
            span(style = if(is_empty) "color: #adb5bd; font-style: italic;" else "color: #212529;", paste0(p_name, pt_info))
          )
        })
      )
    )
  })
  
  output$window_picks_table <- renderDT({
    if (is.null(input$compare_players) || length(input$compare_players) == 0) {
      window_data <- combined_ranking() %>% head(0)
    } else {
      window_data <- combined_ranking() %>%
        filter(player %in% input$compare_players) %>%
        arrange(desc(weighted_pts))
    }
    
    drafted_set <- my_drafted_players()
    global_vor_range <- range(combined_ranking()$vor, na.rm = TRUE)
    
    if (nrow(window_data) > 0) {
      window_data <- window_data %>%
        mutate(
          dyn_threshold = pmax(1.0, 0.35 * coalesce(pos_sd_delta, 2.0)),
          Model_Outlook = case_when(
            net_delta > dyn_threshold    ~ "🔥 Model Likes More",
            net_delta < -dyn_threshold   ~ "❄️ Model Likes Less",
            TRUE                         ~ "⚖️ Matches ESPN"
          ),
          "Rank" = overall_adp,
          "Tier" = paste("Tier", tier_group),
          "Player" = player,
          "Pos" = pos,
          "Team" = team,
          "Model Proj" = round(weighted_pts, 1),
          "ESPN Proj" = round(espn_pts, 1),
          "VOR" = round(vor, 1),
          "SoS" = get_sos_label(sos),
          "Action" = sapply(player, function(p_name) {
            if (p_name %in% drafted_set) {
              as.character(actionButton(
                inputId = paste0("undraft_", gsub("[^a-zA-Z0-9]", "", p_name)),
                label = "Undo",
                class = "btn-danger btn-xs",
                onclick = sprintf("Shiny.setInputValue('undraft_player', '%s', {priority: 'event'})", p_name)
              ))
            } else {
              as.character(actionButton(
                inputId = paste0("draft_", gsub("[^a-zA-Z0-9]", "", p_name)),
                label = "Draft",
                class = "btn-success btn-xs",
                onclick = sprintf("Shiny.setInputValue('draft_player', '%s', {priority: 'event'})", p_name)
              ))
            }
          })
        ) %>%
        select(Action, Rank, Tier, Player, Pos, Team, VOR, SoS, "Model Proj", "ESPN Proj", Model_Outlook)
    } else {
      window_data <- data.frame(
        Action = character(0), Rank = numeric(0), Tier = character(0), 
        Player = character(0), Pos = character(0), Team = character(0), 
        VOR = numeric(0), SoS = character(0), 
        "Model Proj" = numeric(0), "ESPN Proj" = numeric(0), Model_Outlook = character(0)
      )
    }
    
    dt_obj <- datatable(
      window_data,
      escape = FALSE,
      options = list(pageLength = 25, dom = 't', ordering = FALSE),
      rownames = FALSE
    )
    
    if (nrow(window_data) > 0) {
      dt_obj <- dt_obj %>%
        formatStyle(
          'VOR',
          background = styleColorBar(global_vor_range, '#d4edda'),
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
    }
    dt_obj
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