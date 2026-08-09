library(ffanalytics)
library(remotes)
library(dplyr)
library(purrr)
library(stringr)
library(shiny)
library(DT)
library(jsonlite)
library(tidyr)
library(ggplot2)
library(janitor)

#############################################################
# 1. READ & CLEAN FANTASYPROS CSV EXPORTS ###################
#############################################################

fp_raw <- read_csv("fp_flex_proj.csv", show_col_types = FALSE) %>%
  clean_names()

fp_raw_qb <- read_csv("fp_qb_proj.csv", show_col_types = FALSE) %>%
  clean_names()

# Transform and clean Flex data (WR, RB, TE)
fp_clean_flex <- fp_raw %>%
  filter(!is.na(player), player != "") %>%
  mutate(
    pos = str_extract(pos, "^[A-Za-z]+"),
    pos = if_else(is.na(pos), "FLEX", pos),
    
    player = str_squish(player) %>%
      str_remove_all(" (Jr\\.|Sr\\.|II|III|IV)$") %>%
      str_trim(),
    
    data_src = "FantasyPros",
    
    rush_yds     = suppressWarnings(as.numeric(yds_5)),
    rush_tds     = suppressWarnings(as.numeric(tds_6)),
    rec          = suppressWarnings(as.numeric(rec)),
    rec_yds      = suppressWarnings(as.numeric(yds_8)),
    rec_tds      = suppressWarnings(as.numeric(tds_9)),
    fumbles_lost = suppressWarnings(as.numeric(fl)),
    
    pass_yds     = 0,
    pass_tds     = 0,
    pass_int     = 0
  ) %>%
  select(player, pos, team, pass_yds, pass_tds, pass_int, rush_yds, rush_tds, fumbles_lost, data_src, rec, rec_yds, rec_tds)

# Transform and clean QB data
fp_clean_qb <- fp_raw_qb %>%
  filter(!is.na(player), player != "") %>%
  mutate(
    pos = "QB",
    
    player = str_squish(player) %>%
      str_remove_all(" (Jr\\.|Sr\\.|II|III|IV)$") %>%
      str_trim(),
    
    data_src = "FantasyPros",
    
    pass_yds     = suppressWarnings(as.numeric(yds_5)),
    pass_tds     = suppressWarnings(as.numeric(tds_6)),
    pass_int     = suppressWarnings(as.numeric(ints)),
    rush_yds     = suppressWarnings(as.numeric(yds_9)),
    rush_tds     = suppressWarnings(as.numeric(tds_10)),
    fumbles_lost = suppressWarnings(as.numeric(fl)),
    
    rec          = 0,
    rec_yds      = 0,
    rec_tds      = 0
  ) %>%
  select(player, pos, team, pass_yds, pass_tds, pass_int, rush_yds, rush_tds, fumbles_lost, data_src, rec, rec_yds, rec_tds)

# Combine both cleaned FantasyPros sources into a unified frame
fp_csv_final <- bind_rows(fp_clean_flex, fp_clean_qb)


######################################
# 2. SCRAPE NON-FP DATA SOURCES ######
######################################

scraped_data <- scrape_data(
  src = c("CBS", "ESPN", "NumberFire", "FFToday", "FantasySharks", "NFL", "Yahoo"),
  pos = c("QB", "RB", "WR", "TE"),
  season = 2026,
  week = 0
)

raw_stats_all <- bind_rows(scraped_data)

projections <- projections_table(scraped_data) %>%
  add_player_info() %>%
  add_adp() %>%
  add_ecr()

projections_with_source_stats <- raw_stats_all %>%
  left_join(
    projections %>% select(id, pos, points, rank, pos_rank, tier, adp, overall_ecr),
    by = c("id", "pos")
  ) %>%
  mutate(
    player = case_when(
      data_src == "FantasySharks" & str_detect(player, ",") ~ str_replace(player, "^(.*),\\s*(.*)$", "\\2 \\1"),
      TRUE ~ player
    ),
    player = str_squish(player)
  ) %>% 
  select(player, pos, team, pass_yds, pass_tds, pass_int, rush_yds, rush_tds, 
         fumbles_lost, data_src, rec, rec_yds, rec_tds) %>% 
  distinct()


##############################################
# 3. FETCH SLEEPER DATA ######################
##############################################

url_proj <- "https://api.sleeper.app/projections/nfl/2026?season_type=regular&position[]=QB&position[]=RB&position[]=WR&position[]=TE&order_by=ppr"
raw_proj <- fromJSON(url_proj, simplifyDataFrame = FALSE)

sleeper_projections <- map_df(raw_proj, function(p) {
  st <- p$stats %||% list()
  pl <- p$player %||% list()
  
  tibble(
    player_id   = p$player_id %||% NA_character_,
    first_name  = pl$first_name %||% NA_character_,
    last_name   = pl$last_name %||% NA_character_,
    position    = pl$position %||% NA_character_,
    team        = p$team %||% pl$team %||% NA_character_,
    gp          = st$gp %||% NA_real_,
    pts_ppr     = st$pts_ppr %||% 0,
    pts_half    = st$pts_half_ppr %||% 0,
    pts_std     = st$pts_std %||% 0,
    pass_yd     = st$pass_yd %||% 0,
    pass_td     = st$pass_td %||% 0,
    pass_int    = st$pass_int %||% 0,
    rush_yd     = st$rush_yd %||% 0,
    rush_td     = st$rush_td %||% 0,
    rec         = st$rec %||% 0,
    rec_yd      = st$rec_yd %||% 0,
    rec_td      = st$rec_td %||% 0,
    adp_ppr     = st$adp_ppr %||% NA_real_
  )
})

full_sleeper_df <- sleeper_projections %>%
  filter(position %in% c("QB", "RB", "WR", "TE")) %>%
  arrange(desc(pts_ppr)) %>%
  mutate(player = str_c(first_name, " ", last_name)) %>% 
  select(player, position, team, pass_yd, pass_td, pass_int, rush_yd, rush_td, rec, rec_yd, rec_td) %>%
  rename(
    pos      = position,
    pass_yds = pass_yd,
    pass_tds = pass_td,
    rush_yds = rush_yd,
    rush_tds = rush_td,
    rec_yds  = rec_yd,
    rec_tds  = rec_td
  ) %>%
  mutate(
    fumbles_lost = NA_real_,
    data_src     = "sleeper"
  ) %>%
  select(player, pos, team, pass_yds, pass_tds, pass_int, rush_yds, rush_tds, fumbles_lost, data_src, rec, rec_yds, rec_tds)


##############################################
# 4. MASTER BIND (Scraped + Sleeper + FP) ####
##############################################

combined_projections <- bind_rows(
  projections_with_source_stats, 
  full_sleeper_df, 
  fp_csv_final
)

stat_cols <- c("pass_yds", "pass_tds", "pass_int", "rush_yds", "rush_tds", "rec", "rec_yds", "rec_tds")

combined_projections_clean <- combined_projections %>%
  filter(if_any(all_of(stat_cols), ~ !is.na(.x) & .x != 0)) %>%
  mutate(
    team = case_match(
      team,
      "JAX" ~ "JAC",
      "NEP" ~ "NE", "KCC" ~ "KC", "TBB" ~ "TB", "NOS" ~ "NO", "GBP" ~ "GB",
      "SFO" ~ "SF", "GNB" ~ "GB", "TAM" ~ "TB", "KAN" ~ "KC", "NOG" ~ "NO",
      "NEW" ~ "NE", "LVR" ~ "LV", "OAK" ~ "LV", "SDG" ~ "LAC", "STL" ~ "LAR",
      "RAM" ~ "LAR", "ARZ" ~ "ARI", "BLT" ~ "BAL", "CLV" ~ "CLE", "HST" ~ "HOU",
      "WSH" ~ "WAS",
      .default = team
    ),
    player = player %>%
      str_remove_all(" (Jr\\.|Sr\\.|II|III|IV)$") %>%
      str_trim()
  )

#############################################################
# 5. CREATE WEIGHTED PROJECTIONS MODEL ######################
#############################################################

source_weights <- tibble(
  data_src = c("FantasyPros", "FantasySharks", "sleeper", "FFToday", "CBS"),
  weight   = c(1.00,          0.85,          0.80,      0.75,      0.35)
)

stat_cols_weighted <- c("pass_yds", "pass_tds", "pass_int", "rush_yds", "rush_tds", "fumbles_lost", "rec", "rec_yds", "rec_tds")

weighted_projections <- combined_projections_clean %>%
  filter(data_src != "ESPN") %>% 
  left_join(source_weights, by = "data_src") %>%
  mutate(weight = coalesce(weight, 1.0)) %>%
  group_by(player, pos, team) %>%
  mutate(norm_weight = weight / sum(weight, na.rm = TRUE)) %>%
  summarise(
    sources_count = n(),
    sources_used  = paste(sort(unique(data_src)), collapse = ", "),
    across(
      all_of(stat_cols_weighted),
      ~ weighted.mean(.x, w = norm_weight, na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  mutate(across(all_of(stat_cols_weighted), ~ ifelse(is.nan(.x), 0, .x)))

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
        (rec_c      * rules$rec)      +
        (rec_yds_c  * rules$rec_yds)  +
        (rec_tds_c  * rules$rec_tds)
    ) %>%
    select(-ends_with("_c"))
}

##############################
# 6. DRAFT DAY APP (DYNAMIC) #
##############################

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
    calc_league_pts(weighted_projections, league_rules()) %>%
      rename(weighted_pts = total_pts)
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