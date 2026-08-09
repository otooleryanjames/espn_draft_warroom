library(shiny)
library(DT)
library(dplyr)

# Load your processed projections and manual tier mappings
load("app_data.RData")

# ==========================================
# 1. USER INTERFACE (UI)
# ==========================================
ui <- fluidPage(
  titlePanel("Fantasy Football Draft War Room"),
  
  tabsetPanel(
    # Existing or main tab for overall rankings if you have one, 
    # or we can jump straight into the quadrants:
    tabPanel("Draft War Room",
             fluidRow(
               column(6, 
                      h3("Quarterbacks (QB)"),
                      DTOutput("qb_quadrant_table")
               ),
               column(6, 
                      h3("Running Backs (RB)"),
                      DTOutput("rb_quadrant_table")
               )
             ),
             br(),
             fluidRow(
               column(6, 
                      h3("Wide Receivers (WR)"),
                      DTOutput("wr_quadrant_table")
               ),
               column(6, 
                      h3("Tight Ends (TE)"),
                      DTOutput("te_quadrant_table")
               )
             )
    )
  )
)

# ==========================================
# 2. SERVER LOGIC
# ==========================================
server <- function(input, output, session) {
  
  # Helper function to generate styled position tables for the quadrants
  render_position_quadrant <- function(df, position_filter) {
    df %>%
      filter(pos == position_filter) %>%
      select(pos_rank, player, team, weighted_pts, tier) %>%
      datatable(
        options = list(
          pageLength = 25, 
          dom = 't',       # Hides extra search bars to keep it looking like a clean board
          scrollY = "500px",
          scroller = TRUE,
          ordering = FALSE
        ),
        rownames = FALSE,
        colnames = c("Rank", "Player", "Team", "Proj Pts", "Tier")
      ) %>%
      formatStyle(
        'tier',
        backgroundColor = styleInterval(
          # 9 cutoffs create 10 buckets (Tiers 1 through 10)
          c(1, 2, 3, 4, 5, 6, 7, 8, 9), 
          c(
            '#e6f2ff', # Tier 1 (Elite)
            '#cce6ff', # Tier 2
            '#b3d9ff', # Tier 3
            '#e6ffe6', # Tier 4
            '#ccffcc', # Tier 5
            '#fffae6', # Tier 6
            '#fff3cc', # Tier 7
            '#ffe680', # Tier 8
            '#ffcccc', # Tier 9
            '#ff9999'  # Tier 10 (Deep bench)
          )
        )
      )
  }
  
  # Render the four quadrant outputs
  output$qb_quadrant_table <- renderDT({ render_position_quadrant(weighted_projections, "QB") })
  output$rb_quadrant_table <- renderDT({ render_position_quadrant(weighted_projections, "RB") })
  output$wr_quadrant_table <- renderDT({ render_position_quadrant(weighted_projections, "WR") })
  output$te_quadrant_table <- renderDT({ render_position_quadrant(weighted_projections, "TE") })
  
}

# Run the Shiny application
shinyApp(ui = ui, server = server)