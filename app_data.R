library(ffanalytics)
library(remotes)
library(dplyr)
library(purrr)
library(stringr)
library(jsonlite)
library(tidyr)
library(readr)
library(janitor)

# 1. READ & CLEAN FANTASYPROS CSV EXPORTS
fp_raw <- read_csv("fp_flex_proj.csv", show_col_types = FALSE) %>% clean_names()
fp_raw_qb <- read_csv("fp_qb_proj.csv", show_col_types = FALSE) %>% clean_names()

fp_clean_flex <- fp_raw %>%
  filter(!is.na(player), player != "") %>%
  mutate(
    pos = str_extract(pos, "^[A-Za-z]+"),
    pos = if_else(is.na(pos), "FLEX", pos),
    player = str_squish(player) %>% str_remove_all(" (Jr\\.|Sr\\.|II|III|IV)$") %>% str_trim(),
    data_src = "FantasyPros",
    rush_yds = suppressWarnings(as.numeric(yds_5)),
    rush_tds = suppressWarnings(as.numeric(tds_6)),
    rec = suppressWarnings(as.numeric(rec)),
    rec_yds = suppressWarnings(as.numeric(yds_8)),
    rec_tds = suppressWarnings(as.numeric(tds_9)),
    fumbles_lost = suppressWarnings(as.numeric(fl)),
    pass_yds = 0, pass_tds = 0, pass_int = 0
  ) %>%
  select(player, pos, team, pass_yds, pass_tds, pass_int, rush_yds, rush_tds, fumbles_lost, data_src, rec, rec_yds, rec_tds)

fp_clean_qb <- fp_raw_qb %>%
  filter(!is.na(player), player != "") %>%
  mutate(
    pos = "QB",
    player = str_squish(player) %>% str_remove_all(" (Jr\\.|Sr\\.|II|III|IV)$") %>% str_trim(),
    data_src = "FantasyPros",
    pass_yds = suppressWarnings(as.numeric(yds_5)),
    pass_tds = suppressWarnings(as.numeric(tds_6)),
    pass_int = suppressWarnings(as.numeric(ints)),
    rush_yds = suppressWarnings(as.numeric(yds_9)),
    rush_tds = suppressWarnings(as.numeric(tds_10)),
    fumbles_lost = suppressWarnings(as.numeric(fl)),
    rec = 0, rec_yds = 0, rec_tds = 0
  ) %>%
  select(player, pos, team, pass_yds, pass_tds, pass_int, rush_yds, rush_tds, fumbles_lost, data_src, rec, rec_yds, rec_tds)

fp_csv_final <- bind_rows(fp_clean_flex, fp_clean_qb)

# 2. SCRAPE NON-FP DATA SOURCES
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
  select(player, pos, team, pass_yds, pass_tds, pass_int, rush_yds, rush_tds, fumbles_lost, data_src, rec, rec_yds, rec_tds) %>% 
  distinct()

# 3. FETCH SLEEPER DATA
url_proj <- "https://api.sleeper.app/projections/nfl/2026?season_type=regular&position[]=QB&position[]=RB&position[]=WR&position[]=TE&order_by=ppr"
raw_proj <- fromJSON(url_proj, simplifyDataFrame = FALSE)

sleeper_projections <- map_df(raw_proj, function(p) {
  st <- p$stats %||% list()
  pl <- p$player %||% list()
  tibble(
    player_id = p$player_id %||% NA_character_,
    first_name = pl$first_name %||% NA_character_,
    last_name = pl$last_name %||% NA_character_,
    position = pl$position %||% NA_character_,
    team = p$team %||% pl$team %||% NA_character_,
    pts_ppr = st$pts_ppr %||% 0,
    pass_yd = st$pass_yd %||% 0,
    pass_td = st$pass_td %||% 0,
    pass_int = st$pass_int %||% 0,
    rush_yd = st$rush_yd %||% 0,
    rush_td = st$rush_td %||% 0,
    rec = st$rec %||% 0,
    rec_yd = st$rec_yd %||% 0,
    rec_td = st$rec_td %||% 0
  )
})

full_sleeper_df <- sleeper_projections %>%
  filter(position %in% c("QB", "RB", "WR", "TE")) %>%
  arrange(desc(pts_ppr)) %>%
  mutate(player = str_c(first_name, " ", last_name)) %>% 
  select(player, position, team, pass_yd, pass_td, pass_int, rush_yd, rush_td, rec, rec_yd, rec_td) %>%
  rename(pos = position, pass_yds = pass_yd, pass_tds = pass_td, rush_yds = rush_yd, rush_tds = rush_td, rec_yds = rec_yd, rec_tds = rec_td) %>%
  mutate(fumbles_lost = NA_real_, data_src = "sleeper") %>%
  select(player, pos, team, pass_yds, pass_tds, pass_int, rush_yds, rush_tds, fumbles_lost, data_src, rec, rec_yds, rec_tds)

# 4. MASTER BIND & CLEAN
combined_projections <- bind_rows(projections_with_source_stats, full_sleeper_df, fp_csv_final)
stat_cols <- c("pass_yds", "pass_tds", "pass_int", "rush_yds", "rush_tds", "rec", "rec_yds", "rec_tds")

combined_projections_clean <- combined_projections %>%
  filter(if_any(all_of(stat_cols), ~ !is.na(.x) & .x != 0)) %>%
  mutate(
    team = case_match(
      team,
      "JAX" ~ "JAC", "NEP" ~ "NE", "KCC" ~ "KC", "TBB" ~ "TB", "NOS" ~ "NO", "GBP" ~ "GB",
      "SFO" ~ "SF", "GNB" ~ "GB", "TAM" ~ "TB", "KAN" ~ "KC", "NOG" ~ "NO",
      "NEW" ~ "NE", "LVR" ~ "LV", "OAK" ~ "LV", "SDG" ~ "LAC", "STL" ~ "LAR",
      "RAM" ~ "LAR", "ARZ" ~ "ARI", "BLT" ~ "BAL", "CLV" ~ "CLE", "HST" ~ "HOU",
      "WSH" ~ "WAS",
      .default = team
    ),
    player = player %>% str_remove_all(" (Jr\\.|Sr\\.|II|III|IV)$") %>% str_trim()
  )

# 5. CREATE WEIGHTED PROJECTIONS MODEL
source_weights <- tibble(
  data_src = c("FantasyPros", "FantasySharks", "sleeper", "FFToday", "CBS"),
  weight   = c(1.00, 0.85, 0.85, 0.75, 0.65)
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
    across(all_of(stat_cols_weighted), ~ weighted.mean(.x, w = norm_weight, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(across(all_of(stat_cols_weighted), ~ ifelse(is.nan(.x), 0, .x)))

# 2. Define your baseline league scoring rules
league_rules <- list(
  pass_yds = 0.04, pass_tds = 4.00, pass_int = -2.00, 
  rush_yds = 0.10, rush_tds = 6.00, 
  rec = 1.00,      # Adjust to 0.5 for Half-PPR if needed
  rec_yds = 0.10,  rec_tds = 6.00
)

# 3. Helper function to calculate total fantasy points
calc_league_pts <- function(df, rules) {
  df %>%
    mutate(
      total_pts = (coalesce(pass_yds, 0) * rules$pass_yds) +
        (coalesce(pass_tds, 0) * rules$pass_tds) +
        (coalesce(pass_int, 0) * rules$pass_int) +
        (coalesce(rush_yds, 0) * rules$rush_yds) +
        (coalesce(rush_tds, 0) * rules$rush_tds) +
        (coalesce(rec, 0)      * rules$rec)     +
        (coalesce(rec_yds, 0)  * rules$rec_yds) +
        (coalesce(rec_tds, 0)  * rules$rec_tds)
    )
}

# 4. Process weighted projections, calculate points, and assign dynamic percentage-cliff tiers
weighted_projections <- calc_league_pts(weighted_projections, league_rules) %>%
  rename(weighted_pts = total_pts) %>%
  filter(pos %in% c("QB", "RB", "WR", "TE")) %>%
  group_by(pos) %>%
  arrange(desc(weighted_pts)) %>%
  mutate(
    max_pts = max(weighted_pts),
    pct_of_max = weighted_pts / max_pts,
    tier = case_when(
      pct_of_max >= 0.90 ~ 1,
      pct_of_max >= 0.80 ~ 2,
      pct_of_max >= 0.70 ~ 3,
      pct_of_max >= 0.60 ~ 4,
      pct_of_max >= 0.50 ~ 5,
      TRUE               ~ 6
    )
  ) %>%
  ungroup() %>%
  select(-max_pts, -pct_of_max)

# 5. Save everything back out to your RData file for the app
save(
  combined_projections_clean, 
  weighted_projections,
  file = "app_data.RData"
)
