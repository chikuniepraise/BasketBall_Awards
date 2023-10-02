library(dplyr)

# Select necessary columns from player_data dataset
player_col <- player_data %>%
  select(season, nbapersonid, games, games_start, mins)

# Select necessary columns from awards dataset
awards_col <- awards %>%
  select(
    "season", "nbapersonid", "all_nba_points_rk", "Bill Russell NBA Finals MVP",
    "All NBA First Team", "All NBA Second Team", "All NBA Third Team",
    "Most Valuable Player_rk", "Defensive Player Of The Year_rk",
    "all_star_game", "allstar_rk")


# Join the selected datasets
awards_n_players <- awards_col %>%
  left_join(player_col, by = c("nbapersonid", "season"))

View(awards_n_players)

# Define the adjust_minutes function (same as before)

adjust_minutes <- function(minutes, games) {
  adjusted_games <- ifelse(games == 66, games * (82/66), games)
  adjusted_minutes <- minutes * (adjusted_games / games)
  adjusted_minutes
}


player_seasons_outcomes <- awards_n_players %>%
  group_by(nbapersonid, season) %>%
  mutate(
    adjusted_minutes = adjust_minutes(mins, games),
    games_started_adjusted = games_start * (82/72), # Adjust for shortened seasons
    season_outcome = case_when(
      all_nba_points_rk %in% c(5, 6, 7) | `Bill Russell NBA Finals MVP` == 1 | 
        `Most Valuable Player_rk` == 1 | `Defensive Player Of The Year_rk` == 1 ~ "Elite",
      all_star_game == 1 ~ "All-Star",
      games_started_adjusted >= 41 | adjusted_minutes >= 2000 ~ "Starter",
      adjusted_minutes >= 1000 ~ "Rotation",
      adjusted_minutes > 0 ~ "Roster",
      TRUE ~ "Out of the League"
    )
  )

View(player_seasons_outcomes)
# Group by nbapersonid and calculate the highest level of success after first four seasons
career_data <- player_seasons_outcomes %>%
  group_by(nbapersonid) %>%
  summarize(
    career_outcome = max(season_outcome, na.rm = TRUE),
    first_four_years = min(season)
  )

# Join with player_data to get additional player information
career_data <- career_data %>%
  left_join(player_data %>% select(nbapersonid, player, draftyear), by = "nbapersonid")
career_data <- distinct(career_data)
# Print the resulting career dataset
View(career_data)


# Save the career dataset
write.csv(career_data, "career_dataset.csv", row.names = FALSE)
