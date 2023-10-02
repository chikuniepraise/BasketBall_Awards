
install.packages("tidyverse")
# Data sets
library(tidyverse)
# Note, you will likely have to change these paths. If your data is in the same folder as this project, 
# the paths will likely be fixed for you by deleting ../../Data/awards_project/ from each string.
awards <- read_csv("awards_data.csv")
player_data <- read_csv("player_stats.csv")
team_data <- read_csv("team_stats.csv")
rebounding_data <- read_csv("team_rebounding_data_22.csv")



'**QUESTION:** What is the average number of points per game for players in 
the 2007-2021 seasons who won All NBA First, Second, and Third teams
(**not** the All Defensive Teams), as well as for players who were in the All-Star Game 
(**not** the rookie all-star game)?'


nba_n_star <- awards %>%
  filter(season >= 2007 & season <= 2021) %>%
  filter(
    all_nba_points_rk %in% c(5, 6, 7) |
      all_star_game == "TRUE"
  )%>%
  select(
    "season", "nbapersonid", "all_nba_points_rk", "all_star_game", "All NBA First Team", 
    "All NBA Second Team", "All NBA Third Team")


# Selecting only the necessary columns from player_data

game_points <- player_data %>%
  select(nbapersonid, points)

# Merge the filtered awards data with the selected player data
point_data <- nba_n_star %>%
  left_join(game_points, by = "nbapersonid")

str(point_data)


# Calculate the average points per game for each group
average_1st_team <- mean(point_data$points[point_data$`All NBA First Team` == "1"], na.rm = TRUE)
average_2nd_team <- mean(point_data$points[point_data$`All NBA Second Team` == "1"], na.rm = TRUE)
average_3rd_team <- mean(point_data$points[point_data$`All NBA Third Team` == "1"], na.rm = TRUE)
average_all_star <- mean(point_data$points[point_data$all_star_game == "TRUE"], na.rm = TRUE)

# the results
cat("1st Team:", round(average_1st_team, 1), "points per game\n")
cat("2nd Team:", round(average_2nd_team, 1), "points per game\n")
cat("3rd Team:", round(average_3rd_team, 1), "points per game\n")
cat("All-Star:", round(average_all_star, 1), "points per game\n")


# Calculating the average points per game
average_points_ <- mean(point_data$points, na.rm = TRUE)

cat("Average Points Per Game:", average_points_)



### Question 2  

'**QUESTION:** What was the average number of years of experience in the league 
it takes for players to make their first All NBA Selection 
(1st, 2nd, or 3rd team)? 
Please limit your sample to players drafted in 2007 or later who did eventually 
go on to win at least one All NBA selection. For example:
  
- Luka Doncic is in the dataset as 2 years. He was drafted in 2018 and won his 
first All NBA award in 2019 (which was his second season).  
- LeBron James is not in this dataset, as he was drafted prior to 2007.  
- Lu Dort is not in this dataset, as he has not received any All NBA honors.'  

colnames(awards)
colnames(player_data)


# Filtering for players drafted in 2007 or later who won All NBA selection
nba_select <- awards %>%
  filter(
    season >= 2007,
    (all_nba_points_rk %in% c(5, 6, 7))
  )

# Grouping by nbapersonid and calculating the minimum season they won NBA selection
first_nba_season <- nba_select %>%
  group_by(nbapersonid) %>%
  summarize(first_nba_season = min(season))

# Selecting only necessary columns from player data
player_draft <- player_data %>%
  distinct(nbapersonid, .keep_all = TRUE) %>%
  select(nbapersonid, draftyear, points, player)

# Merge with the player data
data_merged <- first_nba_season %>%
  left_join(player_draft, by = "nbapersonid")
View(data_merged)


# Calculating the years of experience to make the first All NBA selection
data_merged$years_to_nba <- data_merged$first_nba_season - data_merged$draftyear

# Printing the result for each player
for (i in 1:nrow(data_merged)) {
  player_name <- data_merged[i, "player"]
  years_to_nba <- data_merged[i, "years_to_nba"]
  draft_year <- data_merged[i, "draftyear"]
  first_nba_year <- data_merged[i, "first_nba_season"]
  
  if (years_to_nba == 1) {
    message <- paste(player_name, "is in the dataset as", years_to_nba, "year. He was drafted in", draft_year,
                     "and won his first All NBA award in", first_nba_year, ".")
  } else if (years_to_nba > 1) {
    message <- paste(player_name, "is in the dataset as", years_to_nba, "years. He was drafted in", draft_year,
                     "and won his first All NBA award in", first_nba_year, ".")
  } else {
    message <- paste(player_name, "is not in this dataset, as he has not received any All NBA honors.")
  }
  
  cat(message, "\n")
}




'### Question 3  

**QUESTION:** There are 73 players in the `player_data` dataset who have 2010 
listed as their draft year. How many of those players have a **career** outcome 
in each of the 6 buckets? 
'

#loading the career dataset
career_data <- read_csv("career_dataset.csv")

# Filtering players with draftyear = 2010
players_draft_year_2010 <- player_data %>%
  filter(draftyear == 2010) %>%
  distinct(nbapersonid)

# Join with career dataset  
year_2010_outcomes <- players_draft_year_2010 %>%
  left_join(career_data, by = "nbapersonid") %>%
  group_by(career_outcome) %>%
  summarise(count = n())

# Filling NAs with 0 
year_2010_outcomes$count[is.na(year_2010_outcomes$count)] <- 0


career_outcomes <- unique(year_2010_outcomes$career_outcome)

for (outcome in career_outcomes) {
  if(!is.na(outcome)) {
    outcome_count <- sum(year_2010_outcomes$career_outcome == outcome, na.rm = TRUE)
    cat(outcome, ": ", outcome_count, " players.\n")
  }
}

