library(dplyr)
library(nnet)
library(ggplot2)
library(tidyr)

# Load the datasets
awards <- read.csv("awards_data.csv")
player_data <- read.csv("player_stats.csv")
career_dataset <- read.csv("career_dataset.csv")


# Selecting relevant columns from player_stats
selected_player_data <- player_data %>%
  select(
    season, nbapersonid, team,
    games, games_start, mins, fgm, fga,
    fgp, fgm3, fga3, fgp3, fgm2, fga2,
    fgp2, efg, ftm, fta, ftp,
    off_reb, def_reb, tot_reb,
    ast, steals, blocks, tov, tot_fouls,
    points, PER, FTr, off_reb_pct,
    def_reb_pct, tot_reb_pct, ast_pct,
    stl_pct, blk_pct, tov_pct, usg,
    OWS, DWS, WS, OBPM, DBPM, BPM, VORP
  )

# Remove duplicates from player_stats
player_stats <- selected_player_data %>%
  distinct(season, nbapersonid, .keep_all = TRUE)

# Merge the cleaned datasets
merged_data <- awards %>%
  left_join(player_stats, by = c("season", "nbapersonid"))

career_data <- career_dataset %>%
  distinct(nbapersonid, .keep_all = TRUE)

merged_data <- merged_data %>%
  left_join(career_data, by = "nbapersonid")

# Define your outcome variable (e.g., All-Star selection)
outcome_variable <- "career_outcome"

# Define the features you want to use for modeling
features <- c(
  "points", "ast", "tot_reb",
  "blocks", "stl_pct", "off_reb_pct",
  "def_reb_pct", "ast_pct", "blk_pct","fgm", 
  "mins", "fgm3", "fga3", "fgp3"
)

# Prepare the data for modeling
model_data <- merged_data %>%
  select(all_of(c(outcome_variable, features))) %>%
  na.omit()


# Example assuming your outcome variable is in a column named "career_outcome"
model_data$career_outcome <- as.integer(factor(model_data$career_outcome))

set.seed(123)
train_indices <- sample(nrow(model_data), 0.8 * nrow(model_data))
train_data <- model_data[train_indices, ]
test_data <- model_data[-train_indices, ]


library(nnet)

model <- multinom(career_outcome ~ ., data = train_data)

predictions <- predict(model, newdata = test_data)
# Example: Confusion Matrix
conf_matrix <- table(test_data$career_outcome, predictions)

# Assuming you have already trained your multinomial logistic regression model and made predictions

# Load necessary libraries
library(ggplot2)
library(tidyr)  # Make sure you have the tidyr library loaded

# Create a data frame with predicted probabilities
probabilities <- predict(model, newdata = test_data, type = "probs")
probabilities_df <- as.data.frame(probabilities)
colnames(probabilities_df) <- levels(factor(test_data$career_outcome))

# Add the actual outcome to the data frame
probabilities_df$Actual <- factor(test_data$career_outcome)

# Create a mapping of numeric class labels to meaningful class names
class_names <- c("Elite", "All-Star", "Starter", "Rotation", "Roster", "Out of League")

# Replace numeric class labels with meaningful class names
probabilities_df$Actual <- factor(probabilities_df$Actual, levels = 1:6, labels = class_names)

# Select the "Elite" class for visualization
class_to_visualize <- "Elite"

# Reshape the data for plotting using gather
probabilities_long <- gather(probabilities_df, key = "Class", value = "Probability", -Actual)

# Create a bar plot for the "Elite" class and other classes
ggplot(probabilities_long, aes(x = Class, y = Probability, fill = Actual)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Predicted Class", y = "Probability") +
  scale_fill_manual(name = "Actual Class", values = setNames(c("blue", "green", "red", "purple", "orange", "gray"), class_names)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste("Predicted Probabilities for", class_to_visualize, "Class"))

