# Running simulations on the remaining regular season schedule, then for playoffs, then for finals
library(elo)
library(dplyr)

# Remainder of regular season
reg_season <- nba.remaining.games.2019.2020
str(reg_season)
head(reg_season)

reg_season$home_team <- as.character(reg_season$home_team)
reg_season$visitor_team <- as.character(reg_season$visitor_team)

elos <- nba.elo.current.2019.2020
head(elos, 30)

elos$team <- as.character(elos$team)
str(elos)

# DF to store elos after each game for plotting
elo_history = data.frame(teams = as.character(elos$team))
elo_history$elo = elos$elo

# List of winners
winners <- c()

matchups <- reg_season[2:3]
head(matchups)
str(matchups)


row = 1 # For test purposes
for (row in 1:nrow(matchups)) {
  
  # Home Team and Away Team
  home <- matchups[row, "home_team"]
  away  <- matchups[row, "visitor_team"]
  
  # Pre-match ratings
  x = subset(elos, team == home)
  elo.A <- as.integer(c(x[3]))
  
  y = subset(elos, team == away)
  elo.B <- as.integer(c(y[3]))
  
  # Probability of winning
  prob.A <- elo.prob(elo.A, elo.B)
  prob.B <- elo.prob(elo.B, elo.A)
  
  # Sample from distribution of size 1 because if repeated the team with higher score is obviously expected to win. this makes it more "random"
  winner <- sample(c(home, away), size=1, prob=c(prob.A, prob.B))
  
  if (winner == home) {
    result = c(1)
  } else if (winner == away) {
    result = c(0)
  }
  
  # Let's update our ratings
  new_elo <- elo.calc(wins.A = result,
                      elo.A = elo.A, 
                      elo.B = elo.B, 
                      k = 20)
  
  # The results come back as a data.frame
  # with team A's new rating in row 1 / column 1
  # and team B's new rating in row 1 / column 2
  teamA_new_elo <- new_elo[1, 1]
  teamB_new_elo <- new_elo[1, 2]
  
  # We then update the ratings for teams A and B
  # and leave the other teams as they were
  elos <- elos %>%
    mutate(elo = if_else(team == home, teamA_new_elo,
                         if_else(team == away, teamB_new_elo, elo)))

}

# After a few minutes, you should get a nice teams data.frame, with the most up-to-date international Elo ratings for June 2018
elos %>%
  arrange(-elo) %>%
  head(30)

# Playoffs
# The top eight teams in each conference (East and West), ranked in order by win-loss records, qualify for the playoffs.

# Conference Championships
# At the end of the playoffs, the top two teams play each other in the Conference Finals, to determine the Conference Champions from each side, who then proceed to play in the NBA Finals.

# NBA Finals (7-Game Series)


















