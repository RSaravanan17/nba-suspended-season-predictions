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


#row = 1 # For test purposes
for (row in 1:nrow(matchups)) {
  
  # Home Team and Away Team
  home <- matchups[row, "home_team"]
  away  <- matchups[row, "visitor_team"]
  
  # Pre-match ratings
  x = subset(elos, team == home)
  elo.A <- (as.integer(c(x[3])) + as.integer(10)) # plus 10/100 represents HCA
  
  y = subset(elos, team == away)
  elo.B <- as.integer(c(y[3]))
  
  # Probability of winning
  prob.A <- elo.prob(elo.A, elo.B)
  prob.B <- elo.prob(elo.B, elo.A)
  
  # Sample from distribution of size 1 because if repeated the team with higher score is obviously expected to win. this makes it more "random"
  winner <- sample(c(home, away), size=1, prob=c(prob.A, prob.B))
  
  # Add winner to list of winners for each matchup
  winners <- c(winners, winner)
  
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
  
  # Add updated elos to elo history (FIX THIS)
  elo_history[, paste(as.character("game"), as.character(row), sep="")] <- elos$elo
  
}

# After a few minutes, you should get a nice teams data.frame, with the most up-to-date international Elo ratings for June 2018
elos %>%
  arrange(-elo) %>%
  head(30)

# Add winners list to the schedule
reg_season$winners = winners
head(reg_season)

# Show Elo History
head(elo_history)

# Playoffs
# The top eight teams in each conference (East and West), ranked in order by win-loss records, qualify for the playoffs.
# Eastern Conference
'
EC <- c("Milwaukee Bucks", "Toronto Raptors", "Boston Celtics", "Miami Heat", "Indiana Pacers", "Philadelphia 76ers", "Brooklyn Nets", "Orlando Magic", 
        "Washington Wizards", "Charlotte Hornets", "Chicago Bulls", "New York Knicks", "Detroit Pistons", "Atlanta HAwks", "Cleveland Cavaliers")
# Western Conference
WC <- c("Los Angeles Lakers", "Los Angeles Clippers", "Denver Nuggets", "Utah Jazz", "Oklahoma City Thunder", "Houston Rockets", "Dallas Mavericks", "Memphis Grizzlies", 
        "Portland Trail Blazers", "New Orleans Pelicans", "Sacramento Kings", "San Antonio Spurs", "Phoenix Suns", "Minnesota Timberwolves", "Golden State Warriors")

eastern_conference = data.frame(team = EC, elo=0, stringsAsFactors = FALSE)
western_conference = data.frame(team = WC, elo=0, stringsAsFactors = FALSE)


for (row in 1:nrow(eastern_conference)) {
  team <- eastern_conference[row, "team"]
  elo  <- eastern_conference[row, "elo"]
  
  team_elo = subset(elos, team == team)
  elo = team_elo
}

for (row in 1:nrow(western_conference)) {
  team <- western_conference[row, "team"]
  elo  <- western_conference[row, "elo"]
  
  team_elo = subset(elos, team == team)
  elo = team_elo
}

# Top 8 in EC
eastern_conference %>%
  arrange(-elo) %>%
  head(8)

# Top 8 in WC
western_conference %>%
  arrange(-elo) %>%
  head(8)
'

# Different Way (TOP 8 TEAMS FROM EACH CONFERENCE)
WC_teams <- elos %>%
  filter(team %in% c("Los Angeles Lakers", "Los Angeles Clippers", "Denver Nuggets", "Utah Jazz", "Oklahoma City Thunder", "Houston Rockets", "Dallas Mavericks", "Memphis Grizzlies", 
                     "Portland Trail Blazers", "New Orleans Pelicans", "Sacramento Kings", "San Antonio Spurs", "Phoenix Suns", "Minnesota Timberwolves", "Golden State Warriors")) %>%
  arrange(-elo) %>%
  head(8)


EC_teams <- elos %>%
  filter(team %in% c("Milwaukee Bucks", "Toronto Raptors", "Boston Celtics", "Miami Heat", "Indiana Pacers", "Philadelphia 76ers", "Brooklyn Nets", "Orlando Magic", 
                     "Washington Wizards", "Charlotte Hornets", "Chicago Bulls", "New York Knicks", "Detroit Pistons", "Atlanta HAwks", "Cleveland Cavaliers")) %>%
  arrange(-elo) %>%
  head(8)

# Each team earns a spot on the playoff bracket, known as a "seed," which will determine what team they will face off against.
# Seeding is based off of win/loss with ties becomign a bit more complicated through further evaluation
# For this project we will seeed based off of elo scores (very low probability of ties in scores). This also makes sense because elos are based on W/L.
# The conference DFs are already ordered by elo, so we can just put 1-8 as seed
WC_teams$seed <- c(1:8)

EC_teams$seed <- c(1:8)

# Technically there is a system in place designed for HCA in playoffs given to the team with the better record/higher seed
# Each NBA playoff series is composed of 4 to 7 games that are played in a 2-2-1-1-1 format. 
# This means that the team with the home-court advantage hosts games 1, 2, 5, and 7 while its opponent hosts games 3, 4, and 6, with games 5-7 only being played if necessary.
# However, because this complicates our implementation, we will ignore HCA for the playoffs and finals (although it does sometimes have an impact on the result)

# 1st Round - Playoffs (16 teams): Best-of-7 Series
# 1 vs 8, 2 vs 7, 3 vs 6, 4 vs 5
# Make dataframe for matchups
playoffs = data.frame(matrix(ncol = 2, nrow = 8))
colnames(playoffs) <- c("team1", "team2")

# Playoff Schedule
playoffs$team1 <- c(WC_teams[1,2], WC_teams[2,2], WC_teams[3,2], WC_teams[4,2], EC_teams[1,2], EC_teams[2,2], EC_teams[3,2], EC_teams[4,2])
playoffs$team2 <- c(WC_teams[8,2], WC_teams[7,2], WC_teams[6,2], WC_teams[5,2], EC_teams[8,2], EC_teams[7,2], EC_teams[6,2], EC_teams[5,2])

first_round <- c()
#row = 1 # For test purposes
#i = 1 # For test purposes
for (row in 1:nrow(playoffs)) {
  
  # Home Team and Away Team
  team1 <- playoffs[row, "team1"]
  team2  <- playoffs[row, "team2"]
  
  # For loop here? for i in 1:7
  series <- c()
  for (i in 1:7) {
    # Pre-match ratings
    x = subset(elos, team == team1)
    elo.A <- (as.integer(c(x[3])))
    
    y = subset(elos, team == team2)
    elo.B <- as.integer(c(y[3]))
    
    # Probability of winning
    prob.A <- elo.prob(elo.A, elo.B)
    prob.B <- elo.prob(elo.B, elo.A)
    
    # Best of 7 series sampling
    winner <- sample(c(team1, team2), size=1, prob=c(prob.A, prob.B), replace=TRUE) # size = 7?
    series <- c(series, winner)
    
    # Add winner to list of winners for each matchup
    #first_round <- c(first_round, winner)
    
    if (winner == team1) {
      result = c(1)
    } else if (winner == team2) {
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
      mutate(elo = if_else(team == team1, teamA_new_elo,
                           if_else(team == team2, teamB_new_elo, elo)))
    
    # Add updated elos to elo history
    elo_history[, paste(as.character("game"), as.character(row), sep="")] <- elos$elo
    
    # Check for best of 7
    team1_wins <- length(which(series == team1))
    team2_wins <- length(which(series == team2))
    if (team1_wins == 4) {
      first_round <- c(first_round, team1)
      break
    } else if (team2_wins == 4) {
      first_round <- c(first_round, team2)
      break
    }
  }
}

#elo_history
first_round


# 2nd Round - Conference Semifinals (8 Teams): Best-of-7 Series
semi = data.frame(matrix(ncol = 2, nrow = 4))
colnames(semi) <- c("team1", "team2")

# Playoff Schedule
# Western Conference ---- Eastern Conference
semi$team1 <- c(first_round[1], first_round[3], first_round[5], first_round[7])
semi$team2 <- c(first_round[2], first_round[4], first_round[6], first_round[8]) 

second_round <- c()
#row = 1 # For test purposes
#i = 1 # For test purposes
for (row in 1:nrow(semi)) {
  
  # Home Team and Away Team
  team1 <- semi[row, "team1"]
  team2  <- semi[row, "team2"]
  
  # For loop here? for i in 1:7
  series <- c()
  for (i in 1:7) {
    # Pre-match ratings
    x = subset(elos, team == team1)
    elo.A <- (as.integer(c(x[3])))
    
    y = subset(elos, team == team2)
    elo.B <- as.integer(c(y[3]))
    
    # Probability of winning
    prob.A <- elo.prob(elo.A, elo.B)
    prob.B <- elo.prob(elo.B, elo.A)
    
    # Best of 7 series sampling
    winner <- sample(c(team1, team2), size=1, prob=c(prob.A, prob.B), replace=TRUE) # size = 7?
    series <- c(series, winner)
    
    # Add winner to list of winners for each matchup
    #first_round <- c(first_round, winner)
    
    if (winner == team1) {
      result = c(1)
    } else if (winner == team2) {
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
      mutate(elo = if_else(team == team1, teamA_new_elo,
                           if_else(team == team2, teamB_new_elo, elo)))
    
    # Add updated elos to elo history
    elo_history[, paste(as.character("game"), as.character(row), sep="")] <- elos$elo
    
    # Check for best of 7
    team1_wins <- length(which(series == team1))
    team2_wins <- length(which(series == team2))
    if (team1_wins == 4) {
      second_round <- c(second_round, team1)
      break
    } else if (team2_wins == 4) {
      second_round <- c(second_round, team2)
      break
    }
  }
}

#elo_history
second_round

# 3rd Round - Conference Championships (4 Teams): Best-of-7 Series
# At the end of the playoffs, the top two teams play each other in the Conference Finals, to determine the Conference Champions from each side, who then proceed to play in the NBA Finals.
conference = data.frame(matrix(ncol = 2, nrow = 2))
colnames(conference) <- c("team1", "team2")

# Playoff Schedule
# Western Conference ---- Eastern Conference
conference$team1 <- c(second_round[1], second_round[3])
conference$team2 <- c(second_round[2], second_round[4]) 

third_round <- c()
#row = 1 # For test purposes
#i = 1 # For test purposes
for (row in 1:nrow(conference)) {
  
  # Home Team and Away Team
  team1 <- conference[row, "team1"]
  team2  <- conference[row, "team2"]
  
  # For loop here? for i in 1:7
  series <- c()
  for (i in 1:7) {
    # Pre-match ratings
    x = subset(elos, team == team1)
    elo.A <- (as.integer(c(x[3])))
    
    y = subset(elos, team == team2)
    elo.B <- as.integer(c(y[3]))
    
    # Probability of winning
    prob.A <- elo.prob(elo.A, elo.B)
    prob.B <- elo.prob(elo.B, elo.A)
    
    # Best of 7 series sampling
    winner <- sample(c(team1, team2), size=1, prob=c(prob.A, prob.B), replace=TRUE) # size = 7?
    series <- c(series, winner)
    
    # Add winner to list of winners for each matchup
    #first_round <- c(first_round, winner)
    
    if (winner == team1) {
      result = c(1)
    } else if (winner == team2) {
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
      mutate(elo = if_else(team == team1, teamA_new_elo,
                           if_else(team == team2, teamB_new_elo, elo)))
    
    # Add updated elos to elo history
    elo_history[, paste(as.character("game"), as.character(row), sep="")] <- elos$elo
    
    # Check for best of 7
    team1_wins <- length(which(series == team1))
    team2_wins <- length(which(series == team2))
    if (team1_wins == 4) {
      third_round <- c(third_round, team1)
      break
    } else if (team2_wins == 4) {
      third_round <- c(third_round, team2)
      break
    }
  }
}

#elo_history
third_round

# NBA Finals (7-Game Series)
finals = data.frame(matrix(ncol = 2, nrow = 1))
colnames(finals) <- c("team1", "team2")

# Playoff Schedule
# Western Conference ---- Eastern Conference
finals$team1 <- c(third_round[1])
finals$team2 <- c(third_round[2]) 

champion <- c()
#row = 1 # For test purposes
#i = 1 # For test purposes
for (row in 1:nrow(finals)) {
  
  # Home Team and Away Team
  team1 <- semi[row, "team1"]
  team2  <- semi[row, "team2"]
  
  # For loop here? for i in 1:7
  series <- c()
  for (i in 1:7) {
    # Pre-match ratings
    x = subset(elos, team == team1)
    elo.A <- (as.integer(c(x[3])))
    
    y = subset(elos, team == team2)
    elo.B <- as.integer(c(y[3]))
    
    # Probability of winning
    prob.A <- elo.prob(elo.A, elo.B)
    prob.B <- elo.prob(elo.B, elo.A)
    
    # Best of 7 series sampling
    winner <- sample(c(team1, team2), size=1, prob=c(prob.A, prob.B), replace=TRUE) # size = 7?
    series <- c(series, winner)
    
    # Add winner to list of winners for each matchup
    #first_round <- c(first_round, winner)
    
    if (winner == team1) {
      result = c(1)
    } else if (winner == team2) {
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
      mutate(elo = if_else(team == team1, teamA_new_elo,
                           if_else(team == team2, teamB_new_elo, elo)))
    
    # Add updated elos to elo history
    elo_history[, paste(as.character("game"), as.character(row), sep="")] <- elos$elo
    
    # Check for best of 7
    team1_wins <- length(which(series == team1))
    team2_wins <- length(which(series == team2))
    if (team1_wins == 4) {
      champion <- c(champion, team1)
      break
    } else if (team2_wins == 4) {
      champion <- c(champion, team2)
      break
    }
  }
}

#elo_history
# CHAMPION
champion

# All stages
first_round
second_round
third_round
champion

# Plot elo_history
library(reshape2)
library(ggplot2)

matplot(t(elo_history[,-c(1)]), type="l")

df_melted = melt(elo_history, id.vars='teams')

ggplot(df_melted, aes(x = variable, y = value)) + 
  geom_line(aes(color = teams, group = teams)) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ggtitle("End of Regular Season, Playoffs, & Finals") +
  xlab("Games") + ylab("Elo Rating")


# Drawbacks
# Best of 7 series do not account for HCA, too complicated to incorporate
# Simulation is random and you could get different outcomes (create a list and find most common answer?)
# Elo is based only off win/loss, HCA, margin, does not account for roster changes, injuries, and other factors
















