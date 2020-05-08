# Work Space


# https://www.kaggle.com/lpkirwin/fivethirtyeight-elo-ratings
# CALCULATE ELO RATINGS
# Value for K that 538 uses
K = 20.
# Amount of points designated for the homecourt advantage
HOME_ADVANTAGE = 100

head(Season.19.20)

games <- Season.19.20

# Create a dictionary for each team with team name and intitialized with elo scores

# # Elo updates will be scaled based on the margin of victory
games['margin'] = games.PTS - games.PTS1

# Three Functions for calculating
elo_pred <- function(elo1, elo2) {
  return(1. / (10. ** (-(elo1 - elo2) / 400.) + 1.))
}

expected_margin <- function(elo_diff) {
  return((7.5 + 0.006 * elo_diff))
}

elo_update <- function(w_elo, l_elo, margin) {
  elo_diff = w_elo - l_elo
  pred = elo_pred(w_elo, l_elo)
  mult = ((margin + 3.) ** 0.8) / expected_margin(elo_diff)
  update = K * mult * (1 - pred)
  return(pred, update)
}

# Iterate over the games dataframe using index numbers, so want to check that nothing is out of order

preds = c()
w_elo = c()
l_elo = c()

# Calculate winner and loser column games['Winner] games['Loser']

# Loop over all rows of the games dataframe
for (row in games.itertuples()) { # itertuples?
  # Get key data from current row
  w = row.Winner
  l = row.Loser
  margin = row.margin
  wloc = row.WLoc # create column for home or neutral games['WLoc]
  
  # Does either team get a home-court advantage?
  w_ad, l_ad, = 0., 0.
  if wloc == "H":
    w_ad += HOME_ADVANTAGE
  elif wloc == "A":
    l_ad += HOME_ADVANTAGE
  
  # Get elo updates as a result of the game
  pred, update = elo_update(elo_dict[w] + w_ad,
                            elo_dict[l] + l_ad, 
                            margin)
  elo_dict[w] += update
  elo_dict[l] -= update
  
  # Save prediction and new Elos for each round
  preds.append(pred)
  w_elo.append(elo_dict[w])
  l_elo.append(elo_dict[l])
}

games['w_elo'] = w_elo
games['l_elo'] = l_elo

# See if elos make sense
games.tail(10)

# Check what the log loss would be on these games
np.mean(-np.log(preds))

# Final step: for each team, pull out the final Elo rating at the end of each regular season.

d = df.copy()
d = d.loc[(d.WTeamID == team_id) | (d.LTeamID == team_id), :]
d.sort_values(['Season', 'DayNum'], inplace=True)
d.drop_duplicates(['Season'], keep='last', inplace=True)
w_mask = d.WTeamID == team_id
l_mask = d.LTeamID == team_id
d['season_elo'] = None
d.loc[w_mask, 'season_elo'] = d.loc[w_mask, 'w_elo']
d.loc[l_mask, 'season_elo'] = d.loc[l_mask, 'l_elo']
out = pd.DataFrame({
  'team_id': team_id,
  'season': d.Season,
  'season_elo': d.season_elo
})
return(out)

df_list = [final_elo_per_season(rs, id) for id in team_ids]
season_elos = pd.concat(df_list)

season_elos.sample(10)

season_elos.to_csv("season_elos.csv", index=None)


# https://www.kaggle.com/robertsturrock/prediction-using-fivethirtyeight-elo-ratings
# PREDICTIONS USING ABOVE ELO RATINGS
library(data.table)
library(magrittr)
library(ggplot2)
library(dplyr)
library(stringr)
library(ggplot2)

# Get data
dg_tournment <- read.csv("../input/mens-machine-learning-competition-2018/NCAATourneyCompactResults.csv")
dseeds_tournament <- read.csv("../input/mens-machine-learning-competition-2018/NCAATourneySeeds.csv")

# We then need to make sure we randomize the winner and loser into team 1 and team 2. That way we can calculate the probability that team 1 wins.
#  add seeding information to the games here (where it is available) as this will be an important predictor for our model.
# keep only win loss and team ids for the dg_tournament data
outcome_tournament <- dg_tournment %>% select(Season, DayNum, WTeamID, LTeamID)
names(outcome_tournament) <- tolower(names(outcome_tournament))

# randomize winning and losing team into team 1 and team 2 (necessary for probabilities later) and drop other ids
outcome_tournament <- outcome_tournament %>% 
  mutate(rand = runif(dim(outcome_tournament)[1]), 
         team1id = ifelse(rand >= 0.5, wteamid, lteamid),
         team2id = ifelse(rand <0.5, wteamid, lteamid),
         team1win = ifelse(team1id == wteamid, 1, 0)) %>% 
  select(-rand, -wteamid,-lteamid)

# Add seeding information to games: 

# make seeds 1-16 without letters (except for certain seed)
dseeds_tournament <- dseeds_tournament %>% 
  mutate(ranking = as.factor((str_replace(Seed, "[A-Z]",""))), 
         rank_num = as.numeric(str_replace(ranking, ".[a-z]","")))
names(dseeds_tournament) <- tolower(names(dseeds_tournament))

# team 1
outcome_tournament <- outcome_tournament %>% 
  left_join(
    select(dseeds_tournament, t1_rank = ranking, t1_rank_n = rank_num, teamid, season), 
    by = c("team1id"="teamid","season"="season")) 

# team 2
outcome_tournament <- outcome_tournament %>% 
  left_join(
    select(dseeds_tournament, t2_rank = ranking, t2_rank_n = rank_num, teamid, season), 
    by = c("team2id"="teamid","season"="season")) 


# replace NA seeds
outcome_tournament <- outcome_tournament %>% mutate(t1_rank = ifelse(is.na(t1_rank), 8.5, t1_rank),
                                                    t2_rank = ifelse(is.na(t2_rank), 8.5, t2_rank),
                                                    t1_rank_n = ifelse(is.na(t1_rank_n), 8.5, t1_rank_n),
                                                    t2_rank_n = ifelse(is.na(t2_rank_n), 8.5, t2_rank_n),
                                                    diff_rank = t1_rank_n - t2_rank_n)

# Then we add in the team specific information (stats for each team's regular season) to the game data.

# Next we add in ELO ratings, covered above
season_elos <- read.csv("../input/fivethirtyeight-elo-ratings/season_elos.csv") %>% rename(teamid = team_id)

# Join team 1 data
outcome_tournament <- outcome_tournament %>% 
  left_join(
    select(season_elos, 
           season, 
           teamid, 
           t1_season_elo = season_elo),
    by = c("team1id" = "teamid","season" = "season"))


# Join team 2 data
outcome_tournament <- outcome_tournament %>% 
  left_join(
    select(season_elos, 
           season, 
           teamid, 
           t2_season_elo = season_elo),
    by = c("team2id" = "teamid","season" = "season"))


# Compute ELO probabilities for the game, and the difference in ELO scores

outcome_tournament <- outcome_tournament %>% 
  mutate(elo_diff = t1_season_elo - t2_season_elo,
         elo_prob_1 = 1/(10^(-elo_diff/400)+1)
  )

# Logistic Regression
# Now we use our input to create a logistic regression and make predictions on the sample data. 
# First we need to bring in the relevant seeding information and ELO ratings for the sample submission pairings.
# Load data
sample_submission <- read.csv("../input/mens-machine-learning-competition-2018/SampleSubmissionStage1.csv")

# Join team data and ranking data
d_ss <- sample_submission

# create individual team ids and season 
d_ss <- d_ss %>% mutate(season = as.numeric(gsub("(.*)_(.*)_(.*)",ID, replacement = "\\1")), 
                        team1id =  as.numeric(gsub("(.*)_(.*)_(.*)",ID, replacement = "\\2")),
                        team2id =  as.numeric(gsub("(.*)_(.*)_(.*)",ID, replacement = "\\3")))

# Add rank data

# team 1
d_ss <- d_ss %>% 
  left_join(
    dplyr::select(dseeds_tournament, t1_rank = ranking, t1_rank_n = rank_num, teamid, season), 
    by = c("team1id"="teamid","season"="season")) 

# team 2
d_ss <- d_ss %>% 
  left_join(
    dplyr::select(dseeds_tournament, t2_rank = ranking, t2_rank_n = rank_num, teamid, season), 
    by = c("team2id"="teamid","season"="season")) 



### Join ELO rating data
season_elos <- read.csv("../input/fivethirtyeight-elo-ratings/season_elos.csv") %>% rename(teamid = team_id)

# Join team 1 data
d_ss <- d_ss %>% 
  left_join(
    select(season_elos, 
           season, 
           teamid, 
           t1_season_elo = season_elo),
    by = c("team1id" = "teamid","season" = "season"))


# Join team 2 data
d_ss <- d_ss %>% 
  left_join(
    select(season_elos, 
           season, 
           teamid, 
           t2_season_elo = season_elo),
    by = c("team2id" = "teamid","season" = "season"))


# Key differences between winner and loser

d_ss <- d_ss %>% 
  mutate(elo_diff = t1_season_elo - t2_season_elo,
         elo_prob_1 = 1/(10^(-elo_diff/400)+1),
         diff_rank = t1_rank_n - t2_rank_n
  )

# Now that we've set that up we can make predictions.
# TRAINING (ON MOST OF SEASON)
train <- outcome_tournament

### Make predictions based on model 
train <- train %>% filter(season <= 2013)

#logistic regression model: differences
model <- glm(team1win ~ 
               diff_rank +
               t1_rank_n +
               t1_season_elo +
               t2_season_elo +
               elo_prob_1
             ,
             
             data = train, family = binomial)
summary(model)

# he summary stats from the model show that the ELO ratings for each team are significant, as are the ranking variables.
# predict on test set
predict <- data.frame(Pred = predict(model, newdata = d_ss, type = 'response'))
d_ss <- d_ss %>% mutate(Pred = predict$Pred) %>% dplyr::select(ID, Pred)

# use original sample 
d_ss_fin <- sample_submission %>% mutate(Pred = d_ss$Pred)

# output 
write.csv(d_ss_fin, "submission_03032018_2.csv", row.names = FALSE)


# SIMULATING NBA GAMES
# https://medium.com/playing-numbers/how-to-simulate-nba-games-in-python-9b533b517e48
# https://www.youtube.com/watch?v=irjTWNV0eAY
# We will be using team points scored and team points scored against to evaluate win probability. 
# As a case study, we will simulate the 2017-2018 NBA finals to determine the expected win probability of each team.
# Data: https://www.kaggle.com/ionaskel/nba-games-stats-from-2014-to-2018
# Notebook: https://github.com/PlayingNumbers/NBASimulator 
gdf = pd.read_csv('nba_games_stats.csv')
gdf.columns

# Based on the column output, we will only need to focus on a few variables: Team, Date, TeamPoints, and Opponent Points.

# Next, we separate the teams we are looking at (Golden State & Cleveland) and trim the data set so that it only includes the 2017-2018 season but not the finals.

gswdf = gdf[gdf.Team == 'GSW']
cldf = gdf[gdf.Team == 'CLE']
gswdf.Date = gswdf.Date.apply(lambda x: pd.to_datetime(x, format='%Y-%m-%d', errors='ignore'))
gswdf = gswdf[(gswdf['Date'] > pd.to_datetime('20171001', format='%Y%m%d', errors='ignore')) & (gswdf['Date'] <= pd.to_datetime('20180531', format='%Y%m%d', errors='ignore'))]
cldf.Date = cldf.Date.apply(lambda x: pd.to_datetime(x, format='%Y-%m-%d', errors='ignore'))
cldf = cldf[(cldf['Date'] > pd.to_datetime('20171001', format='%Y%m%d', errors='ignore'))& (cldf['Date'] <= pd.to_datetime('20180531', format='%Y%m%d', errors='ignore'))]

# With these trimmed data frames, we want to look at the distribution of points scored by and against both teams. 
# We will use these to simulate the outcomes of the finals game.

#creates histograms of games (blue is GSW & orange is CLE)
#points scored (run this first) 
gswdf.TeamPoints.hist()
cldf.TeamPoints.hist()
#points scored against (run this second)
gswdf.OpponentPoints.hist()
cldf.OpponentPoints.hist()

# As mentioned above, the only things that we will need to actually run the simulation are the means and standard deviations of the points scored and points scored against.

gswmeanpts = gswdf.TeamPoints.mean()
clmeanpts = cldf.TeamPoints.mean()
gswsdpts = gswdf.TeamPoints.std()
clsdpts = cldf.TeamPoints.std()
gswmeanopp = gswdf.OpponentPoints.mean()
clmeanopp = cldf.OpponentPoints.mean()
gswsdopp = gswdf.OpponentPoints.std()
clsdopp = cldf.OpponentPoints.std()
print("Golden State Points Mean ", gswmeanpts)
print("Golden State Points SD ", gswsdpts)
print("Cleveland Points Mean ", clmeanpts)
print("Cleveland Points SD ", clsdpts)
print("Golden State OppPoints Mean ", gswmeanopp)
print("Golden State OppPoints SD ", gswsdopp)
print("Cleveland OppPoints Mean ", clmeanopp)
print("Cleveland OppPoints SD ", clsdopp)

# The next step is to build the distributions and to start randomly sampling from them. We use the gaussian (gauss) function from the random module. 
# This function uses a mean and a standard deviation to create a normal distribution. 
# It then takes a random sample from that distribution and produces a value.

#randomly samples from a distribution with a mean=100 and a SD=15
random.gauss(100, 15)

# For each team, we will project their score by taking a sample from their points scored distribution and the points scored against the opponent's distribution. 
# We will average these two numbers. 
# We will keep doing this until our distribution reaches a limit or a satisfactory level of confidence.

"
Example
GSW(Score sample) + CLE(Score against sample) /2 = Projected GSW score
CLE(Score sample) + GSW(Score against sample)/2 = Projected CLE score
If Projected GSW score > Projected CLE score, then we say that Golden state won that game. We repeat this randomized process until the win % stabilizes
"

# We write a function to take these samples from both teams and return the winner.
def gameSim():
  GSWScore = (rnd.gauss(gswmeanpts,gswsdpts)+rnd.gauss(clmeanopp,clsdopp))/2
CLScore = (rnd.gauss(clmeanpts,clsdpts)+rnd.gauss(gswmeanopp,gswsdopp))/2
if int(round(GSWScore)) > int(round(CLScore)):
  return 1
elif int(round(GSWScore)) < int(round(CLScore)):
  return -1
else: return 0

# Once we have this, we create a wrapper function that lets you choose how many times to repeat this process. It also tabulates the results.
def gamesSim(ns):
  gamesout = []
team1win = 0
team2win = 0
tie = 0
for i in range(ns):
  gm = gameSim()
gamesout.append(gm)
if gm == 1:
  team1win +=1 
elif gm == -1:
  team2win +=1
else: tie +=1 
print('GSW Win ', team1win/(team1win+team2win+tie),'%')
print('CLE Win ', team2win/(team1win+team2win+tie),'%')
print('Tie ', tie/(team1win+team2win+tie), '%')
return gamesout
# Finally, we actually simulate the results by running the function:
gamesSim(10000)

# Script to run for any team: https://github.com/PlayingNumbers/NBASimulator/blob/master/NBAGameSimulator.py


# ELO USING PACKAGE
# cran.r-project.org/web/packages/elo/elo.pdf
# https://cran.r-project.org/web/packages/elo/vignettes/elo.html
# https://www.rdocumentation.org/packages/elo/versions/2.1.1
library(elo)
library(EloRating)
# https://gist.github.com/i000313/90b1f3c556b1b1ad8278
###############################################################################
#                       Elo rating system implementation in R                 #
###############################################################################
#
# INTRODUCTION
# The ELO rating system is a method for calculating the relative skill levels 
# of players in Player-Versus-Player games. This rating system is used on 
# chess and also in a number of games, videogames, etc.
# It was originally invented as an improved chess rating system in the 1960s 
# and established at the FIDE-congress 1970 in Siegen (Germany). 
#
##########
# FORMULA
# ELO rating formula after a PlayerA-VS-PlayerB match:
#             RAn = RA + K * (SA - EA)
# WHERE:
# - RAn new ELO rating for player A (rating after the match);
# - RA, current elo rating (rating before the match);
# - K, weighting factor (weight for each victory, draw and defeat);
# - SA, match result of player A (1 for a victory, 0.5 for a draw, 0 for a defeat);
# - EA, expected score for player A. It is a value between 0 and 1 ().
# (Similary the Rating for player B is: RBn = RB + K * (SB - EB))
#
# THE EA (expected score) can be computed by:
# EA = 1 / ( 1 +  10^( (RB - RA) / 400 ) )
# Similarly the expected score for Player B (EB) is:
# EB = 1 / ( 1 +  10^( (RA - RB) / 400 ) )
# WHERE:
# RA, current elo rating of player A.
# RB, current elo rating of player B.
# The EA is allways a value between [0, 1].
#
#############
# EXAMPLE:
# Player A rating: 1200
# Player B rating: 1000
# k = 20
# 
# What is the Player A rating, if he Wins?
# EA = 1 / ( 1 + 10^( (1000-1200)/400) ) = 0.7597
# Rn = 1200 + 20 * (1 - 0.7597) = 1204.805
#
# What is the Player A rating, if he draws?
# Rn = 1200 + 20 * (0.5 - 0.7597) = 1194.805
# 
# What is the Player A rating,, if he loses?
# Rn = 1200 + 20 * (0 - 0.7597) = 1184.805
#
#
# What is the Player B rating, if he Wins?
# EA = 1 / ( 1 + 10^( (1200-1000)/400) ) = 0.2403
# Rn = 1000 + 20 * (1 - 0.2403) = 1015.195
#
# What is the Player A rating, if he draws?
# Rn = 1000 + 20 * (0.5 - 0.2403) = 1005.194
#
# What is the Player A rating, if he loses?
# Rn = 1000 + 20 * (0 - 0.2403) = 995.1940
############
# REFERENCES
#
# - http://en.wikipedia.org/wiki/Elo_rating_system
# - http://bzstats.strayer.de/bzinfo/elo/?lang=en
# - http://www.clubedexadrez.com.br/portal/cxtoledo/calculo.htm
############
#
# @author PSantos
# @date Tue May 27 03:12:29 2014
###############################################################################

#
# This function computes the ELO rating for two players.
#
# @playerARating current elo rating for player A.
# @PlayerBRating current elo rating for player B.  
# @K weighting factor.
# @return a data frame with ELO rating for both players in case of
# win, draw, and losse. This is a 2 (rows) by 4 (columns) data frame.
# 
# Example:
# > eloRating(1200,1000,20)
#         chanceToWin playerAWins draw playerBWins
# playerA          76        1205 1195        1185
# playerB          24         995 1005        1015
#
eloRating=function(playerARating, PlayerBRating, k=32) {
  
  # Expected score for player A and for player B.
  EA <- (1 / (1 + 10^((PlayerBRating - playerARating)/400)))
  EB <- (1 / (1 + 10^((playerARating - PlayerBRating)/400)))
  
  # RAn = RA + K * (SA - EA)
  newRatingPlyAWins  <- playerARating + k * (1 - EA)
  newRatingPlyADraws <- playerARating + k * (0.5 - EA)
  newRatingPlyADefeated  <- playerARating + k * (0 - EA)
  
  # RBn = RB + K * (SB - EB)
  newRatingPlyBWins  <- PlayerBRating + k * (1 - EB)
  newRatingPlyBDraws <- PlayerBRating + k * (0.5 - EB)
  newRatingPlyBDefeated  <- PlayerBRating + k * (0 - EB)
  
  chanceToWin <- round(data.frame(chanceToWin=c(EA, EB)) * 100, digits=0)
  playerAWins  <- round(data.frame(playerAWins=c(newRatingPlyAWins, newRatingPlyBDefeated)), digits=0)
  playerDraw  <- round(data.frame(draw=c(newRatingPlyADraws, newRatingPlyBDraws)), digits=0)
  playerBWins  <- round(data.frame(playerBWins=c(newRatingPlyADefeated, newRatingPlyBWins)), digits=0)
  
  df <- cbind(chanceToWin, playerAWins, playerDraw, playerBWins)
  rownames(df) <- c('playerA', 'playerB')
  return(df)
}

# https://edomt.github.io/Elo-R-WorldCup/
library(dplyr)
matches <- readr::read_csv('results.csv')
# The data is easy enough to understand. There are only 9 variables, with self-explanatory names: 
# date, home_team, away_team, home_score, away_score, tournament, city, country, 
# neutral (whether the match was played on neutral ground, or at the home team's stadium).
# Our historical data is stored in matches, but we'll need to create another data.frame separately, to store each team's Elo rating, and update it after each match.
teams <- data.frame(team = unique(c(matches$home_team, matches$away_team)))
# To start our Elo ratings, we need to assign an initial Elo value to all the teams in our dataset. Traditionally in Elo-based systems this initial value is set to 1500.
teams <- teams %>%
  mutate(elo = 1500)
# For each match, we'll also create a variable that tells us who won. 
# Because of how the 'elo' package works, this variable will take the following values:
# 1 if the home team won;
# 0 if the away team won;
# 0.5 for a draw.
matches <- matches %>%
  mutate(result = if_else(home_score > away_score, 1,
                          if_else(home_score == away_score, 0.5, 0)))
# We can also get rid of a bunch of variables we won't need, and make sure that our historical data is ordered by date.
matches <- matches %>%
  select(date, home_team, away_team, result) %>%
  arrange(date)

# Here's what our two datasets look like now: head(matches) -> date, home_team, away_team, result
# head(teams) -> team, elo
library(elo)
# We'll only be using one function from this package to create our rankings: elo.calc(). This function takes 4 arguments:
# wins.A: whether team A won or not. This is what we've created and stored in our result variable, with 3 possibles values (1, 0, 0.5);
# elo.A: the pre-match Elo value for team A;
# elo.B: the pre-match Elo value for team B;
# k: this is called the K-factor. This is basically how many Elo points are up for grabs in each match.(20)
# The idea is to loop over each game in matches, get the pre-match ratings for both teams, and update them based on the result. 
# We'll get two new ratings, which we'll use to update our data in teams.
for (i in seq_len(nrow(matches))) {
  match <- matches[i, ]
  
  # Pre-match ratings
  teamA_elo <- subset(teams, team == match$home_team)$elo
  teamB_elo <- subset(teams, team == match$away_team)$elo
  
  # Let's update our ratings
  new_elo <- elo.calc(wins.A = match$result,
                      elo.A = teamA_elo,
                      elo.B = teamB_elo,
                      k = 30)
  
  # The results come back as a data.frame
  # with team A's new rating in row 1 / column 1
  # and team B's new rating in row 1 / column 2
  teamA_new_elo <- new_elo[1, 1]
  teamB_new_elo <- new_elo[1, 2]
  
  # We then update the ratings for teams A and B
  # and leave the other teams as they were
  teams <- teams %>%
    mutate(elo = if_else(team == match$home_team, teamA_new_elo,
                         if_else(team == match$away_team, teamB_new_elo, elo)))

# After a few minutes, you should get a nice teams data.frame, with the most up-to-date international Elo ratings for June 2018
teams %>%
  arrange(-elo) %>%
  head

# We still have to do one thing: subset our teams data.frame to only keep the 32 teams that have qualified for the 2018 World Cup.
WC_teams <- teams %>%
  filter(team %in% c("Russia", "Germany", "Brazil", "Portugal", "Argentina", "Belgium",
                     "Poland", "France", "Spain", "Peru", "Switzerland", "England",
                     "Colombia", "Mexico", "Uruguay", "Croatia", "Denmark", "Iceland",
                     "Costa Rica", "Sweden", "Tunisia", "Egypt", "Senegal", "Iran",
                     "Serbia", "Nigeria", "Australia", "Japan", "Morocco", "Panama",
                     "Korea Republic", "Saudi Arabia")) %>%
arrange(-elo)

# Finally, here are our World Cup Elo rankings, from strongest to weakeast team:
print.data.frame(WC_teams)
# We can also look up which teams didn't make it to the World Cup this year, despite high Elo ratings:
teams %>%
  filter(elo > 1800, !team %in% WC_teams$team)

# If you want to use this data for predictions and forecast competitions, here are two things you can do:
# Calulate prob for individual matches
# In the 'elo' package, the elo.prob() function lets you calculate the probability that team A will win a match against team B, given their respective Elo ratings.
# For example, in the opening match of the competition (Russia vs. Saudi Arabia), the probability of Russia winning would be 61%:
russia <- subset(WC_teams, team == "Russia")$elo
saudi_arabia <- subset(WC_teams, team == "Saudi Arabia")$elo
elo.prob(russia, saudi_arabia)

# Simulating the entire competition
# you can use the probability generated by elo.prob() to simulate the outcome of each match (using the sample() function and its prob argument 
# to choose a random winner between Russia and Saudi Arabia, but with a 61% probability of choosing Russia), 
# and update the Elo ratings throughout the competition.
# And if you repeat this process many (thousands of) times, you will get detailed probabilities for each team to make it to the each stage of the competition

# ATTEMPT USING FIFA TO DO BASKETBALL
library(dplyr)
matches <- Season.19.20

matches$Home.Neutral <- as.character(matches$Home.Neutral)
matches$Visitor.Neutral <- as.character(matches$Visitor.Neutral)

# use levels() if factor
teams <- data.frame(team = unique(c(matches$Home.Neutral))) # All 30 teams
teams$team <- as.character(teams$team)

#home_team <- levels(matches$Home.Neutral)
#away_team <- levels(matches$Visitor.Neutral)

teams <- teams %>%
  mutate(elo = 1500)

matches <- matches %>%
  mutate(result = if_else(PTS.1 > PTS, 1, 0))

# add points?
matches <- matches %>%
  select(Date, Home.Neutral, Visitor.Neutral, result)

library(elo)

for (i in seq_len(nrow(matches))) {
  match <- matches[i, ]
  
  # Pre-match ratings
  teamA_elo <- subset(teams, team == match$Home.Neutral)$elo
  teamB_elo <- subset(teams, team == match$Visitor.Neutral)$elo
  
  # Let's update our ratings
  new_elo <- elo.calc(wins.A = match$result,
                      elo.A = teamA_elo,
                      elo.B = teamB_elo,
                      k = 30)
  
  # The results come back as a data.frame
  # with team A's new rating in row 1 / column 1
  # and team B's new rating in row 1 / column 2
  teamA_new_elo <- new_elo[1, 1]
  teamB_new_elo <- new_elo[1, 2]
  
  # We then update the ratings for teams A and B
  # and leave the other teams as they were
  teams <- teams %>%
    mutate(elo = if_else(team == match$Home.Neutral, teamA_new_elo,
                         if_else(team == match$Visitor.Neutral, teamB_new_elo, elo)))
}

teams %>%
  arrange(-elo) %>%
  head(30)

# Playoff Teams
playoff_teams <- teams %>%
  filter(team %in% c("Russia", "Germany", "Brazil", "Portugal", "Argentina", "Belgium",
                     "Poland", "France", "Spain", "Peru", "Switzerland", "England",
                     "Colombia", "Mexico", "Uruguay", "Croatia", "Denmark", "Iceland",
                     "Costa Rica", "Sweden", "Tunisia", "Egypt", "Senegal", "Iran",
                     "Serbia", "Nigeria", "Australia", "Japan", "Morocco", "Panama",
                     "Korea Republic", "Saudi Arabia")) %>%
arrange(-elo)

# Probabilities for individual matches 
# Lakers vs Bucks?
lakers <- subset(teams, team == "Los Angeles Lakers")$elo
bucks <- subset(teams, team == "Milwaukee Bucks")$elo
elo.prob(lakers, bucks)

"
use the probability generated by elo.prob() to simulate the outcome of each match 
(using the sample() function and its prob argument to choose a random winner between Lakers and Saudi Bucks, but with a 52% probability of choosing Lakers), 
and update the Elo ratings throughout the competition.
"

# https://cran.r-project.org/web/packages/elo/vignettes/elo.html
library(elo)
# USES FACTORS FOR TEAMS
"
Naming Schema
Most functions begin with the prefix "elo.", for easy autocompletion.

Vectors or scalars of Elo scores are denoted elo.A or elo.B.

Vectors or scalars of wins by team A are denoted by wins.A.

Vectors or scalars of win probabilities are denoted by p.A.

Vectors of team names are denoted team.A or team.B.
"

# To calculate the probability team.A beats team.B, use elo.prob()
elo.A <- c(1500, 1500)
elo.B <- c(1500, 1600)
elo.prob(elo.A, elo.B)
# To calculate the score update after the two teams play, use elo.update()
wins.A <- c(1, 0)
elo.update(wins.A, elo.A, elo.B, k = 20)
# To calculate the new Elo scores after the update, use elo.calc()
elo.calc(wins.A, elo.A, elo.B, k = 20)
# ELO.RUN()
# To calculate a series of Elo updates, use elo.run(). This function has a formula = and data = interface. We first load the dataset tournament.
matches <- Season.19.20
matches$Home.Neutral <- as.character(matches$Home.Neutral)
matches$Visitor.Neutral <- as.character(matches$Visitor.Neutral)
# formula = should be in the format of wins.A ~ team.A + team.B. The score() function will help to calculate winners on the fly (1 = win, 0.5 = tie, 0 = loss).
matches$wins.A <- matches$PTS.1 > matches$PTS
elo.run(wins.A ~ Home.Neutral + Visitor.Neutral, data = matches, k = 20)
elo.run(score(PTS.1, PTS) ~ Home.Neutral + Visitor.Neutral, data = matches, k = 20)
# For more complicated Elo updates, you can include the special function k() in the formula = argument. Here we're taking the log of the win margin as part of our update.
elo.run(score(PTS.1, PTS) ~ Home.Neutral + Visitor.Neutral +
          k(20*log(abs(PTS.1 - PTS) + 1)), data = matches)
# You can also adjust the home and visitor teams with different k's:
"
k1 <- 20*log(abs(tournament$points.Home - tournament$points.Visitor) + 1)
elo.run(score(points.Home, points.Visitor) ~ team.Home + team.Visitor + k(k1, k1/2), data = tournament)
"
# It's also possible to adjust one team's Elo for a variety of factors (e.g., home-field advantage). 
# The adjust() special function will take as its second argument a vector or a constant.
elo.run(score(PTS.1, PTS) ~ adjust(Home.Neutral, 10) + Visitor.Neutral,
        data = matches, k = 20)
# elo.run() also recognizes if the second column is numeric, and interprets that as a fixed-Elo opponent.
"
matches$elo.Visitor <- 1500
elo.run(score(PTS.1, PTS) ~ Home.Neutral + elo.Visitor,
        data = matches, k = 20)
"
"
The special function regress() can be used to regress Elos back to a fixed value after certain matches. 
Giving a logical vector identifies these matches after which to regress back to the mean. 
Giving any other kind of vector regresses after the appropriate groupings (see, e.g., duplicated(..., fromLast = TRUE)). 
The other three arguments determine what Elo to regress to (to =, which could be a different value for different teams), 
by how much to regress toward that value (by =), and whether to regress teams which aren't actively playing (regress.unused =).
"
# USE AFTER RUNNING ELO FOR PREVIOUS SEASON??
"
tournament$elo.Visitor <- 1500
elo.run(score(points.Home, points.Visitor) ~ team.Home + elo.Visitor +
        regress(half, 1500, 0.2),
        data = tournament, k = 20)
"
# The special function group() doesn't affect elo.run(), but determines matches to group together in as.matrix() (below).
# There are several helper functions that are useful to use when interacting with objects of class "elo.run".
# summary.elo.run() reports some summary statistics.
e <- elo.run(score(PTS.1, PTS) ~ Home.Neutral + Visitor.Neutral,
             data = matches, k = 20)
summary(e)
# NO TIES?
rank.teams(e)
# as.matrix.elo.run() creates a matrix of running Elos.
head(as.matrix(e))
# as.data.frame.elo.run() gives the long version (perfect, for, e.g., ggplot2).
str(as.data.frame(e))
# Finally, final.elos() will extract the final Elos per team.
final.elos(e)
# It is also possible to use the Elos calculated by elo.run() to make predictions on future match-ups.
results <- elo.run(score(PTS.1, PTS) ~ adjust(Home.Neutral, 10) + Visitor.Neutral,
                   data = matches, k = 20)
newdat <- data.frame(
  Home.Neutral = "Los Angeles Lakers",
  Visitor.Neutral = "Milwaukee Bucks"
)
predict(results, newdata = newdat)
# We now get to elo.run2(), a copy of elo.run() (but implemented in R) that allows for custom probability calculations and Elo updates.
# For instance, suppose you want to change the adjustment based on team A's current Elo
custom_update <- function(wins.A, elo.A, elo.B, k, adjust.A, adjust.B, ...)
{
  k*(wins.A - elo.prob(elo.A, elo.B, adjust.B = adjust.B,
                       adjust.A = ifelse(elo.A > 1500, adjust.A / 2, adjust.A)))
}
custom_prob <- function(elo.A, elo.B, adjust.A, adjust.B)
{
  1/(1 + 10^(((elo.B + adjust.B) - (elo.A + ifelse(elo.A > 1500, adjust.A / 2, adjust.A)))/400.0))
}
er2 <- elo.run2(score(PTS.1, PTS) ~ adjust(Home.Neutral, 10) + Visitor.Neutral,
                data = matches, k = 20, prob.fun = custom_prob, update.fun = custom_update)
final.elos(er2)
# Compare this to the results from the default:
er3 <- elo.run(score(PTS.1, PTS) ~ adjust(Home.Neutral, 10) + Visitor.Neutral,
               data = matches, k = 20)
final.elos(er3)
# This example is a bit contrived, as it'd be easier just to use adjust() (actually, this is tested for in the tests), but the point remains.
# All three of the "basic" functions accept formulas as input, just like elo.run().
dat <- data.frame(elo.A = c(1500, 1500), elo.B = c(1500, 1600),
                  wins.A = c(1, 0), k = 20)
form <- wins.A ~ elo.A + elo.B + k(k)
elo.prob(form, data = dat)

elo.update(form, data = dat)

elo.calc(form, data = dat)

# Note that for elo.prob(), formula = can be more succinct:
elo.prob(~ elo.A + elo.B, data = dat)
# We can even adjust the Elos:
elo.calc(wins.A ~ adjust(elo.A, 10) + elo.B + k(k), data = dat)

# COMPARISON MODELS: Win/Loss LOGISTIC REGRESSION
"
The first model computes teams' win percentages, and feeds the differences of percentages into a regression. 
Including an adjustment using adjust() in the formula also includes that in the model. 
You could also adjust the intercept for games played on neutral fields by using the neutral() function.
"
e.winpct <- elo.winpct(score(PTS.1, PTS) ~ Home.Neutral + Visitor.Neutral + group(Date), data = matches,
                       subset = PTS.1 != PTS) # to get rid of ties for now
summary(e.winpct)

rank.teams(e.winpct)

predict(e.winpct, newdata = data.frame(Home.Neutral = "Los Angeles Lakers", Visitor.Neutral = "Milwaukee Bucks", stringsAsFactors = FALSE))
"
tournament$neutral <- replace(rep(0, nrow(tournament)), 30:35, 1)
summary(elo.winpct(score(points.Home, points.Visitor) ~ team.Home + team.Visitor + neutral(neutral) + group(week),
                   data = tournament, subset = points.Home != points.Visitor))
"
"
The models can be built "running", where predictions for the next group of games are made based on past data. 
Consider using the skip= argument to skip the first few groups (otherwise the model might have trouble converging).
"
# Note that predictions from this object use a model fit on all the data.
e.winpct <- elo.winpct(score(points.Home, points.Visitor) ~ team.Home + team.Visitor + group(week), data = tournament,
                       subset = points.Home != points.Visitor, running = TRUE, skip = 5)
summary(e.winpct)

predict(e.winpct, newdata = data.frame(team.Home = "Athletic Armadillos", team.Visitor = "Blundering Baboons", stringsAsFactors = FALSE)) # the same thing

"
It's also possible to compare teams' skills using logistic regression. A matrix of dummy variables is constructed, one for each team, 
where a value of 1 indicates a home team and -1 indicates a visiting team. The intercept then indicates a home-field advantage. 
To denote games played in a neutral setting (that is, without home-field advantage), use the neutral() function. 
In short, the intercept will then be set to 1 - neutral(). 
Including an adjustment using adjust() in the formula also includes that in the model.
"
results <- elo.glm(score(PTS.1, PTS) ~ Home.Neutral + Visitor.Neutral + group(Date), data = matches,
                   subset = PTS.1 != PTS) # to get rid of ties for now
summary(results)
rank.teams(results)

predict(results, newdata = data.frame(team.Home = "Athletic Armadillos", team.Visitor = "Blundering Baboons", stringsAsFactors = FALSE))

summary(elo.glm(score(points.Home, points.Visitor) ~ team.Home + team.Visitor + neutral(neutral) + group(week),
                data = tournament, subset = points.Home != points.Visitor))

"
The models can be built "running", where predictions for the next group of games are made based on past data. 
Consider using the skip= argument to skip the first few groups (otherwise the model might have trouble converging).
"
# Note that predictions from this object use a model fit on all the data.

results <- elo.glm(score(points.Home, points.Visitor) ~ team.Home + team.Visitor + group(week), data = tournament,
                   subset = points.Home != points.Visitor, running = TRUE, skip = 5)
summary(results)

# MARKOV CHAIN
"
It's also possible to compare teams' skills using a Markov-chain-based model, as outlined in Kvam and Sokol (2006). 
In short, imagine a judge who randomly picks one of two teams in a matchup, 
where the winner gets chosen with probability p (here, for convenience, 'k') and the loser with probability 1-p (1-k). 
In other words, we assume that the probability that the winning team is better than the losing team given that it won is k, 
and the probability that the losing team is better than the winning team given that it lost is (1-k). This forms a transition matrix, 
whose stationary distribution gives a ranking of teams. The differences in ranking are then fed into a logistic regession model to predict win status. 
Any adjustments made using adjust() are also included in this logistic regression. 
You could also adjust the intercept for games played on neutral fields by using the neutral() function.
"
mc <- elo.markovchain(score(PTS.1, PTS) ~ Home.Neutral + Visitor.Neutral, data = matches,
                      subset = PTS.1 != PTS, k = 0.7)
summary(mc)

rank.teams(mc)

predict(mc, newdata = data.frame(team.Home = "Athletic Armadillos", team.Visitor = "Blundering Baboons", stringsAsFactors = FALSE))

summary(elo.markovchain(score(points.Home, points.Visitor) ~ team.Home + team.Visitor + neutral(neutral),
                        data = tournament, subset = points.Home != points.Visitor, k = 0.7))

"
These models can also be built "running", where predictions for the next group of games are made based on past data. 
Consider using the skip= argument to skip the first few groups (otherwise the model might have trouble converging).
Note that predictions from this object use a model fit on all the data.
"
mc <- elo.markovchain(score(points.Home, points.Visitor) ~ team.Home + team.Visitor + group(week), data = tournament,
                      subset = points.Home != points.Visitor, k = 0.7, running = TRUE, skip = 5)
summary(mc)

# Note about LRMC
"
Note that by assigning probabilities in the right way, this function emits the Logistic Regression Markov Chain model (LRMC). 
Use the in-formula function k() for this. IMPORTANT: note that k() denotes the probability assigned to the winning team, 
not the home team (for instance). If rH(x) denotes the probability that the home team is better given that they scored x points more 
than the visiting team (allowing for x to be negative), then an LRMC model might look something like this:
"
elo.markovchain(floor(wins.home) ~ team.home + team.visitor + k(ifelse(x > 0, rH(x), 1 - rH(x))))

"
Why do we use floor() here? This takes care of the odd case where teams tie. 
In this case, rH(x) < 0.5 because we expected the home team to win by virtue of being home. 
By default, elo.markovchain() will split any ties down the middle (i.e., 0.5 and 0.5 instead of p and 1-p), 
which isn't what we want; we want the visiting team to get a larger share than the home team. 
Telling elo.markovchain() that the visiting team "won" gives the visiting team its whole share of p.
Alternatively, if h denotes a home-field advantage (in terms of score), the model becomes:
"
elo.markovchain(ifelse(home.points - visitor.points > h, 1, 0) ~ team.home + team.visitor + k(pmax(rH(x), 1 - rH(x))))

"
In this case, the home team "won" if it scored more than h points more than the visiting team. 
Since rH(x) > 0.5 if x > h, then pmax() will assign the proper probability to the pseudo-winning team.
Finally, do note that using neutral() isn't sufficient for adjusting for games played on neutral ground, 
because the adjustment is only taken into account in the logistic regression to produce probabilities, 
not the building of the transition matrix. Therefore, you'll want to also account for neutral wins/losses in k() as well.
"
# COLLEY MATRIX METHOD
"
It's also possible to compare teams' skills using the Colley Matrix method, as outlined in Colley (2002). 
The coefficients to the Colley matrix formulation gives a ranking of teams. 
The differences in ranking are then fed into a logistic regession model to predict win status. Here 'k' denotes how convincing a win is; 
it represents the fraction of the win assigned to the winning team and the fraction of the loss assigned to the losing team. 
Setting 'k' = 1 emits the bias-free method presented by Colley. Any adjustments made using adjust() are also included in this logistic regression. 
You could also adjust the intercept for games played on neutral fields by using the neutral() function.
"
co <- elo.colley(score(points.Home, points.Visitor) ~ team.Home + team.Visitor, data = tournament,
                 subset = points.Home != points.Visitor)
summary(co)

rank.teams(co)

predict(co, newdata = data.frame(team.Home = "Athletic Armadillos", team.Visitor = "Blundering Baboons", stringsAsFactors = FALSE))

summary(elo.colley(score(points.Home, points.Visitor) ~ team.Home + team.Visitor + neutral(neutral),
                   data = tournament, subset = points.Home != points.Visitor))

"
These models can also be built "running", where predictions for the next group of games are made based on past data. 
Consider using the skip= argument to skip the first few groups (otherwise the model might have trouble converging).
Note that predictions from this object use a model fit on all the data.
"
co <- elo.colley(score(points.Home, points.Visitor) ~ team.Home + team.Visitor + group(week), data = tournament,
                 subset = points.Home != points.Visitor, running = TRUE, skip = 5)
summary(co)

predict(co, newdata = data.frame(team.Home = "Athletic Armadillos", team.Visitor = "Blundering Baboons", stringsAsFactors = FALSE)) # the same thing

# MODELING MARGIN OF VICTORY INSTEAD OF WINS
"
elo.glm(), elo.markovchain(), and elo.winpct() all allow for modeling of margins of victory instead of simple win/loss using the mov() function. 
Note that one must set the family='gaussian' argument to get linear regression instead of logistic regression.
"
summary(elo.glm(mov(points.Home, points.Visitor) ~ team.Home + team.Visitor, data = tournament,
                family = "gaussian"))











# https://nbviewer.jupyter.org/github/practicallypredictable/posts/blob/master/basketball/nba/notebooks/nba-simple_elo_ratings.ipynb
# ELO RATINGS




















