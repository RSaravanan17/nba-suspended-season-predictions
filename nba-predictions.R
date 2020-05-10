# WORK SPACE


# Reference: https://edomt.github.io/Elo-R-WorldCup/
library(dplyr)
matches <- Season.19.20
# The data is easy enough to understand. There are only 9 variables, with self-explanatory names: 
# date, Home.Neutral, Visitor.Neutral, PTS.1, PTS, tournament, city, country, 
# neutral (whether the match was played on neutral ground, or at the home team's stadium).
# Our historical data is stored in matches, but we'll need to create another data.frame separately, to store each team's Elo rating, and update it after each match.
matches$Home.Neutral <- as.character(matches$Home.Neutral)
matches$Visitor.Neutral <- as.character(matches$Visitor.Neutral)

# use levels() if factor
teams <- data.frame(team = unique(c(matches$Home.Neutral))) # All 30 teams
teams$team <- as.character(teams$team)

#home_team <- levels(matches$Home.Neutral)
#away_team <- levels(matches$Visitor.Neutral)

# To start our Elo ratings, we need to assign an initial Elo value to all the teams in our dataset. Traditionally in Elo-based systems this initial value is set to 1500.
teams <- teams %>%
  mutate(elo = 1500)
# For each match, we'll also create a variable that tells us who won. 
# Because of how the 'elo' package works, this variable will take the following values:
# 1 if the home team won;
# 0 if the away team won;
# 0.5 for a draw.
matches <- matches %>%
  mutate(result = if_else(PTS.1 > PTS, 1, 0))
# We can also get rid of a bunch of variables we won't need, and make sure that our historical data is ordered by date.
# add points?
matches <- matches %>%
  select(Date, Home.Neutral, Visitor.Neutral, result)

# Here's what our two datasets look like now: head(matches) -> date, Home.Neutral, Visitor.Neutral, result
# head(teams) -> team, elo
library(elo)
# We'll only be using one function from this package to create our rankings: elo.calc(). This function takes 4 arguments:
# wins.A: whether team A won or not. This is what we've created and stored in our result variable, with 3 possibles values (1, 0, 0.5);
# elo.A: the pre-match Elo value for team A;
# elo.B: the pre-match Elo value for team B;
# k: this is called the K-factor. This is basically how many Elo points are up for grabs in each match.(20)
# The idea is to loop over each game in matches, get the pre-match ratings for both teams, and update them based on the result. 
# We'll get two new ratings, which we'll use to update our data in teams.
i = 2
for (i in seq_len(nrow(matches))) {
  match <- matches[i, ] # Clippers beat Lakers
  
  # Pre-match ratings
  teamA_elo <- subset(teams, team == match$Home.Neutral)$elo # 1500
  teamB_elo <- subset(teams, team == match$Visitor.Neutral)$elo #1500
  
  # Let's update our ratings
  new_elo <- elo.calc(wins.A = match$result,
                      elo.A = teamA_elo, # elo.A = 1510
                      elo.B = teamB_elo, # elo.B = 1490
                      k = 20)
  
  # The results come back as a data.frame
  # with team A's new rating in row 1 / column 1
  # and team B's new rating in row 1 / column 2
  teamA_new_elo <- new_elo[1, 1]
  teamB_new_elo <- new_elo[1, 2]
  
  # We then update the ratings for teams A and B
  # and leave the other teams as they were
  teams <- teams %>%
    mutate(elo = if_else(team == match$Home.Neutral, teamA_new_elo,
                         if_else(team == match$Visitor.Neutral, teamB_new_elo, elo))) # updates in data frame
}

# After a few minutes, you should get a nice teams data.frame, with the most up-to-date international Elo ratings for June 2018
teams %>%
  arrange(-elo) %>%
  head(30)

# We still have to do one thing: subset our teams data.frame to only keep the 32 teams that have qualified for the 2018 World Cup.
# The top eight teams in each conference (East and West), ranked in order by win-loss records, qualify for the playoffs.
# At the end of the playoffs, the top two teams play each other in the Conference Finals, to determine the Conference Champions from each side, who then proceed to play in the NBA Finals.
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

# https://cran.r-project.org/web/packages/elo/vignettes/elo.html
library(elo)
# USES FACTORS FOR TEAMS
"
Naming Schema
Most functions begin with the prefix 'elo.', for easy autocompletion.

Vectors or scalars of Elo scores are denoted elo.A or elo.B.

Vectors or scalars of wins by team A are denoted by wins.A.

Vectors or scalars of win probabilities are denoted by p.A.

Vectors of team names are denoted team.A or team.B.
"

# To calculate the probability team.A beats team.B, use elo.prob()
elo.A <- c(1500, 1500)
elo.B <- c(1500, 1600)
elo.A <- c(1623.81)
elo.B <- c(1596.096)
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
The models can be built 'running', where predictions for the next group of games are made based on past data. 
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
The models can be built 'running', where predictions for the next group of games are made based on past data. 
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
These models can also be built 'running', where predictions for the next group of games are made based on past data. 
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
Telling elo.markovchain() that the visiting team 'won' gives the visiting team its whole share of p.
Alternatively, if h denotes a home-field advantage (in terms of score), the model becomes:
"
elo.markovchain(ifelse(home.points - visitor.points > h, 1, 0) ~ team.home + team.visitor + k(pmax(rH(x), 1 - rH(x))))

"
In this case, the home team 'won' if it scored more than h points more than the visiting team. 
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
These models can also be built 'running', where predictions for the next group of games are made based on past data. 
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




# Plot the elos throughout the season??





















