library(tidyverse)
library(mosaic)
library(data.table)
library(magrittr)
library(ggplot2)
library(dplyr)
library(stringr)
library(elo)
library(foreach)
library(rvest)
library(lubridate)

#getwd()
#setwd('/Users/howardyong/Documents/College/School/Spring2020/SDS323_Spring2020')

#(1)==============================================SCRAPING ONLINE BASKETBALL DATA==============================================
yearList <- c('2020')
monthList <- c('october', 'november', 'december', 'january', 'february',
               'march')

df <- data.frame()
for (year in yearList) {
  if (year == '2020') {
    monthList <- c('october', 'november', 'december', 'january', 'february', 'march')
  }
  for (month in monthList) {
    # get webpage
    url <- paste0("https://www.basketball-reference.com/leagues/NBA_", year, 
                  "_games-", month, ".html")
    webpage <- read_html(url)
    
    # get column names
    col_names <- webpage %>% 
      html_nodes("table#schedule > thead > tr > th") %>% 
      html_attr("data-stat")    
    col_names <- c("game_id", col_names)
    
    # extract dates column
    # note that in april, there is a break in the table which just says 
    # "Playoffs". this messes with the data merging later, so we get rid of it
    dates <- webpage %>% 
      html_nodes("table#schedule > tbody > tr > th") %>% 
      html_text()
    dates <- dates[dates != "Playoffs"]
    
    # extract game id
    # we need to remove the NA that is due to the "Playoffs" row in april
    game_id <- webpage %>% 
      html_nodes("table#schedule > tbody > tr > th") %>%
      html_attr("csk")
    game_id <- game_id[!is.na(game_id)]
    
    # extract all columns (except date)
    data <- webpage %>% 
      html_nodes("table#schedule > tbody > tr > td") %>% 
      html_text() %>%
      matrix(ncol = length(col_names) - 2, byrow = TRUE)
    
    # combine game IDs, dates and columns in dataframe for this month, add col names
    month_df <- as.data.frame(cbind(game_id, dates, data), stringsAsFactors = FALSE)
    names(month_df) <- col_names
    
    # add to overall dataframe
    df <- rbind(df, month_df)
  }
}
# change columns to the correct types
df$visitor_pts <- as.numeric(df$visitor_pts)
df$home_pts    <- as.numeric(df$home_pts)
df$attendance  <- as.numeric(gsub(",", "", df$attendance))
df$date_game   <- mdy(df$date_game)
df$box_score_text <- NULL

# add home team winner column
df$home_team_wins <- with(df, ifelse(home_pts > visitor_pts, 1, 0))

# save to file
df
head(df)
tail(df)
write.csv(df, './data/nba-scrape-data-2019-2020.csv')



#(2)==============================================CALCULATING, UPDATING, AND PRODUCING ELO-RATINGS==============================================
nbateams <- data.frame(team = unique(c(df$home_team_name, df$visitor_team_name)))
nbateams <- nbateams %>% mutate(elo=1500)
nbateams$X <- NULL
nbateams

season2016_2017 = read.csv('./data/nba-scrape-data-2016-2017.csv')
season2017_2018 = read.csv('./data/nba-scrape-data-2017-2018.csv')
season2018_2019 = read.csv('./data/nba-scrape-data-2018-2019.csv')
season2019_2020 = read.csv('./data/nba-scrape-data-2019-2020.csv')
head(season2016_2017)
tail(season2016_2017)


#==============================================2016-2017==============================================
#Create different data.frames by team<#>
temp <- paste('team', 1:30, sep="")
for (i in 1:30) {
  assign(paste("team",i,sep=""), cbind(rep(0,250)) )
}

for (i in seq(nrow(season2016_2017))) {
  match <- season2016_2017[i, ]
  
  #Pre-match ratings
  teamA_elo <- subset(nbateams, team==match$home_team_name)$elo
  teamB_elo <- subset(nbateams, team==match$visitor_team_name)$elo
  
  #Update our ratings
  new_elo <- elo.calc(wins.A = match$home_team_wins,
                      elo.A = teamA_elo,
                      elo.B = teamB_elo,
                      k = 20)
  
  #Results reported as data.frame
  #Team A's new rating in row1/column1
  #Team B's new rating in row1/column2
  new_elo
  teamA_new_elo <- new_elo[1,1]
  teamB_new_elo <- new_elo[1,2]
  
  #Update the ratings for Teams A and B and leave other teams as they were
  nbateams <- nbateams %>%
    mutate(elo = if_else(team==match$home_team_name, teamA_new_elo,
                         if_else(team==match$visitor_team_name, teamB_new_elo, elo)))
  
  home_idx = match(match$home_team_name, nbateams$team)
  visitor_idx = match(match$visitor_team_name, nbateams$team)
  temp_home = paste('team', home_idx, sep="")
  temp_visitor = paste('team', visitor_idx, sep="")
  update_home = get(temp_home)
  update_visitor = get(temp_visitor)
  
  for (i in 1:length(update_home)) {
    if (update_home[i,]==0) {
      update_home[i,] = teamA_new_elo
      break
    }
  }
  assign(paste('team', home_idx, sep=""), update_home)
  
  for (i in 1:length(update_visitor)) {
    if (update_visitor[i,]==0) {
      update_visitor[i,] = teamB_new_elo
      break
    }
  }
  assign(paste('team', visitor_idx, sep=""), update_visitor)
}
options(digits=8)
nbateams %>%
  arrange(-elo)
avg_season_elo = 1505

elo_2016_2017 <- sapply(temp, get)
head(elo_2016_2017)
tail(elo_2016_2017)

c = 1
for (team in nbateams$team) {
  colnames(elo_2016_2017)[c] = team
  c = c + 1
}
tail(elo_2016_2017)
write.csv(elo_2016_2017, './data/elo_2016_2017.csv')

nbateams <- nbateams %>% mutate(elo=0.75*elo+.25*avg_season_elo)
nbateams %>%
  arrange(-elo)
#==============================================2017-2018==============================================
#Create different data.frames by team<#>
temp <- paste('team', 1:30, sep="")
for (i in 1:30) {
  assign(paste("team",i,sep=""), cbind(rep(0,250)) )
}

for (i in seq(nrow(season2017_2018))) {
  match <- season2017_2018[i, ]
  
  #Pre-match ratings
  teamA_elo <- subset(nbateams, team==match$home_team_name)$elo
  teamB_elo <- subset(nbateams, team==match$visitor_team_name)$elo
  
  #Update our ratings
  new_elo <- elo.calc(wins.A = match$home_team_wins,
                      elo.A = teamA_elo,
                      elo.B = teamB_elo,
                      k = 20)
  
  #Results reported as data.frame
  #Team A's new rating in row1/column1
  #Team B's new rating in row1/column2
  new_elo
  teamA_new_elo <- new_elo[1,1]
  teamB_new_elo <- new_elo[1,2]
  
  #Update the ratings for Teams A and B and leave other teams as they were
  nbateams <- nbateams %>%
    mutate(elo = if_else(team==match$home_team_name, teamA_new_elo,
                         if_else(team==match$visitor_team_name, teamB_new_elo, elo)))
  
  home_idx = match(match$home_team_name, nbateams$team)
  visitor_idx = match(match$visitor_team_name, nbateams$team)
  temp_home = paste('team', home_idx, sep="")
  temp_visitor = paste('team', visitor_idx, sep="")
  update_home = get(temp_home)
  update_visitor = get(temp_visitor)
  
  for (i in 1:length(update_home)) {
    if (update_home[i,] == 0) {
      update_home[i,] = teamA_new_elo
      break
    }
  }
  assign(paste('team', home_idx, sep=""), update_home)
  
  for (i in 1:length(update_visitor)) {
    if (update_visitor[i,] == 0) {
      update_visitor[i,] = teamB_new_elo
      break
    }
  }
  assign(paste('team', visitor_idx, sep=""), update_visitor)
}
options(digits=8)
nbateams %>%
  arrange(-elo)
avg_season_elo = 1505

elo_2017_2018 <- sapply(temp, get)
c = 1
for (team in nbateams$team) {
  colnames(elo_2017_2018)[c] = team
  c = c + 1
}
write.csv(elo_2017_2018, './data/elo_2017_2018.csv')

nbateams <- nbateams %>% mutate(elo=0.75*elo+.25*avg_season_elo)
nbateams %>%
  arrange(-elo)

#==============================================2018-2019==============================================
#Create different data.frames by team<#>
temp <- paste('team', 1:30, sep="")
for (i in 1:30) {
  assign(paste("team",i,sep=""), cbind(rep(0,250)) )
}

for (i in seq(nrow(season2018_2019))) {
  match <- season2018_2019[i, ]
  
  #Pre-match ratings
  teamA_elo <- subset(nbateams, team==match$home_team_name)$elo
  teamB_elo <- subset(nbateams, team==match$visitor_team_name)$elo
  
  #Update our ratings
  new_elo <- elo.calc(wins.A = match$home_team_wins,
                      elo.A = teamA_elo,
                      elo.B = teamB_elo,
                      k = 20)
  
  #Results reported as data.frame
  #Team A's new rating in row1/column1
  #Team B's new rating in row1/column2
  new_elo
  teamA_new_elo <- new_elo[1,1]
  teamB_new_elo <- new_elo[1,2]
  
  #Update the ratings for Teams A and B and leave other teams as they were
  nbateams <- nbateams %>%
    mutate(elo = if_else(team==match$home_team_name, teamA_new_elo,
                         if_else(team==match$visitor_team_name, teamB_new_elo, elo)))
  
  home_idx = match(match$home_team_name, nbateams$team)
  visitor_idx = match(match$visitor_team_name, nbateams$team)
  temp_home = paste('team', home_idx, sep="")
  temp_visitor = paste('team', visitor_idx, sep="")
  update_home = get(temp_home)
  update_visitor = get(temp_visitor)
  
  for (i in 1:length(update_home)) {
    if (update_home[i,] == 0) {
      update_home[i,] = teamA_new_elo
      break
    }
  }
  assign(paste('team', home_idx, sep=""), update_home)
  
  for (i in 1:length(update_visitor)) {
    if (update_visitor[i,] == 0) {
      update_visitor[i,] = teamB_new_elo
      break
    }
  }
  assign(paste('team', visitor_idx, sep=""), update_visitor)
}
options(digits=8)
nbateams %>%
  arrange(-elo)
avg_season_elo = 1505

elo_2018_2019 <- sapply(temp, get)
c = 1
for (team in nbateams$team) {
  colnames(elo_2018_2019)[c] = team
  c = c + 1
}
write.csv(elo_2018_2019, './data/elo_2018_2019.csv')

nbateams <- nbateams %>% mutate(elo=0.75*elo+.25*avg_season_elo)
nbateams %>%
  arrange(-elo)

#==============================================2019-2020==============================================
#Create different data.frames by team<#>
temp <- paste('team', 1:30, sep="")
for (i in 1:30) {
  assign(paste("team",i,sep=""), cbind(rep(0,250)) )
}

for (i in seq(nrow(season2019_2020))) {
  match <- season2019_2020[i, ]
  
  #Pre-match ratings
  teamA_elo <- subset(nbateams, team==match$home_team_name)$elo
  teamB_elo <- subset(nbateams, team==match$visitor_team_name)$elo
  
  #Update our ratings
  new_elo <- elo.calc(wins.A = match$home_team_wins,
                      elo.A = teamA_elo,
                      elo.B = teamB_elo,
                      k = 20)
  
  #Results reported as data.frame
  #Team A's new rating in row1/column1
  #Team B's new rating in row1/column2
  new_elo
  teamA_new_elo <- new_elo[1,1]
  teamB_new_elo <- new_elo[1,2]
  
  #Update the ratings for Teams A and B and leave other teams as they were
  nbateams <- nbateams %>%
    mutate(elo = if_else(team==match$home_team_name, teamA_new_elo,
                         if_else(team==match$visitor_team_name, teamB_new_elo, elo)))
  
  home_idx = match(match$home_team_name, nbateams$team)
  visitor_idx = match(match$visitor_team_name, nbateams$team)
  temp_home = paste('team', home_idx, sep="")
  temp_visitor = paste('team', visitor_idx, sep="")
  update_home = get(temp_home)
  update_visitor = get(temp_visitor)
  
  for (i in 1:length(update_home)) {
    if (update_home[i,] == 0) {
      update_home[i,] = teamA_new_elo
      break
    }
  }
  assign(paste('team', home_idx, sep=""), update_home)
  
  for (i in 1:length(update_visitor)) {
    if (update_visitor[i,] == 0) {
      update_visitor[i,] = teamB_new_elo
      break
    }
  }
  assign(paste('team', visitor_idx, sep=""), update_visitor)
}
options(digits=8)
nbateams %>%
  arrange(-elo)
avg_season_elo = 1505

elo_2019_2020 <- sapply(temp, get)
c = 1
for (team in nbateams$team) {
  colnames(elo_2019_2020)[c] = team
  c = c + 1
}
write.csv(elo_2019_2020, './data/elo_2019_2020.csv')

names(nbateams)[1] <- paste("Team Name")
names(nbateams)[2] <- paste("Elo Rating")
write.csv(nbateams, './data/nba-elo-current-2019-2020.csv')
nbateams = read.csv('./data/nba-elo-current-2019-2020.csv')
seasonelo = read.csv('./data/elo_2019_2020.csv')

seasonelo$X = NULL
library(reshape2)
elo2019_2020 <- melt(seasonelo)
elo_df <- melt(elo_2019_2020)
elo_df <- elo_df[(elo_df$Elo.Rating > 0),]

colnames(elo_df) <- c("Season.Progress", "Team", "Elo.Rating")
ggplot(elo_df, aes(x=Season.Progress, y=Elo.Rating, color=Team)) + geom_line()

  
  