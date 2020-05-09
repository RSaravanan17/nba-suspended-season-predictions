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




nbateams <- data.frame(team = unique(c(df$home_team_name, df$visitor_team_name)))
nbateams <- nbateams %>% mutate(elo=1500)
nbateams

historical_elo <- data.frame(team = nbateams$team)
historical_elo <- historical_elo %>% mutate(elo=1500)

season2016_2017 = read.csv('./data/nba-scrape-data-2016-2017.csv')
season2017_2018 = read.csv('./data/nba-scrape-data-2017-2018.csv')
season2018_2019 = read.csv('./data/nba-scrape-data-2018-2019.csv')
season2019_2020 = read.csv('./data/nba-scrape-data-2019-2020.csv')
head(season2019_2020)
season2019_2020$home_team_wins

#Copy and paste season data.frame into loop (seq and match) to update elo for consecutive seasons
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
}

options(digits=8)

nbateams %>%
  arrange(-elo)

avg_season_elo = 1505

nbateams <- nbateams %>% mutate(elo=0.75*elo+.25*avg_season_elo)

nbateams %>%
  arrange(-elo)

