# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script gives player positions in `Cleaning the Glass` format for a given season
# Point - Combo - Wing - Forward - Big
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ctg_positions = function(season) {
  pacman::p_load(tidyverse,rvest,hoopR,janitor,glue)
  
  # get players for a given season (NBA Stats):
  players = nba_leaguedashplayerstats(season=season, measure_type="Base") %>% 
    pluck(1) %>% 
    select(PLAYER_ID,PLAYER_NAME,TEAM_ABBREVIATION)
  
  # player position estimates (Basketball Reference):
  options(warn=-1)
  position_est = "https://www.basketball-reference.com/leagues/NBA_{paste0(20,substr(season,6,7))}_play-by-play.html" %>% 
    glue() %>% 
    read_html() %>% 
    html_elements("table") %>% 
    html_table() %>% 
    pluck(1) %>% 
    row_to_names(row_number=1) %>% 
    clean_names("all_caps") %>% 
    group_by(PLAYER) %>% 
    filter(as.numeric(MP)==max(as.numeric(MP))) %>% 
    ungroup() %>% 
    arrange(PLAYER) %>% 
    select(PLAYER_NAME=PLAYER,PG_PERCENT:C_PERCENT) %>% 
    mutate_at(-1, ~as.numeric(gsub("%", "", .))/100) %>% 
    mutate_at(-1, ~ifelse(is.na(.),0,.))
  options(warn=1)
  
  # get player positions in Cleaning the Glass format
  # there will be differences with the CTG results, but the accuravy is high.
  position_est = position_est %>% 
    mutate(POSITION = case_when(
      PG_PERCENT>=0.85 ~ "Point",
      (PG_PERCENT>=0.5 | SG_PERCENT>=0.5) & PG_PERCENT+SG_PERCENT>0.8 ~ "Combo",
      PG_PERCENT+SG_PERCENT+SF_PERCENT>=0.85| SG_PERCENT>=0.85 | SF_PERCENT>=0.85 ~ "Wing",
      C_PERCENT<0.1 & SF_PERCENT>=0.65 | PF_PERCENT>=0.65 | SF_PERCENT+PF_PERCENT>=0.75 ~ "Forward",
      TRUE ~ "Big"
    )) %>% 
    select(PLAYER_NAME,POSITION)
  
  # return the table with player positions
  # change player names to the NBA.com version using the edit distance
  edd <- adist(position_est$PLAYER_NAME, players$PLAYER_NAME)
  ind <- rep(NA,ncol(edd))
  for (i in 1:nrow(players)) {
    ind[i] <- which.min(edd[,i])
  }
  position_est<- position_est[ind,] %>% na.omit() ; rm(i,edd,ind)
  
  bind_cols(players,position_est %>% select(-1)) %>% return()
}