# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script returns boxscore stats for a given game from FIBA Live Stats
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fls_boxscore_team = function(gameid) {
  pacman::p_load(tidyverse,httr,jsonlite,glue,janitor)
  source("https://raw.githubusercontent.com/filippospol/R-basketball/main/scraper%20functions/fls_pbp.R")
  
  mytb = fls_pbp(gameid=gameid)
  
  x = mytb %>% 
    group_by(GAME_ID,TEAM_SHORT) %>% 
    summarise(FG2A = length(which(ACTION_TYPE=="2pt")),
              FG2M = length(which(ACTION_TYPE=="2pt" & SUCCESS==1)),
              FG3A = length(which(ACTION_TYPE=="3pt")),
              FG3M = length(which(ACTION_TYPE=="3pt" & SUCCESS==1)),
              FGA = FG2A+FG3A,
              FGM = FG2M+FG3M,
              FTA = length(which(ACTION_TYPE=="freethrow")),
              FTM = length(which(ACTION_TYPE=="freethrow" & SUCCESS==1)),
              DREB = length(which(ACTION_TYPE=="rebound" & SUB_TYPE=="defensive")),
              OREB = length(which(ACTION_TYPE=="rebound" & SUB_TYPE=="offensive")),
              STL = length(which(ACTION_TYPE=="steal")),
              BLK = length(which(ACTION_TYPE=="block")),
              AST = length(which(ACTION_TYPE=="assist")),
              TOV = length(which(ACTION_TYPE=="turnover")),
              PF = length(which(ACTION_TYPE=="foul")),
              PFD = length(which(ACTION_TYPE=="foulon")),
              FG2_PCT=round(FG2M/FG2A,3),
              FG3_PCT=round(FG3M/FG3A,3),
              FG_PCT=round(FGM/FGA,3),
              FT_PCT=round(FTM/FTA,3),
              PTS=FTM+2*FG2M+3*FG3M) %>% 
    ungroup()
  
  bind_cols(x,
            x[2:1,-(1:2)] %>% 
              rename_with( .fn = function(.x){paste0("OPP_", .x)})) %>% 
    mutate(EFG_PCT = round((FGM+0.5*FG3M)/FGA,3),
           OREB_PCT = round(OREB/(OREB+OPP_DREB),3),
           TOV_PCT = round(TOV/(FGA + 0.44*FTA + TOV),3),
           FTA_RATE = round(FTA/FGA,3),
           FG3A_RATE = round(FG3A/FGA,3),
           #
           OPP_EFG_PCT = round((OPP_FGM+0.5*OPP_FG3M)/OPP_FGA,3),
           OPP_OREB_PCT = round(OPP_OREB/(OPP_OREB+DREB),3),
           OPP_TOV_PCT = round(OPP_TOV/(OPP_FGA + 0.44*OPP_FTA + OPP_TOV),3),
           OPP_FTA_RATE = round(OPP_FTA/OPP_FGA,3),
           OPP_FG3A_RATE = round(OPP_FG3A/OPP_FGA,3)
    ) %>% 
    select(1:23,45:54) %>% return()
}

fls_boxscore2 = function(gameid) {
  myurl = glue("https://fibalivestats.dcd.shared.geniussports.com/data/{gameid}/data.json") %>% 
    as.character()
  mydata = fromJSON(myurl)
  
  
  home_name = mydata$tm$`1`$name
  home_out = list()
  for (i in 1:(mydata$tm$`1` %>% pluck("pl") %>% length())) {
    home_out[[i]] = mydata$tm$`1` %>% pluck("pl") %>% pluck(i) %>% 
      as.data.frame() %>% mutate_all(as.character) %>% mutate(TEAM=home_name)
  }
  
  away_name = mydata$tm$`2`$name
  away_out = list()
  for (i in 1:(mydata$tm$`2` %>% pluck("pl") %>% length())) {
    away_out[[i]] = mydata$tm$`2` %>% pluck("pl") %>% pluck(i) %>% 
      as.data.frame() %>% mutate_all(as.character) %>% mutate(TEAM=away_name)
  }
  
  bind_rows(home_out,away_out) %>% 
    as_tibble() %>% 
    clean_names("all_caps") %>% 
    rename_with(~str_remove(., 'S_')) %>% 
    mutate(PLAYER=paste0(FIRST_NAME," ",FAMILY_NAME),
           MATCHUP=paste0(home_name," vs ",away_name)) %>% 
    select(PLAYER,TEAM,MATCHUP,MIN=MINUTES,PTS=POINTS,
           `2PM`=TWO_POINTERS_MADE,`2PA`=TWO_POINTERS_ATTEMPTED,
           `3PM`=THREE_POINTERS_MADE,`3PA`=THREE_POINTERS_ATTEMPTED,
           FTM=FREE_THROWS_MADE,FTA=FREE_THROWS_ATTEMPTED,
           OREB=REBOUNDS_OFFENSIVE,DREB=REBOUNDS_DEFENSIVE,REB=REBOUNDS_TOTAL,
           AST=ASSISTS,STL=STEALS,BLK=BLOCKS,TOV=TURNOVERS,PF=FOULS_PERSONAL) %>% 
    filter(MIN != "0") %>% 
    mutate_at(5:19, as.numeric) ; rm(home_name,home_out,away_name,away_out,i,mydata,myurl) %>% 
    return()
}
