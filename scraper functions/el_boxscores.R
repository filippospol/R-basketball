# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script scrapes boxscores from the official Euroleague website
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


el_boxscores = function(gamecode, seasoncode) {
  pacman::p_load(tidyverse,httr,jsonlite,janitor,glue)
  
  if (content(GET(url = glue("https://live.euroleague.net/api/Boxscore?gamecode={gamecode}&seasoncode=E{seasoncode}")), "text") %>% 
      suppressMessages() != "") {
    
    myurl = GET(url = glue("https://live.euroleague.net/api/Boxscore?gamecode={gamecode}&seasoncode=E{seasoncode}")) %>% 
      suppressMessages()
    
    raw = fromJSON(content(myurl, "text")) %>% 
      pluck("Stats") %>% 
      as_tibble()
    
    bind_rows(as_tibble(raw[1,]) %>% 
                pull(PlayersStats) %>% 
                pluck(1) %>% 
                as_tibble() %>% 
                clean_names("all_caps") %>% 
                mutate(GAMECODE=gamecode,
                       SEASON=paste0(seasoncode,"-",as.numeric(substr(seasoncode,3,4))+1)),
              as_tibble(raw[2,]) %>% 
                pull(PlayersStats) %>% 
                pluck(1) %>% 
                as_tibble() %>% 
                clean_names("all_caps") %>% 
                mutate(GAMECODE=gamecode,
                       SEASON=paste0(seasoncode,"-",as.numeric(substr(seasoncode,3,4))+1))) %>%
      select(GAMECODE,SEASON,PLAYER_ID,IS_STARTER,PLAYER,TEAM,MIN=MINUTES,PTS=POINTS,FG2M=FIELD_GOALS_MADE2,
             FG2A=FIELD_GOALS_ATTEMPTED2,FG3M=FIELD_GOALS_MADE3,FG3A=FIELD_GOALS_ATTEMPTED3,FTM=FREE_THROWS_MADE,
             GTA=FREE_THROWS_ATTEMPTED,OREB=OFFENSIVE_REBOUNDS,DREB=DEFENSIVE_REBOUNDS,REB=TOTAL_REBOUNDS,AST=ASSISTANCES,STL=STEALS,
             TOV=TURNOVERS,BLK=BLOCKS_FAVOUR,BLKD=BLOCKS_AGAINST,PF=FOULS_COMMITED,PFD=FOULS_RECEIVED,PIR=VALUATION) %>% 
      return()
  }
  
}