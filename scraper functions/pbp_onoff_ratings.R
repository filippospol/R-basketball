# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script scrapes on/off player efficiency from PBP Stats
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

pbp_onoff_ratings = function(season) {
  pacman::p_load(tidyverse,hoopR,httr,jsonlite,glue,janitor)
  
  teams = nba_leaguedashplayerstats(season=season,measure_type="Base") %>% pluck(1) %>% distinct(TEAM_ID) %>% pull()
  
  dfoff = map_df(teams, function(x) {
    myurl=GET(url = glue("https://api.pbpstats.com/get-on-off/nba/stat?Season={season}&SeasonType=Regular%2BSeason&TeamId={x}&Stat=PtsPer100Poss"))
    
    suppressMessages(
      fromJSON(content(myurl, "text")) %>% pluck(1) %>% tibble() %>% 
        clean_names("all_caps") %>% 
        mutate(TEAM_ID=x) %>% 
        select(PLAYER_NAME=NAME,TEAM_ID,MIN=MINUTES_ON,OFF_RATING_ON=ON,OFF_RATING_OFF=OFF,OFF_RATING_ON_OFF=ON_OFF)
    )
  })
  
  dfdef = map_df(teams, function(x) {
    myurl=GET(url = glue("https://api.pbpstats.com/get-on-off/nba/stat?Season={season}&SeasonType=Regular%2BSeason&TeamId={x}&Stat=PtsPer100PossOpponent"))
    
    suppressMessages(
      fromJSON(content(myurl, "text")) %>% pluck(1) %>% tibble() %>% 
        clean_names("all_caps") %>% 
        mutate(TEAM_ID=x) %>% 
        select(PLAYER_NAME=NAME,TEAM_ID,DEF_RATING_ON=ON,DEF_RATING_OFF=OFF,DEF_RATING_ON_OFF=ON_OFF)
    )
  })
  
  left_join(dfoff,dfdef, by=c("PLAYER_NAME","TEAM_ID")) %>% 
    mutate_at(-c(1:3), ~ round(as.numeric(.),2)) %>% 
    mutate(NET_RATING_ON=OFF_RATING_ON-DEF_RATING_ON,NET_RATING_OFF=OFF_RATING_OFF-DEF_RATING_OFF,
           NET_RATING_ON_OFF=OFF_RATING_ON_OFF-DEF_RATING_ON_OFF) %>%
    group_by(PLAYER_NAME) %>% 
    summarise_at(vars(3:11), ~ sum(MIN*.)/sum(MIN)) %>%
    left_join(nba_leaguedashplayerstats(season=season) %>% pluck(1) %>% select(PLAYER_NAME,PLAYER_ID,TEAM_ID,TEAM_ABBREVIATION)
              ,.,by="PLAYER_NAME") %>% 
    return()
}
