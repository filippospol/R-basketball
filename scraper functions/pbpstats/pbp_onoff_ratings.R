# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script scrapes on/off ORtg, DRtg & NetRtg for players on a given season (PBPStats).
# Due to requests limit, this function takes quite some time to print the results. Be patient.
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

pbp_onoff_ratings = function(season) {
  pacman::p_load(tidyverse,purrr,hoopR,httr,jsonlite,glue,janitor)
  # Team IDs:
  teams = nba_leaguedashplayerstats(season=season,measure_type="Base") %>% pluck(1) %>% distinct(TEAM_ID) %>% pull()
  # Offense ON/OFF:
  dfoff = map_df(seq_along(teams), function(x) {
    Sys.sleep(10)
    myurl=GET(url = glue("https://api.pbpstats.com/get-on-off/nba/stat?Season={season}&SeasonType=Regular%2BSeason&TeamId={teams[x]}&Stat=PtsPer100Poss"))
    
    y=suppressMessages(
      fromJSON(content(myurl, "text")) %>% pluck(1) %>% tibble() %>% 
        clean_names("all_caps") %>% 
        mutate(TEAM_ID=teams[x]) %>% 
        select(PLAYER_NAME=NAME,TEAM_ID,MIN=MINUTES_ON,OFF_RATING_ON=ON,OFF_RATING_OFF=OFF,OFF_RATING_ON_OFF=ON_OFF)
    )
    if (x==30) {
      cat(paste0("ON/OFF Offensive Rating for the ",season," season complete.\n"))
      Sys.sleep(2);beepr::beep(10)
    } ; return(y)
  })
  # Defense ON/OFF:
  dfdef = map_df(seq_along(teams), function(x) {
    Sys.sleep(10)
    myurl=GET(url = glue("https://api.pbpstats.com/get-on-off/nba/stat?Season={season}&SeasonType=Regular%2BSeason&TeamId={teams[x]}&Stat=PtsPer100PossOpponent"))
    
    y=suppressMessages(
      fromJSON(content(myurl, "text")) %>% pluck(1) %>% tibble() %>% 
        clean_names("all_caps") %>% 
        mutate(TEAM_ID=teams[x]) %>% 
        select(PLAYER_NAME=NAME,TEAM_ID,DEF_RATING_ON=ON,DEF_RATING_OFF=OFF,DEF_RATING_ON_OFF=ON_OFF)
    )
    if (x==30) {
      cat(paste0("ON/OFF Defensive Rating for the ",season," season complete.\n"))
      Sys.sleep(2);beepr::beep(10)
    } ; return(y)
  })
  
  # Return full table:
  bind_cols(
    dfoff %>% arrange(PLAYER_NAME,TEAM_ID),
    dfdef %>% arrange(PLAYER_NAME,TEAM_ID) %>% select(-c(1:2))
  ) %>% 
    mutate_at(4:9,~ round(as.numeric(.),2)) %>% 
    mutate(NET_RATING_ON=OFF_RATING_ON-DEF_RATING_ON,NET_RATING_OFF=OFF_RATING_OFF-DEF_RATING_OFF,
           NET_RATING_ON_OFF=OFF_RATING_ON_OFF-DEF_RATING_ON_OFF,SEASON=season) %>% 
    left_join(.,
              nba_leaguedashplayerstats(season=season) %>% pluck(1) %>% 
                select(PLAYER_NAME,PLAYER_ID),
              by=c("PLAYER_NAME")) %>% 
    left_join(.,
              nba_leaguedashplayerstats(season=season) %>% pluck(1) %>% 
                select(TEAM=TEAM_ABBREVIATION,TEAM_ID) %>% 
                distinct(TEAM,.keep_all=T),
              by=c("TEAM_ID")) %>%
    select(SEASON,PLAYER_ID,TEAM_ID,PLAYER_NAME,TEAM,
           MIN:NET_RATING_ON_OFF) %>% 
    return()
}
