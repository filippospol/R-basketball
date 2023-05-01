# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script scrapes team general stats from NBA Stats & Inpredictable
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

nba_team_general_stats = function(season) {
  pacman::p_load(tidyverse,hoopR,rvest,glue,janitor)
  
  df1 = left_join(
    nba_leaguedashteamstats(season=season, measure_type="Advanced") %>% 
      pluck(1) %>% 
      select(TEAM_ID,TEAM_NAME,GP,PACE,OFF_RTG=OFF_RATING,DEF_RTG=DEF_RATING,NET_RTG=NET_RATING),
    nba_leaguedashteamstats(season=season, measure_type="Base") %>% 
      pluck(1) %>% 
      mutate(FG2_PCT=as.numeric((as.numeric(FGM)-as.numeric(FG3M))/(as.numeric(FGA)-as.numeric(FG3A)))) %>% 
      select(TEAM_ID,FG_PCT,FG2_PCT,FG3_PCT,FT_PCT),
    by="TEAM_ID"
  )
  
  df2 = left_join(
    nba_leaguedashplayerstats(season=season,measure_type="Advanced") %>% 
      pluck(1) %>% 
      select(TEAM_ID,TEAM_ABBREVIATION) %>% 
      distinct(TEAM_ID, .keep_all=T),
    suppressWarnings(
      "http://stats.inpredictable.com/nba/ssnTeamPoss.php?season={substr(season,1,4)}" %>% 
        glue() %>% 
        read_html() %>% 
        html_element("table") %>% 
        html_table() %>% 
        row_to_names(row_number=1) %>% 
        clean_names("all_caps") %>% 
        select(TEAM_ABBREVIATION=TEAM,AVG_TIME_OF_POSS=SECONDS)
    ),
    by="TEAM_ABBREVIATION"
  ) %>% 
    arrange(TEAM_ABBREVIATION)
  
  left_join(df1,df2,by="TEAM_ID") %>% 
    select(TEAM_ID,TEAM_NAME,TEAM_ABBREVIATION,GP,FG_PCT:FT_PCT,AVG_TIME_OF_POSS,PACE:NET_RTG) %>% 
    return()
}
