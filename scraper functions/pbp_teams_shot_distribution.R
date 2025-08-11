# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script scrapes team shot distribution from PBP Stats.
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

pbp_teams_shot_distribution = function(season,season_type="Regular Season",per_mode="PerGame") {
  pacman::p_load(tidyverse,hoopR,httr,jsonlite,glue,janitor)
  
  if (season_type=="Regular Season") season_type="Regular%20Season"
  
  myurl=GET(glue("https://api.pbpstats.com/get-totals/nba?Season={season}&SeasonType={season_type}&Type=Team&StatType={per_mode}&StarterState=All&StartType=All"))
  
  left_join(
    nba_leaguedashteamstats(season=season) %>% 
      pluck(1) %>% 
      select(TEAM_ID,TEAM_NAME),
    suppressMessages(
      fromJSON(content(myurl , "text")) %>% 
        pluck("multi_row_table_data") %>% 
        tibble() %>% 
        clean_names("all_caps")
    ) %>% 
      mutate(MID_RANGE_FGM=as.numeric(SHORT_MID_RANGE_FGM+LONG_MID_RANGE_FGM),
             MID_RANGE_FGA=as.numeric(SHORT_MID_RANGE_FGA+LONG_MID_RANGE_FGA),
             MID_RANGE_FREQUENCY=MID_RANGE_FGA/(as.numeric(FG2A+FG3A)),
             MID_RANGE_ACCURACY=MID_RANGE_FGM/MID_RANGE_FGA %>% round(3),
             SEASON=season) %>% 
      select(SEASON,TEAM_ID,TEAM_ABBREVIATION,RIM_FREQ=AT_RIM_FREQUENCY,RIM_EFF=AT_RIM_ACCURACY,
             SHORT_MID_FREQ=SHORT_MID_RANGE_FREQUENCY,SHORT_MID_EFF=SHORT_MID_RANGE_ACCURACY,
             LONG_MID_FREQ=LONG_MID_RANGE_FREQUENCY,LONG_MID_EFF=LONG_MID_RANGE_ACCURACY,
             ALL_MID_FREQ=MID_RANGE_FREQUENCY,ALL_MID_EFF=MID_RANGE_ACCURACY,
             CORNER_FREQ=CORNER3FREQUENCY,CORNER_EFF=CORNER3ACCURACY,
             ARC_FREQ=ARC3FREQUENCY,ARC_EFF=ARC3ACCURACY,SECONDS_PER_POSS_OFF,SECONDS_PER_POSS_DEF),
    by="TEAM_ID"
  ) %>% 
    mutate_at(-c(1:4), as.numeric) %>% arrange(TEAM_ABBREVIATION) %>% 
    return()
}
