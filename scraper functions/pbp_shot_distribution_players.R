pbp_shot_distribution_players = function(season,season_type="Regular Season",per_mode="PerGame") {
  pacman::p_load(tidyverse,hoopR,httr,jsonlite,glue,janitor)
  
  if (season_type=="Regular Season") season_type="Regular%20Season"
  
  myurl=GET(glue("https://api.pbpstats.com/get-totals/nba?Season={season}&SeasonType={season_type}&Type=Player&StatType={per_mode}&StarterState=All&StartType=All"))
  
  suppressMessages(
    fromJSON(content(myurl , "text")) %>% 
      pluck("multi_row_table_data") %>% 
      tibble() %>% 
      clean_names("all_caps")
  ) %>% 
    mutate(MID_RANGE_FGM=as.numeric(SHORT_MID_RANGE_FGM+LONG_MID_RANGE_FGM),
           MID_RANGE_FGA=as.numeric(SHORT_MID_RANGE_FGA+LONG_MID_RANGE_FGA),
           MID_RANGE_FREQUENCY=MID_RANGE_FGA/(as.numeric(FG2A+FG3A)),
           MID_RANGE_ACCURACY=MID_RANGE_FGM/MID_RANGE_FGA %>% round(3)) %>% 
    select(PLAYER_ID=ENTITY_ID,PLAYER_NAME=NAME,RIM_FREQ=AT_RIM_FREQUENCY,RIM_EFF=AT_RIM_ACCURACY,
           SHORT_MID_FREQ=SHORT_MID_RANGE_FREQUENCY,SHORT_MID_EFF=SHORT_MID_RANGE_ACCURACY,
           LONG_MID_FREQ=LONG_MID_RANGE_FREQUENCY,LONG_MID_EFF=LONG_MID_RANGE_ACCURACY,
           ALL_MID_FREQ=MID_RANGE_FREQUENCY,ALL_MID_EFF=MID_RANGE_ACCURACY,
           CORNER_FREQ=CORNER3FREQUENCY,CORNER_EFF=CORNER3ACCURACY,
           ARC_FREQ=ARC3FREQUENCY,ARC_EFF=ARC3ACCURACY) %>% 
    mutate_at(-c(1:2), as.numeric) %>% 
    arrange(PLAYER_NAME) %>% 
    return()
}