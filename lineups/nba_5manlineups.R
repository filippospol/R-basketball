# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script retrieves 5-man lineup stats from NBA API for a given season.
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

nba_5manlineups = function(season) {
  pacman::p_load(tidyverse,hoopR,glue,gt,gtExtras)
  
  # Get advanced 5-man lineup stats: 
  rawLineups = nba_leaguedashlineups(group_quantity=5, measure_type="Advanced", season=season) %>% 
    pluck(1) %>% 
    select(LINEUP=GROUP_NAME,LINEUP_ID=GROUP_ID,TEAM_ID,TEAM_ABBREVIATION,
           GP,W,L,MIN,POSS,OFF_RATING,DEF_RATING,NET_RATING)
  
  # Add html styling to lineup and team logo columns:
  styleLineups = suppressWarnings(
    rawLineups %>% 
      mutate(LINEUP_ID=sub("-","",LINEUP_ID),
             TEAM_ID=glue("https://cdn.nba.com/logos/nba/{TEAM_ID}/primary/L/logo.svg")) %>% 
      separate(LINEUP,into=c(paste0("P_",1:5)), sep=" - ") %>% 
      separate(LINEUP_ID,into=c(paste0("ID",1:5)), sep="-") %>% 
      rowwise() %>% 
      mutate_at(6:10, ~ glue(
        "<img src='https://cdn.nba.com/headshots/nba/latest/1040x760/{.}.png' width='50'></img>"
      ))
  ) %>% 
    mutate(LINEUP_HTML=html(paste0(ID1,ID2,ID3,ID4,ID5))) %>% 
    select(LINEUP_HTML,TEAM_ID,GP:NET_RATING) %>% 
    mutate_at(-c(1,2),as.numeric)
  rm(rawLineups)
  return(styleLineups)
}

# NOT RUN
# # Example: 10 random lineups for 2024-25 season in a {gt} table.
# # Error: can't save the table properly, but works well in Viewer tab on R-Studio.
# lineups25 = nba_5manlineups(season="2024-25")
# 
# set.seed(14)
# t = lineups25 %>%
#   select(1:3,7:10) %>% 
#   filter(POSS>=200) %>% 
#   { .[sample(nrow(.), 10), ] } %>% 
#   arrange(-POSS) %>% 
#   gt() %>% 
#   cols_align(align = "center") %>% 
#   fmt_markdown(columns=1) %>% 
#   gt_img_rows(columns=2, height=30) %>% 
#   cols_label(LINEUP_HTML="Lineup",TEAM_ID="Team",GP="Games",POSS="Poss.",
#              OFF_RATING="ORtg",DEF_RATING="DRtg",NET_RATING="NetRtg")