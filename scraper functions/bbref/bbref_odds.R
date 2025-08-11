# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script scrapes the over/under odds by team from Basketball Reference
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


bbref_odds = function(season) {
  pacman::p_load(tidyverse,rvest,glue,janitor)
  
  season0 = paste0(20,substr(season,6,7))
  
  odds = "https://www.basketball-reference.com/leagues/NBA_{season0}_preseason_odds.html" %>% 
    glue() %>% 
    read_html() %>% 
    html_elements("table") %>% 
    html_table() %>% 
    pluck(1) %>% 
    clean_names("all_caps") %>% 
    select(TEAM_NAME=TEAM,PROJECTED_WINS=4,REAL_WINS=5)
  
  odds %>% 
    mutate(PROJECTED_WINS=PROJECTED_WINS+0.5,
           REAL_WINS=as.numeric(substr(REAL_WINS,1,2)),
           SEASON=season,
           TEAM_NAME=if_else(TEAM_NAME=="Los Angeles Clippers","LA Clippers",TEAM_NAME)) %>% 
    select(c(4,1:3)) %>% 
    return()
}
