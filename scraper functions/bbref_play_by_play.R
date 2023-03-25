# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script scrapes play-by-play stats from Basketball Reference
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

bbref_play_by_play = function(season) {
  pacman::p_load(tidyverse,rvest,janitor,glue)
  
  suppressWarnings(
    "https://www.basketball-reference.com/leagues/NBA_{paste0(20,substr(season,6,7))}_play-by-play.html" %>% 
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
      arrange(PLAYER)
  ) %>% 
    select(PLAYER_NAME=PLAYER,TOV_BAD_PASS=BAD_PASS,TOV_LOST_BALL=LOST_BALL,SHOOT_PF=SHOOT,OFF_PF=OFF,
           SHOOT_PFD=SHOOT_2,OFF_PFD=OFF_2,PGA,AND1,BLKD_FGA=BLKD) %>% 
    return()
}