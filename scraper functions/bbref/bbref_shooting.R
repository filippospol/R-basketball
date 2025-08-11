# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script scrapes shooting stats from Basketball Reference.
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

bbref_shooting = function(season) {
  pacman::p_load(tidyverse,rvest,janitor,glue)
  
  suppressWarnings(
    "https://www.basketball-reference.com/leagues/NBA_{paste0(20,substr(season,6,7))}_shooting.html" %>% 
      glue() %>% 
      read_html() %>% 
      html_elements("table") %>% 
      html_table() %>% 
      pluck(1) %>% 
      row_to_names(row_number=1) %>% 
      clean_names("all_caps") %>% 
      mutate(TEAM = case_when(
        TEAM=="BRK" ~ "BKN",
        TEAM=="CHO" ~ "CHA",
        TEAM=="PHO" ~ "PHX",
        TRUE ~ TEAM
      )) %>% 
      group_by(PLAYER) %>% 
      mutate(TEAM=gsub("2TM|3TM|4TM","",paste0(TEAM,collapse="/")),
             TEAM=gsub("^/","",TEAM),
             SEASON=season) %>% 
      filter(as.numeric(MP)==max(as.numeric(MP))) %>%
      ungroup() %>% 
      arrange(PLAYER)
  ) %>% 
    select(SEASON,PLAYER_NAME=PLAYER,TEAM,AVG_FG_DIST=DIST,FREQ_FG2=X2P,FREQ_0_3=X0_3,FREQ_3_10=X3_10,
           FREQ_10_16=X10_16,FREQ_16_FG3=X16_3P,FREQ_FG3=X3P,EFF_FG2=X2P_2,EFF_0_3=X0_3_2,
           EFF_3_10=X3_10_2,EFF_10_16=X10_16_2,EFF_16_FG3=X16_3P_2,ASSISTED_FG2=X2P_3,
           ASSISTED_FG3=X3P_3,FREQ_DUNKS=PERCENT_FGA,DUNKS=NUMBER,FREQ_CORNER_FG3A=PERCENT_3PA,
           CORNER_FG3_PCT=X3P_PERCENT,HEAVES_FGA=ATT,HEAVES_FGM=MD) %>% 
    return()
}
