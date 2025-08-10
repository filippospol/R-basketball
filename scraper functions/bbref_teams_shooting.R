# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script scrapes team shooting stats from Basketball Reference.
# Columns include frequency and efficiency from different zones.
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

bbref_teams_shooting = function(season) {
  pacman::p_load(tidyverse,rvest,glue,janitor)
suppressWarnings(
  "https://www.basketball-reference.com/leagues/NBA_{as.numeric(substr(season,1,4))+1}.html" %>% 
    glue() %>% 
    read_html() %>% 
    html_elements("table") %>% 
    html_table() %>% 
    pluck(length(.)-1) %>% 
    row_to_names(row_number=1) %>% 
    clean_names("all_caps") %>% 
    mutate(SEASON=season) %>%
    select(SEASON,TEAM_NAME=TEAM,AVG_FG_DIST=DIST,FREQ_FG2=X2P,FREQ_0_3=X0_3,FREQ_3_10=X3_10,
           FREQ_10_16=X10_16,FREQ_16_FG3=X16_3P,FREQ_FG3=X3P,EFF_FG2=X2P_2,EFF_0_3=X0_3_2,
           EFF_3_10=X3_10_2,EFF_10_16=X10_16_2,EFF_16_FG3=X16_3P_2,
           CORNER_FREQ=PERCENT_3PA,CORNER_EFF=X3P_PERCENT,
           ASSISTED_FG2=X2P_3,ASSISTED_FG3=X3P_3,)
) %>% 
  mutate(TEAM_NAME=gsub("[*]","",TEAM_NAME)) %>% 
  mutate(TEAM_NAME=dplyr::if_else(TEAM_NAME=="Los Angeles Clippers","LA Clippers",TEAM_NAME)) %>%
  slice_head(n=30) %>%
  return()
}
