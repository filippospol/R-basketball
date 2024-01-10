# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script scrapes the # of starting lineups by team from Basketball Reference
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

bbref_lineups = function(season) {
  pacman::p_load(tidyverse,rvest,glue,janitor,hoopR)

  season0 = paste0(20,substr(season,6,7))

teams = "https://www.basketball-reference.com/leagues/NBA_{season0}_per_game.html" %>% 
  glue() %>% 
  read_html() %>% 
  html_elements("table") %>% 
  html_table() %>% 
  pluck(1) %>% 
  drop_na() %>% 
  filter(!Tm %in% c("Tm","TOT")) %>% 
  distinct(Tm) %>% 
  arrange(Tm) %>% 
  pull()

n_lineups = map_df(teams[1:20], function(x) {
  "https://www.basketball-reference.com/teams/{x}/{season0}_start.html" %>% 
    glue() %>% 
    read_html() %>% 
    html_elements("table") %>% 
    html_table() %>% 
    pluck(2) %>%
    mutate(TEAM_ABBREVIATION=x) %>% 
    clean_names("all_caps") %>% 
    select(5,6,1:3) %>% 
    mutate(STARTING_LINEUP=gsub("\\·","_",STARTING_LINEUP)) %>% 
    separate(STARTING_LINEUP,into=c("P1","P2","P3","P4","P5"),sep="_") %>% 
    mutate_at(2:5, ~ str_trim(.)) %>% 
    rowwise() %>% 
    mutate(STARTING_LINEUP=paste(sort(c(P1,P2,P3,P4,P5)),collapse=" _ "),.before=1) %>% 
    select(-c(2:6))
  
}) %>% 
  group_by(TEAM_ABBREVIATION) %>% 
  summarise(LINEUPS=n())
Sys.sleep(60)
n_lineups = bind_rows(n_lineups,
                      map_df(teams[21:30], function(x) {
                        "https://www.basketball-reference.com/teams/{x}/{season0}_start.html" %>% 
                          glue() %>% 
                          read_html() %>% 
                          html_elements("table") %>% 
                          html_table() %>% 
                          pluck(2) %>%
                          mutate(TEAM_ABBREVIATION=x) %>% 
                          clean_names("all_caps") %>% 
                          select(5,6,1:3) %>% 
                          mutate(STARTING_LINEUP=gsub("\\·","_",STARTING_LINEUP)) %>% 
                          separate(STARTING_LINEUP,into=c("P1","P2","P3","P4","P5"),sep="_") %>% 
                          mutate_at(2:5, ~ str_trim(.)) %>% 
                          rowwise() %>% 
                          mutate(STARTING_LINEUP=paste(sort(c(P1,P2,P3,P4,P5)),collapse=" _ "),.before=1) %>% 
                          select(-c(2:6))
                      }) %>% 
                        group_by(TEAM_ABBREVIATION) %>% 
                        summarise(LINEUPS=n()))

n_lineups$TEAM_ABBREVIATION[3] = "BKN" ; n_lineups$TEAM_ABBREVIATION[5] = "CHA"
n_lineups$TEAM_ABBREVIATION[24] = "PHX"

left_join(n_lineups,
          left_join(nba_leaguedashteamstats(season=season) %>% 
                      pluck(1) %>% 
                      select(TEAM_ID,TEAM_NAME),
                    nba_leaguedashplayerstats(season=season) %>% 
                      pluck(1) %>% 
                      distinct(TEAM_ID,.keep_all=T) %>% 
                      select(TEAM_ID,TEAM_ABBREVIATION),
                    by="TEAM_ID") %>% 
            mutate(TEAM_NAME=ifelse(TEAM_NAME=="LA Clippers","Los Angeles Clippers",TEAM_NAME)),
          by="TEAM_ABBREVIATION") %>% 
    return()
}
