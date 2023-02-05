# load libraries: ----
pacman::p_load(tidyverse,rvest,janitor)

# get position estimates: ----
season="2019-20"

options(warn=-1)
df1 = "https://www.basketball-reference.com/leagues/NBA_{paste0(20,substr(season,6,7))}_play-by-play.html" %>% 
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
  arrange(PLAYER) %>% 
  select(PLAYER_NAME=PLAYER,PG_PERCENT:C_PERCENT) %>% 
  mutate_at(-1, ~as.numeric(gsub("%", "", .))/100) %>% 
  mutate_at(-1, ~ifelse(is.na(.),0,.))
options(warn=1)

df1 = df1 %>% 
  mutate(POSITION = case_when(
    PG_PERCENT>=0.85 ~ "POINT",
    PG_PERCENT+SG_PERCENT>=0.8 ~ "COMBO",
    PG_PERCENT+SG_PERCENT+SF_PERCENT>=0.85| SG_PERCENT>=0.85 | SF_PERCENT>=0.85 ~ "WING",
    SF_PERCENT>=0.7 | PF_PERCENT>=0.7 | SF_PERCENT+PF_PERCENT>=0.7~ "FORWARD",
    TRUE ~ "BIG"
  )) 

