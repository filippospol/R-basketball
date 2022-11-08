# load libraries ----
pacman::p_load(dplyr,rvest)

# get bbref preseason odds and w-l o/u:
bbrefOdds <- "https://www.basketball-reference.com/leagues/NBA_2023_preseason_odds.html" %>% 
  read_html() %>% 
  html_elements("table") %>% 
  html_table() %>% 
  pluck(1) %>% 
  select(c(1,4,5)) %>% 
  mutate(Result = str_sub(Result, start=1, end=5)) %>%
  separate(Result,into=c("W","L"),sep="-",remove=T) %>% 
  mutate(W=as.numeric(W),L=as.numeric(L),
         PROJECTED_WIN_PCT=round((`W-L O/U`+0.5)/82,3),
         REAL_WIN_PCT=round(W/(W+L),3),
         OVER_UNDER_WINS=ifelse(W>`W-L O/U`,"O","U"),
         TEAM_NAME=Team) %>% 
  select(TEAM_NAME,`W-L O/U`,PROJECTED_WIN_PCT,W,L,REAL_WIN_PCT,OVER_UNDER_WINS)
