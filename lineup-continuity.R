pacman::p_load(tidyverse,rvest,ggimage,glue,janitor,hoopR)
source("https://raw.githubusercontent.com/filippospol/R-basketball/main/scraper%20functions/bbref_lineups.R")

season = "2023-24"

# team lineups
df1 = bbref_lineups(season=season)

#O/U pre season predictions
"https://www.sportsoddshistory.com/nba-regular-season-win-total-results-by-team/" %>% 
  read_html() %>% 
  html_elements("table") %>% 
  html_table()

# team winning %
nba_leaguedashteamstats(season=season) %>% 
  pluck(1) %>% 
  select(TEAM_ID,W,L)

continuity %>% 
  mutate(DELTA=REAL_WINS-PROJECTED_WINS,
         CONTINUITY=LINEUPS/(as.numeric(W)+as.numeric(L)),
         LOGO=glue("https://cdn.nba.com/logos/nba/{TEAM_ID}/primary/L/logo.svg")) %>% 
  ggplot(aes(x=CONTINUITY,y=DELTA,image=LOGO)) +
  scale_y_continuous(labels=scales::percent, limits=c(-0.25,0.25)) +
  geom_hline(yintercept=0,linetype=2,alpha=0.4) +
  geom_image(aes(image=LOGO),size=0.06) +
  labs(title=paste('Continuity Chart:',format(Sys.Date(), "%m/%d/%y")),
       subtitle='Source: Basketball Reference',
       x='# of Distinct Starting Lineups / Games Played',
       y='Current W% - Projected W%')
