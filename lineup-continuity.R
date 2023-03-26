pacman::p_load(tidyverse,rvest,ggimage,glue,janitor,hoopR)

source("https://raw.githubusercontent.com/filippospol/R-basketball/main/scraper%20functions/bbref_lineups.R")
source("https://raw.githubusercontent.com/filippospol/R-basketball/main/scraper%20functions/bbref_odds.R")

season = "2022-23"

df1 = bbref_lineups(season=season)
df2 = bbref_odds(season=season)

continuity = left_join(df1,df2,by="TEAM_NAME") %>% 
  left_join(nba_leaguedashteamstats(season=season) %>% 
              pluck(1) %>% 
              select(TEAM_ID,W,L),
            by="TEAM_ID") ; rm(df1,df2)

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
