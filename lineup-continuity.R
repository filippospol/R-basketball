pacman::p_load(tidyverse,rvest,ggimage,glue,janitor,hoopR)
source("https://raw.githubusercontent.com/filippospol/R-basketball/main/scraper%20functions/bbref_lineups.R")

season = "2023-24"

# team lineups
lineups = bbref_lineups(season=season)

#O/U pre season predictions
over_under = "https://www.sportsoddshistory.com/nba-regular-season-win-total-results-by-team/" %>% 
  read_html() %>% 
  html_elements("table") %>% 
  html_table() %>% 
  pluck(1) %>% 
  row_to_names(row_number = 1) %>% 
  clean_names("all_caps") %>% 
  mutate(X23_24=substr(X23_24,1,3)) %>% 
  mutate(X23_24=if_else(TEAM=="Portland Trail Blazers","28.5",X23_24)) %>%
  select(TEAM_NAME=TEAM,PROJECTED_WINS=X23_24)

# team winning %
true_wins = nba_leaguedashteamstats(season=season) %>% 
  pluck(1) %>% 
  select(TEAM_NAME,W,L) %>% 
  mutate(TEAM_NAME=if_else(TEAM_NAME=="LA Clippers","Los Angeles Clippers",TEAM_NAME))

continuity = left_join(
  over_under,true_wins,
  by="TEAM_NAME") %>% 
  left_join(.,lineups, by="TEAM_NAME") %>% 
  select(TEAM_ID,TEAM_ABBREVIATION,TEAM_NAME,PROJECTED_WINS,W,L,LINEUPS) %>% 
  mutate_at(4:7, as.numeric)

continuity %>% 
  mutate(GP=W+L,
         PWIN=PROJECTED_WINS/82,
         TWIN=W/GP,
         DELTA=TWIN-PWIN,
         CONTINUITY=LINEUPS/GP,
         LOGO=glue("https://cdn.nba.com/logos/nba/{TEAM_ID}/primary/L/logo.svg")) %>% 
  ggplot(aes(x=CONTINUITY,y=DELTA,image=LOGO)) +
  scale_y_continuous(labels=scales::percent, limits=c(-0.28,0.28)) +
  geom_hline(yintercept=0,linetype=2,alpha=0.4) +
  geom_image(aes(image=LOGO),size=0.06) +
  labs(title=paste('Continuity Chart:',season),
       subtitle=paste('Source: Basketball Reference | As of',format(Sys.Date(), "%m/%d/%y")),
       x='# of Distinct Starting Lineups / Games Played',
       y='Current W% - Projected W%',
       caption="Chart by: Filippos Polyzos | Inspired by Owen Phillips") +
  theme_minimal() +
  theme(plot.title=element_text(face="bold"),
        plot.subtitle=element_text(color="grey4"),
        plot.caption=element_text(hjust=0),
        axis.title=element_text(face="bold",color="darkgrey"))
