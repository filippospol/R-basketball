# load libraries: ----
pacman::p_load(tidyverse,rvest,ggimage)

# get preseason odds and lineup information: ----
source("https://raw.githubusercontent.com/filippospol/R-projects-NBA/main/basketball-reference/bbref-starting-lineups.R")
source("https://raw.githubusercontent.com/filippospol/R-projects-NBA/main/basketball-reference/bbref-odds.R")

# merge tables into one: ----
df <- merge(numberOfLineups,bbrefOdds,by="TEAM_NAME") %>%
  tibble() %>% 
  mutate_at(-c(1:2,9),as.numeric)

# create lineup continuity and delta columns:
df <- df %>% 
  mutate(DELTA = REAL_WIN_PCT-PROJECTED_WIN_PCT,
         CONTINUITY = round(N_OF_LINEUPS/(W+L),3))

# add team logos:
df$TEAM_ID <- hoopR::nba_leaguedashteamstats(season="2022-23") %>%
  pluck(1) %>% 
  select(TEAM_ID) %>% 
  pull()
df$LOGO <- NA
for (i in 1:nrow(df)) {
  df$LOGO[i] <- gsub("XXX",df$TEAM_ID[i],
                          "https://cdn.nba.com/logos/nba/XXX/global/L/logo.svg")
}
rm(list=setdiff(ls(), "df"))

# create chart: ----
df %>% 
  ggplot(aes(x=CONTINUITY,y=DELTA,label=TEAM_ABBREVIATION)) +
  scale_y_continuous(labels=scales::percent, limits=c(-0.275,0.275)) +
  geom_hline(yintercept=0,linetype=2,alpha=0.4) +
  geom_image(aes(image=LOGO),size=0.07) +
  labs(title=paste('Continuity Chart:',format(Sys.Date(), "%m/%d/%y")),
       subtitle='Source: Basketball Reference | Plot by: @filippos_pol',
       x='# of Distinct Starting Lineups / Games Played',
       y='Current W% - Projected W%')
