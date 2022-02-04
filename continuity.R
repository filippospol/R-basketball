# load libraries: ----
library(tidyverse)
library(showtext)
library(teamcolors)
font_add_google("Roboto", "Roboto")
showtext_auto()

# get preseason odds and lineup information: ----
source("https://raw.githubusercontent.com/filippospol/NBA-Data/main/basketball-reference/bbref-starting-lineups.R")
source("https://raw.githubusercontent.com/filippospol/NBA-Data/main/basketball-reference/bbref-odds.R")

# merge tables into one: ----
df <- merge(numberOfLineups,bbrefOdds,by="TEAM_NAME") %>%
  tibble() %>% 
  mutate_at(-c(1:2,9),as.numeric)

# create lineup continuity and delta columns:
df <- df %>% 
  mutate(DELTA = REAL_WIN_PCT-PROJECTED_WIN_PCT,
         CONTINUITY = round(N_OF_LINEUPS/(W+L),3))

# add team colors:
df <- df %>% 
  mutate(COLOR = as.character(league_pal("nba", 1)))

# create chart: ----
p <- df %>% 
  ggplot(aes(x=CONTINUITY,y=DELTA,label=TEAM_ABBREVIATION)) +
  scale_y_continuous(labels=scales::percent, limits=c(-0.275,0.275)) +
  geom_hline(yintercept=0,linetype=2,alpha=0.4) +
  geom_point(pch=21,size=9, fill="#cfedc6") +
  geom_text(family="Roboto",fontface="bold", size=8) +
  labs(title=paste('Continuity Chart:',format(Sys.Date(), "%m/%d/%y")),
       subtitle='Source: Basketball Reference',
       caption='Plot by: @filippos_pol',
       x='# of Distinct Starting Lineups / Games Played',
       y='Current W% - Projected W%') +
  theme_light() +
  theme(text=element_text(family = "Roboto"),
        plot.title = element_text(hjust=0, size=32,face="bold"),
        plot.subtitle = element_text(hjust=0, size=25, face="italic"),
        plot.caption = element_text(hjust=0, size=18),
        axis.title = element_text(size=22),
        axis.text = element_text(size=20))

# (optional) save chart as .png file: ----
ggsave("continuity.png",p,width=6.5,height=4)
