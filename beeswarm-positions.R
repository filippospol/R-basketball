# load libraries: ----
library(tidyverse)
library(stringi)
library(rvest)
library(ggbeeswarm)
library(showtext)
source("https://raw.githubusercontent.com/filippospol/NBA-Data/main/apiLoad.R")
font_add_google(name = "Roboto", family = "Roboto")
showtext_auto()


# get shooting stats (misc, by zone): ----
playersFilter <- apiLoad("https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=&Weight=") %>% 
  select(PLAYER_ID,PLAYER_NAME,MIN) %>% 
  mutate(MIN=floor(as.numeric(MIN))) %>% 
  filter(MIN >= 100) %>% 
  arrange(PLAYER_NAME)

playersMisc <- apiLoad("https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Misc&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=&Weight=") %>% 
  select(PLAYER_ID,PLAYER_NAME,PTS_OFF_TOV,PTS_2ND_CHANCE,PTS_FB,PTS_PAINT)

source("https://raw.githubusercontent.com/filippospol/NBA-Data/main/players-by-zone.R")

# get player positions: ----
source("https://raw.githubusercontent.com/filippospol/NBA-Data/main/basketball-reference/bbref-position-estimate.R")

# clean data: ----

# manipulate PLAYER_NAME columns -> remove '.', ' Sr', and special letters:
bbrefEstPosition <- bbrefEstPosition %>% 
  mutate(PLAYER_NAME=stri_trans_general(c(PLAYER_NAME),"latin-ascii"))

bbrefEstPosition$PLAYER_NAME <- gsub('\\.','',
                                     gsub(' Sr','',bbrefEstPosition$PLAYER_NAME))
playersByZone$PLAYER_NAME <- gsub('\\.','',
                                  gsub(' Sr','',playersByZone$PLAYER_NAME))
playersMisc$PLAYER_NAME <- gsub('\\.','',
                                gsub(' Sr','',playersMisc$PLAYER_NAME))
playersFilter$PLAYER_NAME <- gsub('\\.','',
                                  gsub(' Sr','',playersFilter$PLAYER_NAME))

bbrefEstPosition <- bbrefEstPosition %>% 
  arrange(PLAYER_NAME)

playersMisc <- playersMisc%>% 
  arrange(PLAYER_NAME)

playersFilter <- playersFilter%>% 
  arrange(PLAYER_NAME)

bbrefEstPosition$PLAYER_NAME[which(bbrefEstPosition$PLAYER_NAME!=
                                     playersMisc$PLAYER_NAME)] <-
  playersMisc$PLAYER_NAME[which(bbrefEstPosition$PLAYER_NAME!=
                                  playersMisc$PLAYER_NAME)]

playersMerged <- inner_join(playersMisc,playersByZone,by=c("PLAYER_ID","PLAYER_NAME"))

# get everything in one table:
df <- inner_join(playersMerged,bbrefEstPosition,by="PLAYER_NAME") %>% 
  select(2,40,9,11:14,3:6,15:38) %>% 
  filter(PLAYER_NAME %in% playersFilter$PLAYER_NAME) %>% 
  mutate_at(-c(1:2),as.numeric) %>% 
  mutate(POSITION_EST = factor(POSITION_EST,
                               levels = c("POINT GUARD","COMBO GUARD","WING","FORWARD","BIG")))

rm(bbrefEstPosition,playersByZone,playersFilter,playersMerged,playersMisc)

# create beeswarm plot: ---- 
p <- df %>% 
  ggplot(aes(x=POSITION_EST,y=PTS_PAINT/GP,fill=POSITION_EST)) +
  # geom_quasirandom()
  geom_jitter(shape=21, alpha=0.6, size=3) +
  scale_fill_brewer(palette = "Set2") +
  labs(x="",y="PITP per game", title="Points in the paint leaders",
       subtitle=paste("Minimum 100 minutes played. Source: stats.NBA.com.",
                      format(Sys.Date(),format="%B %d, %Y")),
       caption="Chart by @filippos_pol") +
  theme_minimal() +
  theme(text=element_text(family = "Roboto"),
        plot.title = element_text(hjust=0, size=32,face="bold"),
        plot.subtitle = element_text(hjust=0, size=25, face="italic"),
        plot.caption = element_text(hjust=0, size=20),
        axis.title = element_text(size=20),
        axis.text = element_text(size=20),
        legend.position = "none")
ggsave("jitter.png",p,bg="#ffffff",width=6,height=4)

