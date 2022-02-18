# load libraries: ----
library(tidyverse)
library(showtext)
library(ggtext)
source("https://raw.githubusercontent.com/filippospol/NBA-Data/main/apiLoad.R")
font_add_google(name = "Roboto", family = "Roboto")
showtext_auto()

# get team logs and info: ----
team_logs <- apiLoad("https://stats.nba.com/stats/leaguegamelog?Counter=1000&DateFrom=&DateTo=&Direction=DESC&LeagueID=00&PlayerOrTeam=T&Season=2021-22&SeasonType=Regular+Season&Sorter=DATE")
team_info <- team_logs %>% 
  group_by(TEAM_ID) %>%
  summarise(TEAM_ABB=unique(TEAM_ABBREVIATION)) %>% 
  arrange(TEAM_ABB)

# get defensive matchup stats: ----
out <- list()
for (i in 1:dim(team_info)[1]){
  out[[i]] <- apiLoad(
    gsub("XXX",team_info$TEAM_ID[i],
         "https://stats.nba.com/stats/leagueseasonmatchups?DateFrom=&DateTo=&DefTeamID=XXX&LeagueID=00&Outcome=&PORound=0&PerMode=Totals&Season=2021-22&SeasonType=Regular+Season")
  )
}
playersMatchups <- do.call("rbind",out)

# get per 100 possessions stats: ----
playersPer100 <- apiLoad("https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Per100Possessions&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=&Weight=")

# clean data: ----
playersPer100 <- playersPer100 %>% 
  mutate(PTS_PER_100 = as.numeric(PTS),
         OFF_PLAYER_ID = PLAYER_ID) %>% 
  select(OFF_PLAYER_ID,PTS_PER_100)

playersMatchups <- playersMatchups %>% 
  select(OFF_PLAYER_ID,OFF_PLAYER_NAME,DEF_PLAYER_ID,DEF_PLAYER_NAME,GP,
         MATCHUP_MIN,PARTIAL_POSS,PLAYER_PTS,MATCHUP_FGM,MATCHUP_FGA,
         MATCHUP_FG_PCT,MATCHUP_TIME_SEC)

df <- left_join(playersMatchups,playersPer100,by="OFF_PLAYER_ID") %>% 
  mutate_at(c(5,7:13),as.numeric)

playersList <- unique(df$OFF_PLAYER_NAME)
myPlayer <- "Jarred Vanderbilt"
if (myPlayer %in% playersList){
  
  # clear environment: ----
  rm(list=setdiff(ls(), c("myPlayer","playersList","df")))
  
  # create a subset for a given player: ----
  dfSubset <- df %>% 
    filter(DEF_PLAYER_NAME == myPlayer) %>% 
    arrange(desc(PARTIAL_POSS)) %>%
    mutate(PTS_WHEN_GUARDED=PLAYER_PTS/PARTIAL_POSS*100,
           PTS_AVERAGE=PTS_PER_100) %>% 
    select(c(1,2,5:8,14:15)) %>% 
    mutate(PTS_DIFF=PTS_WHEN_GUARDED-PTS_AVERAGE) %>% 
    top_n(10,PARTIAL_POSS)
  
  # create matchup report barchart: ----
  dfSubset %>% 
    ggplot(aes(x=reorder(
      paste0("<strong>",OFF_PLAYER_NAME,"</strong><br>",MATCHUP_MIN,", ",PARTIAL_POSS,
             " partial poss.",sep=""),PARTIAL_POSS),
      y=PTS_DIFF)) +
    geom_bar(stat = "identity", position=position_dodge(),
             fill=if_else(dfSubset$PTS_DIFF<0,"#88d8b0","#ff6f69"),
             width=0.75) +
    coord_flip() +
    labs(x="",y="Points/100 Difference",
         title=paste("Defensive Matchups Report:",myPlayer),
         subtitle=paste("Source: stats.NBA.com.",
                        format(Sys.Date(),format="%B %d %Y")),
         caption=paste0(
           "Chart by @filippos_pol<br>Red: scored better than average | Green: worse than average (when guarded by ",myPlayer,")"
         )) +
    theme_minimal() +
    theme(text=element_text(family = "Roboto"),
          plot.title = element_text(hjust=0, size=32,face="bold"),
          plot.subtitle = element_text(hjust=0, size=25, face="italic",
                                       color="#adadad"),
          plot.caption = element_markdown(hjust=0, size=16),
          axis.title = element_markdown(size=20),
          axis.text.x = element_markdown(size=20),
          axis.text.y = element_markdown(size=14),
          legend.text = element_text(size=12)
    )
} else {
  stop("Wrong player name. For a full list of names, type View(playersList).")
}       