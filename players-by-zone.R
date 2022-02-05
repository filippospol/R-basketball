# load libraries ----
library(tidyverse)
library(httr)
library(jsonlite)
library(janitor)
source("https://raw.githubusercontent.com/filippospol/NBA-Data/main/apiLoad.R")

# get raw data: ----

# important array for NBA API
headers = c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'https://stats.nba.com/stats/',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9'
)

# links from stats.nba.com
urlArray <- c(
  # Traditional totals
  "https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=&Weight=",
  # By zone
  "https://stats.nba.com/stats/leaguedashplayershotlocations?College=&Conference=&Country=&DateFrom=&DateTo=&DistanceRange=By+Zone&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="
)

url <- urlArray[2]

raw <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(raw, "text"))

tbByZone <- data.frame(json_resp$resultSets$rowSet)
colnames(tbByZone) <- json_resp$resultSets$headers$columnNames[[2]]

tbTotals <- apiLoad(urlArray[1])


# clean data: ----
tbTotals <- tbTotals %>% 
  select(PLAYER_ID,GP,MIN,
         FGM,FGA,FG3M,FG3A) %>% 
  mutate_at(-c(1:4),as.numeric)

misc <- c("RA","NON_RA","MID_RANGE","LEFT_CORNER","RIGHT_CORNER",
          "ATB","BACKCOURT","CORNER")
names(tbByZone)[7:9] <- paste(misc[1],"_",names(tbByZone)[7:9],sep="")
names(tbByZone)[10:12] <- paste(misc[2],"_",names(tbByZone)[10:12],sep="")
names(tbByZone)[13:15] <- paste(misc[3],"_",names(tbByZone)[13:15],sep="")
names(tbByZone)[16:18] <- paste(misc[4],"_",names(tbByZone)[16:18],sep="")
names(tbByZone)[19:21] <- paste(misc[5],"_",names(tbByZone)[19:21],sep="")
names(tbByZone)[22:24] <- paste(misc[6],"_",names(tbByZone)[22:24],sep="")
names(tbByZone)[25:27] <- paste(misc[7],"_",names(tbByZone)[25:27],sep="")
names(tbByZone)[28:30] <- paste(misc[8],"_",names(tbByZone)[28:30],sep="")

# final table:
playersByZone <- merge(tbTotals,tbByZone,by=c("PLAYER_ID")) %>%
  tibble() %>% 
  select(c(1,8,10,11,2:7,13:36)) %>% 
  mutate_at(-c(1:3),as.numeric) %>% 
  mutate(MIN=round(MIN,1))

rm(json_resp,raw,url,urlArray,headers,apiLoad,tbTotals,tbByZone,misc)
