library(dplyr)
library(httr)
library(jsonlite)

# player headshot (need player id from nba.com)
# mutate(HEADSHOT = paste("https://cdn.nba.com/headshots/nba/latest/1040x760/",PLAYER_ID,".png",sep=""))

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


# get data for a single season / stat type:
# example: misc stats per game
url = "https://stats.nba.com/stats/leaguedashplayerstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Misc&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision="

raw <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(raw, "text"))
data <- data.frame(json_resp$resultSets$rowSet[1])
colnames(data) <- json_resp[["resultSets"]][["headers"]][[1]]


# get data for several seasons / stat types:
# example: clutch advanced and traditional(base) stats for seasons 2019-22

out_trad <- list()
out_adv <- list()
year <- c("2018-19","2019-20","2020-21","2021-22"); i=1

for (i in 1:length(year)) {
  
  url1 = gsub("2021-22", year[i],"https://stats.nba.com/stats/leaguedashplayerclutch?AheadBehind=Ahead+or+Behind&ClutchTime=Last+5+Minutes&College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&PointDiff=5&Rank=N&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight=")
  url2 = gsub("2021-22", year[i],"https://stats.nba.com/stats/leaguedashplayerclutch?AheadBehind=Ahead+or+Behind&ClutchTime=Last+5+Minutes&College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&PointDiff=5&Rank=N&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight=")
  
  raw <- GET(url = url1, add_headers(.headers=headers))
  json_resp <- fromJSON(content(raw, "text"))
  data <- data.frame(json_resp$resultSets$rowSet[1])
  colnames(data) <- json_resp[["resultSets"]][["headers"]][[1]]
  out_trad[[i]] = data
  
  raw <- GET(url = url2, add_headers(.headers=headers))
  json_resp <- fromJSON(content(raw, "text"))
  data <- data.frame(json_resp$resultSets$rowSet[1])
  colnames(data) <- json_resp[["resultSets"]][["headers"]][[1]]
  out_adv[[i]] = data
  
}

trad <- do.call("rbind", out_trad)
adv <- do.call("rbind", out_adv)