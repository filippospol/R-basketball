library(dplyr)
library(httr)
library(jsonlite)

# team logo (need team id from nba.com)
# data %>% 
#   group_by(TEAM_ABBREVIATION) %>% 
#   summarise(ID_TEAM = max(TEAM_ID),
#             LOGO = paste("https://cdn.nba.com/logos/nba/",ID_TEAM,"/global/L/logo.svg",sep=""))

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


# get lineup data
# example: 5 man-lineups advanced stats
url <- "https://stats.nba.com/stats/leaguedashlineups?Conference=&DateFrom=&DateTo=&Division=&GameID=&GameSegment=&GroupQuantity=5&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlusMinus=N&Rank=N&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&TeamID=0&VsConference=&VsDivision="

raw <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(raw, "text"))
data <- data.frame(json_resp$resultSets$rowSet[1])
colnames(data) <- json_resp[["resultSets"]][["headers"]][[1]]