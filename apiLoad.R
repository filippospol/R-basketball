apiLoad <- function(x) {
  
  # this function scrapes data from stats.NBA.com
  # its only input is the url, found at:
  # right click web page > Inspect > Network
  # Inspired by:
  # https://www.owenlhjphillips.com/new-blog/2020/6/11/how-to-download-nba-shot-data-with-r
  
  suppressMessages(library(jsonlite))
  suppressMessages(library(httr))
  suppressMessages(library(tidyverse))
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
  
  # get player logs for a specific time period:
  raw <- GET(url = x, add_headers(.headers=headers))
  json_resp <- fromJSON(content(raw, "text"))
  data <- data.frame(json_resp$resultSets$rowSet[1])
  colnames(data) <- json_resp[["resultSets"]][["headers"]][[1]]
  tibble(data)
}
