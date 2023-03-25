# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script scrapes defensive partial possession matchups for a given season from NBA Stats
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

nba_def_matchups = function(season) {
  pacman::p_load(tidyverse,hoopR,httr,jsonlite,glue,janitor)
  
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
  
  teams = nba_leaguedashteamstats(season=season) %>% pluck(1) %>% pull(TEAM_ID)
  
  df = map_df(teams, function(x) {
    myurl <- GET(url = glue("https://stats.nba.com/stats/leagueseasonmatchups?DateFrom=&DateTo=&DefTeamID={x}&LeagueID=00&Matchup=Defense&Outcome=&PORound=0&PerMode=Totals&Season={season}&SeasonType=Regular%20Season&TeamID="), 
                 add_headers(.headers=headers))
    
    raw = fromJSON(content(myurl, "text")) %>% 
      pluck("resultSets")
    
    db = raw$rowSet %>% as.data.frame() ; names(db) = raw$headers %>% pluck(1)
    db = db %>% as_tibble
  })
  df %>% return()
}
