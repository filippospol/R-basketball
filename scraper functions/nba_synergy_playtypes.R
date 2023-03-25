# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script scrapes Synergy playtype data from the NBA Stats website.
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

nba_synergy_playtypes = function(season,per_mode,playtype,player_or_team,grouping) {
  pacman::p_load(tidyverse,httr,jsonlite,glue,janitor)
  
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
  
  if (!playtype %in% c("Isolation","Transition","PRBallHandler","PRRollMan","Postup","Spotup","Handoff","Cut","OffScreen","OffRebound","Misc")) {
    stop('Wrong playtype value. Must be one of:\n
         Isolation, Transition, PRBallHandler, PRRollMan, Postup, Spotup, Handoff, Cut, OffScreen, OffRebound, Misc')
  }
  
  myurl <- GET(url = glue("https://stats.nba.com/stats/synergyplaytypes?LeagueID=00&PerMode={per_mode}&PlayType={playtype}&PlayerOrTeam={player_or_team}&SeasonType=Regular%20Season&SeasonYear={season}&TypeGrouping={grouping}"), 
               add_headers(.headers=headers))
  
  raw = fromJSON(content(myurl, "text")) %>% 
    pluck("resultSets")
  
  df = raw$rowSet %>% as.data.frame() ; names(df) = raw$headers %>% pluck(1)
  
  tibble(df) %>% return()
  
}