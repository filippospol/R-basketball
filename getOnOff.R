pacman::p_load(httr,jsonlite,tidyverse,glue)

getOnOff = function(season,team_id) {
  
  myurl = glue("https://stats.nba.com/stats/teamplayeronoffdetails?DateFrom=&DateTo=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=Totals&Period=0&PlusMinus=N&Rank=N&Season={season}&SeasonSegment=&SeasonType=Regular%20Season&TeamID={team_id}&VsConference=&VsDivision=")
  
  headers = c(
    `Connection` = 'keep-alive',
    `Accept` = 'application/json, text/plain, */*',
    `x-nba-stats-token` = 'true',
    `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
    `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
    # `x-nba-stats-origin` = 'stats',
    `Sec-Fetch-Site` = 'same-origin',
    `Sec-Fetch-Mode` = 'cors',
    `Referer` = 'https://nba.com/team/',
    `Accept-Encoding` = 'gzip, deflate, br',
    `Accept-Language` = 'en-US,en;q=0.9'
  )
  
  raw <- GET(url = myurl, add_headers(.headers=headers))
  json_resp <- fromJSON(content(raw, "text"))
  
  dfOn = data.frame(json_resp$resultSets$rowSet[2])
  colnames(dfOn) <- json_resp[["resultSets"]][["headers"]][[2]]
  dfOn = tibble(dfOn) %>% 
    separate(VS_PLAYER_NAME,c("LAST","FIRST"),sep=", ") %>% 
    unite(PLAYER_NAME,c("FIRST","LAST"),sep= " ") %>% 
    select(PLAYER_ID=VS_PLAYER_ID,TEAM=TEAM_ABBREVIATION,PLAYER_NAME,MIN_ON=MIN,POSS_ON=POSS,ORTG_ON=OFF_RATING,DRTG_ON=DEF_RATING,
           NET_ON=NET_RATING)
  
  dfOff = data.frame(json_resp$resultSets$rowSet[3])
  colnames(dfOff) <- json_resp[["resultSets"]][["headers"]][[3]]
  dfOff = tibble(dfOff) %>% 
    separate(VS_PLAYER_NAME,c("LAST","FIRST"),sep=", ") %>% 
    unite(PLAYER_NAME,c("FIRST","LAST"),sep= " ") %>% 
    select(PLAYER_ID=VS_PLAYER_ID,MIN_OFF=MIN,POSS_OFF=POSS,ORTG_OFF=OFF_RATING,DRTG_OFF=DEF_RATING,NET_OFF=NET_RATING)
  
  rm(list=setdiff(ls(),c("dfOn","dfOff","season")))
  
  df = left_join(dfOn,dfOff,by="PLAYER_ID") %>% 
    mutate_at(4:13, as.numeric) %>% 
    mutate(MIN=MIN_ON, POSS=POSS_ON,
           ORTG_ONOFF=ORTG_ON-ORTG_OFF, DRTG_ONOFF=DRTG_ON-DRTG_OFF, NET_ONOFF=NET_ON-NET_OFF,SEASON=season) %>% 
    select(c(19,1:3,14,15,6,11,16,7,12,17,8,13,18)) %>% 
    mutate_at(6:14, ~round(.,2)) %>% 
    arrange(-MIN)
  return(df)
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++