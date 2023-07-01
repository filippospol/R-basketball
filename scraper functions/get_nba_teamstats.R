# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script scrapes team statistics from NBA Stats and PBP Stats
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

pacman::p_load(tidyverse,hoopR,httr,jsonlite,glue,janitor)
get_nba_teamstats = function(season,season_type) {
  pacman::p_load(tidyverse,hoopR,httr,jsonlite,glue,janitor)
  source("https://raw.githubusercontent.com/filippospol/R-basketball/main/scraper%20functions/nba_synergy_playtypes.R")
  
  per_mode="Totals"
  if (season_type=="Regular Season") season_type="Regular%20Season"
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # PBP Stats ----
  myurl=GET(glue("https://api.pbpstats.com/get-totals/nba?Season={season}&SeasonType={season_type}&Type=Team&StatType={per_mode}&StarterState=All&StartType=All"))
  
  pbp = suppressMessages(
    fromJSON(content(myurl , "text")) %>% 
      pluck("multi_row_table_data") %>% 
      tibble() %>% 
      clean_names("all_caps")
  ) %>% 
    select(TEAM_ID,TEAM_CODE=TEAM_ABBREVIATION,FG2M,FG2A,FG3M,FG3A,
           ASSISTED2S_PCT,ASSISTED3S_PCT,SECONDS_PER_POSS_OFF,SECONDS_PER_POSS_DEF,
           AT_RIM_FREQUENCY,AT_RIM_ACCURACY,SHORT_MID_RANGE_FREQUENCY,SHORT_MID_RANGE_ACCURACY,
           LONG_MID_RANGE_FREQUENCY,LONG_MID_RANGE_ACCURACY,CORNER3_FREQUENCY=CORNER3FREQUENCY,
           CORNER3_ACCURACY=CORNER3ACCURACY,ARC3_FREQUENCY=ARC3FREQUENCY,AT_RIM_FGA,
           ARC3_ACCURACY=ARC3ACCURACY,SHORT_MID_RANGE_FGA,LONG_MID_RANGE_FGA,
           CORNER3FGA,ARC3FGA)
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # NBA Stats ----
  
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
  
  # Traditional
  myurl = GET(url = glue("https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode={per_mode}&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season={season}&SeasonSegment=&SeasonType={season_type}&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision="), 
              add_headers(.headers=headers))
  
  raw = suppressMessages(
    fromJSON(content(myurl, "text")) %>% 
      pluck("resultSets")
  )
  db = raw$rowSet %>% as.data.frame() ; names(db) = raw$headers %>% pluck(1)
  db = db %>% as_tibble
  nba1 = db %>% select(TEAM_ID,TEAM_NAME,GP,MIN,W,L,FTM,FTA) %>% mutate_at(-c(1,2), as.numeric)
  rm(myurl,raw,db)
  
  # Advanced
  myurl = GET(url = glue("https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode={per_mode}&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season={season}&SeasonSegment=&SeasonType={season_type}&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision="), 
              add_headers(.headers=headers))
  
  raw = suppressMessages(
    fromJSON(content(myurl, "text")) %>% 
      pluck("resultSets")
  )
  db = raw$rowSet %>% as.data.frame() ; names(db) = raw$headers %>% pluck(1)
  db = db %>% as_tibble
  nba2 = db %>% select(TEAM_ID,POSS,ORTG=OFF_RATING,DRTG=DEF_RATING,NETRTG=NET_RATING,PACE,AST_PCT) %>%
    mutate_at(-1, as.numeric)
  rm(myurl,raw,db)
  
  # Four Factors
  myurl = GET(url = glue("https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Four%20Factors&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode={per_mode}&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season={season}&SeasonSegment=&SeasonType={season_type}&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision="), 
              add_headers(.headers=headers))
  
  raw = suppressMessages(
    fromJSON(content(myurl, "text")) %>% 
      pluck("resultSets")
  )
  db = raw$rowSet %>% as.data.frame() ; names(db) = raw$headers %>% pluck(1)
  db = db %>% as_tibble
  nba3 = db %>% select(TEAM_ID,EFG_PCT,FTA_RATE,TOV_PCT=TM_TOV_PCT,OREB_PCT,
                       OPP_EFG_PCT:OPP_OREB_PCT) %>%
    mutate_at(-1, as.numeric)
  rm(myurl,raw,db)
  
  # Hustle 
  myurl = GET(url = glue("https://stats.nba.com/stats/leaguehustlestatsteam?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode={per_mode}&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season={season}&SeasonSegment=&SeasonType={season_type}&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision="), 
              add_headers(.headers=headers))
  
  raw = suppressMessages(
    fromJSON(content(myurl, "text")) %>% 
      pluck("resultSets")
  )
  db = raw$rowSet %>% as.data.frame() ; names(db) = raw$headers %>% pluck(1)
  db = db %>% as_tibble
  nba4 = db %>% select(TEAM_ID,DEFLECTIONS,SCREEN_ASSISTS,CONTESTED_SHOTS_2PT,CONTESTED_SHOTS_3PT,
                       DEF_BOXOUTS,OFF_BOXOUTS) %>%
    mutate_at(-1, as.numeric)
  rm(myurl,raw,db)
  
  # Drives
  myurl = GET(url = glue("https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode={per_mode}&PlayerExperience=&PlayerOrTeam=Team&PlayerPosition=&PtMeasureType=Drives&Season={season}&SeasonSegment=&SeasonType={season_type}&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="), 
              add_headers(.headers=headers))
  
  raw = suppressMessages(
    fromJSON(content(myurl, "text")) %>% 
      pluck("resultSets")
  )
  db = raw$rowSet %>% as.data.frame() ; names(db) = raw$headers %>% pluck(1)
  db = db %>% as_tibble
  nba5 = db %>% select(TEAM_ID,DRIVES) %>%
    mutate_at(-1, as.numeric)
  rm(myurl,raw,db)
  
  # Touches
  myurl = GET(url = glue("https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode={per_mode}&PlayerExperience=&PlayerOrTeam=Team&PlayerPosition=&PtMeasureType=Possessions&Season={season}&SeasonSegment=&SeasonType={season_type}&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="), 
              add_headers(.headers=headers))
  
  raw = suppressMessages(
    fromJSON(content(myurl, "text")) %>% 
      pluck("resultSets")
  )
  db = raw$rowSet %>% as.data.frame() ; names(db) = raw$headers %>% pluck(1)
  db = db %>% as_tibble
  nba6 = db %>% select(TEAM_ID,PTS_PER_TOUCH,ELBOW_TOUCHES,POST_TOUCHES,PAINT_TOUCHES) %>%
    mutate_at(-1, as.numeric)
  rm(myurl,raw,db)
  
  # Catch & Shoot
  myurl = GET(url = glue("https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode={per_mode}&PlayerExperience=&PlayerOrTeam=Team&PlayerPosition=&PtMeasureType=CatchShoot&Season={season}&SeasonSegment=&SeasonType={season_type}&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="), 
              add_headers(.headers=headers))
  
  raw = suppressMessages(
    fromJSON(content(myurl, "text")) %>% 
      pluck("resultSets")
  )
  db = raw$rowSet %>% as.data.frame() ; names(db) = raw$headers %>% pluck(1)
  db = db %>% as_tibble
  nba7 = db %>% select(TEAM_ID,CS_FG3M=CATCH_SHOOT_FG3M,CS_FG3A=CATCH_SHOOT_FG3A,
                       CS_FG3_PCT=CATCH_SHOOT_FG3_PCT) %>%
    mutate_at(-1, as.numeric)
  rm(myurl,raw,db)
  
  # Pull Up
  myurl = GET(url = glue("https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode={per_mode}&PlayerExperience=&PlayerOrTeam=Team&PlayerPosition=&PtMeasureType=PullUpShot&Season={season}&SeasonSegment=&SeasonType={season_type}&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="), 
              add_headers(.headers=headers))
  
  raw = suppressMessages(
    fromJSON(content(myurl, "text")) %>% 
      pluck("resultSets")
  )
  db = raw$rowSet %>% as.data.frame() ; names(db) = raw$headers %>% pluck(1)
  db = db %>% as_tibble
  nba8 = db %>% select(TEAM_ID,PULL_UP_FG3M,PULL_UP_FG3A,PULL_UP_FG3_PCT) %>%
    mutate_at(-1, as.numeric)
  rm(myurl,raw,db)
  
  # Playtype
  nba9 = map_df(c("Isolation","Transition","PRBallHandler","PRRollMan","Postup","Spotup",
                  "Handoff","Cut","OffScreen","OffRebound","Misc"), function(playtype) {
                    nba_synergy_playtypes(season=season,season_type=season_type,
                                          per_mode=per_mode,player_or_team="T",
                                          grouping="offensive",playtype=playtype)
                  }) %>% 
    select(TEAM_ID,PLAY_TYPE,FREQ=POSS_PCT,PPP) %>% 
    mutate(PLAY_TYPE=toupper(PLAY_TYPE)) %>% 
    pivot_wider(names_from=PLAY_TYPE,values_from=c(FREQ,PPP)) %>% 
    mutate_at(-1, as.numeric)
  
  # Passing
  myurl = GET(url = glue("https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode={per_mode}&PlayerExperience=&PlayerOrTeam=Team&PlayerPosition=&PtMeasureType=Passing&Season={season}&SeasonSegment=&SeasonType={season_type}&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="), 
              add_headers(.headers=headers))
  
  raw = suppressMessages(
    fromJSON(content(myurl, "text")) %>% 
      pluck("resultSets")
  )
  db = raw$rowSet %>% as.data.frame() ; names(db) = raw$headers %>% pluck(1)
  db = db %>% as_tibble
  nba10 = db %>% select(TEAM_ID,PASSES=PASSES_MADE,AST,SECONDARY_AST,POTENTIAL_AST) %>%
    mutate_at(-1, as.numeric)
  rm(myurl,raw,db)
  
  nba = plyr::join_all(list(nba1,nba2,nba3,nba4,nba5,nba6,nba7,nba8,nba9,nba10),
                       type="left",by="TEAM_ID") %>% 
    as_tibble() %>% 
    mutate(SEASON=season, .before=1)
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  teams_db = left_join(nba,pbp,by="TEAM_ID") 
  rm(list=setdiff(ls(),"teams_db"))
  
  teams_db %>% 
    mutate(FG2A_RATE=round(FG2A/(FG2A+FG3A),3),FG3A_RATE=round(FG3A/(FG2A+FG3A),3),
           FG2_PCT=round(FG2M/FG2A,3),FG3_PCT=round(FG3M/FG3A,3),
           FT_PCT=round(FTM/FTA,3),RECORD=paste0(W,"-",L),
           W_PCT=round(W/GP,3), CSFG3A_RATE=round(CS_FG3A/FG3A,3),
           PULL_UP_FG3A_RATE=round(PULL_UP_FG3A/FG3A,3)) %>% 
    select(1:4,96,97,5,10,14,11:13,9,95,69,91,93,71,92,94,
           74,75,15,72,73,16:23,85,76,77,87,78,79,88,80,81,
           89,82,83,90,84,86,41:62,24:34,36,98,37,39,99,40,
           63:66) %>% 
    return()
}
