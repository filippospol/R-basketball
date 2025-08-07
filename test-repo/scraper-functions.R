#' This @script includes the functions needed to scrape total stats
#' from main basketball leagues. Namely
#' - NBA
#' - G-League
#' - `NCAA not yet`
#' - Euroleague
#' - Eurocup
#' - Basketball Champions League (BCL)
#' - FIBA Europe Cup

pacman::p_load(tidyverse,hoopR,httr,rvest,jsonlite,janitor,stringr,glue)

nba_scraper = function(season) {
  nba_leaguedashplayerstats(season=season,per_mode="Totals") %>% 
    pluck(1) %>% 
    select(PLAYER=PLAYER_NAME,MIN,PTS,FGA,FGM,FG_PCT,FG3A,FG3M,FG3_PCT,
           FTA,FTM,FT_PCT,OREB,DREB,AST,STL,BLK,TOV,PF) %>% 
    mutate_at(-1, as.numeric) %>% 
    mutate(FG2A=FGA-FG3A,FG2M=FGM-FG3M,FG2_PCT=round(FG2M/FG2A,3),.before=6) %>% 
    mutate(SEASON=season,LEAGUE="NBA",.before=2) %>% 
    arrange(PLAYER) %>% 
    return()
}

gleague_scraper = function(season) {
  season2=as.numeric(substr(season,1,4))+1
  
  suppressWarnings(
    "https://basketball.realgm.com/gleague/stats/{season2}/Totals/Qualified/points/All/desc/1/Regular_Season" %>% 
      glue() %>% 
      read_html() %>% 
      html_elements("table") %>% 
      html_table()
  ) %>% 
    pluck(1) %>% 
    clean_names("all_caps") %>% 
    select(PLAYER,MIN,PTS,FGA,FGM,FG_PCT=FG_PERCENT,
           FG3A=X3PA,FG3M=X3PM,FG3_PCT=X3P_PERCENT,
           FTA,FTM,FT_PCT=FT_PERCENT,OREB=ORB,DREB=DRB,AST,STL,BLK,TOV,PF) %>% 
    mutate(MIN=gsub(",","",MIN)) %>% mutate(PTS=gsub(",","",PTS)) %>% 
    mutate_at(-1, as.numeric) %>% 
    mutate(FG2A=FGA-FG3A,FG2M=FGM-FG3M,FG2_PCT=round(FG2M/FG2A,3),.before=6) %>% 
    mutate(SEASON=season,LEAGUE="G-League",.before=2) %>% 
    arrange(PLAYER) %>% 
    return()
}

elc_scraper = function(season,league) {
  season3=as.numeric(substr(season,1,4))
  suppressWarnings(
    "https://feeds.incrowdsports.com/provider/euroleague-feeds/v3/competitions/{league}/statistics/players/traditional?seasonMode=Single&limit=10000&seasonCode={league}{season3}&statisticMode=accumulated" %>% 
      glue() %>% 
      GET() %>% 
      content(., "text") %>% 
      fromJSON() %>% 
      pluck(2) %>% 
      as_tibble() %>% 
      unnest() %>% 
      unnest() %>% 
      clean_names("all_caps") %>% 
      select(PLAYER=NAME,MIN=MINUTES_PLAYED,PTS=POINTS_SCORED,
             FG2A=TWO_POINTERS_ATTEMPTED,FG2M=TWO_POINTERS_MADE,FG2_PCT=TWO_POINTERS_PERCENTAGE,
             FG3A=THREE_POINTERS_ATTEMPTED,FG3M=THREE_POINTERS_MADE,FG3_PCT=THREE_POINTERS_PERCENTAGE,
             FTA=FREE_THROWS_ATTEMPTED,FTM=FREE_THROWS_MADE,FT_PCT=FREE_THROWS_PERCENTAGE,
             OREB=OFFENSIVE_REBOUNDS,DREB=DEFENSIVE_REBOUNDS,AST=ASSISTS,
             STL=STEALS,BLK=BLOCKS,TOV=TURNOVERS,PF=FOULS_COMMITED) %>% 
      mutate(MIN=round(as.numeric(MIN))) %>% 
      mutate(FGA=FG2A+FG3A,FGM=FG2M+FG3M,FG_PCT=round(FGM/FGA,3),.before=3) %>% 
      mutate_at(c(9,12,15), ~as.numeric(gsub("%","",.x))/100) %>% 
      mutate(SEASON=season,
             LEAGUE=if_else(league=="E","Euroleague","Eurocup"),.before=2) %>% 
      separate(PLAYER,c("LAST","FIRST"),sep=", ") %>% 
      mutate(PLAYER=paste0(str_to_title(FIRST)," ",str_to_title(LAST)),.before=1) %>% 
      select(-c(2,3)) %>% 
      return()
  )
}