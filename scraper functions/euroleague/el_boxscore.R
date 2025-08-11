###
### Scraper function example: Euroleague player boxscores
###

# Load libraries
pacman::p_load(tidyverse,glue,janitor,rvest,httr,jsonlite)

# Function
el_boxscore = function(type="euroleague",gamecode,season) {
  if (type=="euroleague") t="E"
  else t="U"
  
  # access the box score through the Euroleague API
  baseurl = "https://live.euroleague.net/api/Boxscore?gamecode={gamecode}&seasoncode={t}{season}" %>% 
    glue()
  
  if (content(GET(url = glue(baseurl)), "text") %>% 
      suppressMessages() != "") {
    
    # get information about the game (date and matchup)
    dateurl = "https://live.euroleague.net/api/Header?gamecode={gamecode}&seasoncode={t}{season}&disp="
    dateurl = GET(url = glue(dateurl)) %>% 
      suppressMessages()
    
    DATE = fromJSON(content(dateurl, "text")) %>% pluck("Date") ; rm(dateurl)
    
    baseurl = "https://live.euroleague.net/api/Boxscore?gamecode={gamecode}&seasoncode={t}{season}"
    
    myurl = GET(url = glue(baseurl)) %>% 
      suppressMessages()
    
    raw = fromJSON(content(myurl, "text"))
    
    M = fromJSON(content(myurl, "text")) %>% 
      pluck("Stats") %>% 
      as_tibble()
    MATCHUP = c(
      M[1,] %>% 
        pluck(3) %>% 
        pluck(1) %>% 
        as_tibble() %>% 
        pull(4) %>% 
        unique,
      M[2,] %>% 
        pluck(3) %>% 
        pluck(1) %>% 
        as_tibble() %>% 
        pull(4) %>% 
        unique
    ) %>% 
      paste(., collapse=" VS ") ; rm(M)
    
    # get the raw boxscore
    RAW_STATS = fromJSON(content(myurl, "text")) %>% 
      pluck("Stats") %>% 
      as_tibble()
    
    # combine game info with boxscores of each team and return a table
    if (!"" %in% RAW_STATS$Coach) {
      boxscore = bind_rows(as_tibble(RAW_STATS[1,]) %>% 
                             pull(PlayersStats) %>% 
                             pluck(1) %>% 
                             as_tibble() %>% 
                             clean_names("all_caps"),
                           as_tibble(RAW_STATS[2,]) %>% 
                             pull(PlayersStats) %>% 
                             pluck(1) %>% 
                             as_tibble() %>% 
                             clean_names("all_caps")) %>% 
        mutate(MATCHUP=paste0(DATE,", ",MATCHUP)) %>% 
        select(MATCHUP,PLAYER,TEAM,MIN=MINUTES,PTS=POINTS,
               `2PM`=FIELD_GOALS_MADE2,`2PA`=FIELD_GOALS_ATTEMPTED2,
               `3PM`=FIELD_GOALS_MADE3,`3PA`=FIELD_GOALS_ATTEMPTED3,
               `FTM`=FREE_THROWS_MADE,`FTA`=FREE_THROWS_ATTEMPTED,
               DREB=DEFENSIVE_REBOUNDS,OREB=OFFENSIVE_REBOUNDS,
               REB=TOTAL_REBOUNDS,AST=ASSISTANCES,
               STL=STEALS,BLK=BLOCKS_FAVOUR,TOV=TURNOVERS,
               PF=FOULS_COMMITED) %>% 
        separate(PLAYER, c("SUR","FIR"), sep=", ") %>% 
        unite(PLAYER,FIR:SUR, sep=" ") %>% 
        mutate(PLAYER=str_to_title(PLAYER))
      
      rm(baseurl,myurl,raw,RAW_STATS,MATCHUP)
    }
  }
  else {
    boxscore = tibble(
      MATCHUP=NA,PLAYER=NA,TEAM=NA,MIN=NA,PTS=NA,
      `2PM`=NA,`2PA`=NA,`3PM`=NA,`3PA`=NA,`FTM`=NA,`FTA`=NA,
      REB=NA,AST=NA,STL=NA,BLK=NA,TOV=NA
    )
  }
  return(boxscore)
}