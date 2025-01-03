pacman::p_load(tidyverse,rvest,glue,janitor,lubridate)

bbref_game_log = function(gameID) {
  # HTML that contains the boxscore:
  baseHTML = "https://www.basketball-reference.com/boxscores/{gameID}.html" %>% 
    glue() %>% 
    read_html()
  
  # matchup info:
  matchup_date = baseHTML %>% 
    html_nodes("h1") %>% 
    html_text() %>% 
    strsplit(split=" Box Score, ")
  
  # matchup:
  matchup = matchup_date %>%
    pluck(1) %>%
    pluck(1) %>% 
    gsub("at","@",.)
  
  # date:
  date = matchup_date %>%
    pluck(1) %>%
    pluck(2) %>% 
    mdy()
  rm(matchup_date)
  
  # Inactive players full info:
  rawInactive = baseHTML %>% 
    html_nodes('div:not([id]):not([class])') %>% 
    html_text() %>% 
    str_subset("Inactive") %>% 
    pluck(2) %>% 
    str_replace(.,"Inactive","") %>% 
    strsplit(., ':', fixed = TRUE) %>% 
    pluck(1) %>% 
    tail(1) %>% 
    str_split(.,"\\s{2,}") %>% 
    pluck(1) %>% 
    head(2)
  
  # Team acronym and inactive players:
  awayTeam = rawInactive[[1]] %>% 
    str_trim() %>% 
    substr(1,3)
  awayInactive = rawInactive[[1]] %>% 
    str_trim() %>% 
    str_sub(5) %>% 
    strsplit(",") %>% 
    lapply(str_trim) %>% 
    pluck(1)
  
  homeTeam = rawInactive[[2]] %>% 
    str_trim() %>% 
    substr(1,3)
  homeInactive = rawInactive[[2]] %>% 
    str_trim() %>% 
    str_sub(5) %>% 
    strsplit(",") %>% 
    lapply(str_trim) %>% 
    pluck(1)
  
  # Get all tables from the page:
  gameTables = baseHTML %>% 
    html_nodes("table") %>% 
    html_table()
  
  # Get away and home boxscores and add the additional info:
  rawBox = bind_rows(
    gameTables %>% 
      pluck(8) %>% 
      row_to_names(1) %>% 
      clean_names("all_caps") %>% 
      filter(STARTERS != "Reserves") %>% 
      rename(PLAYER_NAME=STARTERS) %>% 
      add_row(PLAYER_NAME=awayInactive,.before=dim(.)[1]) %>% 
      mutate(across(everything(), ~replace_na(.x, "Inactive"))) %>%
      mutate(IS_STARTER=c(rep(T,5),rep(F,dim(.)[1]-5)),.before=MP) %>%
      mutate(GAME_ID=gameID,MATCHUP=matchup,DATE=date,TEAM=awayTeam,
             .before=1),
    gameTables %>% 
      pluck(16) %>% 
      row_to_names(1) %>% 
      clean_names("all_caps") %>% 
      filter(STARTERS != "Reserves") %>% 
      rename(PLAYER_NAME=STARTERS) %>% 
      add_row(PLAYER_NAME=homeInactive,.before=dim(.)[1]) %>% 
      mutate(across(everything(), ~replace_na(.x, "Inactive"))) %>%
      mutate(IS_STARTER=c(rep(T,5),rep(F,dim(.)[1]-5)),.before=MP) %>%
      mutate(GAME_ID=gameID,MATCHUP=matchup,DATE=date,TEAM=homeTeam,
             .before=1)
  ) ; rm(list=setdiff(ls(),"rawBox"))
  
  # Clean data and print table:
  rawBox %>% 
    filter(PLAYER_NAME != "Team Totals") %>% 
    mutate(IS_STARTER=ifelse(grepl("[0-9]",MP),IS_STARTER,NA)) %>% 
    mutate(STATUS=ifelse(grepl("[0-9]",MP),"Active",MP),.before=MP) %>% 
    mutate(across(MP:BPM,~ifelse(grepl("[0-9]",MP),.,NA))) %>% 
    separate(MP,c("M","S"),sep=":") %>% 
    mutate(MP=round(as.numeric(M)+(as.numeric(S)/60),1),.after=STATUS) %>% 
    select(-c(M,S)) %>% 
    mutate_at(9:23, as.numeric) %>% 
    return()
}
