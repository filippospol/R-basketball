pacman::p_load(tidyverse,rvest,janitor,glue)

# Enter season:
season = 2025

# Get players ID codes from the Per Game stats table:
playersID = "https://www.basketball-reference.com/leagues/NBA_{season}_advanced.html" %>% 
  glue() %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_nodes("a") %>% 
  html_attr("href") %>% 
  str_subset(pattern="/players/") %>% 
  unique() %>% 
  sort() %>% 
  gsub(".html","",.)

gamelogs = list() ; k=0
for (i in 1:25) {             # change 25 to length(playersID)
  
  # control scraper traffic on Basketball Reference (max calls per minute = 20)
  k = k+1
  if (k %in% c(seq(20,660,20) + 1)) Sys.sleep(60)
  
  # URL of game logs:
  playerURL = "https://www.basketball-reference.com{playersID[i]}/gamelog/{season}" %>% 
    glue() %>% 
    read_html()
  
  # Extract player name:
  playerName = playerURL %>% 
    html_nodes("h1") %>% 
    html_text() %>% 
    substring(4) %>% 
    gsub(paste0(as.numeric(season)-1,"-",substring(season,3)," Game Log\n\t\t\t"),"",.) %>% 
    str_trim()
  
  # Get game logs:
  gamelogs[[i]] = playerURL %>% 
    html_elements("table") %>% 
    html_table() %>% 
    pluck(8) %>% 
    clean_names("all_caps") %>% 
    filter(DATE !="Date") %>% 
    mutate(PLAYER_NAME=playerName,PLAYER_BBREF_ID=gsub("/players/","",playersID[i]),
           SEASON=paste0(as.numeric(season)-1,"-",substring(season,3)), .before=1)
  
  print(i)
  
}

# rm(playerURL,playerName,i,k,season)

gamelogsAll = bind_rows(gamelogs)