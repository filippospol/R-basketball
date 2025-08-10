pacman::p_load(tidyverse,cluster)
setwd("C:/Users/phili/Documents/shiny-rpubs/player-similarity")
source("import-data.R")
beepr::beep(10)

# Data on per 36 minute format:
rawData = read_csv("data-no-similarity.csv") %>% 
  mutate_at(c(5:9,12,13,15,16,18:24), ~round((./MIN)*36,1)) %>% 
  filter(MIN>=100) %>% 
  mutate(row_num = row_number())

# function that returns the 10 most similar entities from the database:
playerSimilarity = function(player,season,league,same=TRUE) {
  # Calculate similarity = Gower distance:
  gowerPlayers = daisy(rawData[,-c(1:4)],metric="gower")
  
  # locate player on database:
  index = rawData %>% 
    filter(PLAYER==player&SEASON==season&LEAGUE==league) %>% 
    pull(row_num)
  
  # Get similar players
  playersDist = as.matrix(gowerPlayers)[index,]
  
  # player order similarity vector:
  similarPlayers = order(playersDist)
  
  # display the table:
  bind_cols(rawData,tibble(DIST=playersDist)) %>% 
    mutate(REB=OREB+DREB) %>% 
    select(PLAYER,SEASON,LEAGUE,`PTS/36`=PTS,`REB/36`=REB,
           `AST/36`=AST,`FGA/36`=FGA,`3PA/36`=FG3A,`FTA/36`=FTA,FG_PCT,FG3_PCT,FT_PCT) %>% 
    slice(similarPlayers) %>%
    filter(!(PLAYER==player&SEASON==season&LEAGUE==league)) %>%
    {
      if (same==TRUE) {
        filter(.,PLAYER!=player)
      }else {
        filter(.,PLAYER!=player&LEAGUE!=league)
      }
    } %>% 
    head(10) %>% 
    return()
}
# Mario Saint-Supery | Jean Montero | Nadir Hifi
playerSimilarity("Jean Montero","2024-25","Eurocup",same=F)
