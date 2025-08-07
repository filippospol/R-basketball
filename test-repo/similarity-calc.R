pacman::p_load(tidyverse,cluster)
setwd("C:/Users/phili/Documents/shiny-rpubs/player-similarity")
# source("import-data.R")

# Data on per 36 minute format:
rawData = read_csv("data-no-similarity.csv") %>% 
  mutate_at(c(5:9,12,13,15:24), ~round((./MIN)*36,1)) %>% 
  filter(MIN>=100) %>% 
  mutate(row_num = row_number())

# Speficy player/season/league combination:
player = "Nadir Hifi" 
season = "2024-25"
league = "Euroleague"

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
  select(PLAYER,SEASON,LEAGUE,DIST) %>% 
  slice(similarPlayers) %>%
  filter(!(PLAYER==player&SEASON==season&LEAGUE==league)) %>% 
  filter(PLAYER!=player) %>% 
  head(10)
