#' This @script imports the data and performs some cleaning (unique names).
#' It is the first step before calculating the player similarity.

pacman::p_load(tidyverse,stringdist)
setwd("C:/Users/phili/Documents/shiny-rpubs/player-similarity")
source("scraper-functions.R")
seasons = c(year_to_season(2020:2024))
nba = map_df(seasons,nba_scraper)
el = map_df(seasons,~elc_scraper(.x,"E"))
ec = map_df(seasons,~elc_scraper(.x,"U"))

df = bind_rows(nba,el,ec)

# Clean player names: remove white space and convert to title case:
df = df %>% 
  mutate(PLAYER = str_to_title(PLAYER)) ; rm(list=setdiff(ls(),"df"))

# Calculate Levenstein distance for all player name instances:
name_dist = stringdistmatrix(df$PLAYER, df$PLAYER, method = "lv")

# Create a cluster variable for similar name entities:
hc = hclust(as.dist(name_dist))
clusters = cutree(hc, h = 0.1)

# Assign the optimal name for each cluster:
df = df %>% 
  mutate(CLUSTER=clusters)

# Select the most common instance for each player entity:
optimal_names = df %>%
  group_by(CLUSTER) %>%
  summarise(FINAL_NAME = names(sort(table(PLAYER), decreasing = TRUE))[1]) %>% 
  mutate(FINAL_NAME=gsub(" Ii"," II",FINAL_NAME),
         FINAL_NAME=gsub(" Iii"," III",FINAL_NAME),
         FINAL_NAME=gsub(" IIi"," III",FINAL_NAME),
         FINAL_NAME=gsub(" Iv"," IV",FINAL_NAME))

# Embed optimal names in database and store as .csv file:
left_join(df, optimal_names, by = "CLUSTER") %>% 
  select(-c(1,25)) %>% 
  select(PLAYER=FINAL_NAME,1:23) %>% 
  write_csv("data-no-similarity.csv")
rm(list=ls())