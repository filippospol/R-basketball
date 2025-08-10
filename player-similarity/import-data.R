#' This @script imports the data and performs some cleaning (unique names).
#' It is the first step before calculating the player similarity.

# Setup environment:
pacman::p_load(tidyverse,stringdist)
# setwd("C:/Users/phili/Documents/shiny-rpubs/player-similarity")
source("scraper-functions.R")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Season of vectors:
seasons = c(year_to_season(2022:2024))

# Import data per league for given seasons:
nba = map_df(seasons,nba_scraper)
gleague = map_df(seasons,gleague_scraper)
el = map_df(seasons,~elc_scraper(.x,"E"))
ec = map_df(seasons,~elc_scraper(.x,"U"))
bcl = map_df(seasons,bcl_scraper)
europecup = map_df(seasons,europecup_scraper)
nblaus = map_df(seasons,nblaus_scraper)
beepr::beep(10)

# Merge all in a tibble:
df = bind_rows(Filter(function(x) is(x, "tbl_df"), mget(ls())))
rm(list=setdiff(ls(),"df"))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#'  ++++++ CLEAN NAMES
#'  Keep unique values for PLAYER column for each player (e.g. Marcus Morris vs Marcus Morris Sr.)
# Remove white space and convert to title case from names:
# df = df %>% mutate(PLAYER = str_to_title(PLAYER))

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
  summarise(FINAL_NAME = names(sort(table(PLAYER), decreasing = TRUE))[1]) # %>% 
  # mutate(FINAL_NAME=gsub(" Ii"," II",FINAL_NAME),
  #        FINAL_NAME=gsub(" Iii"," III",FINAL_NAME),
  #        FINAL_NAME=gsub(" IIi"," III",FINAL_NAME),
  #        FINAL_NAME=gsub(" Iv"," IV",FINAL_NAME))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Embed optimal names in database and store as .csv file:
left_join(df, optimal_names, by = "CLUSTER") %>% 
  select(-c(1,25)) %>% 
  select(PLAYER=FINAL_NAME,1:23) %>% 
  write_csv("data-no-similarity.csv")
rm(list=ls())
