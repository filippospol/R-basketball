# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script scrapes player/team statistics from the official Euroleague website
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

pacman::p_load(tidyverse,httr,jsonlite,janitor,glue)

el_leaguedashstats = function(player_or_team,season,measure,per_mode) {
  
  pacman::p_load(tidyverse,httr,jsonlite,janitor,glue)
  
  if (! player_or_team %in% c("players","teams")) {
    stop("valid options for player_or_team argument are 'players' and 'teams'.")
  }
  
  if (! measure %in% c("traditional","advanced")) {
    stop("valid options for measure argument are 'traditional' and 'advanced'.")
  }
  
  if (! per_mode %in% c("perGame","accumulated")) {
    stop("valid options for per_mode argument are 'perGame' and 'accumulated'.")
  }
  
  suppressWarnings(
    "https://feeds.incrowdsports.com/provider/euroleague-feeds/v3/competitions/E/statistics/{player_or_team}/{measure}?seasonMode=Single&limit=10000&seasonCode=E{season}&statisticMode={per_mode}" %>% 
      glue() %>% 
      GET() %>% 
      content(., "text") %>% 
      fromJSON() %>% 
      pluck(2) %>% 
      as_tibble() %>% 
      unnest() %>% 
      unnest() %>% 
      clean_names("all_caps") %>% 
      return()
  )
  
}