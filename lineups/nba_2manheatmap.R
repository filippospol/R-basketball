# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script returns a heatmap of 2-man combinations for a given season-team combination.
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

nba_2manheatmap = function(season,team) {
  pacman::p_load(tidyverse,hoopR,glue,janitor)
  
  # Get team ID:
  teams = nba_leaguedashteamstats(season=season) %>% 
    pluck(1) %>% select(TEAM_ID,TEAM=TEAM_NAME)
  teamID = teams %>% filter(TEAM==team) %>% pull(TEAM_ID)
  rm(teams)
  
  # Get net rating stats:
  twoman = suppressWarnings(
    nba_leaguedashlineups(season=season,measure_type="Advanced",team_id=teamID,
                          season_type="Regular Season",group_quantity=2) %>% 
      pluck(1) %>% 
      select(3,10,16,29) %>% 
      separate(GROUP_NAME,into=c("PLAYER_1","PLAYER_2"),sep=" - ") %>% 
      mutate_at(3:5, as.numeric) %>% 
      filter(MIN>=100)
  )
  
  # List of players:
  playersList = c(twoman$PLAYER_1,twoman$PLAYER_2)
  
  # Tibble with players combos and net ratings:
  playersExpanded = tibble(X1=sort(unique(playersList)),
                           X2=sort(unique(playersList))) %>% 
    expand(X1,X2) %>% 
    left_join(twoman[,-c(3,5)], by=c("X1"="PLAYER_1","X2"="PLAYER_2"))
  
  # Convert tibble to symmetric matrix:
  playersMatrix = matrix(data=NA,nrow=length(unique(playersExpanded$X1)),
                         ncol=length(unique(playersExpanded$X2)))
  colnames(playersMatrix) = sort(unique(playersExpanded$X1))
  rownames(playersMatrix) = sort(unique(playersExpanded$X1))
  for (i in 1:nrow(playersMatrix)) {
    for (j in 1:ncol(playersMatrix)) {
      playersMatrix[i,j] = playersExpanded %>% filter(X1==rownames(playersMatrix)[i] & 
                                                        X2==colnames(playersMatrix)[j]) %>% 
        pull(NET_RATING)
    }
  }
  for (i in 1:nrow(playersMatrix)) {
    for (j in 1:ncol(playersMatrix)) {
      if (is.na(playersMatrix[i,j])) {
        playersMatrix[i,j] = playersMatrix[j,i]
      }
    }
  }
  
  # Clear environment:
  rm(list=setdiff(ls(),c("team","season","playersExpanded","playersMatrix")))
  
  # Generate chart:
  playersMatrix %>% 
    as.data.frame() %>%
    rownames_to_column("P1") %>% 
    pivot_longer(-c(P1), names_to = "P2", values_to = "NET") %>% 
    ggplot(aes(x=factor(P1,levels=sort(unique(playersExpanded$X1))),
               y=factor(P2,levels=sort(unique(playersExpanded$X2),decreasing=T)),
               fill=NET, label=NET)) + 
    geom_raster() +
    scale_x_discrete(position = "top") +
    scale_fill_gradient2(low = "#ff6f69",mid="white", high = "#88d8b0", na.value = "transparent") +
    geom_text(size=3,fontface="bold") +
    labs(title=paste(season,team,"2-Man Net Rating Combos"),
         subtitle=paste0("At least 100 minutes per 2-man combo | Source: NBA Stats"), x="", y="",fill="") +
    theme(legend.position="none",
          axis.text.x = element_text(angle = 45,hjust=-0.25))
}