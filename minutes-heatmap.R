# load libraries: ----
pacman::p_load(tidyverse,hoopR)

# load players box scores: ----
dfBox <- load_nba_player_box(seasons=2022) %>% 
  filter(season_type==2) # 3=playoffs

# select team, by abbreviation:
unique(dfBox$team_abbreviation)
myAbb <- unique(dfBox$team_abbreviation)[23]
myTeam <- unique(dfBox$team_name)[23]

# select boxscores for desired team: ----
teamBox <- dfBox %>% 
  filter(team_abbreviation == myAbb) %>% 
  select(athlete_display_name,team_name,min,game_id,game_date) %>% 
  rename(player=athlete_display_name)

# create an order for players, based on minutes per game: ----
playerOrder <- teamBox %>% 
  mutate(min=as.numeric(min)) %>% 
  group_by(player) %>%
  summarise(totMin=sum(min),minPerGame=round(mean(min),1),Games=n()) %>% 
  arrange(desc(totMin)) %>% 
  select(-2)
teamBox <- teamBox %>% 
  mutate(player=factor(player, levels = playerOrder$player))
teamBox$game_number <- teamBox %>% group_indices(game_id)

# filter last N games : ----
n_games = 20
teamBoxFilter <- teamBox %>%
  filter(game_number %in% (unique(teamBox$game_number) %>% tail(n_games)) & 
           min !=0) %>% 
  mutate(player=factor(player))

# create heatmap chart: ----
teamBoxFilter %>%
  ggplot(aes(x=as.factor(game_number), fill=as.integer(min), 
             y=player)) +
  geom_tile() +
  scale_y_discrete(limits = rev(levels(teamBoxFilter$player))) +
  scale_fill_gradient(low="#e0efef",high="#006666", guide="colorbar") +
  geom_text(aes(label = min),fontface="bold") +
  labs(title=paste(myTeam,": Minutes distribution",sep=""),
       subtitle=paste("Last ",n_games," games | Source: stats.NBA.com | Plot by@filippos_pol",sep=""),
       y="", x="Game #",fill="Minutes")
