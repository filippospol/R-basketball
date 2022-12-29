# load libraries: ----
pacman::p_load(tidyverse,hoopR,ggtext,sysfonts,showtext)

# load players box scores: ----
dfBox <- load_nba_player_box(seasons=2023) %>% 
  filter(season_type==2) # 3=playoffs

# select team, by abbreviation:
ABBS = dfBox$team_abbreviation %>% unique() %>% sort()
myAbb = ABBS[18]
myTeam = ABBS[18]
MyID = nba_leaguedashplayerstats(season="2022-23") %>%
  pluck(1) %>% 
  filter(TEAM_ABBREVIATION==myAbb) %>% 
  pull(TEAM_ID) %>% 
  unique()
season=unique(dfBox$season)

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
n_games = max(teamBox$game_number)
teamBoxFilter <- teamBox %>%
  filter(game_number %in% (unique(teamBox$game_number) %>% tail(n_games)) & 
           min !=0) %>% 
  mutate(player=factor(player))

# clear environment: ----
rm(dfBox,playerOrder,teamBox,ABBS,MyID,myTeam,n_games)

# create heatmap chart: ----
# change text sizes, fonts etc
teamBoxFilter %>%
  ggplot(aes(x=as.factor(game_number), fill=as.integer(min), 
             y=player)) +
  geom_tile() +
  scale_y_discrete(limits = rev(levels(teamBoxFilter$player))) +
  scale_fill_gradient(low="#fff6e5",high="#ffa500", guide="colorbar") +
  geom_text(aes(label=min)) +
  labs(title=paste("<img src='",sub("XXX",myAbb,"https://raw.githubusercontent.com/mvpstax/NBA-logos/master/XXX-68x72-6x.png"),
                   "' width='70'></img>",sep=""),
       subtitle=paste("Minutes distribution,",paste0(season-1,"-",str_sub(season,-2, -1)),
                      "| Source: ESPN",sep=" "),
       caption="Chart by: @filippos_pol",
       y="", x="Game #",fill="Minutes") +
  theme(plot.title=element_markdown(face="bold",hjust=0.5),
        plot.subtitle=element_markdown(color="#777777",hjust=0.5))
