# load libraries: ----
library(tidyverse)
library(ggfittext)
library(hoopR)
library(showtext)
font_add_google("Roboto", "Roboto")
showtext_auto()

# load players box scores: ----
dfBox <- load_nba_player_box(seasons=2022)

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
  summarise(minPerGame=round(mean(min),1)) %>% 
  arrange(-minPerGame)
teamBox <- teamBox %>% 
  mutate(player=factor(player, levels = playerOrder$player))
teamBox$game_number <- teamBox %>% group_indices(game_id)

# filter last N games : ----
teamBoxFilter <- teamBox %>%
  filter(game_number %in% (unique(teamBox$game_number) %>% tail(10)) & 
         min !=0) %>% 
  mutate(player=factor(player))

# clear environment: ----
rm(dfBox,playerOrder)
  
# create heatmap chart: ----
p <- teamBoxFilter %>%
  ggplot(aes(x=as.factor(game_number), fill=as.integer(min), 
             y=player)) +
  geom_tile() +
  scale_y_discrete(limits = rev(levels(teamBoxFilter$player))) +
  scale_fill_gradient(low="#e0efef",high="#006666", guide="colorbar") +
  geom_text(aes(label = min),fontface="bold") +
  theme_minimal() +
  labs(title=paste(myTeam,": Minutes distribution",sep=""),
       subtitle=paste("As of ",Sys.Date(),". Data by {hoopR} | @filippos_pol",sep=""),
       y="", x="GAME",fill="Minutes") +
  theme(text = element_text(family="Roboto"),
        plot.title = element_text(hjust=0.5, size=38,face="bold"),
        plot.subtitle = element_text(hjust=0.5, size=30),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title = element_text(size=30),
        axis.text = element_text(size=25),
        legend.position='none')

# (optional) save chart as .png file: ----
ggsave("heatmap.png",p,width=11,height=4.5,bg="#ffffff")
