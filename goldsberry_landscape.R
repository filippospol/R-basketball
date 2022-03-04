library(tidyverse)
library(hoopR)
library(showtext)
library(ggimage)
font_add_google("Sora", "myFont")
showtext_auto()

myDate = format(as.Date("2022/03/04"),"%m/%d/%Y")

dfTeams <- nba_leaguedashteamstats(
  season = "2021-22",
  date_to = myDate,
  season_type = "Regular Season",
  measure_type = "Advanced",
  last_n_games = 15
) %>% 
  pluck("LeagueDashTeamStats") %>% 
  select(TEAM_NAME,TEAM_ID,OFF_RATING,DEF_RATING,NET_RATING) %>% 
  mutate_at(-c(1,2),as.numeric)
dfTeams$LOGO <- NA
for (i in 1:30) {
  dfTeams$LOGO[i] <- gsub("XXX",dfTeams$TEAM_ID[i],
                          "https://cdn.nba.com/logos/nba/XXX/global/L/logo.svg")
} ; rm(i)

p <- dfTeams %>% 
  ggplot(aes(x=OFF_RATING,y=DEF_RATING)) +
  scale_y_reverse() +
  geom_image(aes(image=LOGO),size=0.07) +
  geom_vline(xintercept=mean(dfTeams$OFF_RATING),linetype=1,alpha=0.3) +
  geom_text(aes(label="Offensive Rating",x=min(DEF_RATING)+0.5,
                y=mean(DEF_RATING)-0.5,vjust="inward",hjust="inward"),
            family="myFont",color="black",size=8.5) +
  geom_hline(yintercept=mean(dfTeams$DEF_RATING),linetype=1,alpha=0.3) +
  geom_text(aes(label="Defensive Rating",x=mean(OFF_RATING)+0.1,
                y=min(DEF_RATING),vjust="inward",hjust="inward"),
            color="black",angle=90,size=8.5,family="myFont") +
  labs(title="The Efficiency Landscape",
       subtitle=paste0(myDate," | Last 15 Games Only\n")) +
  theme_void() +
  theme(text=element_text(family = "myFont"),
        plot.title = element_text(hjust=0.5, size=60,face="bold"),
        plot.subtitle = element_text(hjust=0.5, size=35,color="#666666"))
ggsave("p.png",p,width=7,height=5.5,dpi=300,bg="#ffffff")  
