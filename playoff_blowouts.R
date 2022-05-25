# load packages: ----
pacman::p_load(tidyverse,rvest,janitor,ggrepel)

# get data form bbref: ----
s <- 1984:2022
out <- list()
for (i in 1:length(s)) {
  out[[i]] <- gsub("XXX",s[i], "https://www.basketball-reference.com/playoffs/NBA_XXX_games.html") %>% 
    read_html() %>% 
    html_elements("table") %>% 
    html_table() %>% 
    pluck(1) %>% 
    clean_names(.,"all_caps") %>% 
    filter(DATE!="Date") %>% 
    mutate(MATCHUP=paste0(VISITOR_NEUTRAL,"-",HOME_NEUTRAL),
           MOV=as.numeric(PTS)-as.numeric(PTS_2),
           YR=s[i]) %>% 
    select(YR,MATCHUP,MOV) %>% 
    filter(!is.na(MOV))
}

# get MOV descriptive stats for each season: ----
db <- bind_rows(out) %>% 
  group_by(YR) %>% 
  summarise(GAMES=n(),AVG=mean(abs(MOV)), STD=sd(abs(MOV)),
           BLOWOUTS = sum(abs(MOV)>=20)) %>% 
  mutate(BLOW_PCT=round(BLOWOUTS/GAMES,3))
rm(out,i,s)

# create plots: ----
db %>% 
  ggplot(aes(x=AVG,y=STD,label=YR)) +
  geom_label(size=6.5) +
  geom_label(data=db %>% filter(YR==2022),size=6.5,fill="#1ddce2") +
  labs(x="Average",y="Standard Deviation",
       title="Margin of victory in the playoffs",
       subtitle="Data from 1984 playoffs onwards | Source: Basketball Reference | Plot by: @filippos_pol")

db %>% 
  ggplot(aes(x=YR,y=BLOW_PCT)) +
  geom_col(fill="#eeeeee",color="#000000") +
  geom_col(data=db %>% filter(YR==2022),fill="#1ddce2",color="#000000") +
  scale_y_continuous(labels=scales::percent) +
  labs(x="Season",y="% of games that were blow-outs",
       title="Blow-out games (MOV \u2265 20) in the playoffs",
       subtitle="Data from 1984 playoffs onwards | Source: Basketball Reference | Plot by: @filippos_pol",
       caption=paste("As of",format(Sys.Date(),"%m/%d/%Y")))