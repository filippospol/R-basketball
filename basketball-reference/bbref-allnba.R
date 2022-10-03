pacman::p_load(tidyverse,rvest,ggtext,janitor)

all_nba <- "https://www.basketball-reference.com/awards/all_league.html" %>% 
  read_html() %>% 
  html_elements("table") %>% 
  html_table() %>% 
  pluck(1) %>% 
  clean_names("all_caps") %>% 
  filter(LG == "NBA") %>% 
  mutate_at(5:9, ~substr(.,1,nchar(.)-2)) %>%
  mutate(SEASON=paste0(20,str_sub(SEASON,start=-2))) %>% 
  select(SEASON,ALL_NBA_TEAM=TM,X:X_5) %>% 
  slice_head(n=66)

sl = sort(unique(all_nba$SEASON))
pl = lapply(sl, function(x) {
  "https://www.basketball-reference.com/leagues/NBA_xxx_per_game.html" %>% 
    gsub("xxx",x,.) %>% 
    read_html() %>% 
    html_elements("table") %>% 
    html_table() %>% 
    pluck(1) %>% 
    clean_names("all_caps") %>%
    select_if(~ !any(is.na(.))) %>% 
    mutate(G=as.numeric(G),SEASON=x) %>% 
    group_by(PLAYER) %>% 
    filter(G==max(G),PLAYER!="Player") %>% 
    arrange(PLAYER) %>% 
    ungroup() %>% 
    mutate(G=as.numeric(G)) %>% 
    group_by(PLAYER) %>% 
    filter(G==max(G),PLAYER!="Player") %>% 
    arrange(PLAYER) %>% 
    ungroup() %>% 
    select(SEASON,PLAYER,TEAM=TM) 
    # mutate(PLAYER=substr(PLAYER,1,nchar(PLAYER)-1))
}) %>% 
  bind_rows()

# Billups 2009=DEVNER
groupDB = all_nba %>% 
  reshape2::melt(id=c("SEASON","ALL_NBA_TEAM")) %>% 
  tibble() %>% 
  select(SEASON,ALL_NBA_TEAM,PLAYER=value) %>% 
  left_join(pl %>% mutate(PLAYER=gsub("[*]","",PLAYER)), by=c("SEASON","PLAYER")) %>%
  mutate(CONF = if_else(TEAM %in% c(sort(unique(pl$TEAM))[c(1:8,11,14,18,19,21,
                                                            25,27:28,34,38)]),
                        "East","West"),
         TEAM = if_else(TEAM=="TOT","DEN",TEAM)) 
plotDB = groupDB %>% 
  group_by(SEASON) %>% 
  summarise(nE = sum(CONF=="East"),
            nW = sum(CONF=="West")) ; rm(all_nba,pl,sl)

theme_set(theme_minimal() + 
            theme(axis.title=element_blank(),
                  axis.text.y=element_text(size=14),
                  axis.text.x=element_text(size=10),
                  plot.title=element_markdown()) )

plotDB %>% 
  ggplot(aes(x=SEASON,y=nW,group=1)) +
  scale_y_continuous(limits=c(2,12),breaks=seq(2,12,1)) +
  geom_line(col="#C9082A",size=1) +
  geom_point(col="#C9082A",size=2,shape=15) +
  geom_line(y=plotDB$nE,group=1,col="#17408B",size=1) +
  geom_point(y=plotDB$nE,col="#17408B",size=2,shape=15) +
  labs(title="**All-NBA team selections:  <span style='color:#17408B;'>East</span> vs  <span style='color:#C9082A;'>West</span>**",
       subtitle="Since 2000-01 season | Source: Basketball Reference | Plot: Filippos Polyzos")
  