# setup
pacman::p_load(tidyverse,readxl,gt)
setwd("~/r-nba")

# load data
dbBackPicks =  read_csv("backpicks-el-regular.csv")

# select a season and generate table
s="2021-22"
dbBackPicks %>% 
  select(PLAYER,TEAM,SEASON,MIN,PTS_75=PTS,rTS,BOX_CREATION,OFF_LOAD,cTOV,GP) %>% 
  arrange(-OFF_LOAD) %>% 
  mutate(PTS_75=0.75*PTS_75) %>% 
  select(1:4,8,5,6,7,9,10) %>% 
  filter(MIN>=500 & SEASON==s) %>% 
  select(-c(SEASON,GP)) %>% 
  slice_head(n=20) %>% 
  mutate_at(4:8, ~round(.,1)) %>% 
  gt() %>% 
  cols_label(PLAYER="Player",TEAM="Team",PTS_75="Pts/75",BOX_CREATION="BoxC",
             OFF_LOAD="OLoad") %>%
  gt_color_rows(columns=c(4:8), domain=NULL,
                palette = "ggsci::light_green_material") %>%  
  tab_header(title=md(paste("**Euroleague Backpicks Metrics:",s,"Regular Season**")),
             subtitle=html("<div style='color:#8e8e8e;'>Table by: Filippos Polyzos | Source: hackastat.eu</div><br/>")) %>% 
  tab_options(data_row.padding = px(1),
              heading.align = "left",
              heading.title.font.size = 30,
              heading.subtitle.font.size = 20,
              table.font.size = 15) %>% 
  opt_table_font(font = google_font("Roboto"))  
# gtsave("backpicks-el.png")
