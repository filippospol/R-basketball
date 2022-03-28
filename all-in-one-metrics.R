### Load packages -----

library(tidyverse)
library(rvest)
library(janitor)
library(stringi)
setwd("~/R NBA Projects")

#-----------------------------------------------------------------------------------------------------------------------------

### Get data: -----

##### LEBRON
bball_index <- "https://www.bball-index.com/2021-22-lebron-data/" %>% 
  read_html() %>% 
  html_elements("table") %>% 
  html_table() %>% 
  pluck(1) %>% 
  select(c(2,4,7,11:13)) %>% 
  filter(Player!="") %>% 
  mutate_at(3:6,as.numeric)
bball_index$Player <- gsub('\\.','',
                     gsub(' Sr','',bball_index$Player))
bball_index <- bball_index %>% 
  arrange(Player)

##### BPM, WS/48, On/Off NetRtg, TS%, USG%
b1 <- "https://www.basketball-reference.com/leagues/NBA_2022_advanced.html" %>% 
  read_html() %>% 
  html_elements("table") %>% 
  html_table() %>% 
  pluck(1) %>% 
  select(c(2,8,9,19,24,28,26,27)) %>% 
  distinct(Player,.keep_all = T) %>% 
  filter(Player!="Player") %>% 
  mutate_at(2:8, as.numeric) %>% 
  arrange(Player)
  
b2 <- "https://www.basketball-reference.com/leagues/NBA_2022_play-by-play.html" %>% 
  read_html() %>% 
  html_elements("table") %>% 
  html_table() %>% 
  pluck(1) %>% 
  row_to_names(row_number = 1) %>% 
  select(c(2,7,14)) %>% 
  distinct(Player,.keep_all = T) %>% 
  filter(Player!="Player") %>% 
  mutate_at(2:3, as.numeric) %>% 
  arrange(Player)

### Basketball Reference, Per 75 Poss stats:
b3 <- "https://www.basketball-reference.com/leagues/NBA_2022_per_poss.html" %>% 
  read_html() %>% 
  html_elements("table") %>% 
  html_table() %>% 
  pluck(1) %>% 
  select(2,29,23,24,25,26,10,19,16,13) %>% 
  distinct(Player,.keep_all = T) %>% 
  filter(Player!="Player") %>%
  mutate_at(-1, as.numeric) %>% 
  mutate_at(-1,.funs = funs(. * 0.75)) %>% 
  arrange(Player)

bbref <- tibble(
  Reduce(function(x, y) merge(x, y, by='Player'),
         list(b1,b2,b3))) %>% 
  mutate(Player = stri_trans_general(c(Player),"latin-ascii"))
bbref$Player <- gsub('\\.','',
                     gsub(' Sr','',bbref$Player))
bbref <- bbref %>% arrange(Player) ; rm(b1,b2,b3)

##### RPM
out <- list()
out[[1]] <- "http://www.espn.com/nba/statistics/rpm" %>% 
  read_html() %>% 
  html_elements("table") %>% 
  html_table() %>% 
  pluck(1) %>% 
  row_to_names(row_number = 1) %>% 
  select(c(2,8,6,7))

for (i in 2:15) {
  out[[i]] <- paste0("http://www.espn.com/nba/statistics/rpm/_/page/",i) %>%
    read_html() %>% 
    html_elements("table") %>% 
    html_table() %>% 
    pluck(1) %>% 
    row_to_names(row_number = 1)  %>% 
    select(c(2,8,6,7))
}
espn <- do.call("rbind",out) %>% 
  rename(Player=NAME) %>% 
  mutate(Player = gsub("(.*),.*", "\\1", Player)) %>% 
  mutate_at(2:4, as.numeric) %>% 
  arrange(Player)
espn$Player <- gsub('\\.','',
                     gsub(' Sr','',espn$Player))
espn <- espn %>% arrange(Player)

##### RAPM
# update csv file from: 
# http://nbashotcharts.com/rapm
rapm <- read.csv("rapm.csv",header=F) %>% 
  row_to_names(row_number=2) %>% 
  tibble() %>% 
  select(2,33,37,35,3,7,5) %>% 
  mutate_at(-1, as.numeric) %>% 
  rename(Player=playerName,ORAPM=RAPM__Off,DRAPM=RAPM__Def,
         LA_ORAPM=LA_RAPM__Off,LA_DRAPM=LA_RAPM__Def) %>% 
  arrange(Player)
rapm$Player <- gsub('\\.','',
                           gsub(' Sr','',rapm$Player))
rapm <- rapm %>% 
  arrange(Player)

##### RAPTOR, PREDATOR
# update csv file from:
# https://projects.fivethirtyeight.com/nba-model/2022/latest_RAPTOR_by_player.csv
fivethirtyeight <- read.csv("raptor.csv") %>% 
  tibble() %>% 
  select(c(1,14,12,13,20,18,19)) %>% 
  rename(Player=player_name,RAPTOR=raptor_total,ORAPTOR=raptor_offense,
         DRAPTOR=raptor_defense,PREDATOR=predator_total,
         OPREDATOR=predator_offense,DPREDATOR=predator_defense) %>% 
  arrange(Player)
fivethirtyeight$Player <- gsub('\\.','',
                           gsub(' Sr','',fivethirtyeight$Player))
fivethirtyeight <- fivethirtyeight %>% 
  arrange(Player)

##### WPA
out = list()
for(i in 1:14) {
  out[[i]] <- paste0("http://stats.inpredictable.com/nba/ssnPlayer.php?season=2021&team=ALL&pos=ALL&po=0&frdt=2021-10-19&todt=2022-03-15&rate=tot&sort=sWPA&order=DESC&grp=",i) %>%
    read_html() %>% 
    html_elements("table") %>% 
    html_table() %>% 
    pluck(1) %>% 
    row_to_names(row_number = 1) %>% 
    select(Player,WPA) %>% 
    mutate(WPA=as.numeric(WPA))
}
inpredictable <- do.call("rbind",out) %>% 
  arrange(Player)
rm(i,out)
inpredictable$Player <- gsub('\\.','',
                           gsub(' Sr','',inpredictable$Player))
inpredictable <- inpredictable %>% 
  arrange(Player)

allinone <- tibble(
  Reduce(function(x, y) left_join(x, y, by='Player'),
         list(bbref,bball_index,espn,fivethirtyeight,
              inpredictable,rapm)))

input <- allinone[,c(1,20:21,9,11:19,3:4,2,5:6,10,22,25,28,31,34:35)] %>% 
  filter(MP>=100) %>% 
  clean_names()
input[,c(5:13,16:25)] <- round(input[,c(5:13,16:25)],1)
input <- input %>% 
  mutate(per_rank=rank(-per,ties.method="first"),
         ws_48_rank=rank(-ws_48,ties.method="first"),
         bpm_rank=rank(-bpm,ties.method="first"),
         on_off_rank=rank(-on_off,ties.method="first"),
         lebron_rank=rank(-lebron,ties.method="first"),
         rpm_rank=rank(-rpm,ties.method="first"),
         raptor_rank=rank(-raptor,ties.method="first"),
         predator_rank=rank(-predator,ties.method="first"),
         wpa_rank=rank(-wpa,ties.method="first"),
         rapm_rank=rank(-rapm,ties.method="first"))
input$avg_rank <- rowMeans(input[,c(26:35)],na.rm = TRUE)

input <- input %>% 
  arrange(desc(-avg_rank))

rm(list=setdiff(ls(), "input")) ; beepr::beep(2)

#-----------------------------------------------------------------------------------------------------------------------------

### Create tables:  -----
library(gt)

t1 <- input %>% 
  select(c(1:3,5:9,14)) %>% 
  slice(1:10) %>% 
  gt() %>% 
  cols_align(align = "center") %>% 
  cols_label("player"="Player", "team"="Team", "games"="Games",
             "pts"="Points", "trb"="Rebounds", "ast"="Assists",
             "stl"="Steals", "blk"="Blocks", "ts_percent"="TS%") %>% 
  tab_header(title=md("**MVP Ladder: Per 75 Possessions Stats**"),
             subtitle=html("<div style='color:#8e8e8e;'>Top-10 players based on all-in-one metrics rank</div><br/>")) %>% 
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything())) %>% 
  fmt_percent(columns = ts_percent, decimals = 1) %>% 
  tab_source_note(source_note = paste0("Source: Basketball Reference | ",
                                       format(Sys.time(),"%B %d,%Y"))) %>% 
  tab_options(data_row.padding = px(1),
              row.striping.background_color = "#eeeeee",
              heading.align = "left",
              heading.title.font.size = 40,
              heading.subtitle.font.size = 20,
              table.font.size = 15,
              table.border.top.color = "transparent",
              # heading.border.bottom.width = px(3),
              # heading.border.bottom.color = "#4c4c4c",
              heading.border.bottom.color = "transparent",
              column_labels.border.bottom.width = px(3),
              column_labels.border.bottom.color = "#4c4c4c",
              table_body.border.bottom.width = px(3),
              table_body.border.bottom.color = "#4c4c4c",
              table.border.bottom.color = "transparent") %>% 
  opt_table_font(font = google_font("Roboto")) %>% 
  opt_row_striping()
t2 <- input %>% 
  mutate(PER=paste0(per," (",per_rank,")",sep=""),
         `WS/48`=paste0(ws_48," (",ws_48_rank,")",sep=""),
         BPM=paste0(bpm," (",bpm_rank,")",sep=""),
         `On/Off`=paste0(on_off," (",on_off_rank,")",sep=""),
         LEBRON=paste0(lebron," (",lebron_rank,")",sep=""),
         RPM=paste0(rpm," (",rpm_rank,")",sep=""),
         RAPTOR=paste0(raptor," (",raptor_rank,")",sep=""),
         PREDATOR=paste0(predator," (",predator_rank,")",sep=""),
         WPA=paste0(wpa," (",wpa_rank,")",sep=""),
         RAPM=paste0(rapm," (",rapm_rank,")",sep=""),) %>% 
  select(1:3,37:45,36) %>% 
  slice(1:10) %>% 
  gt() %>% 
  cols_align(align = "center") %>% 
  cols_label("player"="Player", "team"="Team", "games"="Games",
             "avg_rank"="Average Rank") %>% 
  tab_header(title=md("**MVP Ladder: All-in-one metrics**"),
             subtitle=html("<div style='color:#8e8e8e;'>Top-10 players based on all-in-one metrics rank</div><br/>")) %>% 
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything())) %>% 
  tab_source_note(source_note = html(paste0("Sources: Basketball Reference, BBall Index, ESPN, FiveThirtyEight, inpredictable<br/>",
                                            format(Sys.time(),"%B %d,%Y")))) %>% 
  tab_options(data_row.padding = px(1),
              row.striping.background_color = "#eeeeee",
              heading.align = "left",
              heading.title.font.size = 40,
              heading.subtitle.font.size = 20,
              table.font.size = 15,
              table.border.top.color = "transparent",
              # heading.border.bottom.width = px(3),
              # heading.border.bottom.color = "#4c4c4c",
              heading.border.bottom.color = "transparent",
              column_labels.border.bottom.width = px(3),
              column_labels.border.bottom.color = "#4c4c4c",
              table_body.border.bottom.width = px(3),
              table_body.border.bottom.color = "#4c4c4c",
              table.border.bottom.color = "transparent") %>% 
  opt_table_font(font = google_font("Roboto")) %>% 
  opt_row_striping()

gtsave(t1,"per75.png") ; gtsave(t2,"all.png")