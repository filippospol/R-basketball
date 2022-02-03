# load libraries ----
library(dplyr)
library(janitor)
library(rvest)

# create a list with all team stats tables ----
bbrefTeams <- "https://www.basketball-reference.com/leagues/NBA_2022.html" %>% 
  read_html() %>% 
  html_elements("table") %>% 
  html_table()

# clean data ----
# row 31 = league averages:

# team standings by conference
for (i in 1:2) {
  bbrefTeams[[i]] <- suppressWarnings(
    bbrefTeams[[i]] %>% 
    rename(TEAM_NAME=1, W_PCT=4, PTS_SCORED=6,PTS_ALLOWED=7) %>% 
    mutate(GB = as.numeric(GB),
           TEAM_NAME = gsub("\\s*\\([^\\)]+\\)","",as.character(TEAM_NAME)))
  )
}

# per game team/opponent stats (5,6)
# total team/opponent stats (7,8)
# per 100 poss. team/opponent stats (9,10)
for (i in 5:10) {
  names(bbrefTeams[[i]])[1:19]<-
    c("RANK","TEAM_NAME","GP","MIN","FGM","FGA","FG_PCT","FG3M","FG3A","FG3_PCT",
      "FG2M","FG2A","FG2_PCT","FTM","FTA","FT_PCT","OREB","DREB","REB")
  bbrefTeams[[i]] <- bbrefTeams[[i]] %>% 
    arrange(TEAM_NAME) %>% 
    select(-1)
} ; rm(i)

# advanced team/opponent stats (11)
bbrefTeams[[11]] <- suppressWarnings(
  bbrefTeams[[11]] %>% 
    row_to_names(row_number=1) %>% 
    clean_names() %>% 
    rename_all(.funs = toupper) %>% 
    arrange(TEAM) %>% 
    select(c(2:17,19:22,24:27)) %>% 
    mutate_at(-1,as.numeric)
)
names(bbrefTeams[[11]]) <-
  c("TEAM_NAME","AGE","W","L","PYTH_W","PYTH_L","MOV","SOS","SRS",
    "OFF_RTG","DEF_RTG","NET-RTG","PACE","FTA_RATE","FG3A_RATE","TS_PCT",
    "EFG_PCT","TOV_PCT","OREB_PCT","FTM_RATE","OPP_EFG_PCT","OPP_TOV_PCT",
    "OPP_OREB_PCT","OPP_FTM_RATE")

# shooting team/opponent stats (12,13)
bbrefTeams[[12]] <- suppressWarnings(
  bbrefTeams[[12]] %>% 
    row_to_names(row_number=1) %>% 
    clean_names() %>% 
    rename_all(.funs = toupper) %>% 
    arrange(TEAM) %>% 
    select(-c(1,7,14,21,24,27,30,32)) %>% 
    mutate_at(-1,as.numeric)
)
names(bbrefTeams[[12]]) <-
  c("TEAM_NAME","GP","MP","FG_PCT","AVG_FGA_DIST",
    "FREQ_FG2","FREQ_0_3_FT","FREQ_3_10_FT","FREQ_10_16_FT","FREQ_16_PLUS_FT",
    "FREQ_FG3", "FG2_PCT","PCT_0_3_FT","PCT_3_10_FT","PCT_10_16_FT",
    "PCT_16_PLUS_FT","FG3_PCT","ASSISTED_FG2","ASSISTED_FG3","FREQ_DUNKS",
    "MADE_DUNKS","FREQ_LAYUPS","MADE_LAYUPS","FREQ_CORNER","PCT_CORNER",
    "HEAVES","HEAVES_MADE")

bbrefTeams[[13]] <- suppressWarnings(
  bbrefTeams[[13]]%>% 
    row_to_names(row_number=1) %>% 
    clean_names() %>% 
    rename_all(.funs = toupper) %>% 
    arrange(TEAM) %>% 
    select(-c(1,7,14,21,24,27,30)) %>% 
    mutate_at(-1,as.numeric)
)
names(bbrefTeams[[13]]) <- names(bbrefTeams[[12]])[-c(26,27)]

# remove unecessary tables:
bbrefTeams <- bbrefTeams[-c(3,4)]

# add names to list objects:
names(bbrefTeams) <-
  c("East_Standings","West_Standings","Team_PerGame","Opponent_Pergame",
    "Team_Totals","Opponent_Totals","Team_Per100","Opponent_Per100",
    "Team_Opponent_Advanced","Team_Shooting","Opponent_Shooting")

# move "League Average" row to bottom:
for (i in c(3:6,9:11)) {
  bbrefTeams[[i]] <- bbrefTeams[[i]][c(1:12,14:31,13),]
} ; rm(i)