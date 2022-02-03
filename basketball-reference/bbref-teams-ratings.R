# load libraries ----
library(dplyr)
library(janitor)
library(rvest)

# get bbref team adjusted ratings:
bbrefRatings <- "https://www.basketball-reference.com/leagues/NBA_2022_ratings.html" %>% 
  read_html() %>% 
  html_elements("table") %>% 
  html_table() %>% 
  `[[`(1) %>% 
  row_to_names(row_number=1) %>% 
  rename(W_PCT=`W/L%`, TEAM_NAME=Team, OFF_RTG=ORtg, DEF_RTG=DRtg,
         NET_RTG=NRtg, MOV_ADJ=`MOV/A`, OFF_RTG_Adj=`ORtg/A`,
         DEF_RTG_ADJ=`DRtg/A`, NET_RTG_ADJ=`NRtg/A`) %>% 
  rename_all(.funs = toupper) %>% 
  mutate_at(c(5:15),as.numeric) %>% 
  arrange(TEAM_NAME) %>% 
  select(-1)
