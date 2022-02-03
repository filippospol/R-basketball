# load libraries ----
library(tidyverse)
library(janitor)
library(rvest)

# get expanded standings & team-vs-team tables: ----
bbrefStandings <- "https://www.basketball-reference.com/leagues/NBA_2022_standings.html#expanded_standings" %>% 
  read_html() %>% 
  html_nodes(xpath = '//comment()') %>%    
  html_text() %>%    
  paste(collapse = '') %>%    
  read_html() %>% 
  html_table()

bbrefStandings[[1]] <- bbrefStandings[[1]] %>% 
  row_to_names(row_number=1) %>% 
  select(-1) %>% 
  rename_all(.funs = toupper) %>%
  rename(TEAM_NAME=TEAM,VS_EAST=E,VS_WEST=W,VS_ATLANTIC=A,VS_CENTRAL=C,
         VS_SOUTHEAST=SE,VS_NORTHWEST=NW,VS_PACIFIC=P,VS_SOUTHWEST=SW,
         PRE_ALLSTAR=PRE,POST_ALLSTAR=POST,MARGIN_LEQ3=`=3`,
         MARGIN_GEQ10=`=10`) %>%
  arrange(TEAM_NAME)
  
# create win and loss columns:
x <- names(bbrefStandings[[1]])[-1]
for (i in 1:length(x)) {
  bbrefStandings[[1]] <- bbrefStandings[[1]] %>% 
    separate(x[i],into=paste(x[[i]],c("_W","_L"),sep=""),sep="-",remove=F)
} ; rm(i)

# win/loss columns per category:
index <- seq(2,65,3)
z <- bbrefStandings[[1]][,which(!1:67 %in% index)]
bbrefStandings[[1]] <- bbrefStandings[[1]][,c(1,index)]

bbrefStandings[[3]] <- z %>% 
  mutate_at(-1,as.numeric)

# team-vs-team in matrix format:
y <- as.matrix(bbrefStandings[[2]][,-c(1,2)])
row.names(y) <- colnames(y)
bbrefStandings[[2]] <- y

# add names to tables
names(bbrefStandings) <- c("ExpandedStandings","TeamvsTeam","Expanded_WL")

rm(list=setdiff(ls(), "bbrefStandings"))