library(dplyr)
library(rvest)

# get team names and abbreviations
url = 'https://en.wikipedia.org/wiki/Wikipedia:WikiProject_National_Basketball_Association/National_Basketball_Association_team_abbreviations'

team_char = url %>% 
  read_html() %>% 
  html_elements("table") %>% 
  html_table() %>% 
  `[[`(1) %>% 
  mutate(X1 = ifelse(X1=='Abbreviation/Acronym', 'TEAM_ABBR',X1),
         X2 = ifelse(X2=='Franchise', 'TEAM_NAME',X2)) %>% 
  janitor::row_to_names(row_number = 1)


# create two objects:
# 1) a table with the number of starting lineups per team
# 2) a list that contains all these lineups for each team
team_summary <- tibble(
  team = team_char$TEAM_ABBR,
  unique_lineups = array(NA, length(team))
)

# edit team abbreviations to match with bbref:
team_summary[2,1] <- "BRK" ; team_summary[4,1] <- "CHO"
team_summary[24,1] <- "PHO"

starting_lineups <- list()

for (i in 1:dim(team_summary)[1]) {
  
  # assign url
  url <- paste("https://www.basketball-reference.com/teams/",
               team_summary$team[i],"/2022_start.html", sep="")
  
  # get table of unique lineups and record
  df2 <- url %>% 
    read_html() %>% 
    html_elements("table") %>% 
    html_table() %>% 
    `[[`(2) %>% 
    select(`Starting Lineup`, G, `W/L%`)
  
  team_summary$unique_lineups[i] <- dim(df2)[1]
  starting_lineups[[i]] <- df2
  rm(df2) ; rm(url)
  
} ; rm(i)
