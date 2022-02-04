# load libraries ----
library(dplyr)
library(janitor)
library(rvest)

# load team names & abbreviations:
wikiTeams <- "https://en.wikipedia.org/wiki/Wikipedia:WikiProject_National_Basketball_Association/National_Basketball_Association_team_abbreviations" %>% 
  read_html() %>% 
  html_elements("table") %>% 
  html_table() %>% 
  `[[`(1) %>% 
  mutate(X1 = ifelse(X1=='Abbreviation/Acronym', 'TEAM_ABBREVIATION',X1),
         X2 = ifelse(X2=='Franchise', 'TEAM_NAME',X2)) %>% 
  row_to_names(row_number = 1)

# create a table to store number of unique starting lineups:
numberOfLineups <- tibble(  TEAM_ABBREVIATION = wikiTeams$TEAM_ABBREVIATION,
                            N_OF_LINEUPS = array(NA, dim(wikiTeams)[1])
)
# edit team abbreviations to match with bbref:
numberOfLineups[2,1] <- "BRK" ; numberOfLineups[4,1] <- "CHO"
numberOfLineups[24,1] <- "PHO"

bbrefLineupList <- list()

for (i in 1:dim(numberOfLineups)[1]) {
  
  # assign url
  url <- paste("https://www.basketball-reference.com/teams/",
               numberOfLineups$TEAM_ABBREVIATION[i],"/2022_start.html",
               sep="")
  
  # get table of unique lineups and record
  df2 <- url %>% 
    read_html() %>% 
    html_elements("table") %>% 
    html_table() %>% 
    `[[`(2) %>% 
    select(`Starting Lineup`, G, W, L, `W/L%`) %>% 
    rename(LINEUP=`Starting Lineup`,GP=G, W_PCT=`W/L%`)
  
  numberOfLineups$N_OF_LINEUPS[i] <- dim(df2)[1]
  bbrefLineupList[[i]] <- df2
  rm(df2) ; rm(url)
}

# (optional): get original abbreviations:
numberOfLineups$TEAM_ABBREVIATION <- wikiTeams$TEAM_ABBREVIATION
numberOfLineups <- merge(numberOfLineups,wikiTeams,by="TEAM_ABBREVIATION") %>% 
  select(TEAM_NAME,TEAM_ABBREVIATION,N_OF_LINEUPS)

rm(wikiTeams,i)
