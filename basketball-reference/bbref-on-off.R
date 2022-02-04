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
originalABB <- wikiTeams$TEAM_ABBREVIATION

# match abreviations with bbref:
wikiTeams[2,1] <- "BRK" ; wikiTeams[4,1] <- "CHO"
wikiTeams[24,1] <- "PHO"

# create a list and store tables for every team:
out <- list()

for (i in 1:dim(wikiTeams)[1]) {
  # raw table:
  df <- suppressWarnings(
    paste("https://www.basketball-reference.com/teams/",
          wikiTeams$TEAM_ABBREVIATION[i],"/2022/on-off", sep="") %>% 
      read_html() %>% 
      html_elements("table") %>% 
      html_table() %>% 
      `[[`(1) %>%
      row_to_names(row_number = 1) %>% 
      clean_names()
  )
  
  # clean data:
  df <- df %>% 
    rename_all(.funs = toupper) %>% 
    filter(PLAYER != "Player" & SPLIT != "")
  
  # create objects for loop:
  n <- dim(df)[1]
  myStep <- seq(1,n-2,3)
  
  for (j in myStep) {
    x <- df$PLAYER[j]
    df$PLAYER[j+1] <- x
    df$PLAYER[j+2] <- x
    rm(x)
  }
  
  # rename columns:
  names(df)[1] <- "PLAYER_NAME"
  names(df)[4:13] <- gsub("\\_PERCENT","_PCT",names(df)[4:13])
  names(df)[14:23] <- paste("OPP_",gsub("\\_PERCENT\\_2|\\_2","",
                                        names(df)[14:23]),sep="")
  names(df)[24:33] <- paste("DIFF_",gsub("\\_PERCENT\\_3|\\_3","",
                                         names(df)[24:33]),sep="")
  df$TEAM_ABBREVIATION <- originalABB[i]
  
  out[[i]] <- df[,c(1,34,2:33)]
}
bbrefOnOff <- do.call("rbind",out)
rm(i,j,n,myStep,originalABB,df,out,wikiTeams)