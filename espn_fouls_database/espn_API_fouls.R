# https://www.reddit.com/r/nba/comments/rjv6zx/dataset_for_technical_fouls_ejections_and/

# required packages: pacman,tidyverse,httr,jsonlite
pacman::p_load(tidyverse,httr,jsonlite,rvest)

# function to return player names and ESPN id's for a given season:
espn_nba_athletes <- function(season) {
  
  # access API
  url <- gsub("XXX",season,
              "https://site.web.api.espn.com/apis/common/v3/sports/basketball/nba/statistics/byathlete?region=us&lang=en&season=XXX&seasontype=2&limit=1000")
  # use JSON to extract the data
  x <- GET(url)
  resp <- fromJSON(content(x, "text"))
  y <- resp$athletes$athlete
  # return data in tibble format
  return(tibble(y) %>% 
           select(id,displayName) %>% 
           mutate(headshot=y$headshot$href) %>% 
           replace_na(
             list(headshot="https://a.espncdn.com/combiner/i?img=/i/teamlogos/leagues/500/nba.png")))
  
}

# function to return a given player's career misc total stats:
espn_nba_misc_totals <- function(athleteID,s_type) {
  if (!s_type %in% 2:3) stop("Error using the function: s_type is either 2 or 3.")
  
  # get link from API
  if (s_type==2) {
    url <- gsub("XXX",athleteID,
                "https://www.espn.com/nba/player/stats/_/id/XXX/type/nba/seasontype/2")
  }
  if (s_type==3) {
    url <- gsub("XXX",athleteID,
                "https://www.espn.com/nba/player/stats/_/id/XXX/type/nba/seasontype/3")
  }
  
  # use html to extract the data
  x <- url %>% 
    read_html() %>% 
    html_elements("table") %>% 
    html_table()
  # return data in tibble format
  if (length(x) !=0 & length(x)>=6) {
    return(
      inner_join(
        bind_cols(x[[5]],x[[6]] %>% select(DQ:FLAG)),
        bind_cols(x[[1]],x[[2]]%>% select(GP,GS)),
        x[[4]]%>% select(PF,TO),
        by=c("season","Team")
      ) %>% 
        mutate(id=athleteID,.before="season")
    )
    
  }
  else return(tibble())
  
} ; Sys.sleep(30)

# -----------------------------------------------------------------------------------------------------------

# players active from season 1991-92 onwards:
# (that is the earliest season available from ESPN API)
# flagrant fouls were first called in 1990-91
s <- 1992:format(Sys.Date(),format="%Y")

# list containing athlete ids per season:
out <- list()

for (i in 1:length(s)) {
  out[[i]] <- espn_nba_athletes(s[i])
}

# merge in one tibble and remove duplicates:
players <- bind_rows(out) %>% distinct() %>% arrange(displayName)
rm(list=setdiff(ls(), c("players","espn_nba_athletes","espn_nba_misc_totals")))

# list with misc playoff stats for every recorded player:
# change playoff appearances if regular season stats are scraped:
out1 <- vector("list",length=nrow(players))
out2 <- vector("list",length=nrow(players))
for (i in 1:nrow(players)) {
  # pause script in case of an 503 error:
  tryCatch(espn_nba_misc_totals(athleteID = players$id[i],s_type = 2) %>% slice_tail(n=1),
           error=function(e) Sys.sleep(90))
  # scrape total misc stats for regular and post season (if available):
  if (dim(espn_nba_misc_totals(athleteID = players$id[i],s_type = 2))[1]!=0){
    if (dim(espn_nba_misc_totals(athleteID = players$id[i],s_type = 3))[1]!=0) {
      out1[[i]] <- espn_nba_misc_totals(athleteID = players$id[i],s_type = 2) %>% 
        mutate(player_experience=row_number()-1, s_type=2) %>% 
        filter(season=="Career") %>% 
        select(-c(2,3))
      out2[[i]] <- espn_nba_misc_totals(athleteID = players$id[i],s_type = 3) %>% 
        mutate(player_experience=row_number()-1, s_type=3) %>% 
        filter(season=="Career") %>% 
        select(-c(2,3))
    }
    else {
      out1[[i]] <- espn_nba_misc_totals(athleteID = players$id[i],s_type = 2) %>% 
        mutate(player_experience=row_number()-1, s_type=2) %>% 
        filter(season=="Career") %>% 
        select(-c(2,3))
    }
  }
}
beepr::beep(2)

# TODO
out <- bind_rows(
  bind_rows(out1) %>% distinct(),
  bind_rows(out2) %>% distinct()
)
# gather all data in one tibble:
db <- Filter(length, out) %>% 
  bind_rows() %>% 
  right_join(players, by="id") %>% 
  arrange(displayName) %>% 
  select(PLAYER=displayName,S_TYPE=s_type,GP,GS,YEARS_PLAYED=player_experience,DQ:FLAG,
         HEAD=headshot)

# create the final tables --> regular / playoffs / career:
db_RS <- db %>% 
  filter(S_TYPE==2) %>% 
  select(-S_TYPE)
db_PO <- db %>% 
  filter(S_TYPE==3) %>% 
  select(-S_TYPE)
db_career <- db %>% 
  group_by(PLAYER) %>% 
  summarise(GP=sum(GP,na.rm=T),DQ=sum(DQ,na.rm=T),EJECT=sum(EJECT,na.rm=T),
            TECH=sum(TECH,na.rm=T),FLAG=sum(FLAG,na.rm=T))
rm(list=setdiff(ls(), c("db_career","db_RS","db_PO")))

# export tables as .csv files:
write.table(db_career,file="fouls_career.csv",sep=",",row.names=FALSE)
write.table(db_PO,file="fouls_PO.csv",sep=",",row.names=FALSE)
write.table(db_RS,file="fouls_RS.csv",sep=",",row.names=FALSE)
