# load libraries: ----
pacman::p_load(tidyverse,rvest,janitor)

# get team abbreviations: ----
y <- "https://en.wikipedia.org/wiki/Wikipedia:WikiProject_National_Basketball_Association/National_Basketball_Association_team_abbreviations" %>% 
  read_html() %>% 
  html_elements("table") %>% 
  html_table() %>% 
  pluck(1) %>% 
  row_to_names(row_number = 1) %>% 
  select(1) %>% pull()
x <- y

# edit team abbreviations to match with bbref: ----
x[2] <- "BRK" ; x[4] <- "CHO" ; x[24] <- "PHO"

# get position estimates: ----
out <- list()
for (i in 1:length(x)) {
  z <- paste("https://www.basketball-reference.com/teams/",x[i],"/2022.html",
             sep="") %>% 
    read_html() %>% 
    html_nodes(xpath = '//comment()') %>%    
    html_text() %>%    
    paste(collapse = '') %>%    
    read_html() %>% 
    html_table()
  
  target <- 0
  for (j in 1:length(z)) {
    if (any(drop_na(z[[j]])=="PG%")) {
      target <- j
      break
    }
  }
  
  df <- z %>% 
    pluck(target) %>% 
    row_to_names(1) %>% 
    clean_names() %>%
    rename(player_name=x) %>% 
    rename_all(.funs = toupper) %>% 
    select(c(2,4:10))
  
  df[,4:8] <- data.frame(sapply(df[,4:8], 
                                function(x) as.numeric(gsub("%", "", x))/100))
  df[is.na(df)] <- 0
  df$TEAM_ABBREVIATION <- y[i]
  df <- df[,c(1,9,2:8)]
  
  out[[i]] <- df
}

# gather all teams' data into one table: ----
db <- bind_rows(out) %>% arrange(PLAYER_NAME)
rm(list=setdiff(ls(), "db"))
