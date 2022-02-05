x <- "https://en.wikipedia.org/wiki/Wikipedia:WikiProject_National_Basketball_Association/National_Basketball_Association_team_abbreviations" %>% 
  read_html() %>% 
  html_elements("table") %>% 
  html_table() %>% 
  `[[`(1) %>% 
  mutate(X1 = ifelse(X1=='Abbreviation/Acronym', 'TEAM_ABBREVIATION',X1),
         X2 = ifelse(X2=='Franchise', 'TEAM_NAME',X2)) %>% 
  row_to_names(row_number = 1) %>% 
  select(TEAM_ABBREVIATION) %>% pull()
y <- x

# edit team abbreviations to match with bbref:
x[2] <- "BRK" ; x[4] <- "CHO" ; x[24] <- "PHO"

# create a list to store info for all teams: ----
out <- list()

# get position estimates:
for (i in 1:length(x)) {
  df <- suppressWarnings(
    paste("https://www.basketball-reference.com/teams/",x[i],"/2022.html",
          sep="") %>% 
      read_html() %>% 
      html_nodes(xpath = '//comment()') %>%    
      html_text() %>%    
      paste(collapse = '') %>%    
      read_html() %>% 
      html_table() %>% 
      `[[`(9) %>% 
      row_to_names(1) %>% 
      clean_names() %>%
      rename(player_name=x) %>% 
      rename_all(.funs = toupper) %>% 
      select(c(2,4:10))
  )
  
  # clean data:
  df[,4:8] <- data.frame(sapply(df[,4:8], 
                                function(x) as.numeric(gsub("%", "", x))/100))
  df[is.na(df)] <- 0
  df$TEAM_ABBREVIATION <- y[i]
  df <- df[,c(1,9,2:8)]
  
  # store data to list:
  out[[i]] <- df ; rm(df)
}

# gather all teams data into one table: ----
bbrefEstPosition <- do.call("rbind",out) ; rm(out,i,x,y)

# group per player
bbrefEstPosition <- bbrefEstPosition %>% 
  group_by(PLAYER_NAME) %>% 
  mutate(MIN=as.numeric(MP)) %>% 
  summarise(MIN=sum(MIN),PG_PERCENT=round(mean(PG_PERCENT),2),
            SG_PERCENT=round(mean(SG_PERCENT),2),
            SF_PERCENT=round(mean(SF_PERCENT),2),
            PF_PERCENT=round(mean(PF_PERCENT),2),
            C_PERCENT=round(mean(C_PERCENT),2))

# create estimated position column: ----
bbrefEstPosition <- bbrefEstPosition %>% 
  mutate(POSITION_EST = case_when(
    PG_PERCENT>0.8 ~ "POINT GUARD",
    PG_PERCENT>0.4 | SG_PERCENT>0.4 & 
      SF_PERCENT-PG_PERCENT<PG_PERCENT ~ "COMBO GUARD",
    SG_PERCENT>0.4 | SF_PERCENT>0.4 & 
      SG_PERCENT+SF_PERCENT>0.65 ~ "WING",
    (SF_PERCENT>0.4 | PF_PERCENT>0.4 | C_PERCENT<0.1) & 
      SF_PERCENT+PF_PERCENT>0.65 & 
      (PF_PERCENT<C_PERCENT | SF_PERCENT>C_PERCENT) ~ "FORWARD",
    TRUE ~ "BIG"
  )
  ) %>% 
  select(1,2,8)
# manually edit some players' positions: ----
bbrefEstPosition$POSITION_EST[
  which(bbrefEstPosition$PLAYER_NAME %in% c(
    "DeMar DeRozan","Kevin Durant","Lauri Markkanen","Andrew Wiggins",
    "Otto Porter Jr.","Juan Toscano-Anderson","Andre Iguodala",
    "Nicolas Batum","Carmelo Anthony","Stanley Johnson","Kyle Anderson",
    "Killian Tillie","Josh Okogie","Brandon Ingram","Josh Giddey",
    "Kenrich Williams","Franz Wagner","Tobias Harris","Cameron Johnson",
    "Robert Covington","Maurice Harkless","Doug McDermott","Joe Ingles",
    "Deni Avdija"
  ))
] <- "FORWARD"
bbrefEstPosition$POSITION_EST[
  which(bbrefEstPosition$PLAYER_NAME %in% c(
    "Eric Gordon","Avery Bradley","Chris Duarte","John Konchar","Max Strus",
    "Grayson Allen","Jordan Nwora","Josh Hart","Garrett Temple",
    "Quentin Grimes","Gary Trent Jr."
  ))
] <- "WING"
bbrefEstPosition$POSITION_EST[
  which(bbrefEstPosition$PLAYER_NAME %in% c(
    "JaMychal Green","Xavier Tillman Sr.","Thanasis Antetokounmpo" 
  ))
] <- "BIG"
bbrefEstPosition$POSITION_EST[
  which(bbrefEstPosition$PLAYER_NAME %in% c("D'Angelo Russell"))
] <- "POINT GUARD"
bbrefEstPosition$POSITION_EST[
  which(bbrefEstPosition$PLAYER_NAME %in% c("Patrick Beverley"))
] <- "COMBO GUARD"
bbrefEstPosition <- bbrefEstPosition %>% arrange(PLAYER_NAME)
