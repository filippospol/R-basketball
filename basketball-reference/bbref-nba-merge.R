pacman::p_load(hoopR,tidyverse,rvest,janitor)

# advanced stats from NBA.com
t1 <- nba_leaguedashplayerstats(season="2019-20",measure_type="Advanced",
                                season_type="Regular Season") %>% 
  pluck(1) %>% 
  select(PLAYER_ID,PLAYER_NAME,TEAM=TEAM_ABBREVIATION,PIE) %>% 
  mutate(PIE=as.numeric(PIE)*100)

# advanced stats from Basketball Reference
t2 <- "https://www.basketball-reference.com/leagues/NBA_2020_advanced.html" %>% 
  read_html() %>% 
  html_elements("table") %>% 
  html_table() %>%
  pluck(1) %>% 
  clean_names("all_caps") %>% 
  group_by(PLAYER) %>% 
  filter(G==max(G),PLAYER!="Player") %>% 
  arrange(PLAYER) %>% 
  ungroup() %>% 
  select(PLAYER,POS,MP,PER,WS_48,BPM) %>% 
  mutate(POS=substr(POS, start = 1, stop = 2)) %>% 
  mutate_at(-c(1,2), as.numeric)

# now the tricky part: use the edit distance metric for the two names columns.
# edit distance counts the number of changes are needed to turn a 
# string s2 into string s1 --> adist("Hello World","Helol Wolrd")
edd <- adist(t1$PLAYER_NAME, t2$PLAYER)

# we get a N x N matrix with the k-th row representing the edit distance
# of the k-th element in t1$PLAYER_NAME with the all the elements of t2$PLAYER.
# the lower the edit distance, the greater similarity two elements have.

# iterating over all the matrix's rows, we identify the pairs with the 
# lowest edit distance and create an array of positions
ind <- rep(NA,ncol(edd))
for (i in 1:nrow(t1)) {
  ind[i] <- which.min(edd[i,])
}

# using the position array, re-arrange the rows of BBREF data according to
# the NBA Stats data
t2 <- t2[ind,] %>% na.omit()

# check if the names match
# View(bind_cols(t1$PLAYER_NAME,t2$PLAYER))

# merge the two tables:
db <- bind_cols(t1,t2[,-1]) %>% 
  select(1,2,3,5,6,4,7:9) %>% 
  filter(MP >=100)
rm(t1,t2,edd,i,ind)

# simple scatter plot
db %>% 
  ggplot(aes(x=WS_48,y=PIE)) +
  geom_point(size=3,color="grey80") +
  geom_smooth(method=lm, se=FALSE, formula="y~x") +
  geom_text(aes(x=-0.05,y=21,label=paste("R-squared:",
                                     summary(lm(PIE~WS_48,data=db))$r.squared %>% round(2)))) +
  theme_minimal()