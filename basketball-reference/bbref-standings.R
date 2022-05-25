# load libraries ----
pacman::p_load(tidyverse,rvest,janitor)

# get expanded standings: ----
exp_standings <- "https://www.basketball-reference.com/leagues/NBA_2022_standings.html#expanded_standings" %>% 
  read_html() %>% 
  html_nodes(xpath = '//comment()') %>%    
  html_text() %>%    
  paste(collapse = '') %>%    
  read_html() %>% 
  html_table() %>% 
  pluck(1) %>% 
  row_to_names(row_number=1) %>% 
  rename_all(.funs = toupper) %>% 
  select(-1)

# edit names: ----
names(exp_standings)[5:6] <- paste0("CONF_",names(exp_standings)[5:6])
names(exp_standings)[7:12] <- paste0("DIV_",names(exp_standings)[7:12])
names(exp_standings)[13:14] <- paste0(names(exp_standings)[13:14],"_ALLSTAR")
names(exp_standings)[15:16] <- c("MOV_LEQ3","MOV_GEQ10")
