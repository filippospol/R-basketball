# load libraries: ----
pacman::p_load(dplyr,rvest,reshape2)

bbrefCOntinuity = "https://www.basketball-reference.com/friv/continuity.html" %>% 
  read_html() %>% 
  html_elements("table") %>% 
  html_table() %>% 
  pluck(1) %>% 
  mutate_at(-1, ~substr(., 1, 2) %>% as.numeric() / 100) %>% 
  select(1,2,19,3:18,20:31) %>% 
  rename("BKN"=3,"NOP"=19,"PHO"=25,"WAS"=31) %>% 
  melt()

