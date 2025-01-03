pacman::p_load(tidyverse,rvest,glue,janitor,lubridate)

bbref_gameid_date = function(season,mm,dd) {
  baseHTML = "https://www.basketball-reference.com/boxscores/?month={mm}&day={dd}&year={season}" %>% 
    glue() %>% 
    read_html()
  
  baseHTML %>% 
    html_nodes("a") %>% 
    html_attr("href") %>% 
    str_subset("/boxscores/20") %>% 
    gsub("/boxscores/","",.) %>% 
    gsub(".html","",.) %>% 
    return()
}
