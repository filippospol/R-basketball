# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script scrapes pbp data from FIBA Live Stats
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fls_pbp = function(gameid) {
  pacman::p_load(tidyverse,httr,jsonlite,glue,janitor)
  
  # scrape raw data:
  myurl = glue("https://fibalivestats.dcd.shared.geniussports.com/data/{gameid}/data.json") %>% 
    as.character()
  mydata = fromJSON(myurl)
  
  # get raw pbp in a separate table
  pbp_raw = mydata %>% pluck("pbp") %>% as_tibble()
  
  
  # clean pbp data:
  pbp_final = pbp_raw %>% 
    mutate(period = ifelse(periodType=="OVERTIME",5,period),
           team_name = ifelse(tno==1, mydata$tm$`1`$name,
                              mydata$tm$`2`$name),
           team_short = ifelse(tno==1, mydata$tm$`1`$shortName,
                               mydata$tm$`2`$shortName)) %>% 
    arrange(period,desc(gt)) %>% 
    clean_names("all_caps") 
  
  # final table:
  bind_cols(pbp_final %>% 
              select(-QUALIFIER),
            pbp_final %>% 
              select(QUALIFIER) %>% 
              mutate(ROW=row_number()) %>% 
              unnest(QUALIFIER, keep_empty=T) %>% 
              select(2,1) %>% 
              group_by(ROW) %>% 
              summarise(Q=paste(QUALIFIER, collapse=", ")) %>% 
              select(-1)) %>% 
    rowwise() %>% 
    mutate(PLAYER_NAME = ifelse(is.na(INTERNATIONAL_FIRST_NAME),NA,
       paste(INTERNATIONAL_FIRST_NAME, 
             INTERNATIONAL_FAMILY_NAME, collapse=", "))) %>% 
    select(1,2,7,8,3,4,5,30,27,28,11:16,QUALIFIER=Q) %>% 
    mutate(GAME_ID=gameid, .before=1) %>%
    return()
}




          



















