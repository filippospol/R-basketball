# FANCY WOWY #
pacman::p_load(tidyverse,hoopR,httr,jsonlite,glue,janitor,gt,gtExtras)

fancy_wowy = function(season, player) {
  pacman::p_load(tidyverse,hoopR,httr,jsonlite,glue,janitor,gt,gtExtras)
  
  pl = nba_leaguedashplayerstats(season=season) %>% 
    pluck(1) %>% 
    select(PLAYER_NAME,PLAYER_ID,TEAM_ID,TEAM=TEAM_ABBREVIATION)
  
  team_id = pl %>% filter(PLAYER_NAME==player) %>% pull(TEAM_ID)
  player_id = pl %>% filter(PLAYER_NAME==player) %>% pull(PLAYER_ID)
  
  master_url =  GET(url = glue("https://api.pbpstats.com/get-wowy-combination-stats/nba?Season={season}&SeasonType=Regular%20Season&TeamId={team_id}&PlayerIds={player_id}&OnlyCommonGames=false"))
  
  mycaption = '<meta name="viewport" content="width=device-width, initial-scale=1"> <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"> <i class="fa fa-twitter"></i> filippos_pol'
  
  if (season=="2023-24") {
    mydate = Sys.Date()
    mysub = html(glue("<div style='color:#8e8e8e;'>{season} season | Games through {mydate} | Source: PBP Stats</div><br/>"))
  } else {
    mysub = html(glue("<div style='color:#8e8e8e;'>{season} season | Source: PBP Stats</div><br/>"))
  }
  
  suppressWarnings(
    suppressMessages(
      fromJSON(content(master_url, "text")) %>% pluck(1) %>% tibble() %>% 
        clean_names("all_caps") 
    ) %>% 
      select(5,6,4,1:3) %>% 
      map_df(rev) %>% 
      mutate(ON=c(glue("<img src='https://cdn.nba.com/headshots/nba/latest/260x190/{player_id}.png' height=55> &nbsp"),""),
             OFF=c("",glue("<img src='https://cdn.nba.com/headshots/nba/latest/260x190/{player_id}.png' height=55 style='filter: grayscale(100%)'> &nbsp"))) %>% 
      mutate(ON = map_chr(ON, ~ paste(., collapse = " ")),
             OFF = map_chr(OFF, ~ paste(., collapse = " "))) %>% 
      mutate_at(1:2, ~ map(., gt::html)) %>%
      gt() %>% 
      fmt_number(columns=(4:6), decimals=1) %>% 
      cols_align(align = "right") %>% 
      cols_label(ON="",OFF="",MINUTES="Minutes",OFF_RTG="Offensive Rating",
                 DEF_RTG="Defensive Rating",NET_RTG="Net Rating") %>% 
      tab_header(
        title = gt::html(glue::glue("<p class='' style='margin: -10px;'></p><img src='https://cdn.nba.com/logos/nba/{team_id}/primary/L/logo.svg' style='height:60px'> <p class='' style='margin: -10px;'></p><b>Team efficiency with {player} on/off the floor")),
        subtitle=mysub
      ) %>% 
      tab_style(style = cell_text(weight = "bold"),
                locations = cells_column_labels(everything())) %>% 
      tab_style(style = cell_fill(color = "#d8d8d8"), 
                locations = cells_column_labels(3:6)) %>% 
      tab_style(style = list(cell_text(color = "#6FCB9F", weight="bold")),
                locations = cells_body(columns = vars(OFF_RTG), rows = OFF_RTG == max(OFF_RTG))) %>% 
      tab_style(style = list(cell_text(color = "#6FCB9F", weight="bold")),
                locations = cells_body(columns = vars(DEF_RTG), rows = DEF_RTG == min(DEF_RTG))) %>% 
      tab_style(style = list(cell_text(color = "#6FCB9F", weight="bold")),
                locations = cells_body(columns = vars(NET_RTG), rows = NET_RTG == max(NET_RTG))) %>% 
      tab_source_note(source_note = md(paste("Table by: Filippos Polyzos |",mycaption))) %>%
      tab_options(data_row.padding = px(1),
                  heading.align = "left",
                  heading.title.font.size = 20,
                  heading.subtitle.font.size = 16,
                  table.font.size = 14,
                  table.border.top.color = "transparent",
                  heading.border.bottom.width = px(4),
                  heading.border.bottom.color = "#4c4c4c",
                  # heading.border.bottom.color = "transparent",
                  column_labels.border.bottom.width = px(2),
                  column_labels.border.bottom.color = "transparent",
                  table_body.border.bottom.width = px(4),
                  table_body.border.bottom.color = "#4c4c4c",
                  table.border.bottom.color = "transparent") %>% 
      opt_table_font(font = google_font("Karla"))
  )
  
}
