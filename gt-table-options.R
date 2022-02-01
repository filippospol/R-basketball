# https://themockup.blog/static/gt-cookbook.html
# https://gt.rstudio.com/articles/intro-creating-gt-tables.html

gt() %>% 
  
  # column alignment
  cols_align(
    align = "left" 
  ) %>%
  
  # convert path to .png
  text_transform(
    locations = cells_body(), 
    fn = function(x) {
      lapply(x, local_image)
    }
  )  %>%
  
  # convert link to .png
  text_transform(
    locations = cells_body(),
    fn = function(x) {
      web_image(url = x) 
    }
  )  %>%
  
  # convert decimal number to %
  fmt_percent(
    columns = ,
    decimals = 1
  ) %>%
  
  # change column labels, old=new
  cols_label(
    ) %>% 
  
  # md(): 4 stars for bold, 2 for italic, <br> for new line
  # title, subtitle 
  tab_header(
    title=,
    subtitle=
  ) %>% 
  
  # caption
  tab_source_note(
    source_note=
  ) %>% 
  
  # modify table options
  tab_options(
  ) %>% 
  
  # add a Google font
  opt_table_font(
    font = google_font("Roboto")
  ) 
