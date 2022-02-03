# https://themockup.blog/static/gt-cookbook.html
# https://gt.rstudio.com/articles/intro-creating-gt-tables.html

gt() %>% 

  cols_align(
    align = "left" 
  ) %>%
  #
  text_transform(
    locations = cells_body(), 
    fn = function(x) {
      lapply(x, local_image)
    }
  ) %>%
  #
  text_transform(
    locations = cells_body(),
    fn = function(x) {
      web_image(url = x) 
    }
  )  %>%
  #
  fmt_percent(
    columns = ,
    decimals = 1
  ) %>%
  #
  cols_label(
    ) %>% 
  # md(): 4 stars for bold, 2 for italic, <br> for new line
  tab_header(
    title=,
    subtitle=
  ) %>% 
  #
  tab_source_note(
    source_note=
  ) %>% 
  #
  tab_options(
  ) %>% 
  #
  opt_table_font(
    font = google_font("Roboto")
  ) 
