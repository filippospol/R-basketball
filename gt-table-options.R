gt() %>% 
  
  # column alignment
  cols_align(
    align = "left" 
  ) %>%
  tab_style(
    style = cell_borders(
      sides = 'right',
      color = "#000000",
      weight = px(1.4),
      style = "solid"
    ),
    locations = cells_body(
      columns = 
    )
  ) %>%
  
  # column border for cells and labels
  tab_style(
    style = cell_borders(
      sides = 'right',
      color = "#000000",
      weight = px(1.4),
      style = "solid"
    ),
    locations = cells_column_labels()
  )  %>% 
  
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
  
  # change row color
  tab_style(
    style = list(
      cell_fill(color = "#f2f2f2")
    ),
    locations = cells_body(
      rows = seq(1,9,2)
    )
  ) %>%
  
  # change column labels, old_name = new_name
  cols_label(
    ) %>% 
  
  # title, subtitle
  tab_header(
    title=md("****"),
    subtitle=md("**")
  ) %>% 
  
  # caption
  tab_source_note(
    md("")
  ) %>% 
  
  # other table options
  tab_options(
    heading.title.font.size = 32,
    heading.subtitle.font.size = 17,
    heading.align = "left",
    column_labels.font.size = 16,
    column_labels.font.weight = 'bold',
    table.font.size = 15,
    data_row.padding = px(3),
    table.border.top.width = px(2),
    table.border.bottom.width = px(2),
    column_labels.border.top.width = px(1.4),
    column_labels.border.bottom.width = px(1.4),
    table_body.border.bottom.width = px(1.4)
  ) %>% 
  
  # add a Google font
  opt_table_font(
    font = list(
      google_font(name = "Roboto"),
      default_fonts())) 
