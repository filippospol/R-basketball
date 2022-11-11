pacman::p_load(gt,gtExtras,ggplot2,sysfonts,showtext)

######################################################################################################
GT TABLE
######################################################################################################


gt() %>% 
  cols_align(align = "center") %>% 
  cols_label() %>%  # old=new
  tab_header(title=md("**Title**"),
             subtitle=html("<div style='color:#8e8e8e;'>Subtitle</div><br/>")) %>% 
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()))  %>%  
  gt_img_rows(columns =, height =) %>%
  gt_highlight_rows(rows = 2, font_weight = "normal") %>%
  fmt_percent(columns = , decimals = 1) %>% 
  tab_footnote(footnote = "", locations = cells_column_labels()) %>%
  gt_color_rows(c(), palette = "ggsci::blue_material") %>%
  tab_source_note(source_note = "") %>% 
  tab_options(data_row.padding = px(1),
              row.striping.background_color = "#eeeeee",
              heading.align = "left",
              heading.title.font.size = 40,
              heading.subtitle.font.size = 20,
              table.font.size = 15,
              table.border.top.color = "transparent",
              # heading.border.bottom.width = px(3),
              # heading.border.bottom.color = "#4c4c4c",
              heading.border.bottom.color = "transparent",
              column_labels.border.bottom.width = px(3),
              column_labels.border.bottom.color = "#4c4c4c",
              table_body.border.bottom.width = px(3),
              table_body.border.bottom.color = "#4c4c4c",
              table.border.bottom.color = "transparent") %>% 
  opt_table_font(font = google_font("Rubik")) %>% 
  opt_row_striping()
  
  
  
######################################################################################################
GGPLOT
######################################################################################################

font_add_google("Source Sans Pro")
showtext_auto()

ggplot() %>%
  theme_minimal() +
  theme(text=element_text(family="Source Sans Pro"),
        plot.title=element_text(size=,face="bold"),
        plot.subtitle=element_text(size=,color="#777777"),
        plot.caption=element_text(size=,color="#777777"))