library(tidyverse)
library(showtext)

font_add_google(name = "Roboto", family = "Roboto")
showtext_auto()
theme_set(theme_light)
theme_update(text=element_text(family = "Roboto"),
             plot.title = element_text(hjust=0, size=28,face="bold"),
             plot.subtitle = element_text(hjust=0, size=20, face="italic"),
             plot.caption = element_text(h_just=0, size=18),
             axis.title = element_text(size=18),
             axis.text = element_text(size=15))





