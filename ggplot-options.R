library(tidyverse)
library(showtext)

font_add_google(name = "Roboto", family = "Roboto")
showtext_auto()
theme_set(theme_bw)
theme_update(text=element_text(family = 'Roboto'),
             plot.title = element_text(hjust=0.5, size=28,face="bold"),
             plot.subtitle = element_text(hjust=0.5, size=20),
             axis.title = element_text(size=18),
             axis.text = element_text(size=15))





