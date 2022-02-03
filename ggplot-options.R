# http://www.sthda.com/english/wiki/ggplot2-essentials
# https://www.datanovia.com/en/blog/category/ggplot2/

font_add_google(name = "Roboto", family = "Roboto")
showtext_auto()

p +
theme_light() +
theme(text=element_text(family = "Roboto"),
             plot.title = element_text(hjust=0, size=28,face="bold"),
             plot.subtitle = element_text(hjust=0, size=20, face="italic"),
             plot.caption = element_text(h_just=0, size=18),
             axis.title = element_text(size=18),
             axis.text = element_text(size=15))
