# http://www.sthda.com/english/wiki/ggplot2-essentials
# https://www.datanovia.com/en/blog/category/ggplot2/
# https://mran.microsoft.com/snapshot/2018-05-23/web/packages/ggimage/vignettes/ggimage.html
# https://cran.r-project.org/web/packages/ggrepel/vignettes/ggrepel.html
# https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
# https://patchwork.data-imaginist.com/
# https://cran.r-project.org/web/packages/ggfittext/ggfittext.pdf
# https://r-charts.com/distribution/ggbeeswarm/
# https://r-charts.com/ranking/ggbump/

font_add_google(name = "Roboto", family = "Roboto")
showtext_auto()

p +
theme_minimal() +
theme(text=element_text(family = "Roboto"),
             plot.title = element_text(hjust=0, size=28,face="bold"),
             plot.subtitle = element_text(hjust=0, size=20, face="italic",color="#999999"),
             plot.caption = element_text(hjust=0, size=18),
             axis.title = element_text(size=18),
             axis.text = element_text(size=15))

ggsave("name.png",p,bg="#ffffff")
