pacman::p_load(tidyverse,tidygraph,ggraph,igraph,glue,ggimage,showtext,sysfonts,ggtext)

tree = tbl_graph(
  nodes=read_csv("coach_nodes.csv") %>% 
    mutate(HEAD=glue("https://nbacoaches.com/wp-content/uploads/2022/11/NBCA-HeadCoach-{gsub(' ','',NODE)}.jpg"),
           INDEX=row_number(),
           height=c(-1,rep(-2,6),rep(-3,5))),
  edges=read_csv('coach_links2.csv') %>% filter(row_number()!=8)
) 

tree %>% 
  ggraph(layout="dendrogram", height=height) +
  geom_edge_link() +
  geom_node_label(aes(label=NODE),nudge_y=-0.1165) +
  geom_image(aes(x = x, y = y, image = HEAD),size=0.07,by='height') +
  theme_void()