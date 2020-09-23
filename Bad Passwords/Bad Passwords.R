#Passwords from TidyTuesday Week 4

library(tidygraph)
library(tidyverse)
library(janitor)
library(ggraph)
library(paletteer)
library(scales)

passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')
passwords <- na.omit(passwords)
passwords <- arrange(passwords, category)

#root - is the first node, that gives birth to several nodes
root <- tibble( to = unique(passwords$category), from = "origin")

groups <- tibble(to = passwords $ password, from = passwords$category)

order <- unique(groups$from)

edges <- bind_rows(root, groups)

nodes <- tibble(name = unique(c(edges$from, edges$to))) %>%
  mutate(group = ifelse(name == edges$to, edges$from, NA)) %>%
  mutate(size = passwords$strength[match(edges$to, passwords$password)],
         size = ifelse(name == "origin", NA, size))

graph <- tbl_graph(edges = edges, nodes = nodes, directed = TRUE)

stem_labels <- create_layout(graph, layout = 'dendrogram', circular = TRUE) %>% 
  filter(leaf == FALSE) %>% 
  mutate(group = name,
         n = count(groups, from) %>% pull(n) %>% c(NA_real_, .)) %>% 
  slice(-1) %>% 
  mutate(percent = n/sum(n, na.rm = TRUE)) %>% 
  mutate(label = str_remove(name, "simple-")) %>%
  mutate(label = str_to_upper(str_replace_all(label, "(?<=.)(?!$)", " ")))
  mutate(label = ifelse(name == "simple-alphanumeric", "A L P H A -\nN U M E R I C", label))
    
  
big_plot <- ggraph(graph, layout = 'dendrogram', circular = TRUE) +
  geom_edge_diagonal(colour = 'grey', alpha = 0.1) + 
  geom_node_text(aes(x*1.3, y*1.3, label = name, colour = group, filter = leaf,
                     angle = -((-node_angle(x, y)+90)%%180)+90,  
                     hjust = ifelse(between(node_angle(x,y), 90, 270), 0, 1)), 
                     size = 2.2) +
  geom_node_text(aes(x = x*3,y = y*3, label = label, colour = group,
                     hjust = ifelse(between(node_angle(x,y), 90, 270), 1, 0)),
                     data = stem_labels) +
  geom_node_text(aes(x = x*2.1,y = y*2.1, label = percent(percent), colour = group,
                     hjust = ifelse(between(node_angle(x,y), 90, 270), 1, 0)), 
                     data = stem_labels) +
  geom_node_point(aes(filter = leaf, colour = group, alpha = 0.1)) +
  annotate("text", x =0, y =0, label = "BAD PASSWORDS", colour = 'grey') +
  labs(x = NULL, y = NULL, 
       title = "Classification of 500 of the Worst Passwords", line = -10) +
      theme(legend.position = "none") +
      expand_limits(x = c(-1.5, 1.5), y = c(-1.5,1.5)) 
       
  
                      




