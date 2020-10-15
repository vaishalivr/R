library(dplyr)
library(tidyverse)
library(tidygraph)
library(ggraph)

letters <- correspondence_data_1585

sources <- letters %>%
  distinct(source) %>%
  rename(label = source)
  
destinations <- letters %>%
  distinct(destination) %>%
  rename(label = destination)

nodes <- full_join(sources, destinations, by="label") %>%
  rowid_to_column("id")

per_route <- letters %>%
    group_by(source, destination) %>%
    count(source, destination) %>%
    rename(weight = n)

edges <- per_route %>%
  left_join(nodes, by=c("source" = "label")) %>%
  rename("from" = "id") %>%
  left_join(nodes, by=c("destination" = "label")) %>%
  rename("to" = "id") 

keeps <- c('from', 'to', 'weight')
edges <- edges[keeps]

routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

plot <- ggraph(routes_tidy, layout = "linear") + 
  geom_edge_arc(aes(width = weight), alpha = 0.8) +
  geom_node_text(aes(label = label)) +
  scale_edge_width(range = c(0.2,2)) +
  theme_graph() +
  labs(
    title = "Letters received by Daniel van der Meulen in 1585",
    subtitle = "Illustrated Below is my First Network Diagram")
