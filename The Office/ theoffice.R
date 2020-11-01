library(schrute)
library(tidyr)
library(tidygraph)
library(ggraph)
library(glue)
library(jkmisc)
library(here)

the_office <- subset(theoffice, select = -c(1,4,5,6,8,9,10,11,12))

#to find out the characters with more than 50 times appearance
nodes <- the_office %>%
  distinct(season, episode, character) %>%
  count(character, sort = TRUE) %>%
  filter(n>50)

# to find out the characters that appear in a single episode
ep_list <- the_office %>%
  distinct(season, episode, character) %>%
  semi_join(nodes) %>%
  group_by(season, episode) %>%
  summarise(to = list(toString(character)))

# to find out how many times these characters have interacted with each other
edges <- the_office %>%
  distinct(season, episode, character) %>%
  semi_join(nodes) %>%
  inner_join(ep_list) %>%
  unnest(to) %>%
  separate_rows(to) %>%
  mutate(to = trimws(to)) %>%
  rename(from = character) %>%
  filter(from != to) %>%
  count(from, to, name = "size") %>%
  left_join(nodes, by = c('from' = 'character')) %>%
  left_join(nodes, by = c('to' = 'character')) %>%
  mutate(temp = if_else(n.x>n.y, paste0(from,to), paste0(to,from))) %>%
  distinct(temp, .keep_all = TRUE) %>%
  subset(select = -c(n.x,n.y,temp))

# to make the graph tibble
# to add as_tibble() only to check, else don't.
graph <-  tbl_graph(nodes, edges)

# finding the most important character to highlight
important <- graph %>%
  mutate(degree = centrality_degree()) %>%
  as_tibble() %>%
  arrange(desc(degree)) %>%
  top_n(1) %>%
  pull(character)

# to find out the important edges
edge_position <- which (nodes$character == important)

#setting characteristics 
plot <- graph %>%
  activate(nodes) %>%
  mutate(colour = if_else(character == important,"#17459D" ,"#646881"),
         stroke = if_else(character == important, 1.5, 0.5),
         alpha = if_else(character == important, 1.5, 0.5)) %>%
  activate(edges) %>%
  mutate(colour = if_else(from == edge_position | to == edge_position, "#17459D", "#646881"),
         stroke = if_else(from == edge_position | to == edge_position, 1.5, 0.5),
         alpha = if_else(from == edge_position | to == edge_position, 1.5, 0.5))  %>%
  ggraph(layout = "linear") +
  geom_edge_arc(aes(edge_colour = colour, edge_alpha = alpha, edge_width = size), show.legend = FALSE) +
  scale_edge_width(range = c(0.5, 3)) +
  geom_node_point(aes(colour = colour), fill = "white", shape= 21, size=18, show.legend = FALSE) +
  geom_node_text(aes(label = character), family = "American Typewriter", size = 3) +
  labs(x = NULL,
       y = NULL,
       title = glue("In *The Office*, {highlight_text('Andy', '#17459D', 'b')} is the glue binding the Main Characters together"),
       subtitle = "Andy's importance was determined using degree centrality for present characters. 
       <br> Thicker the edge, more episodes the connected characters appear in together.",
       caption = "Data: schrute package in R | <br>
                  This graphic was made under the guidance of @JakeKaupp") +
  scale_edge_width_continuous(range = c(0.5, 6)) +
  theme_jk(base_family = "American Typewriter",
           plot_title_family = "American Typewriter",
           plot_title_size = 16,
           subtitle_family = "American Typewriter",
           grid = FALSE,
           caption_family = "American Typewriter",
           markdown = TRUE) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  scale_edge_colour_identity() +
  scale_colour_identity() +
  scale_alpha_identity() +
  scale_fill_identity()
  

dev.copy(png, "TheOffice.png")
dev.print(width = 10, height = 7)
dev.off()
  



  
