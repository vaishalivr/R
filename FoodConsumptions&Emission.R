# Credit to one of the masters of R @jakekaupp

library(magrittr)
library(dplyr)
library(tidyverse)
library(scales)
library(glue)
library(ggtext)
library(here)

food_consumption <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')

# factor() is used to represent catagorical data
# storing catagorical data as factor() ensures modelling function will treart data correctly

plot_data <- food_consumption %>% 
  group_by(food_category) %>%
  summarise_at(vars(consumption:co2_emmission), sum) %>%
  mutate(line_colour = case_when(food_category %in% 
                              c("Beef", "Lamb & Goat", "Milk - inc. cheese", "Pork") ~ "#DD2A7B",
                            TRUE ~ "#a9a9a9")) %>%
  mutate(line_size = case_when(food_category %in% 
                            c("Beef", "Lamb & Goat", "Milk - inc. cheese", "Pork") ~ 0.4,
                          TRUE ~ 0.2)) %>%
  mutate(food_category = case_when(food_category == "Milk - inc. cheese" ~ "Milk (Incl. Cheese)",
                                   food_category == "Nuts inc. Peanut Butter" ~ "Nuts (Incl. Peanut Butter)",
                                   food_category == "Wheat and Wheat Products" ~ "Wheat & Wheat Products",
                                   TRUE ~ food_category)) 

keeps_con <- c("food_category", "consumption", "size")
other_foods_consumption <- plot_data[keeps_con] %>%
  filter(!(food_category %in% c("Milk (Incl. Cheese)", "Beef", "Pork", "Lamb & Goat", "Wheat & Wheat Products"))) %>%
  arrange(desc(consumption)) %>%
  summarize(label = paste(food_category, collapse = "\n"),
            consumption = consumption[food_category == "Rice"])

keeps_co2 <- c("food_category", "co2_emmission", "size")
other_foods_co2 <- plot_data[keeps_co2] %>%
  filter(!(food_category %in%  c("Beef", "Milk (Incl. Cheese)", "Lamb & Goat", "Pork"))) %>%
  arrange(desc(co2_emmission)) %>%
  summarise(label = paste(food_category, collapse = "\n"),
            co2_emmission = co2_emmission[food_category == "Eggs"])

keeps_lab_con <- c("food_category", "consumption", "colour", "size")
label_con <- plot_data[keeps_lab_con] %>%
  filter(food_category %in% c("Milk (Incl. Cheese)", "Wheat & Wheat Products"))

keeps_lab_co2 <- c("food_category", "co2_emmission", "colour", "size")
label_co2 <- plot_data[keeps_lab_co2] %>%
  filter(food_category %in% c("Beef", "Milk (Incl. Cheese)", "Pork", "Lamb & Goat"))

grid_lines <- tibble(x = rep(0, length(seq(0,50000,10000))),
                     xend = rep(1, length(seq(0,50000,10000))),
                      y = seq(0,50000,10000),
                     yend = seq(0,50000,10000)) %>%
  mutate(label = comma(y)) %>%
  mutate(label = if_else(y %in% range(y), glue("{comma(y)} kg/person per year"), label))

# when colour and size are part of dataframe, aes() will only take plotting values.
# all other parameters to be defined outside os aes()

plot <- ggplot(data = plot_data) +
  geom_vline(xintercept = c(0,1), colour = "#a9a9a9") + 
  scale_y_continuous(limits = c(-1000, 55000)) + 
  scale_x_continuous(limits = c(-1,2)) +
  geom_text(label = "Consumption", x=0, y=1.17*(max(plot_data$consumption, plot_data$co2_emmission)), size = 3.5, colour = "#a9a9a9") +
  geom_text(label = expression(paste("C", O[2], " Emmissions")), x=1, y=1.17*(max(plot_data$consumption, plot_data$co2_emmission)), size = 3.5, colour = "#a9a9a9") +
  geom_segment(aes(x=0, xend=1, y=consumption, yend=co2_emmission), color=plot_data$line_colour, size=plot_data$line_size) +
  geom_point(aes(x=0, y=consumption), colour=plot_data$line_colour) +
  geom_point(aes(x=1, y=co2_emmission), colour=plot_data$line_colour) +
  geom_text(data=other_foods_consumption, aes(label=label, x=-0.05, y=consumption), hjust = 1, size = 2.5, nudge_y = -1200, colour = "#a9a9a9") +
  geom_text(data=other_foods_co2, aes(label=label, x=1.05, y=co2_emmission), hjust = 0, size = 2.5, nudge_y = 2200, colour = "#a9a9a9") +
  geom_text(data=label_con, aes(label=food_category, x=0, y=consumption), size=2.5, nudge_x = c(-0.14,-0.17), colour = "#a9a9a9") +
  geom_text(data=label_co2, aes(label=food_category, x=1, y=co2_emmission), size=2.5, nudge_x = c(0.06,0.11,0.15,0.075), colour = "#a9a9a9") +
  geom_segment(data = grid_lines, aes(x=x, xend=xend, y=y, yend=yend), linetype = "dashed", size = 0.2, colour = "#a9a9a9") +
  geom_text(data = grid_lines, aes(x=x, y=y, label=label), hjust = -0.1, nudge_x = c(-0.02, rep(0,4), -0.025), nudge_y = c(-1000, rep(700,5)), size = 3, colour = "#a9a9a9") +
  labs(x=NULL, 
       y=NULL,
       title = (expression(paste('While Low in Consumption, Meat & Dairy Outweigh Other Foods in Their Contribution to C', O[2],' Emissions',sep=''))),
       subtitle = glue("Illustrated below is a slopegraph showing consumption vs carbon dioxide emmission of different food products as in 2019"),
       caption = "Data: TidyTuesday | Graphic: Vaishali Verma") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(colour = "#a9a9a9", hjust = 0.5),
        plot.subtitle = element_text(colour = "#a9a9a9", hjust = 0.5),
        plot.caption = element_text(colour = "#a9a9a9"))
  
ggsave(here("FoodConsumption&Emission.png"), plot, height = 14, width = 11.5)

       