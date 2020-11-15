library(dplyr)
library(ggplot2)
library(viridis)
library(cowplot)
library(extrafont)

font_import(pattern = 'Roboto')

mobile <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/mobile.csv')
landline <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/landline.csv')

asia <- mobile %>%
  distinct(continent, entity) %>%
  filter(continent == "Asia")
asia <- unique(asia$entity)

asia_mobile <- mobile %>%
  filter(entity %in% asia) %>%
  na.omit()

asia_landline <- landline %>%
  filter(entity %in% asia) %>%
  filter(year != 2018 & year != 2019) %>%
  na.omit() 

plot_landline <- ggplot(asia_landline) +
  geom_tile(aes(year, entity, fill = landline_subs)) +
  scale_fill_viridis(option = 'magma') +
  scale_y_discrete(rev(levels(asia_landline$entity))) +
  scale_x_continuous(breaks = c(1990,1995,2000,2005,2010,2015), labels = c("1990", "", "2000", "", "2010","")) +
  theme_minimal() +
  labs(title = "Landline Subscriptions per 100 People", x = "YEAR", y = "") +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 12, colour = "white"),
    axis.text.x = element_text(size =12, face = "bold", colour = "white"),
    axis.text.y = element_text(size =9, colour = "white"),
    axis.ticks.length.x = unit(0.2, "cm"),
    axis.ticks.x = element_line(colour = "white"),
    plot.background = element_rect(fill = 'grey39', colour = NA),
    panel.background = element_rect(fill = 'grey39', colour = NA),
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold", colour = "white"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.key.width = unit(1.5, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.text = element_text(size = 12, face = "bold", colour = "white"),
  )

plot_mobile <- ggplot(asia_mobile) +
  geom_tile(aes(year, entity, fill = mobile_subs)) +
  scale_fill_viridis(option = "magma") +
  scale_y_discrete(rev(levels(asia_mobile$entity))) +
  scale_x_continuous(breaks = c(1990,1995,2000,2005,2010,2015), labels = c("1990", "", "2000", "", "2010","")) +
  theme_minimal() +
  labs(title = "Mobile Subscriptions per 100 People", x = "YEAR", y = "") +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 12, colour = "white"),
    axis.text.x = element_text(size =12, face = "bold", colour = "white"),
    axis.text.y = element_text(size =9, colour = "white"),
    axis.ticks.length.x = unit(0.2, "cm"),
    axis.ticks.x = element_line(colour = "white"),
    plot.background = element_rect(fill = 'grey39', colour = NA),
    panel.background = element_rect(fill = 'grey39', colour = NA),
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold", colour = "white"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.key.width = unit(1.5, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.text = element_text(size = 12, face = "bold", colour = "white"),
  )

plots <- plot_grid(plot_landline, plot_mobile)

ggsave("phone_subs.png", width = 35, height = 20, units = "cm")
