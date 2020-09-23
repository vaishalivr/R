#to copy the source file with it in GitHub

data <- LifeExpectancy

data <-  data %>% 
pivot_wider(names_from = Year, values_from = Value)
colnames(data) <- c("Country", "Year2013", "Year1990")

lab1990 <- paste(data$Country, round(data$Year1990), sep = ", ")
lab2013 <- paste(data$Country, round(data$Year2013), sep = ", ")

# when geom_text is used to add labels, no need to use aes()
plot <- ggplot(data) + 
  geom_segment(aes(x = 0, xend = 23, y = Year1990, yend = Year2013), size = 0.5) +
  ggtitle("Life Expectancy at Birth: 1990 & 2013") + 
  xlim(-8, (23+8)) + ylim(min(data$Year1990, data$Year2013),(max(data$Year1990)+8)) +
  theme(panel.background = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  geom_text(label = lab1990, x = rep.int(0, length(data$Year1990)), y = data$Year1990, hjust = 1.1, size = 3) +
  geom_text(label = lab2013, x = rep.int(23, length(data$Year2013)), y = data$Year2013, hjust = -0.15, size = 3) +
  geom_text(label = "In 1990", x = 0, y = 1.05*(max(data$Year1990, data$Year2013)), hjust = 0.2, size = 5) +
  geom_text(label = "In 2013", x = 23, y = 1.05*(max(data$Year2013, data$Year1990)), hjust = 0.2, size = 5) +
  labs(caption = "Data Source: UN Data, Life Expectancy at Birth, total (years)")
            
  
