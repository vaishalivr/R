# Good to have file names without spaces
data <- read.csv("/Users/vaishali/Documents/Natural_Increase/natural_increase.csv")

# Library viridis for better color options
# Add fill in ggplot, works for pch = 21 to 25, colour = black - for black outline
# Add group aes for tooltip
# Text for tooltip to be added in aes of ggplot, does not work when added to aes 
#of geom_point()
# To modify plot backgroud directly use theme(fill - colour filled in rect. colour - 
#filled in grid lines)
# Scale_size(range, name) to control bubble size
# theme(plot.title) - hjust, vjust

library(viridis)
  p <- ggplot(data, aes(x = birth_rate, y = death_rate, size = natural_increase, 
                       fill = continents, group = country_name, 
                       text = paste("In", country_name, "\nBirth Rate:", birth_rate,
                                    "\nDeath Rate:", death_rate,
                                    "\nRate of Natural Increase:", natural_increase))) + 
   geom_point(alpha = 0.5, pch = 21, colour ="black") +
   scale_color_viridis(discrete = TRUE, option = "A") + 
   scale_size(range = c(0.03, 12), name = "Natural Increase in Population Size") +
   theme(panel.background = element_rect(fill = "#f5f5dc", colour = "#f5f5f5")) +
   ggtitle("Natural Increase In The World") +xlab("Birth Rate") +ylab("Death Rate") +
  theme(plot.title = element_text(vjust = -10, colour = "#00008b")) 
 
#Library plotly to make graphic interactive
library(plotly)
ggplotly(p, tooltip = c("text"))
