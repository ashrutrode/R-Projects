# Load the ggplot2 library
library(ggplot2)
library(ggthemes)

# Sample 2000 data points from the diamonds dataset
sampled_diamonds <- diamonds %>% 
  sample_n(2000)

# Determine the maximum carat and price values
max_carat <- max(sampled_diamonds$carat)
max_price <- max(sampled_diamonds$price)

# Round the maximum price value to the nearest 5000th
max_price_rounded <- ceiling(max_price / 5000) * 5000

# Plotting the diamond dataset
ggplot(data = sampled_diamonds, aes(x = carat, y = price, color = clarity)) +
  geom_point(alpha = 0.5) +
  labs(title = "Diamond Visualization",
       x = "Carat",
       y = "Price",
       color = "Clarity") + 
  theme_economist() +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 20)),  # Add space below the title
        axis.line.y = element_line(color = "black"),  # Add black line for the y-axis
        panel.grid.major = element_line(color = "white", linetype = "dotted"),  # Add light gray subgrid lines
        panel.grid.minor = element_line(color = "white", linetype = "dotted"),  # Add light gray minor grid lines
        legend.title = element_text(size = 10, margin = margin(t = 20)),  # Formatting the title
        legend.text = element_text(size = 8),   # Adjust legend text size
        legend.box.background = element_rect(fill = "transparent", color = "black")) +  # Add black border around legend box
  lims(x = c(0, max_carat), y = c(0, max_price_rounded))  # Set axis limits for x and y axes
