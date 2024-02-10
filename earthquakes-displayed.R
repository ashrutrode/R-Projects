# Load the ggplot2 library
library(ggplot2)
library(ggthemes)
library(patchwork)

# View the data, learn more, see num rows
View(quakes)
?quakes
nrow(quakes)

# function to create graphs
create_graphs <- function() {

  # Create the second bubble scatter plot
  plot1 <- ggplot(quakes, aes(x = mag, y = stations, size = depth)) +
    geom_point(alpha = 0.7, color = "#0072B2") +  # Adjust point color
    geom_smooth(method = "lm", se = FALSE, color = "black", size = 1) +  # Update size to linewidth
    labs(title = "Earthquake Magnitude vs. Number of Stations",
         x = "Magnitude",
         y = "Number of Stations",
         size = "Depth") +
    theme_minimal() +  # Use a minimal theme
    theme(legend.position = "right",  # Adjust legend position
        plot.title = element_text(size = 12, face = "bold"), # Adjust title font size and style
        axis.text = element_text(size = 8), # Adjust axis text size
        axis.title = element_text(size = 8)) # Adjust axis title text size
  
  # Create a histogram for the number of earthquakes per magnitude
  plot2 <- ggplot(quakes, aes(x = mag)) +
    geom_histogram(binwidth = 0.1, fill = "#009E73", color = "black") +
    labs(title = "Magnitude Freq",
         x = "Magnitude",
         y = "Frequency") +
    theme_minimal() +  # Use a minimal theme
    theme(plot.title = element_text(size = 12, face = "bold"), # Adjust title font size and style
          axis.text = element_text(size = 8), # Adjust axis text size
          axis.title = element_text(size = 8)) # Adjust axis title text size
 
  # Create a histogram for the number of earthquakes per magnitude
  plot3 <- ggplot(quakes, aes(x = depth)) +
    geom_histogram(binwidth = 25, fill = "#D55E00", color = "black") +
    labs(title = "Depth Freq",
         x = "Depth",
         y = "Frequency") +
    theme_minimal() +  # Use a minimal theme
    theme(plot.title = element_text(size = 12, face = "bold"), # Adjust title font size and style
          axis.text = element_text(size = 8), # Adjust axis text size
          axis.title = element_text(size = 8)) # Adjust axis title text size
 
  # Create a histogram for the number of earthquakes per magnitude
  plot4 <- ggplot(quakes, aes(x = stations)) +
    geom_histogram(binwidth = 5, fill = "red", color = "black") +
    labs(title = "Num Stations Freq",
         x = "Stations",
         y = "Frequency") +
    theme_minimal() +  # Use a minimal theme
    theme(plot.title = element_text(size = 12, face = "bold"), # Adjust title font size and style
          axis.text = element_text(size = 8), # Adjust axis text size
          axis.title = element_text(size = 8)) # Adjust axis title text size
  
  # Arrange the plots using patchwork
  plot1_half <- plot1 + plot_layout(heights = c(1, 0))
  final_plot <- plot1_half / (plot2 | plot3 | plot4)
  
  # Display the final plot
  final_plot

}
create_graphs()
