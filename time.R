# Load necessary libraries for data manipulation and visualization
library(ggplot2)
library(RColorBrewer)
library(forcats) # For ordered factors

# Create a dataframe with the Landsat sensor information, including start and end dates
sensors_data <- data.frame(
  Sensor = c("Landsat 1 - MSS", "Landsat 2 - MSS", "Landsat 3 - MSS", 
             "Landsat 4 - MSS", "Landsat 5 - MSS", "Landsat 4 - TM", "Landsat 5 - TM"),
  Start = as.Date(c("1972-07-23", "1975-01-22", "1978-03-05", 
                    "1982-07-16", "1984-03-01", "1982-07-16", "1984-03-01")),
  End = as.Date(c("1978-01-31", "1982-02-25", "1983-03-31", 
                  "1993-12-31", "1993-12-31", "1993-12-31", "2013-06-05"))
)

# Convert the 'Sensor' column to an ordered factor based on its unique values
sensors_data$Sensor <- factor(sensors_data$Sensor, levels = unique(sensors_data$Sensor))

# Construct the ggplot object with aesthetics defined for the x and y axes and fill
grafico <- ggplot(sensors_data, aes(xmin = Start, xmax = End, y = fct_inorder(Sensor), fill = Sensor)) +
  geom_rect(aes(ymin = as.numeric(fct_inorder(Sensor)) - 0.45,  # Draw rectangles without borders
                ymax = as.numeric(fct_inorder(Sensor)) + 0.45), color = NA) +
  scale_x_date(limits = c(as.Date("1972-01-01"), as.Date("2014-01-30")), # Set x-axis limits to show from 1972 to just before 2014
               name = NULL, date_breaks = "3 years", date_labels = "%Y") + # Remove axis titles
  scale_y_discrete(name = NULL) + # and customize date breaks and labels
  scale_fill_brewer(palette = "Pastel1") + # Use a pastel color palette
  labs(title = "Operational Timeline of Landsat MSS and TM Sensors") + # Add a bold and larger title
  theme_minimal() + # Use a minimal theme
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14), 
        axis.text.y = element_text(size = 14), # Increase size of axis labels
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        legend.position = "none", # Remove the legend
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Display the plot
print(grafico)

# Save the plot to a file
ggsave("export/Plots/time.png", 
       plot = grafico, 
       width = 20, 
       height = 7, 
       units = "in")
