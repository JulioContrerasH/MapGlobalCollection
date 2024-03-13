# Load the necessary CSV data into sensor_data dataframe
sensor_data <- read.csv("data/csv/spectral_response_function.csv", sep = ";")

# Load the ggplot2 library for plotting
library(ggplot2)

# Ensure that 'Sensor', 'BandNumber', and 'Band' are treated as factors
sensor_data$Sensor <- as.factor(sensor_data$Sensor)
sensor_data$BandNumber <- as.factor(sensor_data$BandNumber)
sensor_data$Band <- as.factor(sensor_data$Band)

# Create a dataframe for placing text labels on the plot
text_labels <- data.frame(
  x = c(0.55, 0.65, 0.75, 0.87), # Hypothetical wavelengths for the labels
  y = rep(1.02, 4), # Slightly above the maximum value of 'Average' for visibility
  labels = c("Green", "Red", "NIR 1", "NIR 2") # Label text
)

# Construct the plot
p <- ggplot(sensor_data, aes(x = Wavelength, y = Average, group = interaction(Band, Sensor), color = Sensor, linetype = Sensor)) +
  geom_line() +
  geom_text(data = text_labels, aes(x = x, y = y, label = labels), vjust = -1, inherit.aes = FALSE) + # Add text without inheriting global aesthetics
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        legend.title = element_text(hjust = 0.5),
        legend.position = c(0.82, 0.8),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.box.background = element_rect(colour = "black")) +
  labs(title = "Relative spectral responses of Landsat 1-5 MSS",
       x = "Wavelength (µm)",
       y = "Relative Spectral Response") +
  scale_color_manual(values = c("L1 MSS" = "blue", "L2 MSS" = "red", "L3 MSS" = "green", "L4 MSS" = "purple", "L5 MSS" = "orange")) +
  scale_linetype_manual(values = c("L1 MSS" = "dashed", "L2 MSS" = "dashed", "L3 MSS" = "dashed", "L4 MSS" = "dashed", "L5 MSS" = "dashed")) +
  guides(color = FALSE) # Remove the color legend

# Display the plot
p

# Save the plot to a file
ggsave("export/Plots/srf.png", 
       plot = p, 
       width = 20, 
       height = 10, 
       units = "in")
