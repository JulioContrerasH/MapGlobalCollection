library(ggplot2)
library(dplyr)

# Load CSV file
tables <- list.files("export/MSS_TM", pattern = ".csv", full.names = TRUE)

# Extract file names for IDs
tb_names <- stringr::str_extract(tables, "(?<=/)[^/]+(?=\\.csv$)")

# Initialize dataframe
df <- data.frame(id = tb_names)

# Read each CSV, calculate date counts, and accumulate them
for(i in 1:length(tables)){
  table <- read.csv(tables[i])
  # Increment count by 1 for each date (considering NA as 0)
  table$DATE_COUNT <- ifelse(is.na(table$DATE), 0, stringr::str_count(table$DATE, "/") + 1)
  df$count[i] <- sum(table$DATE_COUNT)
}

# Assign mission, tier, and sensor values to the dataframe
mission <- c("L1", "L1", "L2", "L2", "L3", "L3", "L4", 
             "L4", "L5", "L5", "L4", "L4", "L5", "L5")
tier <- c("T1", "T2", "T1", "T2", "T1", "T2", "T1", 
          "T2", "T1", "T2", "T1", "T2", "T1", "T2")
sensor <- c("MSS", "MSS", "MSS", "MSS", "MSS", "MSS", "MSS", 
            "MSS", "MSS", "MSS", "TM", "TM", "TM", "TM")
df$mission <- mission
df$tier <- tier
df$sensor <- sensor

# Filter and mutate data for MSS and TM sensors, then combine
df_mss <- df %>% 
  filter(sensor == "MSS") %>% 
  mutate(tier_sensor = paste(tier, "MSS", sep = "-"))
df_tm <- df %>% 
  filter(sensor == "TM") %>% 
  mutate(tier_sensor = paste(tier, "TM", sep = "-"))
df_combined <- rbind(df_mss, df_tm)

# Update sensor column to factor with specific levels
df_combined$sensor <- factor(df_combined$sensor, 
                             levels = c("MSS", "TM"))

# Create faceted bar plot with customized aesthetics
grafico_combined <- ggplot(df_combined, 
                           aes(x = mission, 
                           y = count, 
                           fill = tier_sensor)) +
  geom_col(position = position_dodge(width = 0.7), 
           alpha = 0.8, 
           width = 0.7) +
  scale_fill_manual(values = c("T1-MSS" = "#377EB8", 
                               "T2-MSS" = "#E41A1C", 
                               "T1-TM" = "#03D7CA", 
                               "T2-TM" = "#FF5804")) +
  facet_grid(~sensor, 
             scales = "free_x", 
             space = "free_x", 
             switch = "x") +
  scale_y_continuous(trans = 'log10',
                     breaks = c(0, 10, 100, 1000, 10000, 100000, 1000000),
                     labels = scales::label_scientific(accuracy = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12, face = "bold", vjust = 20), 
        axis.title.x = element_blank(), 
        axis.text.y = element_text(size = 12, face = "bold"), 
        axis.title.y = element_text(size = 16, face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 24), 
        strip.background = element_blank(),
        strip.text.x = element_text(size = 16, face = "bold"), 
        legend.position = c(0.15, 0.93),
        legend.background = element_rect(fill = "white", colour = "black", size = 0.2), 
        legend.key = element_rect(fill = "white", colour = NA),
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 13, face = "bold")) +
  labs(title = "Number of Images by Mission", 
       y = "Number of Images", 
       fill = "Tier and Sensor")

grafico_combined

# Save the plot
ggsave("export/Plots/bar.png", 
       plot = grafico_combined, 
       width = 15, 
       height = 13, 
       units = "in")
