# Load required libraries
library(sf)
library(rgee)
library(stringr)
library(ggplot2)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)

# Initialize Google Earth Engine
ee$Initialize()

#' Add Path-Row Identifier to Images
#'
#' This function adds a PATH_ROW identifier to each image in the ImageCollection.
#' It formats the WRS_PATH and WRS_ROW metadata of the image to a three-digit
#' string and combines them with a dash.
#'
#' @param image Image to add PATH_ROW identifier to.
#' @return Image with added PATH_ROW property.
add_path_row <- function(image) {
  path <- ee$String(ee$Number(image$get('WRS_PATH'))$format('%03d'))
  row <- ee$String(ee$Number(image$get('WRS_ROW'))$format('%03d'))
  path_row <- path$cat('-')$cat(row)
  return(image$set('PATH_ROW', path_row))
}

#' Process Google Earth Engine Data
#'
#' Retrieves PATH_ROW and DATE_ACQUIRED information from an ImageCollection,
#' aggregates dates per path-row, and exports the result to a CSV file.
#'
#' @param dataset_snippet Google Earth Engine dataset identifier.
#' @param csv_file_path Path to save the resulting CSV file.
process_gee_data <- function(dataset_snippet, csv_file_path) {
  ic <- ee$ImageCollection(dataset_snippet)
  ic_with_path_row <- ic$map(add_path_row)
  
  path_row <- ic_with_path_row$reduceColumns(ee$Reducer$toList(), list("PATH_ROW"))$getInfo()
  date <- ic_with_path_row$reduceColumns(ee$Reducer$toList(), list("DATE_ACQUIRED"))$getInfo()
  
  path_row <- as.character(unlist(path_row))
  date <- as.character(unlist(date))
  
  df <- data.frame(PATH_ROW = path_row, DATE = date, stringsAsFactors = FALSE)
  df_grouped <- aggregate(DATE ~ PATH_ROW, data = df, FUN = function(x) paste(x, collapse = "/"))
  
  write.csv(df_grouped, csv_file_path, row.names = FALSE)
}

# Define dataset snippets and output paths
snippets <- c(
  "LANDSAT/LM01/C02/T1",
  "LANDSAT/LM01/C02/T2",
  "LANDSAT/LM02/C02/T1",
  "LANDSAT/LM02/C02/T2",
  "LANDSAT/LM03/C02/T1",
  "LANDSAT/LM03/C02/T2",
  "LANDSAT/LM04/C02/T1",
  "LANDSAT/LM04/C02/T2",
  "LANDSAT/LM05/C02/T1",
  "LANDSAT/LM05/C02/T2",
  "LANDSAT/LT04/C02/T1_TOA",
  "LANDSAT/LT04/C02/T2_TOA",
  "LANDSAT/LT05/C02/T1_TOA",
  "LANDSAT/LT05/C02/T2_TOA"
)
output_paths <- paste0("export/MSS_TM/", gsub("/", "_", snippets), ".csv")

# Process each snippet
for (i in seq_along(snippets)) {
  process_gee_data(snippets[i], output_paths[i])
}

# Read and format WRS data
wrs <- st_read("/home/huerta/Documents/Repos/MapsMSS/data/WRS/WRS2_descending.shp")
formatted_path <- sprintf("%03d", wrs$PATH) 
formatted_row <- sprintf("%03d", wrs$ROW)
wrs["PATH_ROW"] <- paste(formatted_path, formatted_row, sep = "-")

# Merge WRS data with image acquisition dates
table <- read.csv("export/TM/LANDSAT_LT05_C02_T2_TOA.csv")
wrs <- merge(wrs, table, by = "PATH_ROW", all.x = TRUE)

# Count dates per path-row
wrs$DATE_COUNT <- ifelse(is.na(wrs$DATE), 0, str_count(wrs$DATE, "/") + 1)

# Transform and plot data
wrs <- st_transform(wrs, crs = 8857)
wrs$COUNT_FACTORIZED <- cut(wrs$DATE_COUNT,
                            breaks = c(0, 1, 5, 20, 80, 320, Inf),
                            labels = c("Not image", "1-5", "6-20", "21-80", "81-320", "320-600"),
                            include.lowest = TRUE)

# Create a map visualization
borders <- ne_countries(scale = "medium", returnclass = "sf")
borders <- st_transform(borders, crs = 8857)
extend <- st_union(wrs)
extend <- st_transform(extend, crs = 8857)

grafico <- ggplot(wrs) +
  geom_sf(aes(fill = COUNT_FACTORIZED), color = NA) + 
  geom_sf(data = borders, fill = NA, color = "black", size = 0.25) +
  geom_sf(data = extend, fill = NA, color = "black", size = 0.25) +        
  scale_fill_manual(values = viridis::viridis_pal()(6), drop = FALSE) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Landsat 5 TM Tier 2 Global Acquisitions", fill = NULL)

# Save the plot
ggsave("export/TM/LANDSAT_LT05_C02_T2_TOA.png", plot = grafico, width = 6, height = 4, units = "in")
