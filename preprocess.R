# Data location (Now defined in master.R)

# Wave data
# wave_folder <- "/Users/jonas/Library/Mobile Documents/com~apple~CloudDocs/Studium/phd/DATA/CovidBeliefsBehaviorsNormsSurvey/SingleWaveData"
wave_folder <- data_path # Use path from master.R

# Read all wave data (files called 'wave*.txt')
wave_files <- list.files(wave_folder, pattern = "wave.*txt", full.names = TRUE)

# Read and combine wave data files
wave_data <- lapply(wave_files, function(file) {
  df <- read.delim(file, sep="\t")
  df$wave_source <- basename(file) # Add wave source identifier
  return(df)
})

combined_wave_data <- do.call(rbind, wave_data)

# Snapshot data
# snapshot_data_file <- "/Users/jonas/Library/Mobile Documents/com~apple~CloudDocs/Studium/phd/DATA/CovidBeliefsBehaviorsNormsSurvey/SingleWaveData/snapshot.txt"
snapshot_data_file <- file.path(data_path, "snapshot.txt") # Use path from master.R
snapshot_data <- read.delim(snapshot_data_file)

# Codebooks
wave_codebook <- data.frame(
  variable = names(combined_wave_data),
  class = sapply(combined_wave_data, class),
  unique_values = sapply(combined_wave_data, function(x) length(unique(x))),
  first_values = sapply(combined_wave_data, function(x) {
    paste(head(unique(x), 5), collapse = ", ")
  })
)

snapshot_codebook <- data.frame(
  variable = names(snapshot_data),
  class = sapply(snapshot_data, class),
  unique_values = sapply(snapshot_data, function(x) length(unique(x))),
  first_values = sapply(snapshot_data, function(x) {
    paste(head(unique(x), 5), collapse = ", ")
  })
)

# Export codebooks to CSV files in output_path
wave_codebook_file <- file.path(output_path, "wave_codebook.csv")
snapshot_codebook_file <- file.path(output_path, "snapshot_codebook.csv")
write.csv(wave_codebook, wave_codebook_file, row.names = FALSE)
write.csv(snapshot_codebook, snapshot_codebook_file, row.names = FALSE)

cat(paste("Codebooks exported to", wave_codebook_file, "and", snapshot_codebook_file, "\n"))

# Find and keep common variables
common_vars <- intersect(names(combined_wave_data), names(snapshot_data))
cat("Number of common variables:", length(common_vars), "\n")

combined_wave_data_common <- combined_wave_data[, common_vars]
snapshot_data_common <- snapshot_data[, common_vars]

# Add source indicator
combined_wave_data_common$data_source <- "wave"
snapshot_data_common$data_source <- "snapshot"

# Combine datasets
all_data <- rbind(combined_wave_data_common, snapshot_data_common)

cat("Combined dataset dimensions:", dim(all_data), "\n")
cat("Combined dataset columns:", paste(names(all_data), collapse=", "), "\n")

# Filter data by date (July-November 2020)
all_data$start_date <- as.Date(all_data$start_date)

july_2020 <- as.Date("2020-07-01")
november_2020 <- as.Date("2020-11-30")
filtered_data <- all_data[all_data$start_date >= july_2020 & all_data$start_date <= november_2020, ]

cat("Original dataset rows:", nrow(all_data), "\n")
cat("Filtered dataset rows:", nrow(filtered_data), "\n")
cat("Date range in filtered data:", 
    min(filtered_data$start_date, na.rm = TRUE), "to", 
    max(filtered_data$start_date, na.rm = TRUE), "\n")

cat("Number of unique countries in filtered data:", length(unique(filtered_data$geoip_country)), "\n")

# Update all_data
all_data <- filtered_data