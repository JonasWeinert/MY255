# Master script for COVID-19 HBM Analysis Pipeline

# --- 1. Package Management ---
required_packages <- c(
  "dplyr", "lavaan", "ggplot2", "gridExtra", "scales", "forestplot", 
  "tidyr", "metafor", "data.table", "readr", "stringr", "purrr", 
  "maps", "RColorBrewer", "WDI"
)

# Check and install missing packages
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}
cat("All required packages are loaded.\n")

# --- 2. Set Seed for Reproducibility ---
set.seed(123)
cat("Seed set to 123.\n")

# --- 3. Define User Paths ---
# Please modify these paths according to your local setup
data_path <- "/Users/jonas/Library/Mobile Documents/com~apple~CloudDocs/Studium/phd/DATA/CovidBeliefsBehaviorsNormsSurvey/SingleWaveData"
# This is the base output directory. Subdirectories will be created by other scripts.
output_path <- "analysis_outputs" 

# Create the main output directory if it doesn't exist
if (!dir.exists(output_path)) {
  dir.create(output_path, recursive = TRUE)
}
cat(paste("Data path set to:", data_path, "\n"))
cat(paste("Output path set to:", output_path, "\n"))

# --- 4. Source Analysis Scripts ---
# Make sure these scripts are in the same directory as master.R or provide full paths

# Script 1: Preprocessing
cat("Running preprocess.R...\n")
source("preprocess.R")
cat("preprocess.R finished.\n")

# Script 2: Cleaning
cat("Running cleaning.R...\n")
source("cleaning.R")
cat("cleaning.R finished.\n")

# Script 3: Descriptives
cat("Running descriptives.R...\n")
source("descriptives.R")
cat("descriptives.R finished.\n")

# Script 4: Confirmatory Factor Analysis
cat("Running cfa.R...\n")
source("cfa.R")
cat("cfa.R finished.\n")

# Script 5: Structural Equation Modeling
cat("Running sem.R...\n")
source("sem.R")
cat("sem.R functions defined.\n")

# Actually run the SEM analysis using the function defined in sem.R
cat("Executing SEM country analysis...\n")
global_sem_results <- run_country_analysis(run_combined = TRUE, min_sample_size = 100) # Adjust parameters as needed
cat("SEM country analysis finished.\n")

# Script 6: Output Generation
cat("Running outputs.R...\n")
source("outputs.R")
cat("outputs.R finished.\n")

cat("Master script execution complete.\n") 