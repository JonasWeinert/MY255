# outputs.R: Consolidates SEM results and generates final plots and tables.

# --- Configuration: Input and Output Directories ---
# Assumes output_path is defined globally by master.R

# Input: Directory where sem.R saved its results
sem_results_input_dir <- file.path(output_path, "sem_results")
sem_by_country_input_dir <- file.path(sem_results_input_dir, "by_country")

# Output: Base directory for this script's outputs
final_outputs_base_dir <- file.path(output_path, "final_summary_outputs")

# Specific output subdirectories
consolidated_data_dir <- file.path(final_outputs_base_dir, "consolidated_data")
plots_dir <- file.path(final_outputs_base_dir, "plots")
maps_plots_dir <- file.path(plots_dir, "maps")
scatter_plots_dir <- file.path(plots_dir, "scatterplots")
gdp_plots_dir <- file.path(plots_dir, "gdp_plots") # Combined GDP and specific effect vs GDP
filtered_effects_plots_dir <- file.path(plots_dir, "filtered_effects_plots")

# Create all output directories recursively
dir.create(consolidated_data_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(maps_plots_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(scatter_plots_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(gdp_plots_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(filtered_effects_plots_dir, showWarnings = FALSE, recursive = TRUE)

cat(paste("outputs.R: Inputting SEM results from:", sem_results_input_dir, "\n"))
cat(paste("outputs.R: Saving final summaries and plots to:", final_outputs_base_dir, "\n"))

# --- 1. Consolidate SEM Results --- 
list_sem_output_files <- function(sub_dir, file_pattern) {
  target_dir <- file.path(sem_by_country_input_dir, sub_dir)
  # List files directly in country subdirs, not one level deeper
  country_subdirs <- list.dirs(sem_by_country_input_dir, full.names = TRUE, recursive = FALSE)
  
  all_files <- unlist(lapply(country_subdirs, function(csd) {
    list.files(path = csd, pattern = file_pattern, full.names = TRUE)
  }))
  return(all_files)
}

# Adjusted pattern to find files like direct_effects_COUNTRY.csv
direct_effect_files <- list.files(sem_by_country_input_dir, pattern = "direct_effects_.*\\.csv$", recursive = TRUE, full.names = TRUE)
indirect_effect_files <- list.files(sem_by_country_input_dir, pattern = "indirect_effects_.*\\.csv$", recursive = TRUE, full.names = TRUE)

message(paste("Found", length(direct_effect_files), "direct effect CSVs for consolidation."))
message(paste("Found", length(indirect_effect_files), "indirect effect CSVs for consolidation."))

read_and_tag_country <- function(file_path) {
  # Extract country from filename like: .../direct_effects_COUNTRY.csv
  country_name <- sub(".csv$", "", sub(".*_", "", basename(file_path)))
  df <- read_csv(file_path, show_col_types = FALSE)
  if (!("Country" %in% names(df))) {
    df$Country <- country_name
  }
  return(df)
}

if (length(direct_effect_files) > 0) {
  all_direct_effects <- map_df(direct_effect_files, read_and_tag_country)
  write_csv(all_direct_effects, file.path(consolidated_data_dir, "all_direct_effects_consolidated.csv"))
  saveRDS(all_direct_effects, file.path(consolidated_data_dir, "all_direct_effects_consolidated.rds"))
  message(paste("Consolidated direct effects saved. Rows:", nrow(all_direct_effects), "Unique countries:", length(unique(all_direct_effects$Country))))
} else {
  message("No direct effect files found in SEM output for consolidation.")
  all_direct_effects <- data.frame() # Ensure it exists for later checks
}

if (length(indirect_effect_files) > 0) {
  all_indirect_effects <- map_df(indirect_effect_files, read_and_tag_country)
  write_csv(all_indirect_effects, file.path(consolidated_data_dir, "all_indirect_effects_consolidated.csv"))
  saveRDS(all_indirect_effects, file.path(consolidated_data_dir, "all_indirect_effects_consolidated.rds"))
  message(paste("Consolidated indirect effects saved. Rows:", nrow(all_indirect_effects), "Unique countries:", length(unique(all_indirect_effects$Country))))
} else {
  message("No indirect effect files found in SEM output for consolidation.")
  all_indirect_effects <- data.frame() # Ensure it exists
}

# --- 2. Visualizations (only if data was loaded) ---
if (nrow(all_direct_effects) > 0 && "Variance_explained" %in% names(all_direct_effects)) {
  message("Proceeding with visualizations using consolidated direct effects data...")

  # --- 2a. World Map: Variance Explained ---
  variance_map_data <- all_direct_effects %>%
    distinct(Country, Variance_explained) %>% # One row per country for this
    filter(!is.na(Variance_explained)) %>%
    mutate(
      R_squared = suppressWarnings(as.numeric(gsub("%", "", Variance_explained))),
      R_squared = ifelse(is.na(R_squared) | R_squared < 0 | R_squared > 100, NA, R_squared) # R_squared is 0-100 here
    )
  
  world_map_base <- map_data("world")
  # Simple mapping, extend if necessary for your country names vs. map names
  country_name_map <- data.frame(Country = unique(variance_map_data$Country), map_region = unique(variance_map_data$Country))
  country_name_map$map_region <- case_when(
    country_name_map$map_region == "United Kingdom" ~ "UK",
    country_name_map$map_region == "United States" ~ "USA",
    TRUE ~ country_name_map$map_region
  )
  variance_map_data <- variance_map_data %>% left_join(country_name_map, by = "Country")

  variance_plot <- ggplot() +
    geom_map(data = world_map_base, map = world_map_base, aes(long, lat, map_id = region), color = "grey80", fill = "grey95", linewidth = 0.1) +
    geom_map(data = variance_map_data, map = world_map_base, aes(map_id = map_region, fill = R_squared), color = "grey50", linewidth = 0.1) +
    scale_fill_distiller(palette = "Blues", direction = 1, name = "R² (%)", na.value = "grey85", limits = c(0, max(variance_map_data$R_squared, na.rm=TRUE))) +
    labs(title = "SEM Model: Variance Explained (R²) in Adherence by Country") +
    theme_void() + theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
  ggsave(file.path(maps_plots_dir, "variance_explained_world_map.png"), variance_plot, width = 12, height = 7, dpi = 300)
  message("Saved Variance Explained world map.")

  # --- 2b. Scatterplots: Direct Effects by Country ---
  hbm_constructs <- unique(all_direct_effects$Variable)
  individual_effect_plots <- list()
  for (construct_name in hbm_constructs) {
    plot_data <- all_direct_effects %>% filter(Variable == construct_name, !is.na(Direct_beta)) %>% arrange(Direct_beta)
    if(nrow(plot_data) < 1) next
    plot_data$plot_order <- 1:nrow(plot_data)
    label_threshold <- quantile(abs(plot_data$Direct_beta), probs = 0.90, na.rm = TRUE) # Label top 10%
    plot_data$country_label <- ifelse(abs(plot_data$Direct_beta) >= label_threshold, as.character(plot_data$Country), "")
    
    effect_scatter <- ggplot(plot_data, aes(x = plot_order, y = Direct_beta)) +
      geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2, color = "grey70", na.rm = TRUE) +
      geom_point(size = 2.5, color = "steelblue4") +
      ggrepel::geom_text_repel(aes(label = country_label), size = 3, max.overlaps = Inf, na.rm = TRUE) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      theme_minimal(base_size = 10) +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), panel.grid.major.x = element_blank()) +
      labs(title = paste("Direct Effect of", construct_name, "on Adherence"), x = "Countries (Ordered by Effect Size)", y = "Standardized Beta (β)")
    individual_effect_plots[[construct_name]] <- effect_scatter
    ggsave(file.path(scatter_plots_dir, paste0("effect_scatter_", construct_name, ".png")), effect_scatter, width = 8, height = 5, dpi = 300)
  }
  if (length(individual_effect_plots) > 0) {
    combined_effects_plot <- gridExtra::grid.arrange(grobs = individual_effect_plots, ncol = min(3, length(individual_effect_plots)))
    ggsave(file.path(scatter_plots_dir, "all_effects_scatter_combined.png"), combined_effects_plot, width = 15, height = max(5, 4 * ceiling(length(individual_effect_plots)/3)), dpi = 300, limitsize=FALSE)
    message("Saved individual and combined direct effect scatterplots.")
  }
  
  # --- 2c. GDP Analysis ---
  message("Downloading GDP per capita data (NY.GDP.PCAP.CD) from World Bank...")
  # Use WDIcache for potential caching to speed up repeated runs
  # WDIsearch("gdp per capita.*current US")
  gdp_raw_data <- WDI(indicator = "NY.GDP.PCAP.CD", country = "all", start = 2019, end = 2021) # Fetch a couple of recent years
  latest_gdp_data <- gdp_raw_data %>% 
    filter(!is.na(NY.GDP.PCAP.CD)) %>% 
    group_by(iso2c, country) %>% 
    arrange(desc(year)) %>% 
    slice(1) %>% ungroup() %>% 
    select(Country_WB = country, iso2c, Year_GDP = year, GDP_per_capita = NY.GDP.PCAP.CD)

  # Prepare direct effects data for merging with GDP
  effects_for_gdp <- all_direct_effects %>% 
    mutate(R_squared_numeric = suppressWarnings(as.numeric(gsub("%", "", Variance_explained)))) %>%
    mutate(R_squared_valid = !is.na(R_squared_numeric) & R_squared_numeric >= 0 & R_squared_numeric <= 100)
  
  # Approximate matching for country names - this often needs manual refinement!
  # Using countrycode package for more robust matching if available, else simple
  if (requireNamespace("countrycode", quietly = TRUE)) {
    effects_for_gdp$iso2c <- countrycode::countrycode(effects_for_gdp$Country, origin = "country.name", destination = "iso2c", warn = FALSE)
    # Handle cases where countrycode might fail for some specific names in your data
    # e.g. effects_for_gdp$iso2c[effects_for_gdp$Country == "Specific Name"] <- "XY"
  } else {
    # Fallback to very simple matching if countrycode is not there
    effects_for_gdp$iso2c <- effects_for_gdp$Country # This will likely fail for many
    message("countrycode package not found, GDP matching might be poor.")
  }

  gdp_merged_data <- effects_for_gdp %>% left_join(latest_gdp_data, by = "iso2c")

  # Plot: R² vs GDP
  if (nrow(gdp_merged_data %>% filter(R_squared_valid, !is.na(GDP_per_capita))) >= 5) { # Need some points for a meaningful plot
    r2_gdp_plot <- ggplot(gdp_merged_data %>% filter(R_squared_valid), aes(x = GDP_per_capita, y = R_squared_numeric)) +
      geom_point(aes(color = Country.y), alpha=0.7, show.legend=FALSE) + # Color by WB country name to see matches
      ggrepel::geom_text_repel(aes(label = Country), size = 2.5, max.overlaps = Inf, na.rm = TRUE) +
      scale_x_log10(labels = scales::dollar_format()) +
      geom_smooth(method = "lm", se = FALSE, color = "firebrick", linetype = "dashed", na.rm=TRUE) +
      labs(title = "Model Fit (R²) vs. GDP per Capita", x = "GDP per Capita (log scale, current USD)", y = "Variance Explained in Adherence (%)",
           subtitle = paste0("Data from ", length(unique(gdp_merged_data$Country.y[!is.na(gdp_merged_data$GDP_per_capita)])), " countries with matched GDP data.")) +
      theme_light()
    ggsave(file.path(gdp_plots_dir, "r_squared_vs_gdp.png"), r2_gdp_plot, width = 10, height = 7, dpi = 300)
    message("Saved R² vs GDP plot.")
  } else { message("Skipping R² vs GDP plot - insufficient matched data.")}

  # Plots: Individual HBM Construct Effects vs GDP
  for (construct_name in hbm_constructs) {
    plot_data_gdp_eff <- gdp_merged_data %>% filter(Variable == construct_name, !is.na(Direct_beta), !is.na(GDP_per_capita))
    if(nrow(plot_data_gdp_eff) < 5) next # Need a few points

    effect_gdp_plot <- ggplot(plot_data_gdp_eff, aes(x = GDP_per_capita, y = Direct_beta)) +
      geom_point(aes(color = Country.y), alpha=0.7, show.legend=FALSE) +
      ggrepel::geom_text_repel(aes(label = Country), size = 2.5, max.overlaps = Inf, na.rm = TRUE) +
      scale_x_log10(labels = scales::dollar_format()) +
      geom_smooth(method = "lm", se = FALSE, color = "firebrick", linetype = "dashed", na.rm=TRUE) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "grey30") +
      labs(title = paste("Effect of", construct_name, "on Adherence vs. GDP per Capita"), 
           x = "GDP per Capita (log scale, current USD)", y = "Standardized Beta (β)") +
      theme_light()
    ggsave(file.path(gdp_plots_dir, paste0("effect_gdp_", construct_name, ".png")), effect_gdp_plot, width = 10, height = 7, dpi = 300)
  }
  message("Saved individual HBM construct effects vs. GDP plots.")

  # Correlation: HBM Effects with GDP (summary plot)
  cor_data_gdp_effects <- gdp_merged_data %>% 
    filter(!is.na(Direct_beta), !is.na(GDP_per_capita)) %>% 
    group_by(Variable) %>% 
    filter(n() >= 5) %>% # Only calc cor if enough obs
    summarise(Correlation = cor(GDP_per_capita, Direct_beta, use = "pairwise.complete.obs"), .groups = "drop") %>% 
    filter(!is.na(Correlation))

  if(nrow(cor_data_gdp_effects) > 0){
    cor_gdp_plot <- ggplot(cor_data_gdp_effects, aes(x = reorder(Variable, Correlation), y = Correlation, fill = Correlation)) +
      geom_col() + coord_flip() +
      scale_fill_gradient2(low="red3", mid="grey80", high="blue3", midpoint=0) +
      geom_text(aes(label=round(Correlation,2)), hjust = ifelse(cor_data_gdp_effects$Correlation < 0, 1.1, -0.1), size=3.5) +
      labs(title="Correlation: HBM Construct Effects with GDP per Capita", x="HBM Construct", y="Pearson Correlation Coefficient") +
      theme_minimal() + theme(legend.position = "none")
    ggsave(file.path(gdp_plots_dir, "correlation_effects_vs_gdp.png"), cor_gdp_plot, width = 8, height = 6, dpi = 300)
    message("Saved HBM effects vs GDP correlation summary plot.")
  } else { message("Skipping HBM effects vs GDP correlation plot - no valid correlations.")}
  
  # --- 2d. Filtered Scatterplots (only countries with valid R²) ---
  countries_with_valid_r2 <- (all_direct_effects %>% 
                               filter(!is.na(Variance_explained) & suppressWarnings(as.numeric(gsub("%", "", Variance_explained))) >= 0) %>%
                               pull(Country) %>% unique())
  
  if(length(countries_with_valid_r2) > 0) {
    filtered_effects_data <- all_direct_effects %>% filter(Country %in% countries_with_valid_r2)
    
    summary_filtered_effects <- filtered_effects_data %>% 
        group_by(Variable) %>% 
        summarise(Median_Beta = median(Direct_beta, na.rm=TRUE), N_countries = n(), .groups="drop") %>% 
        arrange(desc(abs(Median_Beta)))
    write_csv(summary_filtered_effects, file.path(filtered_effects_plots_dir, "summary_median_effects_valid_r2_countries.csv"))

    median_effects_plot <- ggplot(summary_filtered_effects, aes(x=reorder(Variable, Median_Beta), y=Median_Beta, fill=Median_Beta)) +
        geom_col() + coord_flip() +
        geom_text(aes(label=paste0(round(Median_Beta,2), " (N=", N_countries, ")")), hjust = ifelse(summary_filtered_effects$Median_Beta < 0, 1.1, -0.1), size=3) +
        scale_fill_gradient2(low="red3", mid="grey80", high="blue3", midpoint=0) +
        labs(title="Median Direct Effects on Adherence (Countries with Valid R²)", x="HBM Construct", y="Median Standardized Beta (β)") +
        theme_minimal() + theme(legend.position = "none")
    ggsave(file.path(filtered_effects_plots_dir, "median_effects_barchart_valid_r2.png"), median_effects_plot, width=10, height=7, dpi=300)
    message("Saved summary table and plot for effects from countries with valid R².")
  } else { message("Skipping analyses filtered by valid R² - no such countries found or R² data missing.")}

} else {
  message("Skipping all visualizations in outputs.R as initial consolidated direct effects data is empty or missing key columns.")
}

cat("outputs.R script finished.
")
