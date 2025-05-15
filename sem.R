# library(lavaan) # Handled in master.R
# library(dplyr) # Handled in master.R
# library(ggplot2) # Handled in master.R
# library(forestplot) # Handled in master.R
# library(tidyr) # Handled in master.R
# library(metafor) # Handled in master.R
# library(gridExtra) # Handled in master.R
# library(data.table) # Handled in master.R

# Define base directory for SEM results within the main output_path
sem_base_output_dir <- file.path(output_path, "sem_results")

# Create directories for SEM outputs
sem_by_country_dir <- file.path(sem_base_output_dir, "by_country")
sem_plots_dir <- file.path(sem_base_output_dir, "plots")

dir.create(sem_base_output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(sem_by_country_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(sem_plots_dir, showWarnings = FALSE, recursive = TRUE)

# Define SEM model (shortened comments)
sem_model <- '
  # Measurement model
  Susceptibility =~ risk_infection + risk_community + infection_severity + knowledge_high_risk_groups_people_with_certain_medical_conditions
  Severity       =~ infection_severity + knowledge_existing_treatments + knowledge_symptoms_shortness_of_breath + knowledge_symptoms_fever
  Benefits       =~ effect_mask + effect_hand_washing + distancing_importance + community_norms_mask
  Barriers       =~ future_masks + vaccine_accept + healthcare_avoid_contact
  Cues           =~ info_exposure_past_week + know_positive_case + news_sources_government_health_authorities + news_sources_local_health_workers
  Efficacy       =~ control_infection + prevention_mask + prevention_distancing + prevention_hand_washing
  Adherence      =~ measures_taken_wearing_a_face_mask_or_covering + measures_taken_meter_distance + measures_taken_washing_hands + measures_taken_avoid_sick

  # Structural model (paths to Adherence)
  Severity  ~ a1*Susceptibility + a2*Cues
  Benefits  ~ b1*Susceptibility + b2*Severity + b3*Cues  + b4*Efficacy
  Barriers  ~ c1*Susceptibility + c2*Cues
  Efficacy  ~ d1*Cues
  Adherence ~ e1*Susceptibility + e2*Severity + e3*Benefits + e4*Barriers + e5*Efficacy + e6*Cues

  # Indirect and total effects (definitions remain the same)
  ind_Sus_Sev_Ad     := a1 * e2
  ind_Sus_Ben_Ad     := b1 * e3
  ind_Sus_Bar_Ad     := c1 * e4
  ind_Sus_Sev_Ben_Ad := a1 * b2 * e3
  tot_ind_Sus        := ind_Sus_Sev_Ad + ind_Sus_Ben_Ad + ind_Sus_Bar_Ad + ind_Sus_Sev_Ben_Ad
  tot_Sus            := e1 + tot_ind_Sus

  ind_Sev_Ben_Ad  := b2 * e3
  tot_ind_Sev     := ind_Sev_Ben_Ad
  tot_Sev         := e2 + tot_ind_Sev

  tot_Ben         := e3
  tot_Bar         := e4

  ind_Eff_Ben_Ad  := b4 * e3
  tot_ind_Eff     := ind_Eff_Ben_Ad
  tot_Eff         := e5 + tot_ind_Eff

  ind_Cues_Sev_Ad     := a2 * e2
  ind_Cues_Ben_Ad     := b3 * e3
  ind_Cues_Eff_Ad     := d1 * e5
  ind_Cues_Bar_Ad     := c2 * e4
  ind_Cues_Eff_Ben_Ad := d1 * b4 * e3
  tot_ind_Cues        := ind_Cues_Sev_Ad + ind_Cues_Ben_Ad + ind_Cues_Eff_Ad + ind_Cues_Bar_Ad + ind_Cues_Eff_Ben_Ad
  tot_Cues            := e6 + tot_ind_Cues
'

# Function to run SEM
run_sem_analysis <- function(data, country_name = "all") {
  tryCatch({
    data_filtered <- data[!is.na(data$measures_taken_wearing_a_face_mask_or_covering),]
    
    if(nrow(data_filtered) < 100) {
      message(paste0("Skipping SEM for ", country_name, ": sample size < 100 (N=", nrow(data_filtered), ")."))
      return(NULL)
    }
    
    indicator_vars <- unique(c(
      "risk_infection", "risk_community", "infection_severity", "knowledge_high_risk_groups_people_with_certain_medical_conditions",
      "infection_severity", "knowledge_existing_treatments", "knowledge_symptoms_shortness_of_breath", "knowledge_symptoms_fever",
      "effect_mask", "effect_hand_washing", "distancing_importance", "community_norms_mask",
      "future_masks", "vaccine_accept", "healthcare_avoid_contact",
      "info_exposure_past_week", "know_positive_case", "news_sources_government_health_authorities", "news_sources_local_health_workers",
      "control_infection", "prevention_mask", "prevention_distancing", "prevention_hand_washing",
      "measures_taken_wearing_a_face_mask_or_covering", "measures_taken_meter_distance", "measures_taken_washing_hands", "measures_taken_avoid_sick"
    ))
    
    data_filtered$missing_count <- rowSums(is.na(data_filtered[, indicator_vars]))
    data_sorted <- data_filtered[order(data_filtered$missing_count), ]
    
    data_sample <- if(country_name != "all" && nrow(data_sorted) > 1500) data_sorted[1:1500, ] else data_sorted
    message(paste0("Running SEM for ", country_name, ": using ", nrow(data_sample), " observations (missing values per obs: ", 
                   min(data_sample$missing_count), "-", max(data_sample$missing_count), ")."))
    data_sample$missing_count <- NULL
    
    fit_sem <- sem(sem_model, data = data_sample, missing = "FIML", warn = FALSE, se = "robust.sem") # Using robust SE
    pe <- parameterEstimates(fit_sem, standardized = TRUE)
    sample_size <- nrow(data_sample)
    
    direct_effects <- pe %>% filter(op == "~", lhs == "Adherence")
    std_se <- direct_effects$se # Standardized SEs often available with robust estimators
    # Fallback if std.all SE is NA for some reason
    std_se[is.na(std_se) & !is.na(direct_effects$est) & direct_effects$est != 0] <- abs(direct_effects$se[is.na(std_se)] * direct_effects$std.all[is.na(std_se)] / direct_effects$est[is.na(std_se)])
    std_se[is.na(std_se)] <- 1 / sqrt(sample_size) # Approximation for any remaining NAs

    direct <- data.frame(
      Variable = direct_effects$rhs,
      Direct_beta = round(direct_effects$std.all, 3),
      SE = round(std_se, 3),
      P_value_direct = round(direct_effects$pvalue, 3),
      Country = country_name,
      Sample_size = sample_size
    )
    direct$CI_lower <- round(direct$Direct_beta - 1.96 * direct$SE, 3)
    direct$CI_upper <- round(direct$Direct_beta + 1.96 * direct$SE, 3)
    
    r2_value <- try(inspect(fit_sem, "r2")["Adherence"], silent = TRUE)
    direct$Variance_explained <- if(!inherits(r2_value, "try-error") && !is.null(r2_value)) paste0(round(r2_value * 100), "%") else NA
    
    indirect_effects <- pe %>% filter(op == ":=", lhs %in% c(
      "ind_Sus_Sev_Ad", "ind_Sus_Ben_Ad", "ind_Sus_Bar_Ad", "ind_Sus_Sev_Ben_Ad", "tot_ind_Sus", "tot_Sus",
      "ind_Sev_Ben_Ad", "tot_ind_Sev", "tot_Sev", "tot_Ben", "tot_Bar", "ind_Eff_Ben_Ad", "tot_ind_Eff", "tot_Eff",
      "ind_Cues_Sev_Ad", "ind_Cues_Ben_Ad", "ind_Cues_Eff_Ad", "ind_Cues_Bar_Ad", "ind_Cues_Eff_Ben_Ad", "tot_ind_Cues", "tot_Cues"
    ))
    
    ind_std_se <- indirect_effects$se
    ind_std_se[is.na(ind_std_se) & !is.na(indirect_effects$est) & indirect_effects$est != 0] <- abs(indirect_effects$se[is.na(ind_std_se)] * indirect_effects$std.all[is.na(ind_std_se)] / indirect_effects$est[is.na(ind_std_se)])
    ind_std_se[is.na(ind_std_se)] <- 1 / sqrt(sample_size)

    ind_tot <- data.frame(
      Effect = indirect_effects$lhs,
      Beta = round(indirect_effects$std.all, 3),
      SE = round(ind_std_se, 3),
      P_value = round(indirect_effects$pvalue, 3),
      Country = country_name,
      Sample_size = sample_size
    )
    ind_tot$CI_lower <- round(ind_tot$Beta - 1.96 * ind_tot$SE, 3)
    ind_tot$CI_upper <- round(ind_tot$Beta + 1.96 * ind_tot$SE, 3)
    
    return(list(direct = direct, indirect = ind_tot, fit = fit_sem, n = sample_size))
    
  }, error = function(e) {
    message(paste0("Error in SEM for ", country_name, ": ", e$message))
    return(NULL)
  })
}

# Function to save results
save_results <- function(results, country_name) {
  if(is.null(results)) return(NULL)
  country_dir <- file.path(sem_by_country_dir, country_name)
  dir.create(country_dir, showWarnings = FALSE, recursive = TRUE)
  write.csv(results$direct, file.path(country_dir, paste0("direct_effects_", country_name, ".csv")), row.names = FALSE)
  write.csv(results$indirect, file.path(country_dir, paste0("indirect_effects_", country_name, ".csv")), row.names = FALSE)
  return(results)
}

# Function to create forest plots
create_forest_plots <- function(all_direct_effects) {
  if(nrow(all_direct_effects) == 0 || !all(c("Variable", "Direct_beta", "CI_lower", "CI_upper", "Country", "Sample_size") %in% names(all_direct_effects))){
    message("Forest plot creation skipped: Insufficient data or missing required columns (Variable, Direct_beta, CI_lower, CI_upper, Country, Sample_size).")
    return(NULL)
  }
  direct_paths <- unique(all_direct_effects$Variable)
  plots_created <- FALSE
  
  for(path in direct_paths) {
    path_data <- all_direct_effects %>% filter(Variable == path) %>% arrange(Country)
    if(nrow(path_data) < 2) next
    plots_created <- TRUE
    plot_filename <- file.path(sem_plots_dir, paste0("path_", gsub("[^a-zA-Z0-9]", "_", path), ".png"))
    path_data$CountryLabel <- paste0(path_data$Country, " (n=", path_data$Sample_size, ")")
    
    p <- ggplot(path_data, aes(x = Direct_beta, y = reorder(CountryLabel, Direct_beta), xmin = CI_lower, xmax = CI_upper)) +
      geom_point(size = 3) + geom_errorbarh(height = 0.2) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
      labs(title = paste0("Effect of ", path, " on Adherence"), x = "Standardized Effect Size (Beta)", y = "") +
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    ggsave(plot_filename, p, width = 10, height = max(4, nrow(path_data)*0.3 + 1), device = "png", limitsize = FALSE) # Dynamic height
    message(paste0("Saved forest plot: ", plot_filename))
  }
  if(!plots_created) message("No forest plots created (need >=2 countries per path).")
}

# Main analysis function
run_country_analysis <- function(run_combined = TRUE, min_sample_size = 100) {
  # Use all_scaled from cfa.R which should be in the global environment
  if (!exists("all_scaled")) {
    stop("Object 'all_scaled' not found. Ensure cfa.R has been run and 'all_scaled' is available.")
  }
  all_scaled2 <- all_scaled[!is.na(all_scaled$measures_taken_wearing_a_face_mask_or_covering),]
  countries <- sort(unique(all_scaled2$geoip_country))
  message(paste0("Found ", length(countries), " unique countries for SEM."))
  
  all_direct_effects_list <- list()
  all_indirect_effects_list <- list()
  
  if(run_combined) {
    message("Running SEM for all countries combined...")
    all_results <- run_sem_analysis(all_scaled2, "all")
    saved_results <- save_results(all_results, "all")
    if(!is.null(saved_results)) {
      all_direct_effects_list[["all"]] <- saved_results$direct
      all_indirect_effects_list[["all"]] <- saved_results$indirect
    }
  }
  
  successful_countries <- c()
  for(country in countries) {
    country_data <- all_scaled2[all_scaled2$geoip_country == country,]
    if(sum(!is.na(country_data$measures_taken_wearing_a_face_mask_or_covering)) < min_sample_size) {
      message(paste0("Skipping SEM for ", country, ": valid N < ", min_sample_size))
      next
    }
    country_results <- run_sem_analysis(country_data, country)
    saved_results <- save_results(country_results, country)
    if(!is.null(saved_results)) {
      all_direct_effects_list[[country]] <- saved_results$direct
      all_indirect_effects_list[[country]] <- saved_results$indirect
      successful_countries <- c(successful_countries, country)
    }
  }
  
  all_direct_effects_df <- bind_rows(all_direct_effects_list)
  all_indirect_effects_df <- bind_rows(all_indirect_effects_list)
  
  write.csv(all_direct_effects_df, file.path(sem_base_output_dir, "all_direct_effects.csv"), row.names = FALSE)
  write.csv(all_indirect_effects_df, file.path(sem_base_output_dir, "all_indirect_effects.csv"), row.names = FALSE)
  saveRDS(list(direct = all_direct_effects_df, indirect = all_indirect_effects_df, successful_countries = successful_countries),
          file.path(sem_base_output_dir, "sem_summary_results.rds"))
          
  message(paste0("Successfully analyzed ", length(successful_countries), " countries for SEM."))
  
  if(nrow(all_direct_effects_df) > 0 && length(unique(all_direct_effects_df$Country)) >=2) {
    message("Creating forest plots for SEM direct effects...")
    create_forest_plots(all_direct_effects_df)
  } else {
    message("Forest plots for SEM skipped (not enough data/countries).")
  }
  
  return(list(direct = all_direct_effects_df, indirect = all_indirect_effects_df, successful_countries = successful_countries))
}

# Run the analysis (assuming all_scaled is available from cfa.R)
# This will be called from master.R, so we can remove the direct call here
# global_sem_results <- run_country_analysis(run_combined = TRUE, min_sample_size = 100)
cat("SEM functions defined. Analysis will be run if master.R calls run_country_analysis().\n")