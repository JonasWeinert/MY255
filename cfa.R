# Confirmatory Factor Analysis for HBM Constructs
# library(lavaan) # Handled in master.R
# library(dplyr)  # Handled in master.R

# Define constructs and indicators
model_list <- list(
  Susceptibility = c(
    "risk_infection", "risk_community", "infection_severity",
    "knowledge_high_risk_groups_people_with_certain_medical_conditions"
  ),
  Severity = c(
    "infection_severity", "knowledge_existing_treatments",
    "knowledge_symptoms_shortness_of_breath", "knowledge_symptoms_fever"
  ),
  Benefits = c(
    "effect_mask", "effect_hand_washing", "distancing_importance", "community_norms_mask"
  ),
  Barriers = c("future_masks", "vaccine_accept", "healthcare_avoid_contact"),
  Cues = c(
    "info_exposure_past_week", "know_positive_case",
    "news_sources_government_health_authorities", "news_sources_local_health_workers"
  ),
  Efficacy = c(
    "control_infection", "prevention_mask", "prevention_distancing", "prevention_hand_washing"
  ),
  Adherence = c(
    "measures_taken_wearing_a_face_mask_or_covering", "measures_taken_meter_distance",
    "measures_taken_washing_hands", "measures_taken_avoid_sick"
  )
)

# Scale indicator vars (mean = 0, var = 1)
indicator_vars <- unlist(model_list)
all_scaled <- all_data # Create a copy to keep original all_data intact if needed later
all_scaled[indicator_vars] <- scale(all_data[indicator_vars], center = TRUE, scale = TRUE)

# Create lavaan model strings
lavaan_models <- lapply(names(model_list), function(constr) {
  inds <- model_list[[constr]]
  paste0(constr, " =~ ", paste(inds, collapse = " + "))
})
names(lavaan_models) <- names(model_list)

# Fit each CFA and extract fit data
fit_results <- lapply(names(lavaan_models), function(constr) {
  model_syntax <- lavaan_models[[constr]]
  # Add tryCatch for robustness if some models fail to converge
  fit_attempt <- try(cfa(model_syntax, data = all_scaled, std.lv = FALSE, warn = FALSE, se = "none"), silent = TRUE)
  
  if (inherits(fit_attempt, "try-error")) {
    cat(paste("Warning: CFA for construct", constr, "failed to converge or had issues.\n"))
    return(data.frame(Construct = constr, chisq = NA, df = NA, CFI = NA, TLI = NA, RMSEA = NA, SRMR = NA))
  }
  fit <- fit_attempt
  fm <- fitMeasures(fit, c("chisq","df","cfi","tli","rmsea","srmr"))
  
  data.frame( # Return as data.frame for easier bind_rows
    Construct = constr, chisq = fm["chisq"], df = fm["df"], 
    CFI = fm["cfi"], TLI = fm["tli"], RMSEA = fm["rmsea"], SRMR = fm["srmr"]
  )
})

fit_table <- bind_rows(fit_results)

# Print and save CFA fit indices
print(fit_table)

cfa_output_file <- file.path(output_path, "HBM_CFA_fit_indices.csv")
write.csv(fit_table, cfa_output_file, row.names = FALSE)
cat(paste("CFA fit indices saved to:", cfa_output_file, "\n"))

