indicator_vars <- c(
    # Perceived Susceptibility
    "risk_infection",          # Likelihood for self/others
    "risk_community",          # Community risk
    "knowledge_high_risk_groups_people_of_certain_ages",
    "knowledge_high_risk_groups_people_with_certain_medical_conditions",
    "knowledge_existing_treatments",

    # Perceived Severity
    "infection_severity",      # Seriousness of COVID-19
    # "knowledge_existing_treatments", # Also in Susceptibility
    "knowledge_symptoms_shortness_of_breath",
    "knowledge_symptoms_fever",

    # Perceived Benefits
    "effect_mask",             # Mask effectiveness
    "effect_hand_washing",     # Handwashing effectiveness
    "distancing_importance",   # Importance of distancing
    "community_norms_mask",    # Community mask norms

    # Perceived Barriers
    "future_masks",            # Likelihood of future mask use
    "vaccine_accept",          # Vaccine acceptance
    "healthcare_avoid_contact",# Avoiding healthcare

    # Cues to Action
    "info_exposure_past_week", # COVID-19 info exposure
    "know_positive_case",      # Knowing a positive case
    "news_sources_government_health_authorities",
    "news_sources_local_health_workers",

    # Self-Efficacy
    "control_infection",       # Perceived control over infection
    "prevention_mask",         # Ability to wear mask
    "prevention_distancing",   # Ability to distance
    "prevention_hand_washing", # Ability to clean hands

    # Adherence (Outcome)
    "measures_taken_wearing_a_face_mask_or_covering",
    "measures_taken_meter_distance",
    "measures_taken_washing_hands",
    "measures_taken_avoid_sick"
)

admin_vars <- c(
  "start_date",
  "geoip_country"
)

# Potential exogenous variables
exogenous_vars <- c(
  "age",
  "gender",
  "own_health", 
  "density",
  "education"
)

# Check variable presence in all_data
cat("All indicator vars in all_data:", all(indicator_vars %in% names(all_data)), "\n")
cat("All admin vars in all_data:", all(admin_vars %in% names(all_data)), "\n")
cat("All exogenous vars in all_data:", all(exogenous_vars %in% names(all_data)), "\n")

# Keep only selected variables
all_data <- all_data[, c(indicator_vars, admin_vars, exogenous_vars)]

####### Inspection & Cleaning #######

str(all_data)
summary(all_data)

# library(dplyr) # Handled in master.R

# Get all column names
vars <- names(all_data)

# Create a long data.frame of unique values for each variable
uniques_list <- lapply(vars, function(col) {
  vals <- unique(all_data[[col]])
  data.frame(
    variable     = col,
    var_type     = class(all_data[[col]])[1],
    unique_value = as.character(vals),
    stringsAsFactors = FALSE
  )
})

uniques_df <- bind_rows(uniques_list)

# Write unique values table to output_path
unique_values_file <- file.path(output_path, "unique_values_table.csv")
write.csv(uniques_df,
          file    = unique_values_file,
          row.names = FALSE)
cat(paste("Unique values table saved to:", unique_values_file, "\n"))

# Recode empty strings and "NA" strings to proper NA
all_data <- as.data.frame(lapply(all_data, function(col) {
  if (is.factor(col)) {
    ch <- as.character(col)
    ch[ch == "" | ch == "NA"] <- NA
    return(factor(ch))
  } else if (is.character(col)) {
    ch <- col
    ch[ch == "" | ch == "NA"] <- NA
    return(ch)
  } else {
    return(col) # Numeric/integer/logical unchanged
  }
}), stringsAsFactors = FALSE)

# Recode HBM indicators to numeric (lower response value = less of the construct)
all_data <- all_data %>%
  mutate(
    # Perceived Susceptibility
    risk_infection = as.numeric(factor(risk_infection,
                                       levels = c("Not at all likely", "Slightly likely", "Moderately likely", 
                                                  "Very likely", "Extremely likely")
    )),
    risk_community = as.numeric(factor(risk_community,
                                       levels = c("Not at all dangerous", "Slightly dangerous", 
                                                  "Moderately dangerous", "Very dangerous", "Extremely dangerous")
    )),
    knowledge_high_risk_groups_people_of_certain_ages =
      as.numeric(as.character(knowledge_high_risk_groups_people_of_certain_ages)),
    knowledge_high_risk_groups_people_with_certain_medical_conditions =
      as.numeric(as.character(knowledge_high_risk_groups_people_with_certain_medical_conditions)),
    
    # Perceived Severity
    infection_severity = as.numeric(factor(infection_severity,
                                           levels = c("Not at all serious", "Somewhat serious", "Very serious")
    )),
    knowledge_existing_treatments = as.numeric(factor(knowledge_existing_treatments,
                                                      levels = c("There is currently no drug treatment or vaccine for COVID-19",
                                                                 "There is a vaccine for COVID-19")
    )),
    knowledge_symptoms_shortness_of_breath = case_when(
      knowledge_symptoms_shortness_of_breath == "0" ~ 0,
      knowledge_symptoms_shortness_of_breath == "1" ~ 1,
      TRUE                                       ~ NA_real_
    ),
    knowledge_symptoms_fever = case_when(
      knowledge_symptoms_fever == "0" ~ 0,
      knowledge_symptoms_fever == "1" ~ 1,
      TRUE                            ~ NA_real_
    ),
    
    # Perceived Benefits
    effect_mask = as.numeric(factor(effect_mask,
                                    levels = c("Not effective at all", "Slightly effective", "Moderately effective",
                                               "Very effective", "Extremely effective")
    )),
    effect_hand_washing = as.numeric(factor(effect_hand_washing,
                                            levels = c("Not effective at all", "Slightly effective", "Moderately effective",
                                                       "Very effective", "Extremely effective")
    )),
    distancing_importance = as.numeric(factor(distancing_importance,
                                              levels = c("Not at all important", "Slightly important", "Moderately important",
                                                         "Very important", "Extremely Important")
    )),
    community_norms_mask = as.numeric(as.character(community_norms_mask)),
    
    # Perceived Barriers
    future_masks = as.numeric(factor(future_masks,
                                     levels = c("Never", "Rarely", "When convenient", "Almost always", "Always")
    )),
    vaccine_accept = as.numeric(factor(vaccine_accept,
                                       levels = c("No", "Don't know", "Yes")
    )),
    healthcare_avoid_contact = as.numeric(factor(healthcare_avoid_contact,
                                                 levels = c("No", "Yes")
    )),
    
    # Cues to Action
    info_exposure_past_week = as.numeric(factor(info_exposure_past_week,
                                                levels = c("Nothing", "A little", "A moderate amount", "A lot")
    )),
    know_positive_case = as.numeric(factor(know_positive_case,
                                           levels = c("No", "Prefer not to say", "Yes")
    )),
    news_sources_government_health_authorities = case_when(
      news_sources_government_health_authorities == "1" ~ 1,
      news_sources_government_health_authorities == "0" ~ 0,
      TRUE                                          ~ NA_real_
    ),
    news_sources_local_health_workers = case_when(
      news_sources_local_health_workers == "1" ~ 1,
      news_sources_local_health_workers == "0" ~ 0,
      TRUE                                    ~ NA_real_
    ),
    
    # Self-Efficacy
    control_infection = as.numeric(factor(control_infection,
                                          levels = c("Strongly disagree", "Somewhat disagree", "Neither agree or disagree",
                                                     "Somewhat agree", "Strongly agree")
    )),
    prevention_mask = as.numeric(factor(prevention_mask,
                                        levels = c("Never", "Rarely", "Sometimes", "Often", "Always")
    )),
    prevention_distancing = as.numeric(factor(prevention_distancing,
                                              levels = c("Never", "Rarely", "Sometimes", "Often", "Always")
    )),
    prevention_hand_washing = as.numeric(factor(prevention_hand_washing,
                                                levels = c("Never", "Rarely", "Sometimes", "Often", "Always")
    )),
    
    # Adherence
    measures_taken_wearing_a_face_mask_or_covering = case_when(
      measures_taken_wearing_a_face_mask_or_covering == "1" ~ 1,
      measures_taken_wearing_a_face_mask_or_covering == "0" ~ 0,
      TRUE                                                 ~ NA_real_
    ),
    measures_taken_meter_distance = case_when(
      measures_taken_meter_distance == "1" ~ 1,
      measures_taken_meter_distance == "0" ~ 0,
      TRUE                               ~ NA_real_
    ),
    measures_taken_washing_hands = case_when(
      measures_taken_washing_hands == "1" ~ 1,
      measures_taken_washing_hands == "0" ~ 0,
      TRUE                              ~ NA_real_
    ),
    measures_taken_avoid_sick = case_when(
      measures_taken_avoid_sick == "1" ~ 1,
      measures_taken_avoid_sick == "0" ~ 0,
      TRUE                            ~ NA_real_
    )
  )
