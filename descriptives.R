# Create timeline and histograms of adherence measures
# library(ggplot2) # Handled in master.R
# library(dplyr) # Handled in master.R
# library(gridExtra) # Handled in master.R
# library(scales)  # Handled in master.R

# Define and create subdirectory for descriptive outputs
descriptives_output_dir <- file.path(output_path, "descriptives")
if (!dir.exists(descriptives_output_dir)) {
  dir.create(descriptives_output_dir, recursive = TRUE)
}

###### Timeline of cases per month ######
all_data$month <- format(all_data$start_date, "%Y-%m")

monthly_counts <- all_data %>% # Count cases by month
  group_by(month) %>% 
  summarize(count = n()) %>% 
  arrange(month)

p1 <- ggplot(monthly_counts, aes(x = month, y = count)) + # Plot timeline
  geom_line(group = 1, color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Survey Responses Over Time", x = "Month", y = "Number of Responses") +
  scale_y_continuous(labels = comma)

ggsave(file.path(descriptives_output_dir, "timeline_monthly.pdf"), p1, width = 10, height = 6)

###### Weekly trend ######
all_data$week <- format(all_data$start_date, "%Y-%U") # Convert to weekly data

weekly_counts <- all_data %>% # Count cases by week
  group_by(week) %>% 
  summarize(count = n(), date = min(start_date)) %>% # Use first day of week
  arrange(date)

p2 <- ggplot(weekly_counts, aes(x = date, y = count)) + # Plot weekly timeline
  geom_line(color = "darkblue", size = 0.8) +
  geom_point(color = "darkblue", size = 2, alpha = 0.7) +
  theme_minimal() +
  labs(title = "Weekly Survey Responses", x = "Week", y = "Number of Responses") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(descriptives_output_dir, "timeline_weekly.pdf"), p2, width = 12, height = 6)

###### Histograms of adherence measures ######
adherence_vars <- c(
  "measures_taken_wearing_a_face_mask_or_covering",
  "measures_taken_meter_distance",
  "measures_taken_washing_hands",
  "measures_taken_avoid_sick"
)

var_labels <- c("Wearing Face Mask", "Physical Distancing", "Washing Hands", "Avoiding Sick People")

adherence_data <- data.frame(Measure = character(), Value = numeric(), Label = character())

for (i in seq_along(adherence_vars)) { # Prepare data for plotting
  var <- adherence_vars[i]
  if (var %in% names(all_data)) {
    tmp_data <- data.frame(
      Measure = var_labels[i],
      Value = ifelse(all_data[[var]] == 1, "Yes", 
                    ifelse(all_data[[var]] == 0, "No", "Unknown")),
      stringsAsFactors = FALSE
    )
    adherence_data <- rbind(adherence_data, tmp_data)
  }
}

adherence_data$Value <- factor(adherence_data$Value, levels = c("Yes", "No", "Unknown"))
adherence_data$Measure <- factor(adherence_data$Measure, levels = var_labels)

p3 <- ggplot(adherence_data, aes(x = Value, fill = Value)) + # Create combined histogram
  geom_bar() +
  facet_wrap(~ Measure, scales = "free_y") +
  theme_minimal() +
  labs(title = "Adherence to COVID-19 Protective Measures", x = NULL, y = "Count") +
  scale_fill_manual(values = c("Yes" = "#1a9641", "No" = "#d7191c", "Unknown" = "#bdbdbd")) +
  scale_y_continuous(labels = comma) +
  theme(legend.title = element_blank(), strip.text = element_text(size = 12, face = "bold"), axis.text.y = element_text(size = 10))

ggsave(file.path(descriptives_output_dir, "adherence_measures.pdf"), p3, width = 10, height = 8)

# Create percentage plot (relative frequencies)
adherence_pct <- adherence_data %>%
  group_by(Measure, Value) %>% summarize(count = n()) %>%
  group_by(Measure) %>% mutate(pct = count / sum(count) * 100)

p4 <- ggplot(adherence_pct, aes(x = Measure, y = pct, fill = Value)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  labs(title = "Relative Frequencies of Adherence to COVID-19 Measures", x = NULL, y = "Percentage (%)") +
  scale_fill_manual(values = c("Yes" = "#1a9641", "No" = "#d7191c", "Unknown" = "#bdbdbd")) +
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1, size = 12), axis.text.y = element_text(size = 10)) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), position = position_stack(vjust = 0.5), color = "white", size = 3)

ggsave(file.path(descriptives_output_dir, "adherence_percentages.pdf"), p4, width = 10, height = 6)

# Create a combined figure for paper
combined_plot <- grid.arrange(p1, p2, p3, p4, 
                             layout_matrix = rbind(c(1,1), c(2,2), c(3,4)))

ggsave(file.path(descriptives_output_dir, "combined_descriptives.pdf"), combined_plot, width = 12, height = 15)

cat(paste("Descriptive visualizations saved to:", descriptives_output_dir, "\n"))
