# -------------------------------------------------------------------------
# Question 3: Visualizations using {ggplot2)
# -------------------------------------------------------------------------

# Start log file
log_file <- "question_3_tlg/02_create_visualizations.log"
# Open connections to capture both standard print statements and errors/warnings
con <- file(log_file, open = "wt")
sink(con, type = "output")
sink(con, type = "message", append = TRUE)

cat("Execution summary:\n")
cat("Date:", format(Sys.time()), "\n")
sessionInfo()


library(pharmaverseadam)
library(dplyr)
library(ggplot2)

# =========
# Output 1
# =========

# 1. Clean and prepare the data
ae_severity_data <- pharmaverseadam::adae %>%
  # Filter for Safety population and Treatment-Emergent AEs
  filter(SAFFL == "Y" & TRTEMFL == "Y", !is.na(ACTARM), !is.na(AESEV)) %>%
  # Convert AESEV to a factor with an explicit clinical order (Mild -> Moderate -> Severe)
  mutate(
    AESEV = factor(AESEV, levels = c("MILD", "MODERATE", "SEVERE")),
    ACTARM = factor(ACTARM)
  ) %>%
  # Calculate percentages within each treatment arm
  count(ACTARM, AESEV) %>%
  group_by(ACTARM) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup()

# 2. Generate the Stacked Percentage Bar Chart
# 1. Clean and prepare the data (calculating absolute counts only)
ae_severity_counts <- pharmaverseadam::adae %>%
  # Filter for Safety population and Treatment-Emergent AEs
  filter(SAFFL == "Y" & TRTEMFL == "Y", !is.na(ACTARM), !is.na(AESEV)) %>%
  # Convert AESEV to a factor with an explicit clinical order (Mild -> Moderate -> Severe)
  mutate(
    AESEV = factor(AESEV, levels = c("MILD", "MODERATE", "SEVERE")),
    ACTARM = factor(ACTARM)
  ) %>%
  # Count the occurrences
  count(ACTARM, AESEV)

# 2. Generate the Stacked Count Bar Chart
ae_plot <- ggplot(ae_severity_counts, aes(x = ACTARM, y = n, fill = AESEV)) +
  geom_bar(stat = "identity", position = "stack", width = 0.6) +
  # Add labels showing the raw count 'n' inside each stack segment
  # geom_text(
  #   aes(label = n), 
  #   position = position_stack(vjust = 0.5), 
  #   size = 4, 
  #   color = "white",
  #   fontface = "bold"
  # ) +
  scale_fill_manual(
    values = c("MILD" = "#d46f4d", "MODERATE" = "#1b9e77", "SEVERE" = "#2c7bb6"),
    name = "Severity/Intensity"
  ) +
  labs(
    title = "AE severity distribution by treatment",
    subtitle = "Safety Population",
    x = "Actual Treatment Arm",
    y = "Count of AEs"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40", margin = margin(b = 15)),
    #axis.text.x = element_text(hjust = 1, face = "bold"),
    legend.position = "right",
    panel.grid.major.x = element_blank()
  )

# Print the plot to the device
print(ae_plot)

# Save the plot as a PNG image
if (!dir.exists("question_3_tlg")) {
  dir.create("question_3_tlg", recursive = TRUE)
}

ggsave(
  filename = "question_3_tlg/f_ae_severity_distribution.png",
  plot = ae_plot,
  width = 8,       # standard landscape width in inches
  height = 5.5,    # standard landscape height in inches
  dpi = 300        # high-quality publication resolution
)

# =========================================================================
# Output 2 - Top 10 Most Frequent AEs with 95% CI
# =========================================================================

library(pharmaverseadam)
library(dplyr)
library(ggplot2)
library(tidyr)

# 1. New Denominator: Count unique subjects who have at least one TEAE
n_ae_population <- pharmaverseadam::adae %>% 
  filter(SAFFL == "Y", !is.na(AETERM)) %>% 
  pull(USUBJID) %>% 
  n_distinct()

# 2. Process AE data to find unique subjects per AE term (Incidence)
ae_counts <- pharmaverseadam::adae %>%
  filter(SAFFL == "Y", !is.na(AETERM)) %>%
  # Count unique subjects per AE term (standard safety incidence approach)
  group_by(AETERM) %>%
  summarise(n_subjects = n_distinct(USUBJID), .groups = "drop") %>%
  # Slice for the top 10 highest incidence terms
  slice_max(order_by = n_subjects, n = 10, with_ties = FALSE)

# 3. Calculate Incidence Rates and 95% Confidence Intervals (Exact Binomial Method)
ae_ci_data <- ae_counts %>%
  rowwise() %>%
  mutate(
    # Rate per 100 subjects (%)
    incidence_rate = (n_subjects / n_ae_population) * 100,
    # Clopper-Pearson Exact Binomial CI
    bi_test = list(binom.test(n_subjects, n_ae_population, conf.level = 0.95)),
    ci_lower = bi_test$conf.int[1] * 100,
    ci_upper = bi_test$conf.int[2] * 100
  ) %>%
  ungroup() %>%
  # Reorder factor for clean plotting layout (highest on top)
  mutate(AETERM = reorder(AETERM, incidence_rate))

# 4. Generate the Forest Plot / Horizontal Bar Plot combo
ae_top10_plot <- ggplot(ae_ci_data, aes(x = incidence_rate, y = AETERM)) +
  # Visual backing bar
  #geom_bar(stat = "identity", fill = "#7fa998", alpha = 0.6, width = 0.5) +
  # Point estimation dot
  geom_point(color = "#2d5a27", size = 3) +
  # CHANGED: Use geom_errorbar with xmin/xmax. ggplot2 automatically figures out orientation.
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper), color = "#2d5a27", width = 0.2, linewidth = 0.8) +
  labs(
    title = "Top 10 Most Frequent Adverse Events",
    subtitle = paste0("AE-experiencing Patients (n = ", n_ae_population, "); 95% Clopper-Pearson CIs"),
    x = "Percentage of AE-experiencing Patients (%)",
    y = "Adverse Event Preferred Term"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40", margin = margin(b = 15)),
    axis.text.y = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# 5. Print the plot to the device
print(ae_top10_plot)

# 6. Save the plot as a PNG image
if (!dir.exists("question_3_tlg")) {
  dir.create("question_3_tlg", recursive = TRUE)
}
ggsave(
  filename = "question_3_tlg/f_ae_top10_incidence_ci.png",
  plot = ae_top10_plot,
  width = 8.5,
  height = 5.5,
  dpi = 300
)


# =========================================================================
# END OF SCRIPT: Turn off Logging and close file
# =========================================================================
sink(type = "message")
sink(type = "output")
close(con)