# Example: Longitudinal Data Generation - Issue #95 Solution
# This script demonstrates the intuitive, well-structured solution for longitudinal data generation

library(gsm.datasim)

# ==============================================================================
# 1. QUICK START - One-liner replacement for complex workflow
# ==============================================================================
# This single function call replaces 50+ lines of manual workflow orchestration

study <- quick_longitudinal_study(
  study_name = "Oncology Phase III Trial", 
  participants = 1000, 
  sites = 150, 
  months_duration = 3
)

# Explore the generated study
study$summary()

# Access analytics results intuitively (if pipeline was successful)
if (!is.null(study$analytics)) {
  cat("Site-level analytics available:", nrow(study$analytics$by_site$results), "records\n")
  cat("Country-level analytics available:", nrow(study$analytics$by_country$results), "records\n")
  cat("Study-level analytics available:", nrow(study$analytics$by_study$results), "records\n")
}

# ==============================================================================
# 2. INTUITIVE DOMAIN-BASED CONFIGURATION
# ==============================================================================
# Use human-friendly domain names instead of technical mapping codes

cardio_study <- create_longitudinal_study(
  study_id = "CARDIO-001",
  participants = 500,
  sites = 25,
  timepoints = 6,
  interval = "2 months",
  domains = c("adverse_events", "lab_data", "visits", "queries"),
  include_pipeline = FALSE  # Just raw data for this example
)

cardio_study$summary()

# Access specific domain data across timepoints
ae_timeline <- cardio_study$get_domain_timeline("adverse_events")
cat("Adverse events generated across", length(ae_timeline), "timepoints\n")

# Access specific timepoint data
timepoint_1_data <- cardio_study$get_timepoint(1)
cat("Datasets in timepoint 1:", paste(names(timepoint_1_data), collapse = ", "), "\n")

# ==============================================================================
# 3. FLUENT BUILDER INTERFACE FOR COMPLEX CONFIGURATIONS
# ==============================================================================
# Chain methods for readable, discoverable configuration

complex_study <- create_study("COMPLEX-TRIAL-001") %>%
  with_study_design(participants = 300, sites = 20) %>%
  over_time(start_date = "2023-01-01", snapshots = 8, frequency = "6 weeks") %>%
  with_standard_datasets(adverse_events = TRUE, visits = TRUE, lab_data = TRUE) %>%
  with_adverse_events(rate_per_patient = 2.5, temporal_pattern = "increasing") %>%
  with_data_quality(queries = TRUE, data_changes = TRUE) %>%
  preview()  # Preview configuration before generation

# Generate the data (commented out to avoid long execution)
# complex_results <- complex_study$generate()

# ==============================================================================
# 4. COMPARISON: BEFORE vs. AFTER
# ==============================================================================

cat("\n=== BEFORE (Issue #95 manual workflow) ===\n")
cat("- 50+ lines of complex orchestration code\n")
cat("- Manual package loading and setup\n")  
cat("- Error-prone looping through snapshots\n")
cat("- Manual binding and filtering of results\n")
cat("- Hard-coded workflow configurations\n")
cat("- Repetitive code for different analysis levels\n")

cat("\n=== AFTER (New intuitive solution) ===\n")
cat("- Single function call: quick_longitudinal_study()\n")
cat("- Human-friendly parameter names\n")
cat("- Automatic result organization by analysis level\n")
cat("- Built-in validation and error handling\n")
cat("- Discoverable fluent interface\n")
cat("- Clean separation of concerns\n")

# ==============================================================================
# 5. ADVANCED: CONFIGURATION-DRIVEN APPROACH
# ==============================================================================
# For users who need full control over the configuration

config <- StudyConfig$new(study_id = "ADVANCED-001", participant_count = 200, site_count = 15)
config$set_temporal(start_date = "2024-01-01", snapshot_count = 10, snapshot_width = "months")
config$add_dataset("Raw_AE", enabled = TRUE)
config$add_dataset("Raw_LB", enabled = TRUE)

# Validate configuration
config$validate()
cat("Advanced configuration created and validated successfully\n")

# The key improvement: Clean abstraction that hides complexity while providing flexibility