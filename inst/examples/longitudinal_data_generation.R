# Example: Longitudinal Data Generation - Issue #95 Solution
# This script demonstrates the intuitive, well-structured solution for longitudinal data generation

library(gsm.datasim)


# This single function call replaces 50+ lines of manual workflow orchestration

# Standard study (core mappings)
study <- quick_longitudinal_study(
  study_name = "Oncology Phase III Trial",
  participants = 1000,
  sites = 150,
  months_duration = 3,
  study_type = "standard"
)

# Endpoints study (endpoint mappings)
# endpoints_study <- quick_longitudinal_study(
#   study_name = "Cardio Endpoints Trial",
#   study_type = "endpoints"
# )

# Explore the generated study
study$summary()

# Access analytics results intuitively (if pipeline was successful)
if (!is.null(study$analytics)) {
  cat("Site-level analytics available:", nrow(study$analytics$by_site$results), "records\n")
  cat("Country-level analytics available:", nrow(study$analytics$by_country$results), "records\n")
  cat("Study-level analytics available:", nrow(study$analytics$by_study$results), "records\n")
}


# Use mapping names directly for domains

cardio_study <- create_longitudinal_study(
  study_id = "CARDIO-001",
  participants = 500,
  sites = 25,
  timepoints = 6,
  interval = "2 months",
  domains = c("AE", "LB", "VISIT", "QUERY"),
  include_pipeline = FALSE  # Just raw data for this example
)

cardio_study$summary()

# Access specific domain data across timepoints
ae_timeline <- cardio_study$get_domain_timeline("AE")
cat("Adverse events generated across", length(ae_timeline), "timepoints\n")

# Access specific timepoint data
timepoint_1_data <- cardio_study$get_timepoint(1)
cat("Datasets in timepoint 1:", paste(names(timepoint_1_data), collapse = ", "), "\n")


# Chain methods for readable, discoverable configuration

# Note: The following complex study configuration demonstrates R6 method chaining
complex_study <- create_study("COMPLEX-TRIAL-001")
complex_study$with_study_design(participants = 300, sites = 20)
complex_study$over_time(start_date = "2023-01-01", snapshots = 8, frequency = "6 weeks")
complex_study$with_standard_datasets(adverse_events = TRUE, subject_visits = TRUE, lab_data = TRUE, 
                                     queries = TRUE, data_changes = TRUE, data_entry = FALSE)
complex_study$with_adverse_events(rate_per_patient = 2.5, temporal_pattern = "increasing")
complex_study$preview()  # Preview configuration before generation

# Generate the data (commented out to avoid long execution)
# complex_results <- complex_study$generate()


# For users who need full control over the configuration

config <- StudyConfig$new(study_id = "ADVANCED-001", participant_count = 200, site_count = 15)
config$set_temporal(start_date = "2024-01-01", snapshot_count = 10, snapshot_width = "months")
config$add_dataset("Raw_AE", enabled = TRUE)
config$add_dataset("Raw_LB", enabled = TRUE)

# Validate configuration
config$validate()
cat("Advanced configuration created and validated successfully\n")

# The key improvement: Clean abstraction that hides complexity while providing flexibility
