# Example of fluent interface usage

Example of fluent interface usage

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple study with standard datasets
study_data <- create_study("ONCOLOGY001") %>%
  with_study_design(participants = 200, sites = 15) %>%
  over_time(start_date = "2023-01-01", snapshots = 12, frequency = "months") %>%
  with_standard_datasets(adverse_events = TRUE, subject_visits = TRUE, visit_schedule = TRUE) %>%
  with_adverse_events(rate_per_patient = 2.5, temporal_pattern = "increasing") %>%
  generate()

# Preview configuration before generation
create_study("TRIAL002") %>%
  with_study_design(participants = 100, sites = 10) %>%
  with_standard_datasets() %>%
  preview()

# Custom dataset configuration
study_data <- create_study("CUSTOM001") %>%
  with_study_design(participants = 300, sites = 20) %>%
  add_custom_dataset("Raw_Biomarker",
                    count = function(config) config$study_params$participant_count * 5,
                    depends_on = "Raw_SUBJ") %>%
  generate()
} # }
```
