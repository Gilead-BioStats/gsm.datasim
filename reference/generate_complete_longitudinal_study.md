# Generate Complete Longitudinal Study with Full Pipeline Processing

This function directly addresses the workflow shown in issue \#95 by
providing a single function call to replace the complex manual
orchestration of multiple packages and workflow steps.

## Usage

``` r
generate_complete_longitudinal_study(
  participant_count = 1000,
  site_count = 150,
  study_id = "AA-AA-000-0000",
  snapshot_count = 3,
  snapshot_width = "months",
  start_date = as.Date("2025-02-01"),
  core_mappings = c("AE", "COUNTRY", "DATACHG", "DATAENT", "ENROLL", "LB", "VISIT", "PD",
    "PK", "QUERY", "STUDY", "STUDCOMP", "SDRGCOMP", "SITE", "SUBJ", "IE", "EXCLUSION")
)
```

## Arguments

- participant_count:

  Number of study participants (default: 1000)

- site_count:

  Number of study sites (default: 150)

- study_id:

  Study identifier (default: "AA-AA-000-0000")

- snapshot_count:

  Number of time points (default: 3)

- snapshot_width:

  Time between snapshots (default: "months")

- start_date:

  Study start date

- core_mappings:

  Vector of core mapping types to include

## Value

List containing all pipeline results equivalent to manual workflow

## Examples

``` r
if (FALSE) { # \dontrun{
# Direct replacement for the manual workflow in issue #95
results <- generate_complete_longitudinal_study(
  participant_count = 1000,
  site_count = 150,
  study_id = "AA-AA-000-0000",
  snapshot_count = 3,
  snapshot_width = "months"
)

# Access results similar to manual workflow
site_results <- results$reporting_site
country_results <- results$reporting_country
study_results <- results$reporting_study
} # }
```
