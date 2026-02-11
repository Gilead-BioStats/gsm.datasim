# Generate Longitudinal Clinical Trial Data with Integrated Workflow

Creates a longitudinal study with multiple time points and optional
integrated pipeline processing. This function provides a clean,
intuitive interface for generating realistic clinical trial data over
time.

## Usage

``` r
create_longitudinal_study(
  study_id = "STUDY-001",
  participants = 100,
  sites = 10,
  timepoints = 5,
  interval = "1 month",
  start_date = Sys.Date() - 365,
  domains = c("AE", "LB", "VISIT"),
  include_pipeline = FALSE,
  study_type = "standard"
)
```

## Arguments

- study_id:

  Study identifier

- participants:

  Number of study participants

- sites:

  Number of study sites

- timepoints:

  Number of data collection timepoints

- interval:

  Time interval between collection points (e.g., "1 month", "2 weeks")

- start_date:

  Study start date

- domains:

  Clinical domains to include (e.g., c("AE", "LB", "VISIT"))

- include_pipeline:

  Whether to run full analytics pipeline (raw -\> mapped -\> metrics -\>
  reports)

- study_type:

  Type of study - "standard" uses gsm.kri metrics, "endpoints" uses
  gsm.endpoints metrics

## Value

LongitudinalStudy object containing all generated data and results

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple longitudinal study
study <- create_longitudinal_study(
  study_id = "ONCOLOGY-001",
  participants = 200,
  sites = 15,
  timepoints = 6,
  interval = "2 months",
  domains = c("AE", "LB", "VISIT")
)

# With full analytics pipeline
study_with_analytics <- create_longitudinal_study(
  study_id = "CARDIO-002",
  participants = 500,
  sites = 25,
  timepoints = 12,
  interval = "1 month",
  domains = c("AE", "LB", "VISIT", "QUERY"),
  include_pipeline = TRUE
)

# Access results intuitively
raw_data <- study_with_analytics$raw_data
site_analytics <- study_with_analytics$analytics$by_site
country_analytics <- study_with_analytics$analytics$by_country
} # }
```
