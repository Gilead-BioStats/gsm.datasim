# Quick longitudinal study creation

Creates a complete longitudinal study with sensible defaults and runs
analytics

## Usage

``` r
quick_longitudinal_study(
  study_name = "Clinical Trial",
  participants = 1000,
  sites = 150,
  months_duration = 24,
  study_type = "standard",
  run_analytics = TRUE
)
```

## Arguments

- study_name:

  Name of the study

- participants:

  Number of participants (default 1000)

- sites:

  Number of sites (default 150)

- months_duration:

  Duration in months (default 24)

- study_type:

  Type of study - "standard" or "endpoints"

- run_analytics:

  Whether to run the analytics pipeline (default TRUE)

## Value

LongitudinalStudy object with complete data and analytics
