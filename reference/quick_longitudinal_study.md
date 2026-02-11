# Quick Start: Generate Complete Study with Full Analytics

One-liner function to generate a complete longitudinal study with full
analytics pipeline. This directly replaces the complex manual workflow
from issue \#95.

## Usage

``` r
quick_longitudinal_study(
  study_name = "Clinical Trial",
  participants = 1000,
  sites = 150,
  months_duration = 3,
  study_type = "standard",
  domains = NULL
)
```

## Arguments

- study_name:

  Human-readable study name (automatically formats study ID)

- participants:

  Number of participants (default: 1000)

- sites:

  Number of sites (default: 150)

- months_duration:

  Study duration in months (default: 3)

- study_type:

  Type of study - "standard" for basic clinical domains or "endpoints"
  for comprehensive endpoint analysis

- domains:

  Clinical domains to include (overrides study_type if specified)

## Value

LongitudinalStudy object with complete analytics

## Examples

``` r
if (FALSE) { # \dontrun{
# Standard clinical trial
study <- quick_longitudinal_study("Oncology Phase III", study_type = "standard")

# Comprehensive endpoints study
study <- quick_longitudinal_study("Cardio Endpoints", study_type = "endpoints")

# Custom domains (overrides study_type)
study <- quick_longitudinal_study("Custom Trial",
                                 domains = c("AE", "LB"))
} # }
```
