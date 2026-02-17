# Create Study Configuration

Creates a study configuration list with study parameters, temporal
configuration, and dataset specifications for clinical trial data
generation.

## Usage

``` r
create_study_config(
  study_id = "STUDY001",
  participant_count = 100,
  site_count = 10,
  analytics_package = NULL,
  analytics_workflows = NULL
)
```

## Arguments

- study_id:

  Study identifier

- participant_count:

  Number of participants

- site_count:

  Number of sites

- analytics_package:

  Analytics package to use

- analytics_workflows:

  Specific workflows to run

## Value

A list containing study configuration
