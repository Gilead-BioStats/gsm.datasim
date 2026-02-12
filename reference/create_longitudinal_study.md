# High-Level Study Creation API

This file contains convenience functions for creating studies with
minimal configuration. These functions provide simplified interfaces for
common study types. Create longitudinal study data

## Usage

``` r
create_longitudinal_study(
  study_id = "STUDY-001",
  participants = 100,
  sites = 10,
  timepoints = 5,
  interval = "1 month",
  domains = c("AE", "LB", "VISIT"),
  include_pipeline = TRUE,
  analytics_package = NULL,
  analytics_workflows = NULL
)
```

## Arguments

- study_id:

  Study identifier

- participants:

  Number of participants

- sites:

  Number of sites

- timepoints:

  Number of timepoints

- interval:

  Time between snapshots (e.g., "1 month", "2 weeks")

- domains:

  Clinical domains to include

- include_pipeline:

  Whether to run analytics pipeline

- analytics_package:

  Package containing workflows (optional)

- analytics_workflows:

  Specific workflows to run (optional)

## Value

LongitudinalStudy object with generated data

## Details

Creates a complete longitudinal study with multiple timepoints
