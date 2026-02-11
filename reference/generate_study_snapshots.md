# Generate study data across multiple timepoints

Generate study data across multiple timepoints

## Usage

``` r
generate_study_snapshots(
  study_id,
  participants,
  sites,
  timepoints,
  interval,
  mappings
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

  Time interval between snapshots

- mappings:

  Vector of mapping names to use

## Value

List of raw data for each timepoint
