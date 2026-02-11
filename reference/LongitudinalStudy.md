# Generate Longitudinal Clinical Trial Data with Integrated Workflow

Generate Longitudinal Clinical Trial Data with Integrated Workflow

Generate Longitudinal Clinical Trial Data with Integrated Workflow

## Value

LongitudinalStudy object containing all generated data and results

## Details

Creates a longitudinal study with multiple time points and optional
integrated pipeline processing. This function provides a clean,
intuitive interface for generating realistic clinical trial data over
time.

## Public fields

- `study_id`:

  Study identifier

- `raw_data`:

  List of raw data snapshots

- `config`:

  Study configuration parameters

- `analytics`:

  Processed analytics results Initialize new LongitudinalStudy

## Methods

### Public methods

- [`LongitudinalStudy$new()`](#method-LongitudinalStudy-new)

- [`LongitudinalStudy$summary()`](#method-LongitudinalStudy-summary)

- [`LongitudinalStudy$run_analytics_pipeline()`](#method-LongitudinalStudy-run_analytics_pipeline)

- [`LongitudinalStudy$get_timepoint()`](#method-LongitudinalStudy-get_timepoint)

- [`LongitudinalStudy$get_domain_timeline()`](#method-LongitudinalStudy-get_domain_timeline)

- [`LongitudinalStudy$get_domain_names()`](#method-LongitudinalStudy-get_domain_names)

- [`LongitudinalStudy$clone()`](#method-LongitudinalStudy-clone)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    LongitudinalStudy$new(study_id, raw_data, config)

#### Arguments

- `study_id`:

  Study identifier

- `raw_data`:

  Raw study data snapshots

- `config`:

  Configuration parameters Get summary of study structure

------------------------------------------------------------------------

### Method [`summary()`](https://rdrr.io/r/base/summary.html)

Display comprehensive study summary Run the complete analytics pipeline

#### Usage

    LongitudinalStudy$summary()

------------------------------------------------------------------------

### Method `run_analytics_pipeline()`

Execute full analytics pipeline on study data Get data for specific
timepoint

#### Usage

    LongitudinalStudy$run_analytics_pipeline()

------------------------------------------------------------------------

### Method `get_timepoint()`

#### Usage

    LongitudinalStudy$get_timepoint(timepoint)

#### Arguments

- `timepoint`:

  Timepoint number (1-based) Get specific domain data across all
  timepoints

------------------------------------------------------------------------

### Method `get_domain_timeline()`

#### Usage

    LongitudinalStudy$get_domain_timeline(domain_name)

#### Arguments

- `domain_name`:

  Domain mapping name (e.g., "AE", "LB") Get available domain names

------------------------------------------------------------------------

### Method `get_domain_names()`

Return list of available domain names across all timepoints

#### Usage

    LongitudinalStudy$get_domain_names()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LongitudinalStudy$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples
