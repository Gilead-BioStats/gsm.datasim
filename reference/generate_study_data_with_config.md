# Generate study data using configuration object

Internal function to generate study data based on StudyConfig settings

## Usage

``` r
generate_study_data_with_config(
  config,
  workflow_path = "workflow/1_mappings",
  mappings = NULL,
  package = "gsm.mapping"
)
```

## Arguments

- config:

  StudyConfig object containing all configuration

- workflow_path:

  Path to workflow mappings

- mappings:

  Mapping specifications to use

- package:

  Package containing the workflows

## Value

List of generated study data
