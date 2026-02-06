# Fluent Builder Interface for Clinical Study Data Generation

Fluent Builder Interface for Clinical Study Data Generation

Fluent Builder Interface for Clinical Study Data Generation

## Details

A fluent, chainable interface that makes dataset generation more
intuitive and discoverable through method chaining.

## Public fields

- `config`:

  Internal StudyConfig object Initialize a new StudyBuilder

## Methods

### Public methods

- [`StudyBuilder$new()`](#method-StudyBuilder-new)

- [`StudyBuilder$with_study_design()`](#method-StudyBuilder-with_study_design)

- [`StudyBuilder$over_time()`](#method-StudyBuilder-over_time)

- [`StudyBuilder$with_standard_datasets()`](#method-StudyBuilder-with_standard_datasets)

- [`StudyBuilder$add_custom_dataset()`](#method-StudyBuilder-add_custom_dataset)

- [`StudyBuilder$with_adverse_events()`](#method-StudyBuilder-with_adverse_events)

- [`StudyBuilder$with_study_completion()`](#method-StudyBuilder-with_study_completion)

- [`StudyBuilder$with_data_quality()`](#method-StudyBuilder-with_data_quality)

- [`StudyBuilder$preview()`](#method-StudyBuilder-preview)

- [`StudyBuilder$generate()`](#method-StudyBuilder-generate)

- [`StudyBuilder$clone()`](#method-StudyBuilder-clone)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    StudyBuilder$new(study_id = "STUDY001")

#### Arguments

- `study_id`:

  Study identifier Configure basic study parameters

------------------------------------------------------------------------

### Method `with_study_design()`

#### Usage

    StudyBuilder$with_study_design(
      participants = NULL,
      sites = NULL,
      study_id = NULL
    )

#### Arguments

- `participants`:

  Number of study participants

- `sites`:

  Number of study sites

- `study_id`:

  Study identifier (if different from initialization) Configure temporal
  aspects of the study

------------------------------------------------------------------------

### Method `over_time()`

#### Usage

    StudyBuilder$over_time(
      start_date = NULL,
      snapshots = NULL,
      frequency = NULL,
      end_date = NULL
    )

#### Arguments

- `start_date`:

  Study start date

- `snapshots`:

  Number of snapshots to generate

- `frequency`:

  Time between snapshots (e.g., "months", "2 weeks")

- `end_date`:

  Optional explicit end date Include standard clinical datasets with
  default configurations

------------------------------------------------------------------------

### Method `with_standard_datasets()`

#### Usage

    StudyBuilder$with_standard_datasets(
      adverse_events = TRUE,
      protocol_deviations = TRUE,
      lab_data = FALSE,
      visits = TRUE,
      enrollment = TRUE
    )

#### Arguments

- `adverse_events`:

  Include adverse event data

- `protocol_deviations`:

  Include protocol deviation data

- `lab_data`:

  Include laboratory data

- `visits`:

  Include visit data

- `enrollment`:

  Include enrollment data Add custom dataset with flexible configuration

------------------------------------------------------------------------

### Method `add_custom_dataset()`

#### Usage

    StudyBuilder$add_custom_dataset(
      dataset_type,
      count = NULL,
      depends_on = character(0),
      growth = "linear",
      ...
    )

#### Arguments

- `dataset_type`:

  Type of dataset (e.g., "Raw_CustomDomain")

- `count`:

  Count formula (numeric, function, or expression)

- `depends_on`:

  Dependencies on other datasets

- `growth`:

  How the dataset grows over time ("linear", "exponential", "constant")

- `...`:

  Additional arguments for the dataset generator Configure adverse
  events with specific parameters

------------------------------------------------------------------------

### Method `with_adverse_events()`

#### Usage

    StudyBuilder$with_adverse_events(
      rate_per_patient = 3,
      severity_distribution = c(mild = 0.6, moderate = 0.3, severe = 0.1),
      temporal_pattern = "increasing"
    )

#### Arguments

- `rate_per_patient`:

  Average AEs per patient

- `severity_distribution`:

  Distribution of severity levels

- `temporal_pattern`:

  How AEs occur over time Configure study completion patterns

------------------------------------------------------------------------

### Method `with_study_completion()`

#### Usage

    StudyBuilder$with_study_completion(
      completion_rate = 0.85,
      dropout_pattern = "uniform"
    )

#### Arguments

- `completion_rate`:

  Overall study completion rate (0-1)

- `dropout_pattern`:

  Pattern of dropouts ("early", "late", "uniform") Include data quality
  datasets

------------------------------------------------------------------------

### Method `with_data_quality()`

#### Usage

    StudyBuilder$with_data_quality(
      queries = TRUE,
      data_changes = TRUE,
      data_entry = TRUE
    )

#### Arguments

- `queries`:

  Include query data

- `data_changes`:

  Include data change tracking

- `data_entry`:

  Include data entry tracking Preview the configuration without
  generating data

------------------------------------------------------------------------

### Method `preview()`

Display study configuration summary Generate the configured study data

#### Usage

    StudyBuilder$preview()

------------------------------------------------------------------------

### Method `generate()`

#### Usage

    StudyBuilder$generate(
      workflow_path = "workflow/1_mappings",
      mappings = NULL,
      package = "gsm.mapping"
    )

#### Arguments

- `workflow_path`:

  Path to workflow mappings

- `mappings`:

  Mapping specifications to use

- `package`:

  Package containing the workflows

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    StudyBuilder$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
