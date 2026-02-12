# Study Configuration Class

Study Configuration Class

Study Configuration Class

## Details

Manages study parameters, temporal configuration, and dataset
specifications for clinical trial data generation.

## Public fields

- `study_params`:

  List of basic study parameters

- `temporal_config`:

  List of temporal configuration settings

- `dataset_configs`:

  List of dataset configurations Initialize a new StudyConfig

## Methods

### Public methods

- [`StudyConfig$new()`](#method-StudyConfig-new)

- [`StudyConfig$set_temporal()`](#method-StudyConfig-set_temporal)

- [`StudyConfig$add_dataset()`](#method-StudyConfig-add_dataset)

- [`StudyConfig$remove_dataset()`](#method-StudyConfig-remove_dataset)

- [`StudyConfig$validate()`](#method-StudyConfig-validate)

- [`StudyConfig$clone()`](#method-StudyConfig-clone)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    StudyConfig$new(
      study_id = "STUDY001",
      participant_count = 100,
      site_count = 10,
      analytics_package = NULL,
      analytics_workflows = NULL
    )

#### Arguments

- `study_id`:

  Study identifier

- `participant_count`:

  Number of participants

- `site_count`:

  Number of sites

- `analytics_package`:

  Analytics package to use

- `analytics_workflows`:

  Specific workflows to run Set temporal configuration

------------------------------------------------------------------------

### Method `set_temporal()`

#### Usage

    StudyConfig$set_temporal(
      start_date = NULL,
      snapshot_count = NULL,
      snapshot_width = NULL,
      end_date = NULL
    )

#### Arguments

- `start_date`:

  Study start date

- `snapshot_count`:

  Number of snapshots

- `snapshot_width`:

  Time between snapshots

- `end_date`:

  Study end date Add a dataset configuration

------------------------------------------------------------------------

### Method `add_dataset()`

#### Usage

    StudyConfig$add_dataset(
      dataset_type,
      enabled = TRUE,
      count_formula = NULL,
      growth_pattern = "linear",
      dependencies = character(0),
      custom_args = list()
    )

#### Arguments

- `dataset_type`:

  Type of dataset (e.g., "Raw_AE")

- `enabled`:

  Whether the dataset should be generated

- `count_formula`:

  Formula for calculating record count

- `growth_pattern`:

  How the dataset grows over time

- `dependencies`:

  Dependencies on other datasets

- `custom_args`:

  Additional arguments for the dataset generator Remove a dataset
  configuration

------------------------------------------------------------------------

### Method `remove_dataset()`

#### Usage

    StudyConfig$remove_dataset(dataset_type)

#### Arguments

- `dataset_type`:

  Type of dataset to remove Validate the configuration

------------------------------------------------------------------------

### Method `validate()`

Basic validation of study parameters

#### Usage

    StudyConfig$validate()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    StudyConfig$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
