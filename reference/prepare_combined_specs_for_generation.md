# Generate Snapshots for Single Study Data

This function generates a list of study data snapshots based on the
provided specifications, including participant count, site count, study
ID, and the number of snapshots to be generated. The data is simulated
over a series of months, with key variables such as subject enrollment,
adverse events, and time on study being populated according to the
study's specifications.

## Usage

``` r
prepare_combined_specs_for_generation(combined_specs, desired_specs = NULL)
```

## Arguments

- desired_specs:

  A list of specifications of the data types that should be included.

- SnapshotCount:

  An integer specifying the number of snapshots to generate.

- SnapshotWidth:

  A character specifying the frequency of snapshots, defaults to
  "months". Accepts "days", "weeks", "months" and "years". User can also
  place a number and unit such as "3 months".

- ParticipantCount:

  An integer specifying the number of participants in the study.

- SiteCount:

  An integer specifying the number of sites for the study.

- StudyID:

  A string specifying the study identifier.

- workflow_path:

  A string specifying the path to the workflow mappings.

- mappings:

  A string specifying the names of the workflows to run.

- package:

  A string specifying the package in which the workflows used in
  `MakeWorkflowList()` are located.

- strStartDate:

  A string to denote when the first snapshot of simulated data occurs

## Value

A list of data snapshots, where each element contains simulated data for
a particular snapshot period (typically a month), with variables
populated according to the provided specifications.

## Details

The function generates snapshots over a sequence of months, starting
from `"2012-01-01"`. For each snapshot:

1.  The number of adverse events (`ae_num`) is simulated.

2.  The number of participants screened is determined.

3.  Key variables (such as subject ID, enrollment date, time on study,
    etc.) are populated for each data type specified in
    `combined_specs`.

## Examples

``` r
snapshots <- generate_rawdata_for_single_study(
  SnapshotCount = 3,
  SnapshotWidth = "months",
  ParticipantCount = 50,
  SiteCount = 5,
  StudyID = "ABC",
  workflow_path = "workflow/1_mappings",
  mappings = "AE",
  package = "gsm.mapping",
  strStartDate = "2012-01-01",
  desired_specs = NULL
)
#> Error in generate_rawdata_for_single_study(SnapshotCount = 3, SnapshotWidth = "months",     ParticipantCount = 50, SiteCount = 5, StudyID = "ABC", workflow_path = "workflow/1_mappings",     mappings = "AE", package = "gsm.mapping", strStartDate = "2012-01-01",     desired_specs = NULL): could not find function "generate_rawdata_for_single_study"
```
