# Generate Snapshots for Single Study Data

This function generates a list of study data snapshots based on the
provided specifications, including participant count, site count, study
ID, and the number of snapshots to be generated. The data is simulated
over a series of months, with key variables such as subject enrollment,
adverse events, and time on study being populated according to the
study's specifications.

## Usage

``` r
generate_rawdata_for_single_study(
  SnapshotCount,
  SnapshotWidth,
  ParticipantCount,
  SiteCount,
  StudyID,
  workflow_path,
  mappings,
  package,
  strStartDate = "2012-01-01",
  desired_specs = NULL
)
```

## Arguments

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

- desired_specs:

  A list of specifications of the data types that should be included.

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
#> INFO [2026-03-06 19:08:47]  -- Adding snapshot 1...
#> INFO [2026-03-06 19:08:47]  ---- Adding dataset Raw_SITE...
#> INFO [2026-03-06 19:08:47]  ---- Dataset Raw_SITE added successfully
#> INFO [2026-03-06 19:08:47]  ---- Adding dataset Raw_SUBJ...
#> INFO [2026-03-06 19:08:47]  ---- Dataset Raw_SUBJ added successfully
#> INFO [2026-03-06 19:08:47]  ---- Adding dataset Raw_ENROLL...
#> INFO [2026-03-06 19:08:47]  ---- Dataset Raw_ENROLL added successfully
#> INFO [2026-03-06 19:08:47]  ---- Adding dataset Raw_SV...
#> INFO [2026-03-06 19:08:47]  ---- Dataset Raw_SV added successfully
#> INFO [2026-03-06 19:08:47]  ---- Adding dataset Raw_VISIT...
#> INFO [2026-03-06 19:08:47]  ---- Dataset Raw_VISIT added successfully
#> INFO [2026-03-06 19:08:47]  ---- Adding dataset Raw_AE...
#> INFO [2026-03-06 19:08:47]  ---- Dataset Raw_AE added successfully
#> INFO [2026-03-06 19:08:47]  ---- Adding dataset Raw_DATAENT...
#> INFO [2026-03-06 19:08:47]  ---- Dataset Raw_DATAENT added successfully
#> INFO [2026-03-06 19:08:47]  -- Snapshot 1 added successfully
#> INFO [2026-03-06 19:08:47]  -- Adding snapshot 2...
#> INFO [2026-03-06 19:08:47]  ---- Adding dataset Raw_SITE...
#> INFO [2026-03-06 19:08:47]  ---- Dataset Raw_SITE added successfully
#> INFO [2026-03-06 19:08:47]  ---- Adding dataset Raw_SUBJ...
#> INFO [2026-03-06 19:08:47]  ---- Dataset Raw_SUBJ added successfully
#> INFO [2026-03-06 19:08:47]  ---- Adding dataset Raw_ENROLL...
#> INFO [2026-03-06 19:08:47]  ---- Dataset Raw_ENROLL added successfully
#> INFO [2026-03-06 19:08:47]  ---- Adding dataset Raw_SV...
#> INFO [2026-03-06 19:08:47]  ---- Dataset Raw_SV added successfully
#> INFO [2026-03-06 19:08:47]  ---- Adding dataset Raw_VISIT...
#> INFO [2026-03-06 19:08:47]  ---- Dataset Raw_VISIT added successfully
#> INFO [2026-03-06 19:08:47]  ---- Adding dataset Raw_AE...
#> INFO [2026-03-06 19:08:47]  ---- Dataset Raw_AE added successfully
#> INFO [2026-03-06 19:08:47]  ---- Adding dataset Raw_DATAENT...
#> INFO [2026-03-06 19:08:47]  ---- Dataset Raw_DATAENT added successfully
#> INFO [2026-03-06 19:08:47]  -- Snapshot 2 added successfully
#> INFO [2026-03-06 19:08:47]  -- Adding snapshot 3...
#> INFO [2026-03-06 19:08:47]  ---- Adding dataset Raw_SITE...
#> INFO [2026-03-06 19:08:47]  ---- Dataset Raw_SITE added successfully
#> INFO [2026-03-06 19:08:47]  ---- Adding dataset Raw_SUBJ...
#> INFO [2026-03-06 19:08:47]  ---- Dataset Raw_SUBJ added successfully
#> INFO [2026-03-06 19:08:47]  ---- Adding dataset Raw_ENROLL...
#> INFO [2026-03-06 19:08:47]  ---- Dataset Raw_ENROLL added successfully
#> INFO [2026-03-06 19:08:47]  ---- Adding dataset Raw_SV...
#> INFO [2026-03-06 19:08:47]  ---- Dataset Raw_SV added successfully
#> INFO [2026-03-06 19:08:47]  ---- Adding dataset Raw_VISIT...
#> INFO [2026-03-06 19:08:47]  ---- Dataset Raw_VISIT added successfully
#> INFO [2026-03-06 19:08:47]  ---- Adding dataset Raw_AE...
#> INFO [2026-03-06 19:08:47]  ---- Dataset Raw_AE added successfully
#> INFO [2026-03-06 19:08:47]  ---- Adding dataset Raw_DATAENT...
#> INFO [2026-03-06 19:08:47]  ---- Dataset Raw_DATAENT added successfully
#> INFO [2026-03-06 19:08:47]  -- Snapshot 3 added successfully
```
