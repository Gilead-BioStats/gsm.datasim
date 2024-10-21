# Test Data Simulator

## Overview
The `{gsm.datasim}` package provides functionality to produce sample data for use in the testing and development of applications and packages related to Risk-Based Quality Monitoring (RBQM) of Clinical Trials. 
Given inputs regarding the synthetic Study's desired ID, Participant Count, Site Count, Snapshot Count, and the Key Risk Indicators to be constructed, the package generates data pertaining to:

- Participant demographics
- Adverse effects
- Trial discontinuation
- Lab results
- Visit information
- Study and site location
- I/E criteria
- Study metadata

By Default, the package contructs data that would create all KRIs that are available in the [`{gsm}`](https://github.com/Gilead-BioStats/gsm) package.

## Installation
You can install the development version of `{gsm.datasim}` from GitHub with:
```
# install.packages("pak")
pak::pak("Gilead-BioStats/gsm.datasim@dev")
```

## Sample Code
```
 # Generate raw data using specified parameters
 data <- raw_data_generator(ParticipantCount = 100, SiteCount = 10, StudyID = "Study01", SnapshotCount = 5)

 # Generate raw data using specified parameters and KRIs
 data <- raw_data_generator(ParticipantCount = 100,
                            SiteCount = 10,
                            StudyID = "Study01",
                            SnapshotCount = 5,
                            workflow_path = "workflow/2_metrics",
                            kris = c("kri0001", "kri0002", "kri0003"))

 # Generate raw data using a template file
 data <- raw_data_generator()
```
