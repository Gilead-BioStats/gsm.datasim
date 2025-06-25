devtools::load_all()
library(dplyr)

# generate data for KRI reports:
core_mappings <- c("AE", "COUNTRY", "DATACHG", "DATAENT", "ENROLL", "LB", "PK",
                   "PD", "QUERY", "STUDY", "STUDCOMP", "SDRGCOMP", "SITE", "SUBJ", "VISIT")

single_result <- generate_rawdata_for_single_study(SnapshotCount = 3,
                                                   SnapshotWidth = "months",
                                                   ParticipantCount = 50,
                                                   SiteCount = 5,
                                                   StudyID = "ABC",
                                                   workflow_path = "workflow/1_mappings",
                                                   mappings = core_mappings,
                                                   package = "gsm.mapping",
                                                   desired_specs = NULL)
# Step 1 - Create Mapped Data Layer - filter, aggregate and join raw data to create mapped data layer
mappings_wf <- MakeWorkflowList(strNames = core_mappings, strPath = "workflow/1_mappings", strPackage = "gsm.mapping")
mappings_spec <- CombineSpecs(mappings_wf)
lRaw <- Ingest(single_result[[1]], mappings_spec)
mapped <- RunWorkflows(mappings_wf, lRaw)

# Below is the code to run multiple snapshots for multiple studies
# Highly memory intensive, make sure hardware can handle
#
# result <- raw_data_generator(template_path = "~/gsm.datasim/inst/template.csv",
#                              workflow_path = "workflow/1_mappings",
#                              mappings = core_mappings,
#                              package = "gsm.mapping")

result2 <- raw_data_generator(template_path = "~/gsm.datasim/inst/small_template.csv")

# generate data for endpoint reports:
endpoint_mappings <- c("AntiCancer", "Baseline", "Death", "OverallResponse", "Randomization", "STUDCOMP",
                       "VISIT", "Consents", "STUDY", "SUBJ")

basic_sim <- generate_rawdata_for_single_study(
  SnapshotCount = 1,
  SnapshotWidth = "months",
  ParticipantCount = 100,
  SiteCount = 10,
  StudyID = "ABC",
  workflow_path = "workflow/1_mappings",
  mappings = endpoint_mappings,
  package = "gsm.mapping",
  desired_specs = NULL
)


# generate gilda data for monitoring visit app:
gilda_data <- generate_rawdata_for_single_study(
  SnapshotCount = 1,
  SnapshotWidth = "months",
  ParticipantCount = 1000,
  SiteCount = 10,
  StudyID = "ABC",
  workflow_path = "workflow",
  mappings = "gilda_STUDY",
  package = "monitoring.visit.app",
  desired_specs = NULL
)

gilda_data_template <- raw_data_generator(template_path = "~/gsm.datasim/inst/template.csv",
                             workflow_path = "workflow/",
                             mappings = "gilda_STUDY",
                             package = "monitoring.visit.app")
