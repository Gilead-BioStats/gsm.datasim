devtools::load_all()
library(dplyr)
core_mappings <- c("AE", "COUNTRY", "DATACHG", "DATAENT", "ENROLL", "LB",
                   "PD", "QUERY", "STUDY", "STUDCOMP", "SDRGCOMP", "SITE", "SUBJ")

single_result <- generate_rawdata_for_single_study(SnapshotCount = 3,
                                                   SnapshotWidth = "months",
                                                   ParticipantCount = 50,
                                                   SiteCount = 5,
                                                   StudyID = "ABC",
                                                   workflow_path = "workflow/1_mappings",
                                                   mappings = core_mappings,
                                                   package = "gsm.mapping",
                                                   desired_specs = NULL)

# Below is the code to run multiple snapshots for multiple studies
# Highly memory intensive, make sure hardware can handle
#
# result <- raw_data_generator(template_path = "~/gsm.datasim/inst/template.csv",
#                              workflow_path = "workflow/1_mappings",
#                              mappings = core_mappings,
#                              package = "gsm.mapping")

result2 <- raw_data_generator(template_path = "~/gsm.datasim/inst/small_template.csv")


# Below provides sufficient flagging across all kri's except kri0008
set.seed(123)

lSource_ <- gsm.datasim::generate_rawdata_for_single_study(
  SnapshotCount = 5,
  SnapshotWidth = "months",
  ParticipantCount = 1000,
  SiteCount = 200,
  StudyID = "ABC",
  workflow_path = "workflow/1_mappings",
  mappings = core_mappings,
  package = "gsm.mapping",
  desired_specs = NULL
)

lSource <- lSource_[[5]]
