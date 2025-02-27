devtools::load_all()
library(dplyr)

single_result <- generate_rawdata_for_single_study(SnapshotCount = 3,
                                                   SnapshotWidth = "months",
                                                   ParticipantCount = 50,
                                                   SiteCount = 5,
                                                   StudyID = "ABC",
                                                   workflow_path = "workflow/1_mappings",
                                                   mappings = c("AntiCancer", "Baseline", "Consents", "Death", "OverallResponse", "StudyCompletion"),
                                                   package = "gsm.mapping",
                                                   desired_specs = NULL)

result <- raw_data_generator(template_path = "~/gsm.datasim/inst/template.csv",
                             workflow_path = "workflow/1_mappings",
                             mappings = c("AntiCancer", "Baseline", "Consents", "Death", "OverallResponse", "StudyCompletion"),
                             package = "gsm.mapping")

result2 <- raw_data_generator(template_path = "~/gsm.datasim/data-raw/small_template.csv")
