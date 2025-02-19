devtools::load_all()
library(dplyr)

single_result <- generate_rawdata_for_single_study(SnapshotCount = 3,
                                                   ParticipantCount = 50,
                                                   SiteCount = 5,
                                                   StudyID = "ABC",
                                                   workflow_path = "workflows/1_mappings",
                                                   mappings = c("AntiCancer", "Baseline", "Consents", "Death", "OverallResponse", "StudyCompletion"),
                                                   package = "gsm.endpoints",
                                                   desired_specs = NULL)

result <- raw_data_generator(template_path = "~/gsm.datasim/data-raw/template.csv",
                             workflow_path = "workflows/1_mappings",
                             mappings = c("AntiCancer", "Baseline", "Consents", "Death", "OverallResponse", "StudyCompletion"),
                             package = "gsm.endpoints")

result2 <- raw_data_generator(template_path = "~/gsm.datasim/data-raw/small_template.csv")
