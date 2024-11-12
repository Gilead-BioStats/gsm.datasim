remotes::install_github("Gilead-BioStats/gsm")
library(gsm)
devtools::load_all()

data <- raw_data_generator(ParticipantCount = 100, SiteCount = 10, StudyID = "Study01", SnapshotCount = 5)

#step 0- Ingest source data
mappings_wf <- MakeWorkflowList(strPath = "workflow/1_mappings")
mappings_spec <- CombineSpecs(mappings_wf)
lRaw <- Ingest(data$Study01$`2012-05-31`, mappings_spec)

#Step 1- Create mapped data layer
mappings_wf <- MakeWorkflowList(strPath = "workflow/1_mappings")
mapped <- RunWorkflows(mappings_wf, lRaw)

# Step 2 - Create Metrics - calculate metrics using mapped data
metrics_wf <- MakeWorkflowList(strPath = "workflow/2_metrics")
analyzed <- RunWorkflows(metrics_wf, mapped)

# Step 3 - Create Reporting Layer - create reports using metrics data
reporting_wf <- MakeWorkflowList(strPath = "workflow/3_reporting")
reporting <- RunWorkflows(reporting_wf, c(mapped, list(lAnalyzed = analyzed, lWorkflows = metrics_wf)))

# Step 4 - Create KRI Reports - create KRI report using reporting data
module_wf <- MakeWorkflowList(strPath = "workflow/4_modules")
lReports <- RunWorkflows(module_wf, reporting)
