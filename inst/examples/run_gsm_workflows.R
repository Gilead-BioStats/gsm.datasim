library(gsm.core)
library(gsm.mapping)
library(gsm.kri)
library(gsm.reporting)
library(gsm.endpoints)
devtools::load_all()

data <- generate_rawdata_for_single_study(
  SnapshotCount = 1,
  SnapshotWidth = 'months',
  ParticipantCount = 100,
  SiteCount = 10,
  StudyID = "Study01",
  "workflow/1_mappings",
  "workflow/1_mappings" %>%
    system.file(package = "gsm.mapping") %>%
    list.files() %>%
    stringr::str_replace('\\.yaml$', ''),
  "gsm.mapping"
)

#step 0- Ingest source data
mappings_wf <- MakeWorkflowList(strPath = "workflow/1_mappings", strPackage = "gsm.mapping")
mappings_spec <- CombineSpecs(mappings_wf)
lRaw <- Ingest(data[[1]], mappings_spec)

#Step 1- Create mapped data layer
mappings_wf <- MakeWorkflowList(strPath = "workflow/1_mappings", strPackage = "gsm.mapping")
mapped <- RunWorkflows(mappings_wf, lRaw)

# ----
# {gsm.kri}
# ----

# Step 2 - Create Metrics - calculate metrics using mapped data
metrics_wf <- MakeWorkflowList(strPath = "workflow/2_metrics", strPackage = 'gsm.kri')
analyzed <- RunWorkflows(metrics_wf, mapped)

# Step 3 - Create Reporting Layer - create reports using metrics data
reporting_wf <- MakeWorkflowList(strPath = "workflow/3_reporting", strPackage = 'gsm.reporting')
reporting <- RunWorkflows(reporting_wf, c(mapped, list(lAnalyzed = analyzed, lWorkflows = metrics_wf)))

# Step 4 - Create KRI Reports - create KRI report using reporting data
module_wf <- MakeWorkflowList(strPath = "workflow/4_modules", strPackage = 'gsm.kri')
lReports <- RunWorkflows(module_wf, reporting)

# ----
# {gsm.endpoints}
# ----

#Step 1- Create mapped data layer for endpoints
load_all('../gsm.endpoints')
mappings_wf_ep <- MakeWorkflowList(strPath = "inst/workflow/1_mappings", strPackage = "gsm.endpoints")
options("yaml.eval.expr" = TRUE)
mapped_ep <- RunWorkflows(mappings_wf_ep, mapped)

# Step 2 - Create Metrics - calculate metrics using mapped data
endpoints_wf <- MakeWorkflowList(strPath = "inst/workflow/2_metrics", strPackage = 'gsm.endpoints')
analyzed_ep <- RunWorkflows(endpoints_wf, mapped_ep)

# Step 3 - Create Reporting Layer - create reports using metrics data
#reporting_wf <- MakeWorkflowList(strPath = "workflow/3_reporting", strPackage = 'gsm.reporting')
#reporting <- RunWorkflows(reporting_wf, c(mapped, list(lAnalyzed = analyzed, lWorkflows = metrics_wf)))

# Step 4 - Create KRI Reports - create KRI report using reporting data
module_wf <- MakeWorkflowList(strPath = "inst/workflow/4_modules", strPackage = 'gsm.endpoints')
lReports <- RunWorkflows(module_wf, c(mapped_ep, analyzed_ep))

