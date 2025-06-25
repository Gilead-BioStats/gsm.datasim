generate_risk_signals_report <- function(lData) {
  mapping_wf <- gsm.core::MakeWorkflowList(strPath = "workflow/1_mappings")
  mapped <- gsm.core::RunWorkflows(mapping_wf, lData, bKeepInputData = TRUE)

  # Step 2 - Create Analysis Data - Generate 12 KRIs
  kri_wf <- gsm.core::MakeWorkflowList(strPath = "workflow/2_metrics", strNames = "kri")
  kris <- gsm.core::RunWorkflows(kri_wf, mapped)

  cou_wf <- gsm.core::MakeWorkflowList(strPath = "workflow/2_metrics", strNames = "cou")
  cous <- gsm.core::RunWorkflows(cou_wf, mapped)

  # Step 3 - Create Reporting Data - Import Metadata and stack KRI Results

  reporting_wf_site <- gsm.core::MakeWorkflowList(strPath = "workflow/3_reporting")
  reporting_site <- gsm.core::RunWorkflows(
    reporting_wf_site,
    c(mapped, list(lAnalyzed = kris, lWorkflows = kri_wf))
  )

  reporting_wf_country <- gsm.core::MakeWorkflowList(strPath = "workflow/3_reporting")
  reporting_country <- gsm.core::RunWorkflows(
    reporting_wf_country,
    c(mapped, list(lAnalyzed = cous, lWorkflows = cou_wf))
  )

  return(
    list(
      lAnalysis_site = kris,
      lAnalysis_country = cous,
      lReporting_site = reporting_site,
      lReporting_country = reporting_country
    )
  )
}
