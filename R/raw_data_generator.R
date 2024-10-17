#' Generate Raw Data for Study Snapshots
#'
#' This function generates raw data for study snapshots based on provided participant count,
#' site count, study ID, and snapshot count. If any of these parameters are `NULL`, it loads
#' a template CSV file to iterate over multiple study configurations. The generated raw data
#' is saved as an RDS file.
#'
#' @param ParticipantCount An integer specifying the number of participants for the study.
#' If `NULL`, the function will use values from the template file.
#' @param SiteCount An integer specifying the number of sites for the study. If `NULL`, the function
#' will use values from the template file.
#' @param StudyID A string specifying the study identifier. If `NULL`, the function will use values
#' from the template file.
#' @param SnapshotCount An integer specifying the number of snapshots for the study. If `NULL`,
#' the function will use values from the template file.
#' @param template_path A string specifying the path to the template CSV file. Default is
#' `"~/gsm.datasim/data-raw/template.csv"`.
#' @param workflow_path A string specifying the path to the workflow mappings. Default is
#' `"workflow/1_mappings"`.
#'
#' @return A list of raw data generated for each study snapshot, saved as an RDS file in `"data-raw/raw_data.RDS"`.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item If `ParticipantCount`, `SiteCount`, `StudyID`, or `SnapshotCount` is `NULL`, the function reads
#'   the `template.csv` file to get the necessary parameters for multiple studies.
#'   \item It generates raw data for study snapshots based on either provided parameters or the template file.
#'   \item The generated data is saved as an RDS file and returned as a list.
#' }
#'
#' @importFrom gsm MakeWorkflowList
#'
#' @examples
#' \dontrun{
#' # Generate raw data using specified parameters
#' data <- raw_data_generator(ParticipantCount = 100, SiteCount = 10, StudyID = "Study01", SnapshotCount = 5)
#'
#' # Generate raw data using a template file
#' data <- raw_data_generator()
#' }
#'
#' @export
raw_data_generator <- function(ParticipantCount=NULL,
                               SiteCount=NULL,
                               StudyID=NULL,
                               SnapshotCount=NULL,
                               template_path = "~/gsm.datasim/data-raw/template.csv",
                               workflow_path = "workflow/1_mappings") {

  wf_mapping <- gsm::MakeWorkflowList(
    strPath = workflow_path
  )
  combined_specs <- CombineSpecs(wf_mapping)

  if (any(is.null(c(ParticipantCount, SiteCount, StudyID, SnapshotCount)))) {
    template <- read.csv(template_path)
    lRaw <- list()
    for (i in rownames(template)) {
      curr_vars <- template[i, ]
      lRaw[[curr_vars$StudyID]] <- generate_snapshot(curr_vars$SnapshotCount,
                                                     curr_vars$ParticipantCount,
                                                     curr_vars$SiteCount,
                                                     curr_vars$StudyID,
                                                     combined_specs)
    }

  } else {
    lRaw[[StudyID]] <- generate_snapshot(SnapshotCount,
                                         ParticipantCount,
                                         SiteCount,
                                         StudyID,
                                         combined_specs)
  }

  saveRDS(lRaw, "data-raw/raw_data.RDS")
  return(lRaw)
}


