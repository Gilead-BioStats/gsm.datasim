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
#' @param kris A string or array of strings specifying the KRIs that will be used to
#' determine the spec. Default is `NULL`.
#' @param package A string specifying the package in which the workflows used in `MakeWorkflowList()` are located. Default is "gsm".
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
#' # Generate raw data using specified parameters and KRIs
#' data <- raw_data_generator(ParticipantCount = 100,
#'                            SiteCount = 10,
#'                            StudyID = "Study01",
#'                            SnapshotCount = 5,
#'                            workflow_path = "workflow/2_metrics",
#'                            kris = c("kri0001", "kri0002", "kri0003"))
#'
#' # Generate raw data using a template file
#' data <- raw_data_generator()
#' }
#'
#' @export
raw_data_generator <- function(
    ParticipantCount = NULL,
    SiteCount = NULL,
    StudyID = NULL,
    SnapshotCount = NULL,
    template_path = "~/gsm.datasim/data-raw/template.csv",
    workflow_path = "workflow/1_mappings",
    kris = NULL,
    package = "gsm"
) {
  # Load workflow mappings and combine specifications
  wf_mapping <- gsm::MakeWorkflowList(strPath = workflow_path, strNames = kris, strPackage = package)
  wf_req <-  gsm::MakeWorkflowList(strPath =  "workflow/1_mappings", strNames = c("SUBJ", "STUDY", "SITE"), strPackage = "gsm")
  wf_all <- c(wf_mapping, wf_req)
  combined_specs <- CombineSpecs(wf_all)

  #check to see if Raw_Site Raw_Study and Raw_Subj are in the spec

  # Initialize the list to store raw data
  raw_data_list <- list()

  # Check if any of the key parameters are NULL
  if (any(is.null(c(ParticipantCount, SiteCount, StudyID, SnapshotCount)))) {
    # Read the template CSV file
    template <- read.csv(template_path, stringsAsFactors = FALSE)

    # Generate raw data for each study configuration in the template
    raw_data_list <- lapply(seq_len(nrow(template)), function(i) {
      curr_vars <- template[i, ]
      generate_snapshot(
        SnapshotCount = curr_vars$SnapshotCount,
        ParticipantCount = curr_vars$ParticipantCount,
        SiteCount = curr_vars$SiteCount,
        StudyID = curr_vars$StudyID,
        combined_specs = combined_specs
      )
    })

    # Assign study IDs as names to the list elements
    names(raw_data_list) <- template$StudyID

  } else {
    # Generate raw data for the single study configuration provided
    raw_data_list[[StudyID]] <- generate_snapshot(
      SnapshotCount = SnapshotCount,
      ParticipantCount = ParticipantCount,
      SiteCount = SiteCount,
      StudyID = StudyID,
      combined_specs = combined_specs
    )
  }

  # Save the raw data list to an RDS file
  saveRDS(raw_data_list, file = file.path("data-raw", "raw_data.RDS"))

  return(raw_data_list)
}
