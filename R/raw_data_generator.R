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
#' `"~/gsm.datasim/inst/template.csv"`.
#' @param workflow_path A string specifying the path to the workflow mappings. Default is
#' `"workflow/1_mappings"`.
#' @param generate_reports A boolean, specifying whether or not to produce reports upon execution. Default is FALSE.
#' @param mappings A string specifying the names of the workflows to run.
#' @param package A string specifying the package in which the workflows used in `MakeWorkflowList()` are located. Default is "gsm".
#' @param save A boolean, specifying whether or not this should be saved out as an RDS
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
#' @examples
#' \dontrun{
#' # Generate raw data using specified parameters
#' data <- raw_data_generator(ParticipantCount = 100, SiteCount = 10, StudyID = "Study01", SnapshotCount = 5)
#'
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
    template_path = "~/gsm.datasim/inst/template.csv",
    workflow_path = "workflow/1_mappings",
    generate_reports = FALSE,
    mappings = NULL,
    package = "gsm.mapping",
    save = FALSE
) {
  # Initialize the list to store raw data
  raw_data_list <- list()

  # Check if any of the key parameters are NULL
  if (any(is.null(c(ParticipantCount, SiteCount, StudyID, SnapshotCount)))) {
    # Read the template CSV file
    template <- read.csv(template_path, stringsAsFactors = FALSE)

    # Generate raw data for each study configuration in the template
    raw_data_list <- lapply(seq_len(nrow(template)), function(i) {

      curr_vars <- template[i, ]
      logger::log_info(glue::glue("Adding {curr_vars$StudyID}..."))
      tictoc::tic()
      res <- generate_rawdata_for_single_study(
        SnapshotCount = curr_vars$SnapshotCount,
        SnapshotWidth = curr_vars$SnapshotWidth,
        ParticipantCount = curr_vars$ParticipantCount,
        SiteCount = curr_vars$SiteCount,
        StudyID = curr_vars$StudyID,
        workflow_path = workflow_path,
        mappings = mappings,
        package = package
      )

      logger::log_info(glue::glue("Added {curr_vars$StudyID} successfully"))
      tictoc::toc()

      res
    })

    # Assign study IDs as names to the list elements
    names(raw_data_list) <- template$StudyID

  } else {
    # Generate raw data for the single study configuration provided
    raw_data_list[[StudyID]] <- generate_rawdata_for_single_study(
      SnapshotCount = SnapshotCount,
      ParticipantCount = ParticipantCount,
      SiteCount = SiteCount,
      StudyID = StudyID,
      workflow_path = workflow_path,
      mappings = mappings,
      package = package
    )
  }

  if (generate_reports) {
    raw_data_list <- lapply(raw_data_list, function(study_data) {
      create_snapshot_reports(study_data)
    })
  }

  # Save the raw data list to an RDS file
  if(save) {
    save_data_on_disk(raw_data_list)

    logger::log_info(glue::glue("Dataset saved successfully!"))
  }

  return(raw_data_list)
}
