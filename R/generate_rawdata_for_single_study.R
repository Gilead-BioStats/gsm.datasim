#' Generate Snapshots of Study Data
#'
#' This function generates a list of study data snapshots based on the provided specifications,
#' including participant count, site count, study ID, and the number of snapshots to be generated.
#' The data is simulated over a series of months, with key variables such as subject enrollment,
#' adverse events, and time on study being populated according to the study's specifications.
#'
#' @param SnapshotCount An integer specifying the number of snapshots to generate.
#' @param ParticipantCount An integer specifying the number of participants in the study.
#' @param SiteCount An integer specifying the number of sites for the study.
#' @param StudyID A string specifying the study identifier.
#' @param combined_specs A list of specifications for the raw data variables, where each element contains
#' variable-generating functions for different data types (e.g., "Raw_AE", "Raw_ENROLL").
#' @param desired_specs A list of specifications of the data types that should be included. Default values include:
#' 'Raw_STUDY', 'Raw_SITE', 'Raw_SUBJ', 'Raw_ENROLL', 'Raw_AE', 'Raw_PD'.
#'
#' @return A list of data snapshots, where each element contains simulated data for a particular snapshot
#' period (typically a month), with variables populated according to the provided specifications.
#'
#' @details
#' The function generates snapshots over a sequence of months, starting from `"2012-01-01"`. For each snapshot:
#' \enumerate{
#'   \item The number of adverse events (`ae_num`) is simulated.
#'   \item The number of participants screened is determined.
#'   \item Key variables (such as subject ID, enrollment date, time on study, etc.) are populated for each data type
#'   specified in `combined_specs`.
#' }
#'
#' @importFrom dplyr case_when
#'
#' @examples
#' \dontrun{
#' # Define variable-generating functions
#' some_function <- function(...) { ... }
#'
#' # Generate 5 snapshots of study data
#' snapshots <- generate_snapshot(
#'   SnapshotCount = 5,
#'   ParticipantCount = 100,
#'   SiteCount = 10,
#'   StudyID = "Study01",
#'   combined_specs = list(
#'     "Raw_AE" = list("subjid" = some_function, ...),
#'     "Raw_ENROLL" = list("subjid" = some_function, ...)
#'   )
#' )
#' }
#'
#' @export
generate_rawdata_for_single_study <- function(SnapshotCount,
                                                 ParticipantCount,
                                                 SiteCount,
                                                 StudyID,
                                                 combined_specs,
                                                 desired_specs = NULL) {
  # Generate start and end dates for snapshots
  start_dates <- seq(as.Date("2012-01-01"), length.out = SnapshotCount, by = "months")
  end_dates <- seq(as.Date("2012-02-01"), length.out = SnapshotCount, by = "months") - 1


  combined_specs <- preprocess_specs(combined_specs, desired_specs)

  subject_count <- count_gen(ParticipantCount, SnapshotCount)
  counts <- list(
    "Raw_STUDY" = rep(1, SnapshotCount),
    "Raw_SITE" = count_gen(SiteCount, SnapshotCount),
    "Raw_SUBJ" = subject_count,
    "Raw_ENROLL" = enrollment_count_gen(subject_count),
    "Raw_SV" = subject_count
  )

  # ae_count <- subject_count * 3
  # pd_count <- subject_count * 3
  # sdrgcomp_count <- subject_count %/% 2
  # studcomp_count <- subject_count %/% 10

  # print(subject_count)
  # print(site_count)
  # print(enrollment_count)
  # print("--------------")

  snapshots <- list()

  # Generate snapshots using lapply
  for (snapshot_idx in seq_len(SnapshotCount)) {
    # Initialize list to store data types
    logger::log_info(glue::glue(" -- Adding snapshot {snapshot_idx}..."))
    data <- list()
    previous_data <- if (snapshot_idx == 1) list() else snapshots[[snapshot_idx - 1]]

    MinDate <- start_dates[snapshot_idx]
    MaxDate <- end_dates[snapshot_idx]
    GlobalMaxDate <- max(end_dates)

    # Loop over each raw data type specified in combined_specs
    for (data_type in names(combined_specs)) {
      logger::log_info(glue::glue(" ---- Adding dataset {data_type}..."))

      # Determine the number of records 'n' based on data_type
      n <- counts[[data_type]][snapshot_idx]

      yaml_spec <- parse_yaml_spec(glue::glue("~/gsm.datasim/inst/datasets/{data_type}.yaml"))

      # Extract all 'external' entries from 'required_vars'
      external_names <- unique(unlist(lapply(yaml_spec$required_vars, function(e) e[["external"]]), use.names = FALSE))

      # Retrieve the values of these variables and assign names
      for (el in external_names) {
        external[[el]] <- get(el)
      }

      variable_data <- create_dataset(data_type, n, data, previous_data, yaml_spec, combined_specs, external)
      # Combine variables into a data frame
      data[[data_type]] <- as.data.frame(variable_data)
      logger::log_info(glue::glue(" ---- Dataset {data_type} added successfully"))

    }

    # if (nrow(data$Raw_ENROLL) > 0) {
    #
    #   to_subj <- data$Raw_ENROLL %>%
    #     dplyr::select(subjid, enrollyn)
    #
    #   data$Raw_SUBJ <- data$Raw_SUBJ %>%
    #     dplyr::rows_upsert(to_subj, by = "subjid") %>%
    #     dplyr::mutate(
    #       enrolldt = dplyr::if_else(enrollyn == "N", as.Date(NA), enrolldt),
    #       timeonstudy = dplyr::if_else(enrollyn == "N", NA, timeonstudy)
    #     )
    # }
    snapshots[[snapshot_idx]] <- data
    logger::log_info(glue::glue(" -- Snapshot {snapshot_idx} added successfully"))

  }

  # Assign snapshot end dates as names
  names(snapshots) <- as.character(end_dates)
  return(snapshots)
}

preprocess_specs <- function(combined_specs, desired_specs) {
  # Specify the desired first few elements in order
  desired_order <- c("Raw_STUDY", "Raw_SITE", "Raw_SUBJ", "Raw_ENROLL", "Raw_SV")
  if (!("Raw_SV" %in% names(combined_specs))) {
    combined_specs$Raw_SV <- list(
      subjid = list(required = TRUE),
      foldername = list(required = TRUE),
      instancename = list(required = TRUE),
      visit_dt = list(required = TRUE)
    )
  }
  desired_order <- desired_order[desired_order %in% names(combined_specs)]

  # Rearrange the elements
  combined_specs <- combined_specs[c(desired_order, setdiff(names(combined_specs), desired_order))]


  #Substitute if needed
  if (!is.null(desired_specs)) {
    combined_specs <- combined_specs[desired_specs]
  }

}
