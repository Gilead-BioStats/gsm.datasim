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
generate_rawdata_for_single_snapshot <- function(SnapshotCount,
                                                 ParticipantCount,
                                                 SiteCount,
                                                 StudyID,
                                                 combined_specs,
                                                 desired_specs = c("Raw_STUDY",
                                                                   "Raw_SITE",
                                                                   "Raw_SUBJ",
                                                                   "Raw_ENROLL",
                                                                   "Raw_AE",
                                                                   "Raw_PD")) {
  # Generate start and end dates for snapshots
  start_dates <- seq(as.Date("2012-01-01"), length = SnapshotCount, by = "months")
  end_dates <- seq(as.Date("2012-02-01"), length = SnapshotCount, by = "months") - 1

  # Specify the desired first few elements in order
  desired_order <- c("Raw_STUDY", "Raw_SITE", "Raw_SUBJ", "Raw_ENROLL")

  # Rearrange the elements
  combined_specs <- combined_specs[c(desired_order, setdiff(names(combined_specs), desired_order))]
  combined_specs <- combined_specs[desired_specs]


  subject_count <- count_gen(ParticipantCount, SnapshotCount)
  site_count <- count_gen(SiteCount, SnapshotCount)
  enrollment_count <- lapply(subject_count, screened)
  ae_count <- subject_count * 3
  pd_count <- subject_count * 3


  snapshots <- list()

  # Generate snapshots using lapply
  for (snapshot_idx in seq_len(SnapshotCount)) {
    # Initialize list to store data types

    data <- list()

    if (snapshot_idx == 1) {
      data$Raw_STUDY <- as.data.frame(Raw_STUDY(data, combined_specs,
                                                StudyID = StudyID,
                                                SiteCount = SiteCount,
                                                ParticipantCount = ParticipantCount))
    } else {
      data$Raw_STUDY <- snapshots[[1]]$Raw_STUDY
    }

    # Loop over each raw data type specified in combined_specs
    for (data_type in names(combined_specs)) {
      if (data_type == "Raw_STUDY") next

      # Determine the number of records 'n' based on data_type
      n <- dplyr::case_when(
        data_type == "Raw_AE" ~ ae_count[snapshot_idx],
        data_type == "Raw_ENROLL" ~ unlist(enrollment_count[snapshot_idx]),
        data_type == "Raw_SITE" ~ site_count[snapshot_idx],
        data_type == "Raw_PD" ~ pd_count[snapshot_idx],
        data_type == "Raw_SUBJ" ~ subject_count[snapshot_idx],
        TRUE ~ subject_count[snapshot_idx]
      )
      generator_func <- data_type
      # Determine arguments based on variable name
      args <- switch(data_type,
                     Raw_SITE = list(data, combined_specs, n_sites = n, split_vars = list("Country_State_City")),
                     Raw_SUBJ = list(data, combined_specs, n_subj = n, startDate = start_dates[snapshot_idx],
                                     endDate = end_dates[snapshot_idx], split_vars = list("subject_site_synq",
                                                                          "subjid_subject_nsv",
                                                                          "enrollyn_enrolldt_timeonstudy")),
                     Raw_ENROLL = list(data, combined_specs, n_enroll = n, split_vars = list("subject_to_enrollment")),
                     list(data, combined_specs, n = n)  # Default case
      )

      variable_data <- do.call(generator_func, args)

      # Combine variables into a data frame
      data[[data_type]] <- as.data.frame(variable_data)
    }

    to_subj <- data$Raw_ENROLL %>%
      dplyr::select(subjid, enrollyn)

    data$Raw_SUBJ <- data$Raw_SUBJ %>%
      dplyr::rows_upsert(to_subj, by = "subjid") %>%
      dplyr::mutate(
        enrolldt = dplyr::if_else(enrollyn == "N", as.Date(NA), enrolldt),
        timeonstudy = dplyr::if_else(enrollyn == "N", NA, timeonstudy)
      )

    snapshots[[snapshot_idx]] <- data
  }

  # Assign snapshot end dates as names
  names(snapshots) <- as.character(end_dates)
  return(snapshots)
}

