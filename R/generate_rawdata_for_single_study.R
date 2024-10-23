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
generate_rawdata_for_single_study <- function(SnapshotCount,
                                                 ParticipantCount,
                                                 SiteCount,
                                                 StudyID,
                                                 combined_specs) {
  # Generate start and end dates for snapshots
  start_dates <- seq(as.Date("2012-01-01"), length = SnapshotCount, by = "months")
  end_dates <- seq(as.Date("2012-02-01"), length = SnapshotCount, by = "months") - 1

  current_subjects_count <- 0

  current_subjects_count_gen <- function(current_subjects_count,
                                         ParticipantCount,
                                         SnapshotCount,
                                         snapshot_idx) {
    start <- current_subjects_count + 1
    iteration <- ParticipantCount %/% SnapshotCount
    end <- snapshot_idx * iteration
    result <- sample(start:end, size = 1)

    if (snapshot_idx == SnapshotCount) result <- ParticipantCount

    return(result)
  }

  # Generate snapshots using lapply
  snapshots <- lapply(seq_len(SnapshotCount), function(snapshot_idx) {
    current_subjects_count <<- current_subjects_count_gen(current_subjects_count,
                                                         ParticipantCount,
                                                         SnapshotCount,
                                                         snapshot_idx)
    # Simulate the number of adverse events and screened participants
    ae_num <- sample(seq(current_subjects_count * 2, current_subjects_count * 3), 1)
    pd_num <- sample(seq(current_subjects_count * 2, current_subjects_count * 3), 1)
    query_num <- sample(seq(current_subjects_count * 20, current_subjects_count * 40), 1)
    dataent_num <- sample(seq(current_subjects_count * 150, current_subjects_count * 200), 1)
    datachg_num <- dataent_num * 7
    studcomp_num <- sample(seq(current_subjects_count %/% 12, current_subjects_count  %/% 8), 1)
    sdrgcomp_num <- sample(seq(current_subjects_count %/% 2.5, current_subjects_count  %/% 1.5), 1)
    screened_res <- screened(current_subjects_count)

    # Initialize list to store data types
    data_list <- list()

    # Loop over each raw data type specified in combined_specs
    for (data_type in sort(names(combined_specs), decreasing = TRUE)) {
      specs <- combined_specs[[data_type]]
      # Determine the number of records 'n' based on data_type
      n <- dplyr::case_when(
        data_type %in% c("Raw_AE", "Mapped_AE") ~ ae_num,
        data_type %in% c("Raw_ENROLL", "Mapped_ENROLL") ~ screened_res,
        data_type == "Raw_STUDY" ~ 1,
        data_type %in% c("Raw_PD", "Mapped_PD") ~ pd_num,
        data_type %in% c("Raw_QUERY", "Mapped_QUERY") ~ query_num,
        data_type %in% c("Raw_DATAENT", "Mapped_DATAENT") ~ dataent_num,
        data_type == "Raw_SITE" ~ SiteCount,
        data_type %in% c("Raw_DATACHG", "Mapped_DATACHG") ~ datachg_num,
        data_type %in% c("Raw_STUDCOMP", "Mapped_STUDCOMP") ~ studcomp_num,
        data_type %in% c("Raw_SDRGCOMP", "Mapped_SDGRCOMP") ~ sdrgcomp_num,
        TRUE ~ current_subjects_count
      )
      # Generate data for each variable in specs
      variable_data <- lapply(names(specs), function(var_name) {
        #browser()
        generator_func <- var_name

        # Determine arguments based on variable name
        args <- switch(var_name,
                       subjid = if (data_type != "Raw_SUBJ") list(n, data_list$Raw_SUBJ) else list(n),
                       studyid = list(StudyID),
                       num_plan_site = list(SiteCount),
                       num_plan_subj = list(ParticipantCount),
                       enrollyn = if (data_type != "Raw_SUBJ") list(n, FALSE) else list(n),
                       enrolldt = list(n, start_dates[snapshot_idx], end_dates[snapshot_idx]),
                       timeonstudy = list(n, start_dates[snapshot_idx], end_dates[snapshot_idx]),
                       list(n)  # Default case
        )

        # Generate data using the generator function
        do.call(generator_func, args)
      })
      names(variable_data) <- names(specs)

      # Combine variables into a data frame
      data_list[[data_type]] <- as.data.frame(variable_data)
    }

    data_list
  })

  # Assign snapshot end dates as names
  names(snapshots) <- as.character(end_dates)
  return(snapshots)
}
