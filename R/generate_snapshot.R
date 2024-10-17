#' Generate Snapshots of Study Data
#'
#' This function generates a list of study data snapshots based on the provided specifications,
#' including participant count, site count, study ID, and the number of snapshots to be generated.
#' The data is simulated over a series of 12 months, with key variables such as subject enrollment,
#' adverse events, and time on study being populated according to the study's specifications.
#'
#' @param SnapshotCount An integer specifying the number of snapshots to generate.
#' @param ParticipantCount An integer specifying the number of participants in the study.
#' @param SiteCount An integer specifying the number of sites for the study.
#' @param StudyID A string specifying the study identifier.
#' @param combined_specs A list of specifications for the raw data variables, where each element contains
#' variable metadata for different data types (e.g., "Raw_AE", "Raw_ENROLL").
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
#' @importFrom dplyr bind_rows case_when
#'
#' @examples
#' \dontrun{
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
generate_snapshot <- function(SnapshotCount, ParticipantCount, SiteCount, StudyID, combined_specs) {
  lSnapshots <- list()

  startDates <- seq(as.Date("2012-01-01"), length = SnapshotCount, by = "months")
  endDates <- seq(as.Date("2012-02-01"), length = SnapshotCount, by = "months") - 1

  for (snapshot_number in 1:SnapshotCount)  {
    lData <- list()

    ## Simulate data for 12 months
    ae_num <- sample(seq(from = ParticipantCount,
                         to = ParticipantCount * 2),1)
    screened_res <- screened(ParticipantCount)

    for (raw_data_name in sort(names(combined_specs), decreasing = TRUE)) {
      raw_data_meta <- combined_specs[[raw_data_name]]
      new_chunk <- list()
      for (var in names(raw_data_meta)) {
        res <- NULL

        n <- dplyr::case_when(
          raw_data_name == "Raw_AE" ~ ae_num,
          raw_data_name == "Raw_ENROLL" ~ screened_res,
          .default = ParticipantCount
        )


        #if (var == "invid") browser()
        if (var == "subjid" & raw_data_name != "Raw_SUBJ") {
          res <- do.call(var, list(n, lData$Raw_SUBJ))
        }

        if (var == "studyid") {
          res <- do.call(var, list(StudyID))
        }

        if (var == "num_plan_site") {
          res <- do.call(var, list(SiteCount))
        }

        if (var == "num_plan_subj") {
          res <- do.call(var, list(n))
        }

        if (var == "enrollyn" & raw_data_name != "Raw_SUBJ") {
          res <- do.call(var, list(n, FALSE))
        }

        if (var == "enrolldt") {
          res <- do.call(var, list(n,
                                   startDates[snapshot_number],
                                   endDates[snapshot_number]))
        }

        if (var == "timeonstudy") {
          res <- do.call(var, list(n,
                                   startDates[snapshot_number],
                                   endDates[snapshot_number]))
        }

        if (is.null(res)) {
          res <- do.call(var, list(n))

        }

        new_chunk[[var]] <- res
      }
      lData[[raw_data_name]] <- dplyr::bind_rows(lData[[raw_data_name]], new_chunk)
    }

    ind <- as.character(endDates[[snapshot_number]])
    lSnapshots[[ind]] <- lData

  }
  return(lSnapshots)
}
