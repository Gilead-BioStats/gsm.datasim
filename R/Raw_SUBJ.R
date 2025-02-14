Raw_SUBJ <- function(data, previous_data, spec, startDate, endDate, ...) {

  inps <- list(...)

  curr_spec <- spec$Raw_SUBJ

  if ("Raw_SUBJ" %in% names(previous_data)) {
    dataset <- previous_data$Raw_SUBJ
    previous_row_num <- nrow(dataset)
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n_subj - previous_row_num
  if (n == 0) return(dataset)

  if (!("siteid" %in% names(curr_spec))) {
    curr_spec$siteid <- list(required = TRUE)
  }

  if (!("enrolldt" %in% names(curr_spec))) {
    curr_spec$enrolldt <- list(required = TRUE)
  }

  if (all(c("siteid", "invid", "country") %in% names(curr_spec))) {
    curr_spec$subject_site_synq <- list(required = TRUE)
    curr_spec$siteid <- NULL
    curr_spec$invid <- NULL
    curr_spec$country <- NULL

  }

  if (all(c("subjid", "subject_nsv") %in% names(curr_spec))) {
    curr_spec$subjid_subject_nsv <- list(required = TRUE)
    curr_spec$subjid <- NULL
    curr_spec$subject_nsv <- NULL
  }

  if (all(c("enrollyn", "enrolldt", "timeonstudy", "firstparticipantdate", "firstdosedate", "timeontreatment") %in% names(curr_spec))) {
    curr_spec$enrollyn_enrolldt_timeonstudy_firstparticipantdate_firstdosedate_timeontreatment <- list(required = TRUE)
    curr_spec$enrolldt <- NULL
    curr_spec$timeonstudy <- NULL
    curr_spec$enrollyn <- NULL

    curr_spec$firstparticipantdate <- NULL
    curr_spec$firstdosedate <- NULL
    curr_spec$timeontreatment <- NULL
  }

  args <- list(
    studyid = list(n, data$Raw_STUDY$protocol_number[[1]]),
    subjid_subject_nsv = list(n, previous_data$Raw_SUBJ$subjid),
    subject_site_synq = list(n, data$Raw_SITE),
    enrollyn_enrolldt_timeonstudy_firstparticipantdate_firstdosedate_timeontreatment = list(n, startDate, endDate),
    default = list(n)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_SUBJ, ...)

  # Recalculate for all data
  res$timeonstudy <- timeonstudy(n, res$enrolldt, endDate)

  return(res)
}
