#' Generate Raw EXCLUSION Data
#'
#' Generate Raw EXCLUSION data for exclusion criteria analysis.
#' This creates a minimal exclusion criteria dataset.
#'
#' @inheritParams Raw_STUDY
#' @returns a data.frame for exclusion criteria data
#' @family internal
#' @keywords internal
#' @noRd
Raw_EXCLUSION <- function(data, previous_data, spec, ...) {
  inps <- list(...)

  curr_spec <- spec$Raw_EXCLUSION

  if ("Raw_EXCLUSION" %in% names(previous_data)) {
    dataset <- previous_data$Raw_EXCLUSION
    previous_row_num <- nrow(dataset)
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n_EXCLUSION - previous_row_num
  if (n == 0) {
    return(dataset)
  }

  # Simple implementation - create basic exclusion criteria data
  args <- list(
    subjid = list(n, external_subjid = data$Raw_SUBJ$subjid),
    studyid = list(n, data$Raw_STUDY$protocol_number[[1]]),
    default = list(n)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_EXCLUSION, ...)

  return(res)
}