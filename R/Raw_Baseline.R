#' Generate Raw Baseline Imaging Data
#'
#' Generate Raw Baseline Imaging Data based on `Baseline.yaml` from `gsm.mapping`.
#'
#' @inheritParams Raw_STUDY
#' @param startDate The beginning of dates to which the Baseline Imaging date can occur.
#'
#' @returns a data.frame pertaining to the raw dataset plugged into `Baseline.yaml`
#' @family internal
#' @keywords internal
#' @noRd
Raw_Baseline <- function(data, previous_data, spec, startDate, ...) {
  inps <- list(...)

  curr_spec <- spec$Raw_Baseline

  if ("Raw_Baseline" %in% names(previous_data)) {
    dataset <- previous_data$Raw_Baseline
    previous_row_num <- nrow(dataset)
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n - previous_row_num
  if (n == 0) return(dataset)

  if (all(c("scan_dt") %in% names(curr_spec))) {
    curr_spec$scan_dt <- list(required = TRUE)
  }

  args <- list(
    subjid = list(n, external_subjid = data$Raw_SUBJ$subjid, replace = FALSE),
    scan_dt = list(n, startDate),
    default = list(n)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_Baseline, ...)

  return(res)
}

scan_dt <- function(n, startDate, ...) {
  as.Date(startDate + sample(0:2, n, replace = TRUE))
}
