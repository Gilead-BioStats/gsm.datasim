#' Generate Raw Anticancer Data
#'
#' Generate Raw Anticancer Data based on `Anticancer.yaml` from `gsm.mapping`.
#'
#' @inheritParams Raw_STUDY
#' @param startDate The beginning of dates to which the Anti-cancer treatments can occur.
#'
#' @returns a data.frame pertaining to the raw dataset plugged into `Anticancer.yaml`
#' @family internal
#' @keywords internal
#' @noRd
Raw_AntiCancer <- function(data, previous_data, spec, startDate, ...) {
  inps <- list(...)

  curr_spec <- spec$Raw_AntiCancer

  if ("Raw_AntiCancer" %in% names(previous_data)) {
    dataset <- previous_data$Raw_AntiCancer
    previous_row_num <- nrow(dataset)
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n - previous_row_num
  if (n == 0) return(dataset)

  if (all(c("cmtrt", "cmst_dt") %in% names(curr_spec))) {
    curr_spec$cmtrt <- list(required = TRUE)
    curr_spec$cmst_dt <- list(required = TRUE)
  }

  args <- list(
    subjid = list(n, external_subjid = data$Raw_SUBJ$subjid, replace = FALSE),
    studyid = list(n, data$Raw_STUDY$protocol_number[[1]]),
    cmst_dt = list(n, startDate),
    default = list(n)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_AntiCancer, ...)

  return(res)
}

cmtrt <- function(n, ...) {
  sample(c("RADIOTHERAPY", "OTHER CHEMOTHERAPY"), n, replace = TRUE)
}

cmst_dt <- function(n, startDate, ...) {
  as.Date(startDate + sample(-5:5, n, replace = TRUE))
}
