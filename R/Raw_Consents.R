#' Generate Raw Consents Data
#'
#' Generate Raw Consents Data based on `Consents.yaml` from `gsm.mapping`.
#'
#' @inheritParams Raw_STUDY
#' @param startDate The beginning of dates to which the changes in consent can occur.
#' @returns a data.frame pertaining to the raw dataset plugged into `Consents.yaml`
#' @family internal
#' @keywords internal
#' @noRd
Raw_Consents <- function(data, previous_data, spec, startDate, ...) {
  inps <- list(...)

  curr_spec <- spec$Raw_Consents

  if ("Raw_Consents" %in% names(previous_data)) {
    dataset <- previous_data$Raw_Consents
    previous_row_num <- nrow(dataset)
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n - previous_row_num
  if (n == 0) {
    return(dataset)
  }

  if (all(c("cons_dt", "constype", "conscat") %in% names(curr_spec))) {
    curr_spec$cons_dt <- list(required = TRUE)
    curr_spec$constype <- list(required = TRUE)
    curr_spec$conscat <- list(required = TRUE)
  }

  args <- list(
    subjid = list(n, external_subjid = data$Raw_SUBJ$subjid, replace = FALSE),
    studyid = list(n, data$Raw_STUDY$protocol_number[[1]]),
    cons_dt = list(n, startDate),
    default = list(n)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_Consents, ...)

  return(res)
}

constype <- function(n, ...) {
  rep("WITHDRAWAL OF CONSENT", n)
}

conscat <- function(n, ...) {
  rep("MAIN STUDY CONSENT", n)
}

cons_dt <- function(n, startDate, ...) {
  as.Date(startDate)
}
