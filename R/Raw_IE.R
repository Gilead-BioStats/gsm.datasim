#' Generate Raw IE Data
#'
#' Generate Raw IE based on `IE.yaml` from `gsm.mapping`.
#'
#' @inheritParams Raw_STUDY
#' @returns a data.frame pertaining to the raw dataset plugged into `IE.yaml`
#' @family internal
#' @keywords internal
#' @noRd

Raw_IE <- function(data, previous_data, spec, ...) {
  inps <- list(...)

  curr_spec <- spec$Raw_IE

  if ("Raw_IE" %in% names(previous_data)) {
    dataset <- previous_data$Raw_IE
    previous_row_num <- nrow(dataset)
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n - previous_row_num
  if (n == 0) return(dataset)

  args <- list(
    subjid = list(n, external_subjid = data$Raw_SUBJ$subjid),
    default = list(n)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_IE, ...)

  return(res)
}

tiver_std <- function(n, data, previous_data, ...) {
}
ietest_std <- function(n, data, previous_data, ...) {
}
ietestcd_std <- function(n, data, previous_data, ...) {
}
ieorres_std <- function(n, data, previous_data, ...) {
}
iecat_std <- function(n, data, previous_data, ...) {
}
