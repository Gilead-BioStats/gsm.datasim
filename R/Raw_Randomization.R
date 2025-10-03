#' Generate Raw Randomization Data
#'
#' Generate Raw Randomization Data based on `Randomization.yaml` from `gsm.mapping`.
#'
#' @inheritParams Raw_STUDY
#' @param startDate The beginning of dates to which the randomization dates can occur.
#'
#' @returns a data.frame pertaining to the raw dataset plugged into `Randomization.yaml`
#' @family internal
#' @keywords internal
#' @noRd
Raw_Randomization <- function(data, previous_data, spec, startDate, ...) {
  inps <- list(...)

  curr_spec <- spec$Raw_Randomization

  if ("Raw_Randomization" %in% names(previous_data)) {
    dataset <- previous_data$Raw_Randomization
    previous_row_num <- nrow(dataset)
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n - previous_row_num
  if (n == 0) {
    return(dataset)
  }

  if (all(c("rgmn_dt") %in% names(curr_spec))) {
    curr_spec$rgmn_dt <- list(required = TRUE)
  }

  if (all(c("subjid", "invid", "country") %in% names(curr_spec))) {
    curr_spec$subjid_invid_country <- list(required = TRUE)
    curr_spec$subjid <- NULL
    curr_spec$invid <- NULL
    curr_spec$country <- NULL
  }

  args <- list(
    studyid = list(n, data$Raw_STUDY$protocol_number[[1]]),
    subjid_invid_country = list(n, data$Raw_SUBJ),
    rgmn_dt = list(n, startDate),
    default = list(n)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_Randomization, ...)

  return(res)
}

subjid_invid_country <- function(n, Raw_SUBJ_data, ...) {
  res <- Raw_SUBJ_data[
    sample(nrow(Raw_SUBJ_data), n),
    c("subjid", "invid", "country")
  ]
  return(list(
    subjid = res$subjid,
    invid = res$invid,
    country = res$country
  ))
}

rgmn_dt <- function(n, startDate, ...) {
  as.Date(startDate)
}
