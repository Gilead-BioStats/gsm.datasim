#' Generate Raw STUDCOMP Data
#'
#' Generate Raw STUDCOMP based on `STUDCOMP.yaml` from `gsm.mapping`.
#'
#' @inheritParams Raw_STUDY
#' @returns a data.frame pertaining to the raw dataset plugged into `STUDCOMP.yaml`
#' @family internal
#' @keywords internal
#' @noRd

Raw_STUDCOMP <- function(data, previous_data, spec, ...) {
  # Function body for Raw_SDRGCOMP
  inps <- list(...)

  curr_spec <- spec$Raw_STUDCOMP

  if ("Raw_STUDCOMP" %in% names(previous_data)) {
    dataset <- previous_data$Raw_STUDCOMP
    previous_row_num <- nrow(dataset)
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n - previous_row_num
  if (n == 0) return(dataset)

  args <- list(
    subjid = list(n, data$Raw_SUBJ$subjid, replace = FALSE),
    default = list(n)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_STUDCOMP, ...)

  return(res)
}

Raw_StudyCompletion <- function(data, previous_data, spec, ...) {
  # Function body for Raw_StudyCompletion
  inps <- list(...)

  curr_spec <- spec$Raw_StudyCompletion

  if ("Raw_StudyCompletion" %in% names(previous_data)) {
    dataset <- previous_data$Raw_StudyCompletion
    previous_row_num <- nrow(dataset)
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n - previous_row_num
  if (n == 0) return(dataset)

  args <- list(
    subjid = list(n, data$Raw_SUBJ$subjid, replace = FALSE),
    default = list(n)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_StudyCompletion, ...)

  return(res)
}

compyn <- function(n, ...) {
  # Function body for compyn
  sample(c("", "N"),
         prob = c(0.1, 0.9),
         n,
         replace = TRUE)
}

compreas <- function(n, ...) {
  rep("TERMINATION OF MEDICATION", n)
}

completion_date <- function(n, ...) {
  rep(as.Date(Sys.Date()), n)
}
