#' Generate Raw AE Data
#'
#' Generate Raw AE Data based on `AE.yaml` from `gsm.mapping`.
#'
#' @inheritParams Raw_STUDY
#' @param startDate The beginning of dates to which the AEs can occur.
#' @param endDate The end of dates to which the AEs can occur.
#'
#' @returns a data.frame pertaining to the raw dataset plugged into `AE.yaml`
#' @family internal
#' @keywords internal
#' @noRd
Raw_AE <- function(data, previous_data, spec, startDate, endDate, ...) {
  inps <- list(...)

  curr_spec <- spec$Raw_AE

  if ("Raw_AE" %in% names(previous_data)) {
    dataset <- previous_data$Raw_AE
    previous_row_num <- nrow(dataset)
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n - previous_row_num
  if (n == 0) return(dataset)

  if (all(c("aeser", "aest_dt", "aeen_dt", "mdrpt_nsv", "mdrsoc_nsv", "aetoxgr") %in% names(curr_spec))) {
    curr_spec$aeser <- list(required = TRUE)
    curr_spec$aest_dt_aeen_dt <- list(required = TRUE)
    curr_spec$aest_dt <- NULL
    curr_spec$aeen_dt <- NULL
    curr_spec$mdrpt_nsv_mdrsoc_nsv <- list(required = TRUE)
    curr_spec$mdrpt_nsv <- NULL
    curr_spec$mdrsoc_nsv <- NULL
    curr_spec$aetoxgr <- list(required = TRUE)
  }

  subjs <- data$Raw_SUBJ$subjid

  args <- list(
    subjid = list(n, external_subjid = sample(subjs, round(length(unique(subjs))/5))),
    aest_dt_aeen_dt = list(n, startDate, endDate),
    mdrpt_nsv_mdrsoc_nsv = list(n),
    studyid = list(n, data$Raw_STUDY$protocol_number[[1]]),
    default = list(n, startDate)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_AE, ...)

  return(res)
}

aeser <- function(n, ...) {
  # Function body for aeser
  sample(c("Y", "N"), n, replace = TRUE)
}

aeongo <- function(n, ...) {
  # Function body for aeser
  sample(c("Y", "N"), n, replace = TRUE)
}

aerel <- function(n, ...) {
  # Function body for aeser
  sample(c("Y", "N"), n, replace = TRUE)
}

aest_dt <- function(n, startDate, endDate, ...) {
  sample(seq(as.Date(startDate), as.Date(endDate), by = "day"), n, replace = TRUE)
}
aeen_dt <- function(n, aestartDate, ... ) {
  as.Date(aestartDate) + sample(1:3, n, replace = TRUE)
}
mdrpt_nsv <- function(x){
  return(x$mdrpt_nsv)
}
mdrsoc_nsv <- function(x){
  return(x$mdrsoc_nsv)
}

mdrpt_nsv_mdrsoc_nsv <- function(n, ...) {
   joined_term <- clindata_ae %>% slice_sample(n = n, replace = TRUE)
   return(list(
     mdrpt_nsv = mdrpt_nsv(joined_term),
     mdrsoc_nsv = mdrsoc_nsv(joined_term)
   ))
}
aetoxgr <- function(n, ...) {
  sample(1:5, n, replace = TRUE)
}

aest_dt_aeen_dt <- function(n, startDate, endDate, ...) {
  aest_dat <- aest_dt(n, startDate, endDate, ...)
  aeen_dat <- aeen_dt(n, aest_dat, ...)
  return(list(
    aest_dt = aest_dat,
    aeen_dt = aeen_dat
  ))
}
