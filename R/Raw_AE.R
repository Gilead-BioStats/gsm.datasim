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
    curr_spec$aeser <- NULL
    curr_spec$aest_dt_aeen_dt <- list(required = TRUE)
    curr_spec$mdrpt_nsv <- NULL
    curr_spec$mdrsoc_nsv <- NULL
    curr_spec$aetoxgr <- NULL
  }

  args <- list(
    subjid = list(n, external_subjid = data$Raw_SUBJ$subjid),
    aest_dt_aeen_dt = list(n, startDate, endDate),
    default = list(n)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_AE, ...)

  return(res)
}
