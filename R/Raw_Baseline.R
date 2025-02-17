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

  if (all(c("baseline_scan_date") %in% names(curr_spec))) {
    curr_spec$baseline_scan_date <- list(required = TRUE)
  }

  args <- list(
    subjid = list(n, external_subjid = data$Raw_SUBJ$subjid),
    baseline_scan_date = list(n, startDate),
    default = list(n)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_Baseline, ...)

  return(res)
}

baseline_scan_date <- function(n, startDate, ...) {
  as.Date(startDate + sample(0:2, n, replace = TRUE))
}
