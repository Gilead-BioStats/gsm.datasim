Raw_SDRGCOMP <- function(data, previous_data, spec, ...) {
  # Function body for Raw_SDRGCOMP
  inps <- list(...)

  curr_spec <- spec$Raw_SDRGCOMP

  if ("Raw_SDRGCOMP" %in% names(previous_data)) {
    dataset <- previous_data$Raw_SDRGCOMP
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

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_SDRGCOMP, ...)

  return(res)
}
