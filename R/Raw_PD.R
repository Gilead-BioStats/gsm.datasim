Raw_PD <- function(data, previous_data, spec, ...) {
  inps <- list(...)

  curr_spec <- spec$Raw_PD

  if ("Raw_PD" %in% names(previous_data)) {
    dataset <- previous_data$Raw_PD
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

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_PD, ...)

  return(res)
}

deemedimportant <- function(n, ...) {
  # Function body for deemedimportant
  sample(c("Yes", "No"), n, replace = TRUE)
}
