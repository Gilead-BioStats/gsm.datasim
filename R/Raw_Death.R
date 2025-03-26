Raw_Death <- function(data, previous_data, spec, ...) {
  inps <- list(...)

  curr_spec <- spec$Raw_Death

  if ("Raw_Death" %in% names(previous_data)) {
    dataset <- previous_data$Raw_Death
    previous_row_num <- nrow(dataset)
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n - previous_row_num
  if (n == 0) return(dataset)

  if (all(c("death_dt") %in% names(curr_spec))) {
    curr_spec$death_dt <- list(required = TRUE)
  }

  args <- list(
    subjid = list(n, external_subjid = data$Raw_SUBJ$subjid),
    default = list(n)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_Death, ...)

  return(res)
}

death_dt <- function(n, ...) {
  as.Date(sample(c(NA, Sys.Date()), n, replace = TRUE, prob = c(0.9, 0.1)))
}
