Raw_AntiCancer <- function(data, previous_data, spec, startDate, ...) {
  inps <- list(...)

  curr_spec <- spec$Raw_AntiCancer

  if ("Raw_AntiCancer" %in% names(previous_data)) {
    dataset <- previous_data$Raw_AntiCancer
    previous_row_num <- nrow(dataset)
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n - previous_row_num
  if (n == 0) return(dataset)

  if (all(c("act_treatment", "act_date") %in% names(curr_spec))) {
    curr_spec$act_treatment <- list(required = TRUE)
    curr_spec$act_date <- list(required = TRUE)
  }

  args <- list(
    subjid = list(n, external_subjid = data$Raw_SUBJ$subjid),
    act_date = list(n, startDate),
    default = list(n)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_AntiCancer, ...)

  return(res)
}

act_treatment <- function(n, ...) {
  sample(c("RADIOTHERAPY", "OTHER CHEMOTHERAPY"), n, replace = TRUE)
}

act_date <- function(n, startDate, ...) {
  as.Date(startDate + sample(-5:5, n, replace = TRUE))
}
