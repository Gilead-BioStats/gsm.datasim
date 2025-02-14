
Raw_SITE <- function(data, previous_data, spec, ...) {

  inps <- list(...)

  curr_spec <- spec$Raw_SITE

  if ("Raw_SITE" %in% names(previous_data)) {
    dataset <- previous_data$Raw_SITE
    previous_row_num <- nrow(dataset)
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n_sites - previous_row_num
  if (n == 0) return(dataset)


  if (all(c("Country", "State", "City") %in% names(curr_spec))) {
    curr_spec$Country_State_City <- list(required = TRUE)
    curr_spec$Country <- NULL
    curr_spec$State <- NULL
    curr_spec$City <- NULL

  }

  if (!("siteid" %in% names(curr_spec))) {
    curr_spec$siteid <- list(required = TRUE)
  }


  # Function body for Raw_SITE
  args <- list(
    studyid = list(n, data$Raw_STUDY$protocol_number[[1]]),
    default = list(n)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_SITE, ...)


  return(res)
}
