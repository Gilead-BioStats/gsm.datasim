Raw_DATACHG <- function(data, previous_data, spec, ...) {
  # Function body for Raw_DATACHG
  # Function body for Raw_SDRGCOMP
  inps <- list(...)

  curr_spec <- spec$Raw_DATACHG

  if ("Raw_DATACHG" %in% names(previous_data)) {
    dataset <- previous_data$Raw_DATACHG
    previous_row_num <- length(unique(dataset$subject_nsv))
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n - previous_row_num
  if (n == 0) return(dataset)

  if (!("visnam" %in% names(curr_spec))) {
    curr_spec$visnam <- list(required = TRUE)
  }

  if (!("form" %in% names(curr_spec))) {
    curr_spec$form <- list(required = TRUE)
  }

  if (!("field" %in% names(curr_spec))) {
    curr_spec$field <- list(required = TRUE)
  }

  forms <- generate_form_df(32)

  if (all(c("subject_nsv", "visnam") %in% names(curr_spec))) {
    curr_spec$subject_nsv_visit_repeated <- list(required = TRUE)
    curr_spec$subject_nsv <- NULL
    curr_spec$visnam <- NULL
  }


  subject_nsvs <- subject_nsv(n, data$Raw_SUBJ$subjid,
                              subject_nsv = data$Raw_SUBJ$subject_nsv, replace = FALSE)
  subject_nsv_visits <- data$Raw_SV %>%
    dplyr::left_join((data$Raw_SUBJ %>% dplyr::select(subjid, subject_nsv)), by =  dplyr::join_by(subjid)) %>%
    dplyr::filter(subject_nsv %in% subject_nsvs) %>%
    dplyr::select(subject_nsv, instancename)

  all_n <- nrow(subject_nsv_visits) * nrow(forms)

  args <- list(
    subject_nsv_visit_repeated = list(nrow(forms), subject_nsv_visits),
    default = list(all_n, subject_nsv_visits, forms)
  )


  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_DATACHG, ...)

  return(res)
}

subject_nsv_visit_repeated <- function(n, data, ...) {
  res <- repeat_rows(n, data)
  return(list(
    subject_nsv = res$subject_nsv,
    visnam = res$instancename
  ))
}

form <- function(n, subject_nsv_visits, forms, ...) {
  rep(forms$form, nrow(subject_nsv_visits))

}

field <- function(n, subject_nsv_visits, forms, ...) {
  rep(forms$field, nrow(subject_nsv_visits))

}

n_changes <- function(n, ...) {

  # Function body for n_changes
  sample(0:6,
         prob = c(0.74, 0.22, 0.03, 0.005, 0.003, 0.0019, 0.0001),
         n,
         replace = TRUE)
}
