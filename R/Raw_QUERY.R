Raw_QUERY <- function(data, previous_data, spec, ...) {
  inps <- list(...)

  curr_spec <- spec$Raw_QUERY

  if ("Raw_QUERY" %in% names(previous_data)) {
    dataset <- previous_data$Raw_QUERY
    previous_row_num <- length(unique(dataset$subjid))
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n - previous_row_num
  if (n == 0) return(dataset)

  # Function body for Raw_QUERY
  if (!("visnam" %in% names(curr_spec))) {
    curr_spec$visnam <- list(required = TRUE)
  }

  entries_per_subj_visit <- 2

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

  all_n <- nrow(subject_nsv_visits) * entries_per_subj_visit

  args <- list(
    subject_nsv_visit_repeated = list(entries_per_subj_visit, subject_nsv_visits),
    default = list(all_n)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_QUERY, ...)

  return(res)

}
