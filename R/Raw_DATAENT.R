Raw_DATAENT <- function(data, previous_data, spec, ...) {

  inps <- list(...)

  curr_spec <- spec$Raw_DATAENT

  if ("Raw_DATAENT" %in% names(previous_data)) {
    dataset <- previous_data$Raw_DATAENT
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

  form <- paste0("form", 1:8)
  forms <- data.frame(
    form = form
  )

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


  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_DATAENT, ...)

  return(res)
}

data_entry_lag <- function(n, ...) {
  # Function body for data_entry_lag
  sample(0:20,
         prob = c(0.25, 0.18, 0.14, 0.10, 0.07, 0.05, 0.05, 0.04, 0.03, 0.02, 0.02,
                  rep(0.005, 10)),
         n,
         replace = TRUE)
}
