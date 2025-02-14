Raw_STUDY <- function(data, previous_data, spec, ...) {
  inps <- list(...)

  curr_spec <- spec$Raw_STUDY

  if ("Raw_STUDY" %in% names(previous_data)) {
    dataset <- previous_data$Raw_STUDY
  } else {
    dataset <- NULL
  }

  args <- list(
    studyid = list(1, inps$StudyID),
    num_plan_site = list(inps$SiteCount),
    num_plan_subj = list(inps$ParticipantCount),
    act_fpfv = list(inps$MinDate, inps$MaxDate, dataset$act_fpfv),
    est_fpfv = list(inps$MinDate, inps$MaxDate, dataset$est_fpfv),
    est_lpfv = list(inps$GlobalMaxDate -30, inps$GlobalMaxDate, dataset$est_lpfv),
    est_lplv = list(inps$GlobalMaxDate + 30, inps$GlobalMaxDate + 120, dataset$est_lplv),
    phase = list(1, external_phase = c("P1", "P2", "P3", "P4")),
    default = list(1)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_STUDY, ...)
  return(res)
}
