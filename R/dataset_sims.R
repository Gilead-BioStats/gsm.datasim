combination_var_splitter <- function(variable_data, split_vars) {
  for (split_var_name in split_vars) {
    # Step 1: Find the index of the sublist in the main list
    sublist_index <- which(names(variable_data) == split_var_name)

    # Step 2: Extract the elements of the sublist
    sublist_elements <- variable_data[[sublist_index]]

    # Step 3: Remove the sublist from the main list
    variable_data[[sublist_index]] <- NULL

    # Step 4: Insert the sublist elements into the main list at the original position
    variable_data <- append(variable_data, sublist_elements, after = sublist_index - 1)
  }

  return(variable_data)
}


add_new_var_data <- function(dataset, vars, args, orig_curr_spec, ...) {
  internal_args <- list(...)

  variable_data <- lapply(names(vars), function(var_name) {
    generator_func <- var_name
    if (!(var_name %in% names(args))) {
      curr_args <- args$default
    } else {
      curr_args <- args[[var_name]]
      if (!(var_name %in% names(dataset))) {
        curr_args[[var_name]] <- NULL
      } else {
        curr_args[[var_name]] <- dataset[[var_name]]
      }
    }

    # Generate data using the generator function
    do.call(generator_func, curr_args)
  })


  names(variable_data) <- names(vars)
  if ("split_vars" %in% names(internal_args)) {
    variable_data <- combination_var_splitter(variable_data, internal_args$split_vars)
  }

  variable_data <- variable_data %>%
    as.data.frame() %>%
    rename_raw_data_vars_per_spec(orig_curr_spec)


  if (!is.null(dataset)) {
    return(dplyr::bind_rows(dataset, variable_data))
  } else {
    return(variable_data)
  }
}

Raw_STUDY <- function(data, previous_data, spec, ...) {
  inps <- list(...)

  curr_spec <- spec$Raw_STUDY

  if ("Raw_STUDY" %in% names(previous_data)) {
    dataset <- previous_data$Raw_STUDY
  } else {
    dataset <- NULL
  }

  if (!("phase" %in% names(curr_spec))) {
    curr_spec$phase <- list(required = TRUE)
  }

  args <- list(
               studyid = list(1, inps$StudyID),
               num_plan_site = list(inps$SiteCount),
               num_plan_subj = list(inps$ParticipantCount),
               act_fpfv = list(inps$MinDate, inps$MaxDate, dataset$act_fpfv),
               est_fpfv = list(inps$MinDate, inps$GlobalMaxDate, dataset$est_fpfv),
               default = list(1)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_STUDY, ...)
  return(res)
}



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



Raw_SUBJ <- function(data, previous_data, spec, startDate, endDate, ...) {

  inps <- list(...)

  curr_spec <- spec$Raw_SUBJ

  if ("Raw_SUBJ" %in% names(previous_data)) {
    dataset <- previous_data$Raw_SUBJ
    previous_row_num <- nrow(dataset)
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n_subj - previous_row_num
  if (n == 0) return(dataset)

  if (!("siteid" %in% names(curr_spec))) {
    curr_spec$siteid <- list(required = TRUE)
  }

  if (!("enrolldt" %in% names(curr_spec))) {
    curr_spec$enrolldt <- list(required = TRUE)
  }

  if (all(c("siteid", "invid", "country") %in% names(curr_spec))) {
    curr_spec$subject_site_synq <- list(required = TRUE)
    curr_spec$siteid <- NULL
    curr_spec$invid <- NULL
    curr_spec$country <- NULL

  }

  if (all(c("subjid", "subject_nsv") %in% names(curr_spec))) {
    curr_spec$subjid_subject_nsv <- list(required = TRUE)
    curr_spec$subjid <- NULL
    curr_spec$subject_nsv <- NULL
  }

  if (all(c("enrollyn", "enrolldt", "timeonstudy") %in% names(curr_spec))) {
    curr_spec$enrollyn_enrolldt_timeonstudy <- list(required = TRUE)
    curr_spec$enrolldt <- NULL
    curr_spec$timeonstudy <- NULL
    curr_spec$enrollyn <- NULL

  }

  args <- list(
    studyid = list(n, data$Raw_STUDY$protocol_number[[1]]),
    subjid_subject_nsv = list(n, previous_data$Raw_SUBJ$subjid),
    subject_site_synq = list(n, data$Raw_SITE),
    enrollyn_enrolldt_timeonstudy = list(n, startDate, endDate),
    default = list(n)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_SUBJ, ...)

  # Recalculate for all data
  res$timeonstudy <- timeonstudy(n, res$enrolldt, endDate)

  return(res)
}

Raw_ENROLL <- function(data, previous_data, spec, ...) {
  inps <- list(...)

  curr_spec <- spec$Raw_ENROLL

  if ("Raw_ENROLL" %in% names(previous_data)) {
    dataset <- previous_data$Raw_ENROLL
    previous_row_num <- nrow(dataset)
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n_enroll - previous_row_num
  if (n == 0) return(dataset)

  if (all(c("invid", "country", "subjid", "subjectid", "enrollyn") %in% names(curr_spec))) {
    curr_spec$subject_to_enrollment <- list(required = TRUE)
    curr_spec$invid <- NULL
    curr_spec$country <- NULL
    curr_spec$subjid <- NULL
    curr_spec$subjectid <- NULL
    curr_spec$enrollyn <- NULL
  }

  args <- list(
    studyid = list(n, data$Raw_STUDY$protocol_number[[1]]),
    subject_to_enrollment = list(n, data, previous_data$Raw_ENROLL$subjid),
    default = list(n)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_ENROLL, ...)

  return(res)
}



Raw_AE <- function(data, previous_data, spec, ...) {
  inps <- list(...)

  curr_spec <- spec$Raw_AE

  if ("Raw_AE" %in% names(previous_data)) {
    dataset <- previous_data$Raw_AE
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

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_AE, ...)

  return(res)
}


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

Raw_SV <- function(data, previous_data, spec, startDate, ...) {
  inps <- list(...)
  if ("Raw_SV" %in% names(previous_data)) {
    dataset <- previous_data$Raw_SV
    previous_row_num <- length(unique(dataset$subjid))
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n - previous_row_num
  if (n == 0) return(dataset)

  possible_visits <- data.frame(
    foldername = c("Screening", "Unscheduled", "Unscheduled", "Unscheduled", "Unscheduled",
                   "Unscheduled", "Day 1", "Week 4", "Week 8", "Week 12", "Week 16", "Week 20",
                   "Week 24", "Week 28", "Week 36", "Week 44", "Week 32", "Week 40", "Week 48",
                   "Week 56", "Week 64", "Week 72", "Week 80", "Week 88", "Week 96", "Week 108",
                   "Week 120", "Week 132", "Unscheduled", "Unscheduled", "Early Study Drug Discontinuation",
                   "Unscheduled", "Unscheduled", "Unscheduled", "Follow-up Week 12", "Follow-up Week 24",
                   "Follow-up Week 4", "Follow-up Week 8", "Follow-up Week 16", "Follow-up Week 20",
                   "Week 144", "Unscheduled", "Unscheduled", "Unscheduled", "Unscheduled", "Unscheduled",
                   "Unscheduled"),
    instancename = c("Screening", "Unscheduled (5)", "Unscheduled (4)", "Unscheduled (3)", "Unscheduled (1)",
                     "Unscheduled (2)", "Day 1 (1)", "Week 4 (1)", "Week 8 (1)", "Week 12 (1)", "Week 16 (1)",
                     "Week 20 (1)", "Week 24 (1)", "Week 28 (1)", "Week 36 (1)", "Week 44 (1)", "Week 32 (1)",
                     "Week 40 (1)", "Week 48 (1)", "Week 56 (1)", "Week 64 (1)", "Week 72 (1)", "Week 80 (1)",
                     "Week 88 (1)", "Week 96 (1)", "Week 108 (1)", "Week 120 (1)", "Week 132 (1)",
                     "Unscheduled (7)", "Unscheduled (6)", "Early Study Drug Discontinuation (1)",
                     "Unscheduled (9)", "Unscheduled (8)", "Unscheduled (10)", "Follow-up Week 12 (1)",
                     "Follow-up Week 24 (1)", "Follow-up Week 4 (1)", "Follow-up Week 8 (1)",
                     "Follow-up Week 16 (1)", "Follow-up Week 20 (1)", "Week 144 (1)", "Unscheduled (11)",
                     "Unscheduled (15)", "Unscheduled (14)", "Unscheduled (13)", "Unscheduled (12)",
                     "Unscheduled (16)")
  )

  curr_spec <- spec$Raw_SV


  if (!("subjid" %in% names(curr_spec))) {
    curr_spec$subjid <- list(required = TRUE)
  }

  if (!("foldername" %in% names(curr_spec))) {
    curr_spec$foldername <- list(required = TRUE)
  }

  if (!("instancename" %in% names(curr_spec))) {
    curr_spec$instancename <- list(required = TRUE)
  }

  if (!("visit_dt" %in% names(curr_spec))) {
    curr_spec$visit_dt <- list(required = TRUE)
  }

  if (all(c("subjid") %in% names(curr_spec))) {
    curr_spec$subjid_repeated <- list(required = TRUE)
    curr_spec$subjid <- NULL
  }


  subjs <- subjid(n, external_subjid = data$Raw_SUBJ$subjid, replace = FALSE)
  args <- list(
    subjid_repeated = list(nrow(possible_visits), subjs),
    visit_dt = list(n, subjs, startDate, possible_visits),
    default = list(n, subjs, possible_visits)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_SV, ...)

  return(res)
}


Raw_LB <- function(data, previous_data, spec, ...) {
  # Function body for Raw_LB
  inps <- list(...)

  curr_spec <- spec$Raw_LB


  if ("Raw_LB" %in% names(previous_data)) {
    dataset <- previous_data$Raw_LB
    previous_row_num <- length(unique(dataset$subjid))
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }


  n <- inps$n - previous_row_num
  if (n == 0) return(dataset)

  tests <- data.frame(
    battrnam = c("CHEMISTRY PANEL", "CHEMISTRY PANEL", "CHEMISTRY PANEL", "CHEMISTRY PANEL",
                 "HEMATOLOGY&DIFFERENTIAL PANEL", "HEMATOLOGY&DIFFERENTIAL PANEL",
                 "CHEMISTRY PANEL", "CHEMISTRY PANEL", "CHEMISTRY PANEL", "CHEMISTRY PANEL",
                 "CHEMISTRY PANEL", "HEMATOLOGY&DIFFERENTIAL PANEL", "HEMATOLOGY&DIFFERENTIAL PANEL",
                 "CHEMISTRY PANEL", "CHEMISTRY PANEL", "CHEMISTRY PANEL",
                 "HEMATOLOGY&DIFFERENTIAL PANEL", "HEMATOLOGY&DIFFERENTIAL PANEL", "CHEMISTRY PANEL",
                 "CHEMISTRY PANEL", "HEMATOLOGY&DIFFERENTIAL PANEL", "HEMATOLOGY&DIFFERENTIAL PANEL",
                 "HEMATOLOGY&DIFFERENTIAL PANEL", "HEMATOLOGY&DIFFERENTIAL PANEL",
                 "HEMATOLOGY&DIFFERENTIAL PANEL", "CHEMISTRY PANEL", "HEMATOLOGY&DIFFERENTIAL PANEL",
                 "HEMATOLOGY&DIFFERENTIAL PANEL", "HEMATOLOGY&DIFFERENTIAL PANEL",
                 "HEMATOLOGY&DIFFERENTIAL PANEL", "CHEMISTRY PANEL", "HEMATOLOGY&DIFFERENTIAL PANEL",
                 "HEMATOLOGY&DIFFERENTIAL PANEL", "CHEMISTRY PANEL", "CHEMISTRY PANEL",
                 "CHEMISTRY PANEL", "CHEMISTRY PANEL", "CHEMISTRY PANEL", "CHEMISTRY PANEL",
                 "CHEMISTRY PANEL", "CHEMISTRY PANEL", "CHEMISTRY PANEL",
                 "HEMATOLOGY&DIFFERENTIAL PANEL", "CHEMISTRY PANEL", "CHEMISTRY PANEL"),
    lbtstnam = c("ALT (SGPT)", "AST (SGOT)", "Albumin-QT", "Alkaline Phosphatase", "Basophils",
                 "Basophils (%)", "Calcium (EDTA)", "Calcium Corrected for Albumin",
                 "Cholesterol (High Performance)", "Creatine Kinase", "Direct Bilirubin",
                 "Eosinophils", "Eosinophils (%)", "GGT", "Globulin-QT", "Glucose (2dp SI)",
                 "Hematocrit", "Hemoglobin", "Indirect Bili", "LDH", "Lymphocytes",
                 "Lymphocytes (%)", "MCH", "MCHC", "MCV", "Magnesium-PS", "Monocytes",
                 "Monocytes (%)", "Neutrophils", "Neutrophils (%)", "Phosphorus", "Platelets",
                 "RBC", "Serum Bicarbonate", "Serum Chloride", "Serum Potassium", "Serum Sodium",
                 "Serum Uric Acid", "Total Bilirubin", "Total Protein", "Triglycerides (GPO)",
                 "Urea Nitrogen", "WBC", "Creatinine(Rate Blanked)-2dp", "CHM.CCA.00.00")
  )


  if (!("battrnam" %in% names(curr_spec))) {
    curr_spec$battrnam <- list(required = TRUE)
  }

  if (!("lbtstnam" %in% names(curr_spec))) {
    curr_spec$lbtstnam <- list(required = TRUE)
  }

  if (!("visnam" %in% names(curr_spec))) {
    curr_spec$visnam <- list(required = TRUE)
  }

  if (all(c("subjid", "visnam") %in% names(curr_spec))) {
    curr_spec$subj_visit_repeated <- list(required = TRUE)
    curr_spec$subjid <- NULL
    curr_spec$visnam <- NULL
  }

  subjs <- subjid(n, external_subjid = data$Raw_SUBJ$subjid, replace = FALSE)
  subj_visits <- data$Raw_SV %>%
    dplyr::filter(subjid %in% subjs) %>%
    dplyr::select(subjid, instancename)

  all_n <- nrow(subj_visits) * nrow(tests)

  args <- list(
    subj_visit_repeated = list(nrow(tests), subj_visits),
    default = list(all_n, subj_visits, tests)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_LB, ...)

  return(res)
}

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
    phase = list(n, external_phase = data$Raw_STUDY$phase[1], replace = TRUE),
    default = list(n)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_SDRGCOMP, ...)

  return(res)
}

Raw_STUDCOMP <- function(data, previous_data, spec, ...) {
  # Function body for Raw_SDRGCOMP
  inps <- list(...)

  curr_spec <- spec$Raw_STUDCOMP

  if ("Raw_STUDCOMP" %in% names(previous_data)) {
    dataset <- previous_data$Raw_STUDCOMP
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

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_STUDCOMP, ...)

  return(res)
}

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

Mapped_SUBJ <- function(data, previous_data, spec, ...) {
  # Function body for Mapped_SUBJ
}

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
